package org.mockito

import org.mockito.internal.handler.ByNameParamCache
import scala.quoted.*
import scala.reflect.ClassTag

/**
 * Scala 3 macro that gathers by-name and varargs parameter info at compile time and registers it in the runtime cache for ScalaMockHandler.
 */
object ReflectionMacro {

  inline def registerByNameAndVarArgInfo[T](): Unit = ${ registerImpl[T] }

  def registerImpl[T: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[T].dealias
    val typeSymbol = tpe.typeSymbol

    case class MethodInfo(name: String, jvmParamTypes: List[Expr[Class[?]]], byNameOrVarArgIndices: Set[Int])

    // Use memberType to get the method type with proper type parameter resolution
    // Include inherited methods by walking base classes
    val allMethods = {
      val seen = scala.collection.mutable.Set.empty[String] // track by fullName to avoid duplicates
      (typeSymbol.declarations ++ tpe.baseClasses.flatMap(_.declarations))
        .filter(s => s.isDefDef && !s.isClassConstructor && seen.add(s.fullName))
    }

    val methodInfos = allMethods.flatMap { sym =>
      val methodType   = tpe.memberType(sym)
      val isJavaMethod = sym.flags.is(Flags.JavaDefined)

      def collectParams(tpe: TypeRepr, baseIdx: Int): List[(TypeRepr, Int, Boolean, Boolean)] =
        tpe match {
          case MethodType(names, paramTypes, resultType) =>
            val params = paramTypes.zipWithIndex.map { case (pt, i) =>
              val isByName = pt match {
                case ByNameType(_) => true
                case _             => false
              }
              val isVarArg = pt match {
                case AnnotatedType(_, annot) if annot.tpe.typeSymbol.fullName == "scala.annotation.internal.Repeated" => true
                case t if t.typeSymbol.fullName == "scala.<repeated>"                                                 => true
                case _                                                                                                => false
              }
              (pt, baseIdx + i, isByName, isVarArg)
            }
            params ++ collectParams(resultType, baseIdx + paramTypes.length)
          case PolyType(_, _, resultType) =>
            collectParams(resultType, baseIdx)
          case _ => Nil
        }

      val params                = collectParams(methodType, 0)
      val byNameOrVarArgIndices = params.collect {
        case (_, idx, true, _) => idx
        case (_, idx, _, true) => idx
      }.toSet

      if byNameOrVarArgIndices.isEmpty then None
      else {
        val jvmParamTypes = params.map { case (pt, _, isByName, isVarArg) =>
          if isByName then '{ classOf[scala.Function0[?]] }
          else if isVarArg then {
            if isJavaMethod then {
              // Java varargs use array types at JVM level, not Seq
              // Extract element type from the repeated type
              val elemType = pt match {
                case AnnotatedType(underlying, _) =>
                  underlying match {
                    case AppliedType(_, List(elem)) => elem
                    case _                          => TypeRepr.of[Object]
                  }
                case AppliedType(_, List(elem)) => elem
                case _                          => TypeRepr.of[Object]
              }
              // Use java.lang.reflect.Array to get the array class at runtime
              val elemClassExpr = jvmClassExpr(elemType)
              '{ java.lang.reflect.Array.newInstance($elemClassExpr, 0).getClass }
            } else '{ classOf[scala.collection.immutable.Seq[?]] }
          } else jvmClassExpr(pt)
        }
        Some(MethodInfo(sym.name, jvmParamTypes, byNameOrVarArgIndices))
      }
    }

    if methodInfos.isEmpty then '{ () }
    else {
      val classExpr = Expr.summon[ClassTag[T]] match {
        case Some(ct) => '{ $ct.runtimeClass }
        case None     => '{ classOf[Any] }
      }

      val registrations = methodInfos.map { info =>
        val nameExpr       = Expr(info.name)
        val paramTypesExpr = Expr.ofList(info.jvmParamTypes)
        val indicesExpr    = Expr(info.byNameOrVarArgIndices)
        '{ ($nameExpr, $paramTypesExpr, $indicesExpr) }
      }
      val registrationsExpr = Expr.ofList(registrations)

      '{
        val clazz = $classExpr
        if clazz != classOf[Any] && !ByNameParamCache.get(clazz).isDefined then {
          val infos       = $registrationsExpr
          val methodInfos = infos.flatMap { case (name, paramTypes, indices) =>
            try {
              val method = clazz.getMethod(name, paramTypes*)
              Some((method, indices))
            } catch {
              case _: NoSuchMethodException => None
            }
          }
          if methodInfos.nonEmpty then ByNameParamCache.register(clazz, methodInfos)
        }
      }
    }
  }

  private def jvmClassExpr(using Quotes)(tpe: quotes.reflect.TypeRepr): Expr[Class[?]] = {
    import quotes.reflect.*
    tpe.dealias.simplified.widen match {
      case t if t =:= TypeRepr.of[Boolean] => '{ java.lang.Boolean.TYPE }
      case t if t =:= TypeRepr.of[Byte]    => '{ java.lang.Byte.TYPE }
      case t if t =:= TypeRepr.of[Short]   => '{ java.lang.Short.TYPE }
      case t if t =:= TypeRepr.of[Int]     => '{ java.lang.Integer.TYPE }
      case t if t =:= TypeRepr.of[Long]    => '{ java.lang.Long.TYPE }
      case t if t =:= TypeRepr.of[Float]   => '{ java.lang.Float.TYPE }
      case t if t =:= TypeRepr.of[Double]  => '{ java.lang.Double.TYPE }
      case t if t =:= TypeRepr.of[Char]    => '{ java.lang.Character.TYPE }
      case t if t =:= TypeRepr.of[Unit]    => '{ java.lang.Void.TYPE }
      case t                               =>
        val name     = t.typeSymbol.fullName
        val nameExpr = Expr(name)
        '{
          try Class.forName($nameExpr)
          catch { case _: Exception => classOf[Object] }
        }
    }
  }

  /** Extract extra interfaces from refined types (e.g., `Foo with Bar` → List(classOf[Foo], classOf[Bar])) */
  inline def extraInterfacesImpl[T]: List[Class[?]] = ${ extractExtraInterfaces[T] }

  private def extractExtraInterfaces[T: Type](using Quotes): Expr[List[Class[?]]] = {
    import quotes.reflect.*

    def collectTypes(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
      case AndType(left, right) => collectTypes(left) ++ collectTypes(right)
      case other                => List(other)
    }

    val allTypes = collectTypes(TypeRepr.of[T])

    if allTypes.size <= 1 then '{ List.empty }
    else {
      // Return ALL types from the intersection; the caller will filter out the primary mock type
      val classExprs = allTypes.map { t =>
        val name     = t.typeSymbol.fullName
        val nameExpr = Expr(name)
        '{
          try Some(Class.forName($nameExpr))
          catch { case _: Exception => None }
        }
      }
      val listExpr = Expr.ofList(classExprs)
      '{ $listExpr.flatten }
    }
  }
}
