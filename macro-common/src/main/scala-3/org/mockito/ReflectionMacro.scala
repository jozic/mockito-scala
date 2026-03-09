package org.mockito

import org.mockito.internal.handler.ByNameParamCache
import scala.quoted.*
import scala.reflect.ClassTag

/**
 * Scala 3 macro that gathers by-name and varargs parameter info at compile time and registers it in the runtime cache for ScalaMockHandler.
 */
object ReflectionMacro {

  inline def registerByNameAndVarArgInfo[T](using classTag: ClassTag[T]): Unit = ${ registerImpl[T]('classTag) }

  def registerImpl[T: Type](classTagExpr: Expr[ClassTag[T]])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[T].dealias
    val typeSymbol = tpe.typeSymbol

    case class MethodInfo(name: String, jvmParamTypes: List[Expr[Class[?]]], byNameOrVarArgIndices: Set[Int], returnsValueClass: Boolean)

    // Use memberType to get the method type with proper type parameter resolution
    // Include inherited methods by walking base classes
    val allMethods = {
      val seen = scala.collection.mutable.Set.empty[String] // track by fullName to avoid duplicates
      (typeSymbol.declarations ++ tpe.baseClasses.flatMap(_.declarations))
        .filter(s => s.isDefDef && !s.isClassConstructor && seen.add(s.fullName))
    }

    val methodInfos = allMethods.flatMap { methodSym =>
      val methodType   = tpe.memberType(methodSym)
      val isJavaMethod = methodSym.flags.is(Flags.JavaDefined)

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
      val returnsValueClass = {
        def resultType(tpe: TypeRepr): TypeRepr =
          tpe match {
            case MethodType(_, _, rt) => resultType(rt)
            case PolyType(_, _, rt)   => resultType(rt)
            case rt                   => rt
          }

        val rt: TypeRepr = methodSym.tree match {
          case dd: DefDef => dd.returnTpt.tpe
          case _          => resultType(methodType)
        }
        val normalized = rt.dealias.simplified
        val returnSym  = normalized.typeSymbol
        returnSym.isClassDef && returnSym != defn.AnyValClass && (normalized <:< TypeRepr.of[AnyVal])
      }

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
      Some(MethodInfo(methodSym.name, jvmParamTypes, byNameOrVarArgIndices, returnsValueClass))
    }

    if methodInfos.isEmpty then '{ () }
    else {
      val classExpr = '{ $classTagExpr.runtimeClass }

      val registrations = methodInfos.map { info =>
        val nameExpr              = Expr(info.name)
        val paramTypesExpr        = Expr.ofList(info.jvmParamTypes)
        val indicesExpr           = Expr(info.byNameOrVarArgIndices)
        val returnsValueClassExpr = Expr(info.returnsValueClass)
        '{ ($nameExpr, $paramTypesExpr, $indicesExpr, $returnsValueClassExpr) }
      }
      val registrationsExpr = Expr.ofList(registrations)

      '{
        val clazz = $classExpr
        if clazz != classOf[Any] then {
          val infos       = $registrationsExpr
          val methodInfos = infos.flatMap { case (name, paramTypes, indices, returnsValueClass) =>
            try {
              val method = clazz.getMethod(name, paramTypes*)
              Some((method, indices, returnsValueClass))
            } catch {
              case _: NoSuchMethodException => None
            }
          }

          val byNameInfos = methodInfos.collect { case (method, indices, _) if indices.nonEmpty => (method, indices) }
          if byNameInfos.nonEmpty && !ByNameParamCache.get(clazz).isDefined then ByNameParamCache.register(clazz, byNameInfos)

          if methodInfos.nonEmpty then
            ByNameParamCache.registerReturnsValueClass(methodInfos.map { case (method, _, returnsValueClass) =>
              (method, returnsValueClass)
            })
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
