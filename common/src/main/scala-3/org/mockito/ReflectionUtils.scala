package org.mockito

import org.mockito.JavaReflectionUtils.resolveWithJavaGenerics
import org.mockito.invocation.InvocationOnMock

import java.lang.reflect.Method
import scala.reflect.ClassTag
import scala.util.Try as uTry

object ReflectionUtils {

  /**
   * Get the return type of a method invocation, resolving generics if possible
   */
  private[mockito] def returnType(invocation: InvocationOnMock): Class[?] = {
    val javaReturnType = invocation.method.getReturnType

    // Scala 3 note: We can't use Scala reflection like in Scala 2 (no WeakTypeTag/universe)
    // Fall back to Java reflection for generic resolution
    if javaReturnType == classOf[Object] then resolveWithJavaGenerics(invocation).getOrElse(javaReturnType)
    else javaReturnType
  }

  /**
   * Check if a method returns a value class (extends AnyVal)
   *
   * Scala 3 note: Without runtime Scala reflection, we check if the return type extends AnyVal using Java reflection
   */
  private[mockito] def returnsValueClass(invocation: InvocationOnMock): Boolean = {
    val returnType = invocation.method.getReturnType
    // Check if it's a known value class type or extends AnyVal
    // This is a conservative check - may miss some value classes
    uTry {
      classOf[AnyVal].isAssignableFrom(returnType)
    }.getOrElse(false)
  }

  /**
   * Extract extra interfaces from a refined type (e.g., `mock[Foo with Bar]` extracts `Bar` as extra interface).
   */
  inline def extraInterfaces[T: ClassTag]: List[Class[?]] = {
    val allTypes = ReflectionMacro.extraInterfacesImpl[T]
    val primary  = implicitly[ClassTag[T]].runtimeClass
    allTypes.filterNot(_ == primary)
  }

  /**
   * Find methods with lazy (by-name) parameters or varargs. In Scala 3, this reads from the ByNameParamCache which is populated at compile time by the ReflectionMacro inline macro
   * called during mock creation.
   */
  def methodsWithLazyOrVarArgs(classes: Seq[Class[?]]): Seq[(Method, Set[Int])] =
    classes.flatMap { clazz =>
      org.mockito.internal.handler.ByNameParamCache.get(clazz).getOrElse(Seq.empty)
    }
}
