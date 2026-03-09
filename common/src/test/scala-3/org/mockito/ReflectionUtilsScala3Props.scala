package org.mockito

import org.mockito.invocation.InvocationOnMock
import org.scalacheck.Prop.*
import org.scalacheck.Properties
import org.mockito.internal.handler.ByNameParamCache

import java.lang.reflect.{ Method, Proxy }
import scala.reflect.ClassTag

object ReflectionUtilsScala3Props {
  final class UserId(val value: String) extends AnyVal
  final class IntId(val value: Int)     extends AnyVal

  trait RefBackedValueClassReturnType {
    def id: UserId
  }

  trait PrimitiveBackedValueClassReturnType {
    def id: IntId
  }

  trait GenericBoundedReturnType {
    def id[T <: AnyVal](value: T): T
  }

  trait TypeMemberBoundedReturnType {
    type Id <: AnyVal
    def id: Id
  }
}

class ReflectionUtilsScala3Props extends Properties("ReflectionUtilsScala3") {
  import ReflectionUtilsScala3Props.*

  private def invocationWithMethod(targetMethod: Method): InvocationOnMock = {
    val handler = new java.lang.reflect.InvocationHandler {
      override def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef =
        if method.getName == "getMethod" then targetMethod
        else null
    }

    Proxy
      .newProxyInstance(getClass.getClassLoader, Array(classOf[InvocationOnMock]), handler)
      .asInstanceOf[InvocationOnMock]
  }

  private def registerMetadata(): Unit = {
    given ClassTag[RefBackedValueClassReturnType]       = ClassTag(classOf[RefBackedValueClassReturnType])
    given ClassTag[PrimitiveBackedValueClassReturnType] = ClassTag(classOf[PrimitiveBackedValueClassReturnType])
    given ClassTag[GenericBoundedReturnType]            = ClassTag(classOf[GenericBoundedReturnType])
    given ClassTag[TypeMemberBoundedReturnType]         = ClassTag(classOf[TypeMemberBoundedReturnType])
    ReflectionMacro.registerByNameAndVarArgInfo[RefBackedValueClassReturnType]
    ReflectionMacro.registerByNameAndVarArgInfo[PrimitiveBackedValueClassReturnType]
    ReflectionMacro.registerByNameAndVarArgInfo[GenericBoundedReturnType]
    ReflectionMacro.registerByNameAndVarArgInfo[TypeMemberBoundedReturnType]
  }

  property("returnsValueClass detects value class return types") = {
    registerMetadata()
    val refBackedMethod       = classOf[RefBackedValueClassReturnType].getMethod("id")
    val primitiveBackedMethod = classOf[PrimitiveBackedValueClassReturnType].getMethod("id")

    all(
      ReflectionUtils.returnsValueClass(invocationWithMethod(refBackedMethod)) ?= true,
      ReflectionUtils.returnsValueClass(invocationWithMethod(primitiveBackedMethod)) ?= true
    )
  }

  property("returnsValueClass differs from plain JVM AnyVal check for primitive-backed value classes") = {
    registerMetadata()
    val method     = classOf[PrimitiveBackedValueClassReturnType].getMethod("id")
    val invocation = invocationWithMethod(method)

    all(
      ReflectionUtils.returnsValueClass(invocation) ?= true,
      classOf[AnyVal].isAssignableFrom(method.getReturnType) ?= false
    )
  }

  property("returnsValueClass differs from plain JVM AnyVal check for generic AnyVal bounds") = {
    registerMetadata()
    val method     = classOf[GenericBoundedReturnType].getMethod("id", classOf[Object])
    val invocation = invocationWithMethod(method)

    all(
      ReflectionUtils.returnsValueClass(invocation) ?= false,
      classOf[AnyVal].isAssignableFrom(method.getReturnType) ?= true
    )
  }

  property("returnsValueClass differs from plain JVM AnyVal check for type members bounded by AnyVal") = {
    registerMetadata()
    val method     = classOf[TypeMemberBoundedReturnType].getMethod("id")
    val invocation = invocationWithMethod(method)

    all(
      ByNameParamCache.getReturnsValueClass(method).isDefined ?= true,
      ByNameParamCache.getReturnsValueClass(method).contains(false) ?= true,
      ReflectionUtils.returnsValueClass(invocation) ?= false,
      classOf[AnyVal].isAssignableFrom(method.getReturnType) ?= true
    )
  }
}
