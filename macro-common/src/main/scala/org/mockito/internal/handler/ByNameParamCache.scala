package org.mockito.internal.handler

import java.lang.reflect.Method
import java.util.concurrent.ConcurrentHashMap

/**
 * Runtime cache for by-name and varargs parameter indices. Populated at compile time via inline macros in Scala 3, or via runtime reflection in Scala 2.
 */
object ByNameParamCache {
  private val cache = new ConcurrentHashMap[Class[?], Seq[(Method, Set[Int])]]()

  def get(clazz: Class[?]): Option[Seq[(Method, Set[Int])]] =
    Option(cache.get(clazz))

  def register(clazz: Class[?], info: Seq[(Method, Set[Int])]): Unit =
    cache.putIfAbsent(clazz, info)
}
