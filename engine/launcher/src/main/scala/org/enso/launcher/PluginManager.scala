package org.enso.launcher

/**
  * Default implementation of the [[internal.PluginManager]] using the default
  * [[Environment]].
  */
object PluginManager extends internal.PluginManager(Environment)
