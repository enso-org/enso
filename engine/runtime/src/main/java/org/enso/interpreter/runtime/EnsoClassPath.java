package org.enso.interpreter.runtime;

import java.lang.module.Configuration;
import java.lang.module.ModuleFinder;
import java.nio.file.Path;
import java.util.Collections;

/** Representation of an Enso library class path. */
public final class EnsoClassPath {
  private static final EnsoClassPath EMPTY = new EnsoClassPath(null, null, null);
  private final ModuleLayer.Controller cntrl;
  private final ModuleLayer layer;
  final ClassLoader loader;

  private EnsoClassPath(ModuleLayer.Controller cntrl, ModuleLayer layer, ClassLoader loader) {
    this.cntrl = cntrl;
    this.layer = layer;
    this.loader = loader;
  }

  static EnsoClassPath create(Path file) {
    java.lang.module.ModuleFinder finder = ModuleFinder.of(file);
    java.util.List<java.lang.String> moduleNames =
        finder.findAll().stream().map(mod -> mod.descriptor().name()).toList();
    if (moduleNames.isEmpty()) {
      return EMPTY;
    } else {
      java.lang.ModuleLayer parent = ModuleLayer.boot();
      java.util.List<java.lang.module.Configuration> parentCfgs =
          Collections.singletonList(parent.configuration());
      java.util.List<java.lang.ModuleLayer> parentModules = Collections.singletonList(parent);
      java.lang.ClassLoader parentLoader = parent.findLoader("java.base");
      java.lang.module.Configuration cfg =
          Configuration.resolve(finder, parentCfgs, finder, moduleNames);
      java.lang.ModuleLayer.Controller cntrl =
          ModuleLayer.defineModulesWithOneLoader(cfg, parentModules, parentLoader);
      java.lang.ModuleLayer layer = cntrl.layer();
      java.lang.ClassLoader loader = layer.findLoader(moduleNames.get(0));
      return new EnsoClassPath(cntrl, layer, loader);
    }
  }
}
