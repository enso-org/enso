package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleLogger;
import java.lang.module.Configuration;
import java.lang.module.FindException;
import java.lang.module.ModuleFinder;
import java.lang.module.ResolutionException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.logging.Level;

/** Representation of an Enso library class path. */
public final class EnsoClassPath {
  static final EnsoClassPath EMPTY = new EnsoClassPath("empty", null, null, null);
  private final Object id;
  private final ModuleLayer.Controller cntrl;
  private final ModuleLayer layer;
  final ClassLoader loader;

  private EnsoClassPath(
      Object id, ModuleLayer.Controller cntrl, ModuleLayer layer, ClassLoader loader) {
    this.id = id;
    if (cntrl != null) {
      // cannot be null
      layer.getClass();
    }
    this.cntrl = cntrl;
    this.layer = layer;
    this.loader = loader;
  }

  private static ModuleLayer bootLayer() {
    var myLayer = EnsoClassPath.class.getModule().getLayer();
    if (myLayer != null) {
      return myLayer;
    } else {
      return ModuleLayer.boot();
    }
  }

  static EnsoClassPath create(Path file, List<EnsoClassPath> parents, TruffleLogger log) {
    try {
      return createImpl(file, parents);
    } catch (FindException | ResolutionException e) {
      var sb = new StringBuilder();
      sb.append("Cannot instantiate modules at ").append(file);
      log.log(Level.FINE, sb.toString(), e);
      for (var cp : parents) {
        sb.append("\n  with parent ").append(cp);
      }
      e.setStackTrace(
          Arrays.asList(e.getStackTrace()).stream().limit(10).toArray(StackTraceElement[]::new));
      log.log(Level.SEVERE, sb.toString(), e);
      return EnsoClassPath.EMPTY;
    }
  }

  private static EnsoClassPath createImpl(Path file, List<EnsoClassPath> parents) {
    var locations = new ArrayList<Path>();
    var moduleNames = new ArrayList<String>();
    MODULE_LOOP:
    for (var mod : ModuleFinder.of(file).findAll()) {
      if (bootLayer().findModule(mod.descriptor().name()).isPresent()) {
        continue;
      }
      for (var p : parents) {
        if (p.layer != null && p.layer.findModule(mod.descriptor().name()).isPresent()) {
          continue MODULE_LOOP;
        }
      }
      moduleNames.add(mod.descriptor().name());
      var uri = mod.location().get();
      locations.add(Path.of(uri));
    }
    if (moduleNames.isEmpty() && parents.isEmpty()) {
      return EMPTY;
    } else {
      var finder = ModuleFinder.of(locations.toArray(new Path[0]));
      ModuleLayer.Controller cntrl;
      if (parents.isEmpty()) {
        var parent = bootLayer();
        var parentLoader = parent.findLoader("java.base");
        var parentCfgs = Collections.singletonList(parent.configuration());
        var parentModules = Collections.singletonList(parent);
        var cfg =
            Configuration.resolveAndBind(finder, parentCfgs, ModuleFinder.ofSystem(), moduleNames);
        cntrl = ModuleLayer.defineModulesWithOneLoader(cfg, parentModules, parentLoader);
      } else {
        var parentCfgs = new ArrayList<Configuration>();
        var parentLayers = new ArrayList<ModuleLayer>();
        for (var cp : parents) {
          if (cp.layer == null) {
            continue;
          }
          parentLayers.add(cp.layer);
          parentCfgs.add(cp.layer.configuration());
        }
        if (parentLayers.isEmpty()) {
          parentLayers.add(ModuleLayer.boot());
          parentCfgs.add(ModuleLayer.boot().configuration());
        }
        var parentLoader = bootLayer().findLoader("java.base");
        var cfg =
            Configuration.resolveAndBind(finder, parentCfgs, ModuleFinder.ofSystem(), moduleNames);
        cntrl = ModuleLayer.defineModulesWithOneLoader(cfg, parentLayers, parentLoader);
      }
      var layer = cntrl.layer();
      var loader =
          !moduleNames.isEmpty() ? layer.findLoader(moduleNames.get(0)) : parents.get(0).loader;

      registerLayer(layer);
      return new EnsoClassPath(file, cntrl, layer, loader);
    }
  }

  @SuppressWarnings("unchecked")
  private static void registerLayer(ModuleLayer moduleLayer) {
    var props = System.getProperties();
    Collection<ModuleLayer> layers;
    if (props.get("enso.class.path") instanceof Collection registeredLayers) {
      layers = registeredLayers;
    } else {
      layers = new LinkedHashSet<>();
      props.put("enso.class.path", layers);
    }
    layers.add(moduleLayer);
  }

  @Override
  public String toString() {
    return "EnsoClassPath[id=" + id + "]";
  }
}
