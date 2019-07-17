package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import org.enso.interpreter.runtime.AtomConstructor;
import org.enso.interpreter.runtime.GlobalCallTarget;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class GlobalScope {

  private final Map<String, GlobalCallTarget> globalNames = new HashMap<>();
  private final Map<String, AtomConstructor> constructors = new HashMap<>();

  public GlobalScope() {
    registerBuiltinConstructors();
  }

  private void registerBuiltinConstructors() {
    registerConstructor(AtomConstructor.UNIT);
    registerConstructor(AtomConstructor.NIL);
    registerConstructor(AtomConstructor.CONS);
  }

  public void registerConstructor(AtomConstructor constructor) {
    constructors.put(constructor.getName(), constructor);
  }

  public void registerName(String name) {
    this.globalNames.put(name, new GlobalCallTarget(null));
  }

  public void updateCallTarget(String name, RootCallTarget rootBinding) {
    GlobalCallTarget globalTarget = this.globalNames.get(name);

    if (globalTarget == null) {
      this.globalNames.put(name, new GlobalCallTarget(rootBinding));
    } else {
      globalTarget.setTarget(rootBinding);
    }
  }

  public Optional<GlobalCallTarget> getGlobalCallTarget(String name) {
    return Optional.ofNullable(this.globalNames.get(name));
  }

  public Optional<AtomConstructor> getConstructor(String name) {
    return Optional.ofNullable(this.constructors.get(name));
  }
}
