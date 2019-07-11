package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.enso.interpreter.runtime.GlobalCallTarget;

public class GlobalScope {

  private final Map<String, GlobalCallTarget> globalNames = new HashMap<>();

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
}
