package org.enso.interpreter.dsl.builtins;

public record ClassName(String pkg, String name) {
  public String fullyQualifiedName() {
    return pkg + "." + name;
  }

  public String jvmFriendlyFullyQualifiedName() {
    return pkg + "." + jvmFriendlyName();
  }

  public String jvmFriendlyName() {
    return name.replaceAll("=", "Equals").replaceAll("<", "Less").replaceAll(">", "Greater");
  }
}
