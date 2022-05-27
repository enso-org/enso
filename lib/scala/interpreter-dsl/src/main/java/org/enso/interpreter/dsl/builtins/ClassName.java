package org.enso.interpreter.dsl.builtins;

public record ClassName(String pkg, String name) {
    public String fullyQualifiedName() {
        return pkg + "." + name;
    }
}
