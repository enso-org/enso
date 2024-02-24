package org.enso.base.enso_cloud;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.ServiceLoader;
import java.util.stream.Collectors;

public abstract class DataLinkSPI {
  private static final ServiceLoader<DataLinkSPI> loader =
      ServiceLoader.load(DataLinkSPI.class, DataLinkSPI.class.getClassLoader());

  public void reload() {
    loader.reload();
  }

  public Value findDataLinkType(String name) {
    var providers = loader.stream().filter(provider -> provider.get().getTypeName().equals(name)).toList();
    if (providers.isEmpty()) {
      return null;
    }

    if (providers.size() > 1) {
      var modules = providers.stream().map(provider -> provider.get().getModuleName()).collect(Collectors.joining(", "));
      throw new IllegalStateException("Error: Multiple Data Link providers found for type: " + name + ". The clashing definitions are in the following modules: " + modules + ".");
    }

    return providers.get(0).get().getTypeObject();
  }

  public Value getTypeObject() {
    final var context = Context.getCurrent().getBindings("enso");
    final var module = context.invokeMember("get_module", getModuleName());
    return module.invokeMember("get_type", getTypeName());
  }

  protected abstract String getModuleName();

  protected abstract String getTypeName();
}
