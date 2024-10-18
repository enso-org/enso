package org.enso.base.enso_cloud;

import java.util.ServiceLoader;
import java.util.stream.Collectors;
import org.enso.base.lookup.Lookup;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/**
 * An interface for data link parser providers. A class providing this interface can register an
 * Enso type that defines how to `parse` a specific type of datalink. The `parse` method on that
 * type should return a configured datalink instance that can later be `read`.
 */
public abstract class DataLinkSPI {
  private static final Lookup<DataLinkSPI> loader =
      Lookup.lookup((layer) -> ServiceLoader.load(layer, DataLinkSPI.class));

  public void reload() {
    loader.reload();
  }

  public static Value findDataLinkType(String name) {
    var providers =
        loader.stream().filter(provider -> provider.get().getLinkTypeName().equals(name)).toList();
    if (providers.isEmpty()) {
      return null;
    }

    if (providers.size() > 1) {
      var modules =
          providers.stream()
              .map(provider -> provider.get().getModuleName())
              .collect(Collectors.joining(", "));
      throw new IllegalStateException(
          "Error: Multiple Data Link providers found for type: "
              + name
              + ". The clashing definitions are in the following modules: "
              + modules
              + ".");
    }

    return providers.get(0).get().getTypeObject();
  }

  public Value getTypeObject() {
    return EnsoMeta.getType(getModuleName(), getTypeName());
  }

  protected abstract String getModuleName();

  protected abstract String getTypeName();

  protected abstract String getLinkTypeName();
}
