package org.enso.base.file_format;

import java.util.ArrayList;
import java.util.Objects;
import java.util.ServiceLoader;
import java.util.stream.Collectors;
import org.enso.base.lookup.Lookup;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public abstract class FileFormatSPI {
  private static final Lookup<FileFormatSPI> loader =
      Lookup.lookup((l) -> ServiceLoader.load(l, FileFormatSPI.class));

  public static Value[] get_types(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.stream().map(provider -> provider.get().getTypeObject()).toArray(Value[]::new);
  }

  public static Value findFormatForDataLinkSubType(String subType) {
    Objects.requireNonNull(subType, "subType must not be null/Nothing.");

    var providers = new ArrayList<FileFormatSPI>();
    for (var s : loader) {
      if (subType.equalsIgnoreCase(s.getDataLinkFormatName())) {
        providers.add(s);
      }
    }
    if (providers.isEmpty()) {
      return null;
    }

    if (providers.size() > 1) {
      var modules =
          providers.stream().map(s -> s.getModuleName()).collect(Collectors.joining(", "));
      throw new IllegalStateException(
          "Error: Multiple Format providers found for format: "
              + subType
              + ". The clashing definitions are in the following modules: "
              + modules
              + ".");
    }

    return providers.get(0).getTypeObject();
  }

  public Value getTypeObject() {
    return EnsoMeta.getType(getModuleName(), getTypeName());
  }

  protected abstract String getModuleName();

  protected abstract String getTypeName();

  /**
   * An optional method that allows this format to be parsed as a selected format in data-links.
   *
   * <p>If a format overrides this method to return a non-null format name (corresponding to the
   * "subType" field in a data-link format entry, see `dataLinkSchema.json` for more details), then
   * the corresponding Enso type should provide a `from` conversion, which will be able to construct
   * a configured format instance from its JSON representation (which should be consistent with the
   * schema).
   */
  protected String getDataLinkFormatName() {
    return null;
  }
}
