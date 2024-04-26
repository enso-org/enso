package org.enso.base.file_format;

import java.util.Objects;
import java.util.ServiceLoader;
import java.util.stream.Collectors;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public abstract class FileFormatSPI {
  private static final ServiceLoader<FileFormatSPI> loader =
      ServiceLoader.load(FileFormatSPI.class, FileFormatSPI.class.getClassLoader());

  public static Value[] get_types(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.stream().map(provider -> provider.get().getTypeObject()).toArray(Value[]::new);
  }

  public static Value findFormatForDataLinkSubType(String subType) {
    Objects.requireNonNull(subType, "subType must not be null/Nothing.");

    var providers =
        loader.stream()
            .filter(provider -> subType.equalsIgnoreCase(provider.get().getDataLinkFormatName()))
            .toList();
    if (providers.isEmpty()) {
      return null;
    }

    if (providers.size() > 1) {
      var modules =
          providers.stream()
              .map(provider -> provider.get().getModuleName())
              .collect(Collectors.joining(", "));
      throw new IllegalStateException(
          "Error: Multiple Format providers found for format: "
              + subType
              + ". The clashing definitions are in the following modules: "
              + modules
              + ".");
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
