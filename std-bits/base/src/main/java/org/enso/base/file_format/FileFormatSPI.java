package org.enso.base.file_format;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.ServiceLoader;

public abstract class FileFormatSPI {
  private static final ServiceLoader<FileFormatSPI> loader =
      ServiceLoader.load(FileFormatSPI.class, FileFormatSPI.class.getClassLoader());

  public static Value[] get_types(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.stream().map(provider -> provider.get().getTypeObject()).toArray(Value[]::new);
  }

  public Value getTypeObject() {
    final var context = Context.getCurrent().getBindings("enso");
    final var module = context.invokeMember("get_module", getModuleName());
    return module.invokeMember("get_type", getTypeName());
  }

  protected abstract String getModuleName();

  protected abstract String getTypeName();
}
