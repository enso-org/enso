package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import org.enso.interpreter.runtime.scope.ModuleScope;

import java.io.IOException;

/** Represents a source module with a known location. */
public class Module {
  private ModuleScope cachedScope = null;
  private final TruffleFile file;

  /**
   * Creates a new module.
   *
   * @param file file in which this module sources are located
   */
  public Module(TruffleFile file) {
    this.file = file;
  }

  /**
   * Parses the module sources. The results of this operation are cached.
   *
   * @param context context in which the parsing should take place
   * @return the scope defined by this module
   * @throws IOException when the source file could not be read
   */
  public ModuleScope requestParse(Context context) throws IOException {
    if (cachedScope == null) {
      cachedScope = context.createScope();
      context.parse(file, cachedScope);
    }
    return cachedScope;
  }
}
