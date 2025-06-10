package org.enso.base.read;

import java.util.List;
import org.enso.base.spi.EnsoService;
import org.enso.base.spi.EnsoServiceLoader;
import org.graalvm.polyglot.Value;

/**
 * An SPI for specifying return types to the `read_many` operation.
 *
 * <p>The `read_many` operation can take return types provided from various libraries. This SPI
 * ensures that it can be aware of all the available types from the loaded libraries. If a library
 * registers a return type here, it will be available for autoscoping resolution and will appear in
 * the dropdown. Registered types must provide methods `get_dropdown_options`, `resolve` and
 * `make_return`. See `Standard.Base.Data.Read.Return_As` for examples.
 */
public abstract class ReadManyReturnSPI extends EnsoService {
  private static final EnsoServiceLoader<ReadManyReturnSPI> loader =
      EnsoServiceLoader.load(ReadManyReturnSPI.class);

  public static List<Value> get_types(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.getTypeObjects();
  }
}
