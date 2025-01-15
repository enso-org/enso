package org.enso.compiler.phase;

import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.IdMap;
import scala.Some$;

public final class IdMapProcessor {

  private IdMapProcessor() {}

  /**
   * Applies the map with external ids to the module ir.
   *
   * @param ir the module ir.
   * @param idMap the provided IdMap.
   */
  public static Module updateIr(Module ir, IdMap idMap) {
    if (idMap == null) {
      return null;
    }

    var values = idMap.values();

    return ir.mapExpressions(expression -> setLocation(values, expression));
  }

  private static Expression setLocation(Map<Location, UUID> values, Expression expression) {
    var identifiedLocationOption = expression.location();
    if (identifiedLocationOption.isDefined()) {
      var location = identifiedLocationOption.get().location();
      var externalId = values.get(location);
      if (externalId != null) {
        return expression
            .setLocation(Some$.MODULE$.apply(new IdentifiedLocation(location, externalId)))
            .mapExpressions(expression1 -> setLocation(values, expression1));
      }
    }
    return expression.mapExpressions(expression1 -> setLocation(values, expression1));
  }
}
