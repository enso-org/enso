package org.enso.compiler.core.ir.expression.warnings;

import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Warning;
import scala.Function1;

/** Warnings for unreachable code. */
public sealed interface Unreachable extends Warning {
  /**
   * A warning for unreachable branches in a case expression.
   *
   * @param identifiedLocation
   */
  record Branches(IdentifiedLocation identifiedLocation) implements Unreachable {

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Unreachable case branches" + atLocation();
    }

    private String atLocation() {
      String atLocation;
      if (location().isDefined()) {
        atLocation = " at location " + location().get();
      } else {
        atLocation = "";
      }
      return atLocation;
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {atLocation()};
    }
  }
}
