package org.enso.compiler.core.ir.expression.warnings;

import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Warning;
import scala.Function1;

/** Warnings about unused language entities. */
public sealed interface Unused extends Warning
    permits Unused.FunctionArgument, Unused.PatternBinding, Unused.Binding {

  /** The name that is unused. */
  Name name();

  /**
   * A warning about an unused function argument.
   *
   * @param name the name that is unused
   */
  record FunctionArgument(Name name) implements Unused {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Unused function argument " + name.name() + ".";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {name.name()};
    }

    @Override
    public IdentifiedLocation identifiedLocation() {
      return name.identifiedLocation();
    }

    @Override
    public String toString() {
      return "Unused.FunctionArgument(" + name.name() + ")";
    }
  }

  /**
   * A warning about an unused pattern binding.
   *
   * @param name the name that is unused
   */
  record PatternBinding(Name name) implements Unused {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Unused pattern binding " + name.name() + ".";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {name.name()};
    }

    @Override
    public IdentifiedLocation identifiedLocation() {
      return name.identifiedLocation();
    }

    @Override
    public String toString() {
      return "Unused.PatternBinding(" + name.name() + ")";
    }
  }

  /**
   * A warning about an unused binding.
   *
   * @param name the name that is unused
   */
  record Binding(Name name) implements Unused {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Unused variable " + name.name() + ".";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {name.name()};
    }

    @Override
    public IdentifiedLocation identifiedLocation() {
      return name.identifiedLocation();
    }

    @Override
    public String toString() {
      return "Unused.Binding(" + name.name() + ")";
    }
  }
}
