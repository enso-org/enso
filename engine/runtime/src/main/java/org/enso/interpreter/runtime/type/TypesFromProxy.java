package org.enso.interpreter.runtime.type;

import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;

/**
 * TypesFromProxy provides a single static method `fromTypeSystem` which converts from type-system
 * type names to atoms.
 *
 * <p>It is a proxy because it could easily be placed inside {@link
 * org.enso.interpreter.runtime.builtin.Builtins}. Except that by the time Builtins is compiled it
 * requires ConstantsGen to be present, which is being generated via the TypeProcessor at a late
 * stage. Similarly {@link org.enso.interpreter.runtime.type.Types} requires ConstantsGen to be in
 * the same package which creates a catch-22 situation.
 */
public class TypesFromProxy {

  /**
   * Convert from type-system type names to atoms.
   *
   * @param builtins a reference to {@link org.enso.interpreter.runtime.builtin.Builtins} where all
   *     builtins can be referenced from
   * @param typeName the fully qualified type name as defined in {@link Constants} or {@link
   *     ConstantsGen}
   * @return the associated {@link org.enso.interpreter.runtime.callable.atom.Atom} if it exists,
   *     and {@code null} otherwise
   */
  public static Atom fromTypeSystem(Builtins builtins, String typeName) {
    switch (typeName) {
      case ConstantsGen.ANY:
        return builtins.any().newInstance();
      case ConstantsGen.ARRAY:
        return builtins.array().newInstance();
      case ConstantsGen.BOOLEAN:
        return builtins.bool().getBool().newInstance();
      case ConstantsGen.DECIMAL:
        return builtins.number.getDecimal().newInstance();
      case ConstantsGen.ERROR:
        return builtins.dataflowError().newInstance();
      case ConstantsGen.FUNCTION:
        return builtins.function().newInstance();
      case ConstantsGen.INTEGER:
        return builtins.number.getInteger().newInstance();
      case ConstantsGen.MANAGED_RESOURCE:
        return builtins.managedResource().newInstance();
      case ConstantsGen.NOTHING:
        return builtins.nothing().newInstance();
      case ConstantsGen.NUMBER:
        return builtins.number.getNumber().newInstance();
      case ConstantsGen.PANIC:
        return builtins.panic().newInstance();
      case ConstantsGen.REF:
        return builtins.ref().newInstance();
      case ConstantsGen.TEXT:
        return builtins.text().newInstance();
      default:
        return null;
    }
  }
}
