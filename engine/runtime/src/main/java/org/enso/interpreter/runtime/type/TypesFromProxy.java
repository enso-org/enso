package org.enso.interpreter.runtime.type;

import org.enso.compiler.exception.CompilerError;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Type;

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
  public static Type fromTypeSystem(Builtins builtins, String typeName) {
    switch (typeName) {
      case ConstantsGen.ANY:
        return builtins.any();
      case ConstantsGen.ARRAY:
        return builtins.array();
      case ConstantsGen.BOOLEAN:
        return builtins.bool().getType();
      case ConstantsGen.DATE:
        return builtins.date();
      case ConstantsGen.DATE_TIME:
        return builtins.dateTime();
      case ConstantsGen.DECIMAL:
        return builtins.number().getDecimal();
      case ConstantsGen.ERROR:
        return builtins.dataflowError();
      case ConstantsGen.FUNCTION:
        return builtins.function();
      case ConstantsGen.FILE:
        return builtins.file();
      case ConstantsGen.INTEGER:
        return builtins.number().getInteger();
      case ConstantsGen.MANAGED_RESOURCE:
        return builtins.managedResource();
      case ConstantsGen.NOTHING:
        return builtins.nothing();
      case ConstantsGen.NUMBER:
        return builtins.number().getNumber();
      case ConstantsGen.PANIC:
        return builtins.panic();
      case ConstantsGen.REF:
        return builtins.ref();
      case ConstantsGen.TEXT:
        return builtins.text();
      case ConstantsGen.TIME_OF_DAY:
        return builtins.timeOfDay();
      case ConstantsGen.TIME_ZONE:
        return builtins.timeZone();
      case ConstantsGen.VECTOR:
        return builtins.vector();
      default:
        throw new CompilerError("Invalid builtin type " + typeName);
    }
  }
}
