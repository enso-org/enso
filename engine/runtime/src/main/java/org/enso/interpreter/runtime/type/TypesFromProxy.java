package org.enso.interpreter.runtime.type;

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
   * @return the associated {@link Type} if it exists and {@code null} otherwise
   */
  public static Type fromTypeSystem(Builtins builtins, String typeName) {
    return switch (typeName) {
      case ConstantsGen.ANY -> builtins.any();
      case ConstantsGen.ARRAY -> builtins.array();
      case ConstantsGen.BOOLEAN -> builtins.bool().getType();
      case ConstantsGen.DATE -> builtins.date();
      case ConstantsGen.DATE_TIME -> builtins.dateTime();
      case ConstantsGen.DURATION -> builtins.duration();
      case ConstantsGen.DECIMAL -> builtins.number().getDecimal();
      case ConstantsGen.NAN -> builtins.number().getNaN();
      case ConstantsGen.ERROR -> builtins.dataflowError();
      case ConstantsGen.FUNCTION -> builtins.function();
      case ConstantsGen.FILE -> builtins.file();
      case ConstantsGen.INTEGER -> builtins.number().getInteger();
      case ConstantsGen.MANAGED_RESOURCE -> builtins.managedResource();
      case ConstantsGen.NOTHING -> builtins.nothing();
      case ConstantsGen.NUMBER -> builtins.number().getNumber();
      case ConstantsGen.PANIC -> builtins.panic();
      case ConstantsGen.REF -> builtins.ref();
      case ConstantsGen.TEXT -> builtins.text();
      case ConstantsGen.TIME_OF_DAY -> builtins.timeOfDay();
      case ConstantsGen.TIME_ZONE -> builtins.timeZone();
      case ConstantsGen.VECTOR -> builtins.vector();
      default -> null;
    };
  }
}
