package org.enso.compiler.dump.igv;

import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;

final class Utils {
  private Utils() {}

  static String label(Object obj) {
    return switch (obj) {
      case Literal.Text txt -> "Literal.Text ('" + txt.text() + "')";
      case Literal.Number num -> "Literal.Number (" + num.value() + ")";
      case Name.Literal lit -> "Name.Literal ('" + lit.name() + "')";
      case Definition.Type tp -> "Definition.Type ('" + tp.name().name() + "')";
      case Definition.SugaredType tp -> "Definition.SugaredType ('" + tp.name().name() + "')";
      case Import.Module imp -> "Import.Module ('" + imp.name().name() + "')";
      case Export.Module exp -> "Export.Module ('" + exp.name().name() + "')";
      case Method.Explicit m -> "Method.Explicit ('" + m.methodName().name() + "')";
      case Method.Conversion c -> "Method.Conversion ('" + c.methodName().name() + "')";
      case Method.Binding b -> "Method.Binding ('" + b.methodName().name() + "')";
      case DefinitionArgument.Specified arg -> "DefinitionArgument.Specified ('"
          + arg.name().name()
          + "')";
      case Expression.Binding b -> "Expression.Binding ('" + b.name().name() + "')";
      case Pattern.Name n -> "Pattern.Name ('" + n.name().name() + "')";
      case Pattern.Constructor c -> "Pattern.Constructor ('" + c.constructor().name() + "')";
      default -> defaultLabel(obj);
    };
  }

  static String hash(Object obj) {
    return Integer.toHexString(System.identityHashCode(obj));
  }

  private static String strippedClassName(String fqn) {
    return fqn.replace("org.enso.compiler.core.ir.", "");
  }

  private static String defaultLabel(Object obj) {
    return strippedClassName(obj.getClass().getName());
  }
}
