package org.enso.compiler.docs;

import static org.enso.scala.wrapper.ScalaConversions.asJava;

import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function.Lambda;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.resolve.MethodDefinitions;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.pkg.QualifiedName;

final class DocsUtils {
  private static final String ANY = "Standard.Base.Any.Any";

  private DocsUtils() {}

  static String toSignature(Method.Explicit m) {
    var sb = new StringBuilder();
    sb.append(m.methodName().name());
    var ret = processFnBody(m.body(), m.isStatic(), sb);
    sb.append(" -> ").append(ret);
    return sb.toString();
  }

  private static String processFnBody(Expression body, boolean isStatic, StringBuilder sb) {
    if (body instanceof Lambda fn) {
      var first = isStatic;
      for (var a : asJava(fn.arguments())) {
        if (first) {
          first = false;
          continue;
        }
        sb.append(" ").append(toSignature(a));
      }
      return extractTypeOrAny(fn.body());
    }
    return ANY;
  }

  static String toSignature(Method.Conversion m) {
    var sb = new StringBuilder();
    sb.append(m.methodName().name());
    processFnBody(m.body(), true, sb);
    return sb.toString();
  }

  static String toSignature(Definition.Data d) {
    var sb = new StringBuilder();
    sb.append(d.name().name());
    for (var a : asJava(d.arguments())) {
      sb.append(" ").append(toSignature(a));
    }
    return sb.toString();
  }

  static String toFqnOrSimpleName(Name ir) {
    var typeNameOpt = ir.passData().get(MethodDefinitions.INSTANCE);
    if (typeNameOpt.isDefined()) {
      var typeName = (BindingsMap.Resolution) typeNameOpt.get();
      return typeName.target().qualifiedName().toString();
    }
    return ir.name();
  }

  static String toSignature(DefinitionArgument a) {
    var sb = new StringBuilder();
    if (a.suspended()) {
      sb.append("~");
    }
    var name = argName(a);
    sb.append(name);
    if (!ConstantsNames.SELF_ARGUMENT.equals(name)) {
      var type = extractTypeOrAny(a);
      sb.append(":").append(type);
    }
    if (a.defaultValue().isDefined()) {
      sb.append("=");
    }
    return sb.toString();
  }

  private static String argName(DefinitionArgument arg) {
    if (isBlankArg(arg)) {
      return "_";
    } else {
      return arg.name().name();
    }
  }

  /** Returns true if the argument represents an underscore (blank) argument. */
  private static boolean isBlankArg(DefinitionArgument arg) {
    if (arg.name() instanceof Name.Literal lit) {
      var loc = lit.identifiedLocation();
      if (loc != null) {
        // This checks if the argument used to be an underscore.
        return loc.length() == 1 && lit.name().length() > 1;
      }
    }
    return false;
  }

  private static String extractTypeOrAny(IR ir) {
    var meta = ir.passData().get(TypeSignatures$.MODULE$);
    if (meta.isDefined()) {
      var sigMeta = (TypeSignatures.Signature) meta.get();
      var sign = sigMeta.signature();
      var sigFqn = extractFqnOrNull(sign);
      if (sigFqn != null) {
        return sigFqn.toString();
      }
      var type = extractFromSignature(sign);
      return type;
    } else {
      if (ir instanceof Application.Force force) {
        return extractTypeOrAny(force.target());
      }
      var fqn = extractFqnOrNull(ir);
      return fqn == null ? ANY : fqn.toString();
    }
  }

  private static String extractFromSignature(Expression sign) {
    return switch (sign) {
      case Application.Prefix app -> {
        var typeConstructor = extractFqnOrNull(app.function());
        if (typeConstructor == null) {
          yield null;
        }
        var sb = new StringBuilder();
        sb.append("(");
        sb.append(typeConstructor);
        for (var a : asJava(app.arguments())) {
          var fqn = extractTypeOrAny(a.value());
          sb.append(" ");
          sb.append(fqn);
        }
        sb.append(")");
        yield sb.toString();
      }
      case Set.Union union -> extractSet(asJava(union.operands()), "|");
      case Set.Intersection inter -> extractSet(collectInter(inter, new ArrayList<>()), "&");
      case Type.Error error -> {
        var typ = extractFromSignature(error.typed());
        var err = extractFromSignature(error.error());
        yield typ + "!" + err;
      }
      default -> {
        var fqn = extractFqnOrNull(sign);
        yield fqn == null ? ANY : fqn.toString();
      }
    };
  }

  private static List<Expression> collectInter(Expression ir, List<Expression> append) {
    if (ir instanceof Set.Intersection inter) {
      var left = collectInter(inter.left(), append);
      return collectInter(inter.right(), left);
    } else {
      append.add(ir);
      return append;
    }
  }

  private static String extractSet(List<Expression> operands, String sep) {
    var sb = new StringBuilder();
    for (var op : operands) {
      if (sb.isEmpty()) {
        sb.append("(");
      } else {
        sb.append(sep);
      }
      var opType = extractTypeOrAny(op);
      sb.append(opType);
    }
    sb.append(")");
    return sb.toString();
  }

  private static QualifiedName extractFqnOrNull(IR ir) {
    var typeNameOpt = ir.passData().get(TypeNames$.MODULE$);
    if (typeNameOpt.isDefined()) {
      var typeName = (BindingsMap.Resolution) typeNameOpt.get();
      return typeName.target().qualifiedName();
    }
    return null;
  }
}
