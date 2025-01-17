package org.enso.compiler.dump;

import static org.enso.scala.wrapper.ScalaConversions.asJava;

import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function.Lambda;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.pkg.QualifiedName;

final class DocsUtils {
  DocsUtils() {}

  static String toSignature(Method.Explicit m) {
    var sb = new StringBuilder();
    sb.append(m.methodName().name());
    if (m.body() instanceof Lambda fn) {
      var first = m.isStatic();
      for (var a : asJava(fn.arguments())) {
        if (first) {
          first = false;
          continue;
        }
        sb.append(" ").append(toSignature(a));
      }
      var ret = extractTypeOrNull(fn.body());
      if (ret != null) {
        sb.append(" -> ").append(ret);
      }
    }
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

  static String toSignature(DefinitionArgument a) {
    var sb = new StringBuilder();
    if (a.suspended()) {
      sb.append("~");
    }
    sb.append(a.name().name());
    var type = extractTypeOrNull(a);
    if (type != null) {
      sb.append(":").append(type);
    }
    if (a.defaultValue().isDefined()) {
      sb.append("=");
    }
    return sb.toString();
  }

  private static String extractTypeOrNull(IR ir) {
    var meta = ir.passData().get(TypeSignatures$.MODULE$);
    if (meta.isDefined()) {
      var sigMeta = (TypeSignatures.Signature) meta.get();
      var sigFqn = extractFqnOrNull(sigMeta.signature());
      if (sigFqn != null) {
        return sigFqn.toString();
      }
      var type =
          switch (sigMeta.signature()) {
            case Application.Prefix app -> {
              var typeConstructor = extractFqnOrNull(app.function());
              if (typeConstructor == null) {
                yield null;
              }
              var sb = new StringBuilder();
              sb.append("(");
              sb.append(typeConstructor);
              for (var a : asJava(app.arguments())) {
                var fqn = extractFqnOrNull(a.value());
                assert fqn != null : "No FQN for " + a;
                sb.append(" ");
                sb.append(fqn);
              }
              sb.append(")");
              yield sb.toString();
            }
            case Set.Union union -> extractSet(asJava(union.operands()), "|");
            default -> null;
          };
      return type;
    } else {
      var fqn = extractFqnOrNull(ir);
      return fqn == null ? null : fqn.toString();
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
      var opType = extractTypeOrNull(op);
      assert opType != null;
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
