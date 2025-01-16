package org.enso.compiler.dump;

import static org.enso.scala.wrapper.ScalaConversions.asJava;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Function.Lambda;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
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
      for (var a : asJava(fn.arguments())) {
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

  private static QualifiedName extractTypeOrNull(IR ir) {
    var meta = ir.passData().get(TypeSignatures$.MODULE$);
    if (meta.isDefined()) {
      var sig = (TypeSignatures.Signature) meta.get();
      var typeNameOpt = sig.signature().passData().get(TypeNames$.MODULE$);
      if (typeNameOpt.isDefined()) {
        var typeName = (BindingsMap.Resolution) typeNameOpt.get();
        return typeName.target().qualifiedName();
      }
    }
    return null;
  }
}
