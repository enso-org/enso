package org.enso.interpreter.runtime;

import java.util.Collection;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.ir.AscriptionReason;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.errors.Resolution;
import org.enso.interpreter.node.typecheck.TypeCheckValueNode;

final class IrTruffleUtils {
  static TypeCheckValueNode extractAscribedType(
      EnsoContext ctx, AscriptionReason comment, Expression t) {
    return new CreateTypeCheckNodes(ctx, false, comment).extractAscribedType(t);
  }

  /*
    static TypeCheckValueNode extractAscribedTypeAll(EnsoContext ctx, AscriptionReason comment, Expression t) {
      var checkNode = new CreateTypeCheckNodes(ctx, true, comment).extractAscribedType(t);
      return TypeCheckValueNode.allTypes(true, checkNode);
    }
  */
  private static class CreateTypeCheckNodes
      extends org.enso.compiler.pass.analyse.types.TypeCheckAlgorithm<
          TypeCheckValueNode, CompilerError> {
    private final EnsoContext ctx;
    private final boolean allTypes;
    private final AscriptionReason comment;

    private CreateTypeCheckNodes(EnsoContext ctx, boolean allTypes, AscriptionReason reason) {
      this.ctx = ctx;
      this.allTypes = reason.isAllTypes();
      this.comment = reason;
    }

    @Override
    protected TypeCheckValueNode forName(CompilerContext.Module module, String name) {
      var m = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
      var typ = m.getScope().getType(name, true);
      if (typ == ctx.getBuiltins().any()) {
        if (allTypes) {
          return TypeCheckValueNode.allOf(comment, new TypeCheckValueNode[1]);
        } else {
          // no check for Any unless need to discover all types
          return null;
        }
      }
      return TypeCheckValueNode.single(comment, typ);
    }

    @Override
    protected TypeCheckValueNode forPolyglot(CompilerContext.Module module, String name) {
      var m = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
      var typ = m.getScope().getPolyglotSymbolSupplier(name);
      return TypeCheckValueNode.meta(comment, typ);
    }

    @Override
    protected TypeCheckValueNode forAnyType(Expression t) {
      return null;
    }

    @Override
    protected TypeCheckValueNode forFunction(Type.Function fn) {
      var typ = ctx.getTopScope().getBuiltins().function();
      return TypeCheckValueNode.single(comment, typ);
    }

    @Override
    protected TypeCheckValueNode forError(Resolution err) {
      return TypeCheckValueNode.fail("unresolved symbol " + err.originalName().name());
    }

    @Override
    protected TypeCheckValueNode forOneOf(Collection<? extends TypeCheckValueNode> arr) {
      if (arr.contains(null)) {
        return null;
      } else {
        return TypeCheckValueNode.oneOf(comment, arr.toArray(TypeCheckValueNode[]::new));
      }
    }

    @Override
    protected TypeCheckValueNode forAllOf(Collection<? extends TypeCheckValueNode> arr) {
      return TypeCheckValueNode.allOf(comment, arr.toArray(TypeCheckValueNode[]::new));
    }
  }
}
