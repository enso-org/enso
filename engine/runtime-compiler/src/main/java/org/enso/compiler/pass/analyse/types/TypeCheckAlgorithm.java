package org.enso.compiler.pass.analyse.types;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.errors.Resolution;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.scala.wrapper.ScalaConversions;

/**
 * Generic algorithm to convert type signature from IR to higher level structure.
 *
 * @param <R> representation of the higher level structure
 * @param <E> exception to propagate to signal an error
 */
public abstract class TypeCheckAlgorithm<R, E extends Exception> {
  /**
   * Finds a representation of a type check for a given name in the context of a provided module.
   *
   * @param module the module
   * @param name the name of a type inside of that module
   */
  protected abstract R forName(CompilerContext.Module module, String name) throws E;

  /**
   * Finds a representation of a polyglot type in a given module.
   *
   * @param module the module
   * @param name name of the polyglot symbol
   */
  protected abstract R forPolyglot(CompilerContext.Module module, String name);

  /**
   * No special type for expression found.
   *
   * @param t the expression
   * @return value or "any type"
   */
  protected abstract R forAnyType(Expression t) throws E;

  /**
   * Representation of a function type.
   *
   * @param fn the function IR
   * @return the type check for function type
   */
  protected abstract R forFunction(Type.Function fn);

  /**
   * Type check for an errorneous type.
   *
   * @param err the resolution that fails
   * @return the type check representing the resolution
   */
  protected abstract R forError(Resolution err);

  /**
   * Combines multiple type checks into one that is satisfied when any of the provided ones is
   * satisfied.
   *
   * @param arr the type checks to combine
   * @return combined type check
   */
  protected abstract R forOneOf(Collection<? extends R> arr) throws E;

  /**
   * Combines multiple type checks into one so that all of them has to be satisfied.
   *
   * @param arr the type checks to combine
   * @return combined type check
   */
  protected abstract R forAllOf(Collection<? extends R> arr) throws E;

  /**
   * Converts provided expression into type representation.
   *
   * @param t the expression
   * @return the representation of the expression
   * @throws E any exception if raised by one of the "combinator" {@code forXyz} methods
   */
  public final R extractAscribedType(Expression t) throws E {
    return switch (t) {
      case Set.Union u -> {
        var oneOf = u.operands().map(st -> extractAscribedTypeRaise(RuntimeException.class, st));
        var arr = ScalaConversions.asJava(oneOf);
        yield forOneOf(arr);
      }
      case Set.Intersection i -> {
        var arr = new ArrayList<R>();
        collectIntersections(i, arr);
        yield forAllOf(arr);
      }
      case Application.Prefix p -> extractAscribedType(p.function());
      case Type.Function fn ->
          // find a function type
          forFunction(fn);
      case Type.Error typeWithError ->
          // When checking a `a ! b` type, we ignore the error part as it is only used for
          // documentation
          // purposes and is not checked.
          extractAscribedType(typeWithError.typed());
      case Type.Context typeInContext ->
          // Type contexts aren't currently really used. But we should still check the base type.
          extractAscribedType(typeInContext.typed());
      case Resolution err -> forError(err);
      default -> {
        var res =
            MetadataInteropHelpers.getMetadataOrNull(
                t, TypeNames$.MODULE$, BindingsMap.Resolution.class);
        if (res != null) {
          switch (res.target()) {
            case BindingsMap.ResolvedType binding -> {
              var module = binding.module().unsafeAsModule("convert to module");
              var name = binding.tp().name();
              yield forName(module, name);
            }
            case BindingsMap.ResolvedPolyglotSymbol ps -> {
              var mod = ps.module().unsafeAsModule("convert to module");
              var sym = ps.symbol();
              yield forPolyglot(mod, sym.name());
            }
            default -> {}
          }
        }
        yield forAnyType(t);
      }
    };
  }

  @SuppressWarnings("unchecked")
  private final <T extends Exception> R extractAscribedTypeRaise(Class<T> ignore, Expression t)
      throws T {
    try {
      return extractAscribedType(t);
    } catch (Exception ex) {
      throw (T) ex;
    }
  }

  private void collectIntersections(Set.Intersection app, List<R> arr) throws E {
    var left = app.left();
    if (left instanceof Set.Intersection leftInter) {
      collectIntersections(leftInter, arr);
    } else {
      arr.add(extractAscribedType(left));
    }
    var right = app.right();
    if (right instanceof Set.Intersection rightInter) {
      collectIntersections(rightInter, arr);
    } else {
      arr.add(extractAscribedType(right));
    }
  }
}
