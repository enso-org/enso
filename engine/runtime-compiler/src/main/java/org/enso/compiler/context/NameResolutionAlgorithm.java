package org.enso.compiler.context;

import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.resolve.GlobalNames$;
import scala.Option;

/**
 * Encapsulates the logic for resolving a variable name.
 *
 * <p>The same logic is needed in two places:
 *
 * <ol>
 *   <li>in the runtime ({@link
 *       org.enso.interpreter.runtime.IrToTruffle.ExpressionProcessor#processName}),
 *   <li>in the type checker ({@link
 *       org.enso.compiler.pass.analyse.types.TypePropagation#processName}).
 * </ol>
 *
 * <p>To ensure that all usages stay consistent, they should all rely on the logic implemented in
 * this class, customizing it to the specific needs of the context in which it is used.
 *
 * @param <ResultType> The result type of the resolution process.
 * @param <LocalNameLinkType> The type describing a link to a local name in a current scope.
 *     Depending on the context this may be a Graph.Link (in the compiler) or a FramePointer (in the
 *     runtime).
 * @param <MetadataType> Type of the metadata that is associated with the name IR that is resolved.
 */
public abstract class NameResolutionAlgorithm<ResultType, LocalNameLinkType, MetadataType> {

  /**
   * Resolves a name to {@code ResultType}.
   *
   * @param name literal name to be resolved
   * @param meta Nullable metadata gathered from the {@code name} IR.
   * @return The result of the resolution process.
   */
  public final ResultType resolveName(Literal name, MetadataType meta) {
    if (meta != null) {
      var maybeLocalLink = findLocalLink(meta);
      if (maybeLocalLink.isDefined()) {
        return resolveLocalName(maybeLocalLink.get());
      }
    }

    BindingsMap.Resolution global =
        MetadataInteropHelpers.getMetadataOrNull(
            name, GlobalNames$.MODULE$, BindingsMap.Resolution.class);
    if (global != null) {
      BindingsMap.ResolvedName resolution = global.target();
      return resolveGlobalName(resolution);
    }

    if (name.name().equals(ConstantsNames.FROM_MEMBER)) {
      return resolveFromConversion();
    }

    return resolveUnresolvedSymbol(name.name());
  }

  /**
   * Finds a local link in the current scope from the given {@code metadata}.
   *
   * @param metadata Not null.
   * @return The local link if found, {@code None} otherwise.
   */
  protected abstract Option<LocalNameLinkType> findLocalLink(MetadataType metadata);

  protected abstract ResultType resolveLocalName(LocalNameLinkType localLink);

  protected abstract ResultType resolveGlobalName(BindingsMap.ResolvedName resolvedName);

  protected abstract ResultType resolveFromConversion();

  protected abstract ResultType resolveUnresolvedSymbol(String symbolName);
}
