package org.enso.compiler.context;

import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.AliasAnalysis;
import org.enso.compiler.pass.analyse.JavaInteropHelpers;
import org.enso.compiler.pass.resolve.GlobalNames$;
import scala.Option;

import java.util.Optional;

/**
 * Encapsulates the logic for resolving a variable name.
 * <p>
 * The same logic is needed in two places:
 * 1. in the runtime ({@link org.enso.interpreter.runtime.IrToTruffle.processName}),
 * 2. in the type checker ({@link org.enso.compiler.pass.analyse.types.TypeInference.processName}).
 * <p>
 * To ensure that all usages stay consistent, they should all rely on the logic implemented in this class,
 * customizing it to the specific needs of the context in which it is used.
 *
 * @param <ResultType>        The result type of the resolution process.
 * @param <LocalNameLinkType> The type describing a link to a local name in a current scope.
 *                            Depending on the context this may be a Graph.Link (in the compiler) or a FramePointer (in the runtime).
 */
public abstract class NameResolution<ResultType, LocalNameLinkType> {
  public ResultType run(Name.Literal name) {
    AliasAnalysis.Info.Occurrence occurrenceMetadata = JavaInteropHelpers.getAliasAnalysisOccurrenceMetadata(name);
    var maybeLocalLink = findLocalLink(occurrenceMetadata);
    if (maybeLocalLink.isDefined()) {
      return resolveLocalName(maybeLocalLink.get());
    }

    Optional<BindingsMap.Resolution> global = MetadataInteropHelpers.getOptionalMetadata(name, GlobalNames$.MODULE$, BindingsMap.Resolution.class);
    if (global.isPresent()) {
      BindingsMap.ResolvedName resolution = global.get().target();
      return resolveGlobalName(resolution);
    }

    if (name.name().equals(ConstantsNames.FROM_MEMBER)) {
      return resolveFromConversion();
    }

    return resolveUnresolvedSymbol(name.name());
  }

  protected abstract Option<LocalNameLinkType> findLocalLink(AliasAnalysis.Info.Occurrence occurrenceMetadata);

  protected abstract ResultType resolveLocalName(LocalNameLinkType localLink);

  protected abstract ResultType resolveGlobalName(BindingsMap.ResolvedName resolvedName);

  protected abstract ResultType resolveFromConversion();

  protected abstract ResultType resolveUnresolvedSymbol(String symbolName);
}
