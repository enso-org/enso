package org.enso.compiler.phase.exports;

import java.util.List;
import java.util.stream.Collectors;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedMethod;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotField;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotSymbol;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.pkg.QualifiedName;

/**
 * An error thrown when there are multiple conflicting resolutions for a symbol. Note that, it is OK
 * for example to have multiple extension methods resolved for a symbol. But not if one symbol
 * resolves to a type and a module.
 */
public final class ConflictingResolutionsError extends CompilerError {
  private ConflictingResolutionsError(String msg) {
    super(msg);
  }

  public static ConflictingResolutionsError create(
      QualifiedName moduleName, List<ResolvedName> conflictingResolutions) {
    assert conflictingResolutions.size() > 1;
    var resolutionNames =
        conflictingResolutions.stream()
            .map(ConflictingResolutionsError::resolvedNameToString)
            .collect(Collectors.toUnmodifiableList());
    var sb = new StringBuilder();
    sb.append("Conflicting resolutions in module '");
    sb.append(moduleName);
    sb.append("': ");
    sb.append(resolutionNames);
    sb.append(". ");
    sb.append(
        "Probably caused by a name conflict of a defined entity in the module and an export.");
    return new ConflictingResolutionsError(sb.toString());
  }

  private static String resolvedNameToString(ResolvedName resolvedName) {
    return switch (resolvedName) {
      case ResolvedType type -> "Type '" + type.qualifiedName() + "'";
      case ResolvedConstructor resolvedConstructor -> {
        var typeName = resolvedConstructor.tpe().qualifiedName().toString();
        var consName = resolvedConstructor.cons().name();
        yield "Constructor '" + typeName + "." + consName + "'";
      }
      case ResolvedMethod resolvedMethod -> "Method '" + resolvedMethod.qualifiedName() + "'";
      case ResolvedModule resolvedModule -> "Module '" + resolvedModule.qualifiedName() + "'";
      case ResolvedPolyglotField resolvedPolyglotField -> "Polyglot field '"
          + resolvedPolyglotField.qualifiedName()
          + "'";
      case ResolvedPolyglotSymbol resolvedPolyglotSymbol -> "Polyglot symbol '"
          + resolvedPolyglotSymbol.qualifiedName()
          + "'";
      default -> throw new UnsupportedOperationException("unimplemented: " + resolvedName);
    };
  }
}
