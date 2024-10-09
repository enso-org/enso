package org.enso.compiler.core.ir.module.scope;

import java.util.List;
import org.enso.compiler.core.ir.JName;
import org.enso.compiler.core.ir.module.JScope;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

/** Module-level import statements. */
public interface JImport extends JScope {
  @IRNode
  interface JModule extends JImport {
    @IRChild
    JName.JQualified name();

    @IRChild(required = false)
    JName.JLiteral rename();

    boolean isAll();

    @IRChild(required = false)
    List<JName.JLiteral> onlyNames();

    @IRChild(required = false)
    List<JName.JLiteral> hiddenNames();

    boolean isSynthetic();

    /**
     * Checks whether the import statement allows use of the given exported name.
     *
     * <p>Note that it does not verify if the name is actually exported by the module, only checks
     * if it is syntactically allowed.
     *
     * @param name the name to check
     * @return whether the name could be accessed or not
     */
    default boolean allowsAccess(String name) {
      if (!isAll()) {
        return false;
      }
      if (onlyNames() != null) {
        return onlyNames().stream().anyMatch(n -> n.name().equals(name));
      }
      if (hiddenNames() != null) {
        return hiddenNames().stream().noneMatch(n -> n.name().equals(name));
      }
      return true;
    }
  }
}
