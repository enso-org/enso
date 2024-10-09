package org.enso.compiler.core.ir.module.scope;

import java.util.List;
import org.enso.compiler.core.ir.JName;
import org.enso.compiler.core.ir.module.JScope;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

public interface JExport extends JScope {
  @IRNode
  interface JModule extends JExport {
    @IRChild
    JName.JQualified name();

    @IRChild(required = false)
    JName.JLiteral rename();

    @IRChild(required = false)
    List<JName.JLiteral> onlyNames();

    boolean isSynthetic();

    /**
     * Gets the name of the module visible in the importing scope, either the original name or the
     * rename.
     *
     * @return the name of this export visible in code
     */
    default JName getSimpleName() {
      if (rename() != null) {
        return rename();
      } else {
        return name().parts().get(name().parts().size() - 1);
      }
    }

    /**
     * Checks whether the export statement allows use of the given exported name.
     *
     * <p>Note that it does not verify if the name is actually exported by the module, only checks
     * if it is syntactically allowed.
     *
     * @param name the name to check
     * @return whether the name could be accessed or not
     */
    default boolean allowsAccess(String name) {
      if (onlyNames() != null) {
        return onlyNames().stream().anyMatch(n -> n.name().equalsIgnoreCase(name));
      }
      return true;
    }
  }
}
