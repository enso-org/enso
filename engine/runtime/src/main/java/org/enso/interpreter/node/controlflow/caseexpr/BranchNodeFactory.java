package org.enso.interpreter.node.controlflow.caseexpr;

import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import com.oracle.truffle.api.RootCallTarget;

/**
 * BranchNodeFactory is a SAM-type to allow for storing the mapping between
 * AtomConstructors and corresponding BranchNode.
 *
 * SAM-type is necessary because types like
 * `PolyglotBranchNode.build _`

 */
public interface BranchNodeFactory {
    BranchNode create(AtomConstructor atomConstructor, RootCallTarget target);
}
