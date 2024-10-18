package org.enso.compiler.core.ir.module;

import org.enso.compiler.core.IR;
import org.enso.runtime.parser.dsl.IRNode;

/** A representation of constructs that can only occur in the top-level module scope */
@IRNode
public interface JScope extends IR {}
