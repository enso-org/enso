package org.enso.interpreter.node.expression.builtin.system;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(params = {"exit_code", "stdout", "stderr"})
public class SystemProcessResult extends Builtin {}
