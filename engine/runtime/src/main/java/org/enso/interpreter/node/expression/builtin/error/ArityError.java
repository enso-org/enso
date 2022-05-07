package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(params = {"expected_min", "expected_max", "actual"})
public class ArityError extends Builtin {}
