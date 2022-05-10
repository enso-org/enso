package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(params = {"prim_root_file", "prim_config"})
public class ProjectDescription extends Builtin {}
