package org.enso.interpreter.node.expression.builtin.ordering;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

/** A hidden builtin. Only conversions with target type of Comparable are visible. */
@BuiltinType
public class Comparable extends Builtin {}
