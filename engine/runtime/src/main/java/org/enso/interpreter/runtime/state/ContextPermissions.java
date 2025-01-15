package org.enso.interpreter.runtime.state;

/** Fields correspond to the constructors of {@code Standard.Base.Runtime.Context} builtin type. */
record ContextPermissions(boolean input, boolean output, boolean dataflowStacktrace) {}
