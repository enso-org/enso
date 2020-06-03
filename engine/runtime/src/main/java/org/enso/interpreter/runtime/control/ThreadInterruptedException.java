package org.enso.interpreter.runtime.control;

import com.oracle.truffle.api.nodes.ControlFlowException;

/** Thrown when guest code discovers a thread interrupt. */
public class ThreadInterruptedException extends RuntimeException {}
