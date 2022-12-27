package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.expression.builtin.special.*;
import org.enso.interpreter.runtime.callable.function.Function;

public class Special {
  private final Function newRef;
  private final Function readRef;
  private final Function writeRef;
  private final Function runThread;
  private final Function joinThread;

  public Special(EnsoLanguage language) {
    newRef = NewRefMethodGen.makeFunction(language);
    readRef = ReadRefMethodGen.makeFunction(language);
    writeRef = WriteRefMethodGen.makeFunction(language);
    runThread = RunThreadMethodGen.makeFunction(language);
    joinThread = JoinThreadMethodGen.makeFunction(language);
  }

  public Function getNewRef() {
    return newRef;
  }

  public Function getReadRef() {
    return readRef;
  }

  public Function getWriteRef() {
    return writeRef;
  }

  public Function getRunThread() {
    return runThread;
  }

  public Function getJoinThread() {
    return joinThread;
  }
}
