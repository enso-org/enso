package org.enso.interpreter.service;

import java.util.concurrent.CompletionStage;
import java.util.function.Supplier;

public interface GuestExecutionService {
  <T> CompletionStage<T> submitExecution(Supplier<T> c);
}
