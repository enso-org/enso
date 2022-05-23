package org.enso.interpreter.runtime.data;

import java.io.IOException;

@FunctionalInterface
public interface FunctionThrowingIOException<T, R> {
  R apply(T t) throws IOException;
}
