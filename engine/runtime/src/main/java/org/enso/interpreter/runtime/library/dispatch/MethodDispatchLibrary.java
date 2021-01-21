package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.PanicException;

@GenerateLibrary
@GenerateLibrary.DefaultExport(DefaultLongExports.class)
@GenerateLibrary.DefaultExport(DefaultDoubleExports.class)
@GenerateLibrary.DefaultExport(DefaultBooleanExports.class)
public abstract class MethodDispatchLibrary extends Library {
  public static LibraryFactory<MethodDispatchLibrary> getFactory() {
    return FACTORY;
  }

  public static MethodDispatchLibrary getUncached() {
    return FACTORY.getUncached();
  }

  public static class NoSuchMethodException extends Exception {}

  private static final LibraryFactory<MethodDispatchLibrary> FACTORY =
      LibraryFactory.resolve(MethodDispatchLibrary.class);

  @GenerateLibrary.Abstract(ifExported = {"getFunctionalDispatch"})
  public boolean hasFunctionalDispatch(Object receiver) {
    return false;
  }

  public boolean hasSpecialDispatch(Object receiver) {
    return false;
  }

  @GenerateLibrary.Abstract(ifExported = {"hasFunctionalDispatch"})
  public Function getFunctionalDispatch(Object receiver, UnresolvedSymbol symbol)
      throws NoSuchMethodException {
    throw new NoSuchMethodException();
  }
  ;
}
