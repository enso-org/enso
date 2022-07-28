package org.enso.interpreter.runtime.library.types;

import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;
import org.enso.interpreter.runtime.data.Type;

@GenerateLibrary
public abstract class TypesLibrary extends Library {
  public static LibraryFactory<TypesLibrary> getFactory() {
    return FACTORY;
  }

  public static TypesLibrary getUncached() {
    return FACTORY.getUncached();
  }

  private static final LibraryFactory<TypesLibrary> FACTORY =
      LibraryFactory.resolve(TypesLibrary.class);

  public Type getType(Object receiver) {
    return null;
  }

  public boolean hasSpecialMethodDispatch(Object receiver) {
    return false;
  }
}
