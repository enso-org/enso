package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.lang.ref.PhantomReference;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** A runtime representation of a managed resource. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "resource", stdlibName = "Standard.Base.Runtime.Managed_Resource.Managed_Resource")
public final class ManagedResource implements EnsoObject {
  private final Object resource;
  private final PhantomReference<ManagedResource> phantomReference;

  /**
   * Creates a new managed resource.
   *
   * @param resource the underlying resource
   * @param factory factory to create reference
   */
  public ManagedResource(
      Object resource,
      java.util.function.Function<ManagedResource, PhantomReference<ManagedResource>> factory) {
    this.resource = resource;
    this.phantomReference = factory.apply(this);
  }

  /**
   * @return the underlying resource
   */
  public Object getResource() {
    return resource;
  }

  /**
   * @return the phantom reference tracking this managed resource
   */
  public PhantomReference<ManagedResource> getPhantomReference() {
    return phantomReference;
  }

  @Builtin.Method(
      description =
          "Makes an object into a managed resource, automatically finalized when the returned"
              + " object is garbage collected.")
  @Builtin.Specialize
  public static ManagedResource register(EnsoContext context, Object resource, Function function) {
    return context.getResourceManager().register(resource, function);
  }

  @Builtin.Method(
      description =
          "Takes the value held by the managed resource and removes the finalization callbacks,"
              + " effectively making the underlying resource unmanaged again.")
  @Builtin.Specialize
  public Object take(EnsoContext context) {
    context.getResourceManager().take(this);
    return this.getResource();
  }

  @Builtin.Method(
      name = "finalize",
      description = "Finalizes a managed resource, even if it is still reachable.")
  @Builtin.Specialize
  public void close(EnsoContext context) {
    context.getResourceManager().close(this);
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().managedResource();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().managedResource();
  }
}
