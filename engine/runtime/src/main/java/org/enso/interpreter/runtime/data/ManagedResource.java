package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.lang.ref.PhantomReference;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.BuiltinObject;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * An Enso runtime representation of a managed resource.
 *
 * <p><b>Instance of this class</b> is convoluted with instances playing various roles:
 *
 * <ul>
 *   <li>this {@link ManagedResource} points to {@code resource}
 *   <li>this {@link ManagedResource} points to {@link PhantomReference} that is "phantom
 *       referencing" {@code this}
 *   <li>the implementation of {@link PhantomReference} is {@code Item} in {@link ResourceManager}
 *   <li>the {@code Item} <em>"phantom referencing"</em> {@code this} {@link ManagedResource} is
 *       stored in {@link ResourceManager} {@code pendingItems} collection.
 *   <li>the {@code Item} has a pointer to the {@code resource} as well
 *   <li>the {@code Item} has a pointer to the {@code finalizer} function
 * </ul>
 *
 * Once all this braided chunk of objects is eligible for GC because <b>nobody holds pointer to
 * {@link ManagedResource}</b>, the {@code Item} is put into {@link ResourceManager} {@code
 * referenceQueue} and process by the intricate machinery of {@link ResourceManager} and its {@code
 * ProcessItems} processor.
 */
@ExportLibrary(InteropLibrary.class)
@Builtin(pkg = "resource", stdlibName = "Standard.Base.Runtime.Managed_Resource.Managed_Resource")
public final class ManagedResource extends BuiltinObject {
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

  @Override
  protected String builtinName() {
    return "Managed_Resource";
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
  public static ManagedResource register_builtin(
      EnsoContext context, Object resource, Function function, boolean systemCanFinalize) {
    return context.getResourceManager().register(resource, function, systemCanFinalize);
  }

  @Builtin.Method(
      description =
          "Takes the value held by the managed resource and removes the finalization callbacks,"
              + " effectively making the underlying resource unmanaged again.")
  @Builtin.Specialize
  @SuppressWarnings("generic-enso-builtin-type")
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
  @TruffleBoundary
  public String toDisplayString(boolean allowSideEffects, @Bind("$node") Node node) {
    var type = getBuiltinType(node);
    return type.getName()
        + " "
        + InteropLibrary.getUncached().toDisplayString(resource, allowSideEffects);
  }

  @ExportMessage.Ignore
  @Override
  public Object toDisplayString(boolean allowSideEffects) {
    throw CompilerDirectives.shouldNotReachHere();
  }
}
