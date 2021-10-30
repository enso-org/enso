package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.TruffleObject;

import java.lang.ref.PhantomReference;

/** A runtime representation of a managed resource. */
public class ManagedResource implements TruffleObject {
  private final Object resource;
  private PhantomReference<ManagedResource> phantomReference;

  /**
   * Creates a new managed resource.
   *
   * @param resource the underlying resource
   */
  public ManagedResource(Object resource) {
    this.resource = resource;
    this.phantomReference = null;
  }

  /** @return the underlying resource */
  public Object getResource() {
    return resource;
  }

  /** @return the phantom reference tracking this managed resource */
  public PhantomReference<ManagedResource> getPhantomReference() {
    return phantomReference;
  }

  /**
   * Sets the value of the reference used to track reachability of this managed resource.
   *
   * @param phantomReference the phantom reference tracking this managed resource.
   */
  public void setPhantomReference(PhantomReference<ManagedResource> phantomReference) {
    this.phantomReference = phantomReference;
  }
}
