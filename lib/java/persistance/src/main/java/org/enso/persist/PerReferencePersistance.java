package org.enso.persist;

import java.io.IOException;
import org.enso.persist.Persistance.Reference;

final class PerReferencePeristance extends Persistance<Reference> {
  static final Persistance<Reference> INSTANCE = new PerReferencePeristance();

  private PerReferencePeristance() {
    super(Reference.class, true, 4320);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void writeObject(Reference ref, Output out) throws IOException {
    if (ref.isDeferredWrite()) {
      throw new IOException("Write shall never be deferred");
    }
    var obj = ref.get(Object.class);
    out.writeObject(obj);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected Reference readObject(Input in) throws IOException, ClassNotFoundException {
    var ref = in.readReference(Object.class);
    return ref;
  }
}
