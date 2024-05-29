package org.enso.persist;

import static org.enso.persist.PerGenerator.INLINED_REFERENCE_ID;

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
      var refId = PerGenerator.registerReference(out, ref);
      out.writeInt(refId);
    } else {
      out.writeInt(INLINED_REFERENCE_ID);
      var obj = ref.get(Object.class);
      out.writeObject(obj);
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  protected Reference readObject(Input in) throws IOException, ClassNotFoundException {
    var refId = in.readInt();
    if (refId != INLINED_REFERENCE_ID) {
      return PerInputImpl.findReference(in, refId);
    } else {
      var ref = in.readReference(Object.class);
      return ref;
    }
  }
}
