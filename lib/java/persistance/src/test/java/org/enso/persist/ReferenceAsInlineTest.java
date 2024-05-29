package org.enso.persist;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import org.enso.persist.Persistance.Reference;
import org.junit.Test;
import org.openide.util.lookup.ServiceProvider;

/**
 * This test case verifies the order of writing references down. There are two tests. One creates
 * {@code Chain} of objects with <em>deferred</em> references and one creates them with <em>inlined
 * (eager)</em> references. There is a shared counter and during serialization each object from the
 * chain <em>picks a number up</em>. As a result the {@link Chain#getNumber()} values are opposite
 * in each of the test. That demonstrates the difference between <em>inlined and deferred</em>
 * serialization of {@link Reference} objects.
 *
 * <p>This test is using {@link Persistance.Output#writeInline(Class<T>, T)} and {@link
 * Persistance.Input#readInline(Class<T>)} methods to persist the reference. See {@link
 * ReferenceAsObjectTest} for similar yet different test.
 */
public class ReferenceAsInlineTest {
  private static Chain eagerChain(String id, int[] counter, Chain next) {
    Reference<Chain> ref = next == null ? Reference.none() : Reference.of(next, false);
    return new Chain(id, counter, ref);
  }

  private static Chain lazyChain(String id, int[] counter, Chain next) {
    Reference<Chain> ref = next == null ? Reference.none() : Reference.of(next, true);
    return new Chain(id, counter, ref);
  }

  @Test
  public void persitChainEagerly() throws Exception {
    var counter = new int[] {0};
    var ch0 = eagerChain("ch0", counter, null);
    var ch1 = eagerChain("ch1", counter, ch0);
    var ch2 = eagerChain("ch2", counter, ch1);
    var ch3 = eagerChain("ch3", counter, ch2);
    var ch4 = eagerChain("ch4", counter, ch3);
    var ch5 = eagerChain("ch5", counter, ch4);

    var item = PersistanceTest.serde(Chain.class, ch5, -1);
    for (var i = 0; i < 6; i++) {
      var down = 5 - i;
      assertEquals("ch" + down, item.id);
      assertEquals("Also counting down", 6 - i, item.getNumber());
      item = item.getNext();
    }
    assertNull("Six objects and no more", item);
  }

  @Test
  public void persitChainLazily() throws Exception {
    var counter = new int[] {0};
    var ch0 = lazyChain("ch0", counter, null);
    var ch1 = lazyChain("ch1", counter, ch0);
    var ch2 = lazyChain("ch2", counter, ch1);
    var ch3 = lazyChain("ch3", counter, ch2);
    var ch4 = lazyChain("ch4", counter, ch3);
    var ch5 = lazyChain("ch5", counter, ch4);

    var item = PersistanceTest.serde(Chain.class, ch5, -1);
    for (var i = 0; i < 6; i++) {
      var down = 5 - i;
      assertEquals("ch" + down, item.id);
      assertEquals(i + 1, item.getNumber());
      item = item.getNext();
    }
    assertNull("Six objects and no more", item);
  }

  public static final class Chain {
    private final String id;
    private final Reference<Chain> next;
    private int[] counter;

    Chain(String id, int[] counter, Reference<Chain> next) {
      this.id = id;
      this.counter = counter;
      this.next = next;
    }

    int getNumber() {
      return this.counter[0];
    }

    Chain getNext() {
      return this.next.get(Chain.class);
    }

    @SuppressWarnings("unchecked")
    Chain(Persistance.Input in) throws IOException {
      this.id = in.readUTF();
      this.next = in.readInline(Reference.class);
      this.counter = new int[] {in.readInt()};
    }

    final void write(Persistance.Output out) throws IOException {
      // serialize own ID
      out.writeUTF(id);
      // serialize the reference then
      out.writeInline(Reference.class, next);

      // obtain a unique getNumber
      this.counter[0]++;
      // save it by cloning the counter for itself
      this.counter = this.counter.clone();

      out.writeInt(this.counter[0]);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class ChainPersistance extends Persistance<Chain> {

    public ChainPersistance() {
      super(Chain.class, true, 54288453);
    }

    @Override
    protected void writeObject(Chain obj, Output out) throws IOException {
      obj.write(out);
    }

    @Override
    protected Chain readObject(Input in) throws IOException, ClassNotFoundException {
      return new Chain(in);
    }
  }
}
