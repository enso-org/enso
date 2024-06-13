package org.enso.persist;

import static org.enso.persist.PerUtils.raise;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.function.Function;

/**
 * Central persistance class. Use static {@link Persistance#write write} method to turn a graph of
 * JVM objects into a {@code byte[]}. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="write"}
 *
 * <p>Use sibling static {@link Persistance#read readO} method to read the byte buffer back into
 * their memory representation. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="read"}
 *
 * <h2>Manual Persistance</h2>
 *
 * Unlike typical Java serialization (which tries to make things automatic), this framework requires
 * one to implement the persistance manually. For each class that one wants to support, one has to
 * implement subclass {@link Persistance} and implement its {@link Persistance#writeObject} and
 * {@link Persistance#readObject} method. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="manual"} <br>
 * There is a semi-automatic way to generate such subclasses of {@link Persistance} via the {@link
 * Persistable @Persistable} annotation.
 *
 * @param <T> type this persistance subclass operates on
 */
public abstract class Persistance<T> implements Cloneable {
  final Class<T> clazz;
  final boolean includingSubclasses;
  final int id;

  /**
   * Constructor for subclasses to register persistance for certain {@code clazz}. Sample
   * registration: <br>
   * {@snippet file="org/enso/persist/PersistanceTest.java" region="manual"} <br>
   * Each persistance requires unique ID. A stream created by {@link #write(Object, Function<Object,
   * Object>)} and read by {@link #read(byte[], Function<Object, Object>)} contains a header derived
   * from the all the IDs present in the system. When versioning the protocol and implementation:
   *
   * <ul>
   *   <li>when you change something really core in the Persitance itself - change the header
   *   <li>when you add or remove a Persistance implementation the version changes (computed from
   *       all the IDs)
   *   <li>when you change format of some Persitance.writeObject method - change its ID
   * </ul>
   *
   * @param clazz the class persistance is written for
   * @param includingSubclasses should the persistance also apply to subclasses
   * @param id unique ID
   */
  protected Persistance(Class<T> clazz, boolean includingSubclasses, int id) {
    this.clazz = clazz;
    this.includingSubclasses = includingSubclasses;
    this.id = id;
  }

  final Persistance<?> newClone() {
    try {
      return (Persistance<?>) clone();
    } catch (CloneNotSupportedException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  /**
   * Handle serialization of provided object.
   *
   * @param obj the object to serialize
   * @param out the stream to persist the objec to
   * @throws IOException thrown on I/O errors
   */
  protected abstract void writeObject(T obj, Output out) throws IOException;

  /**
   * Handle deserialization on an object.
   *
   * @param in stream to read an instance of the object from
   * @return instance of the deserialized object
   * @throws IOException thrown on I/O errors
   * @throws ClassNotFoundException thrown when class loading fails
   */
  protected abstract T readObject(Input in) throws IOException, ClassNotFoundException;

  /** Prints the {@code clazz} and {@code id} values. */
  @Override
  public final String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Persistance{");
    sb.append("clazz=").append(clazz.getName());
    sb.append(", id=").append(id);
    sb.append('}');
    return sb.toString();
  }

  /** Extended output interface for {@link #writeObject(Object)} method. */
  public static sealed interface Output extends DataOutput permits PerGenerator.ReferenceOutput {
    /**
     * Writes an object "inline" - as a value.
     *
     * @param <T> the type of the class
     * @param clazz the class to use to locate {@link Persistance} implementation
     * @param obj the object to write down
     * @throws IOException when an I/O problem happens
     */
    public abstract <T> void writeInline(Class<T> clazz, T obj) throws IOException;

    /**
     * Writes an object as a "reference". Objects written by this method are shared - each {@code
     * obj} is stored only once and referenced from all the locations it is used.
     *
     * @param obj the object to write down
     * @throws IOException when an I/O problem happens
     */
    public abstract void writeObject(Object obj) throws IOException;
  }

  /** Extended input interface for the {@link #writeObject(T, Output)} method. */
  public static sealed interface Input extends DataInput permits PerInputImpl {
    /**
     * Reads objects written down by {@link Output#writeInline}.
     *
     * @param <T> the type to read
     * @param clazz class that identifies {@link Persistance} to use for reading
     * @return the read in object
     * @throws IOException when an I/O problem happens
     */
    public abstract <T> T readInline(Class<T> clazz) throws IOException;

    /**
     * Reads a reference to object written down by {@link Output#writeObject(Object)}.
     *
     * @return the read in object
     * @throws IOException when an I/O problem happens
     */
    public abstract Object readObject() throws IOException;

    /**
     * Reads a reference to an object written down by {@link Output#writeObject(Object)} but without
     * reading the object itself. The object can then be obtained <em>"later"</em> via the {@link
     * Reference#get(Class)} method. The object is <b>not cached</b> and is loaded again and again
     * whenever the {@link Reference#get(Class)} method is called.
     *
     * @param <T> the type to read
     * @param clazz the expected type of the object to read
     * @return a reference allowing to read the object later
     * @throws IOException when an I/O problem happens
     */
    public <T> Reference<T> readReference(Class<T> clazz) throws IOException;
  }

  final void writeInline(Object obj, Output out) throws IOException {
    writeObject(clazz.cast(obj), out);
  }

  final T readWith(Input in) {
    try {
      return readObject(in);
    } catch (IOException | ClassNotFoundException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  /**
   * Read object written down by {@link #write} from an array. <br>
   * {@snippet file="org/enso/persist/PersistanceTest.java" region="read"}
   *
   * @param arr the stored bytes
   * @param readResolve either {@code null} or function to call for each object being stored to
   *     provide a replacement
   * @return the read object
   * @throws java.io.IOException when an I/O problem happens
   */
  public static Reference<?> read(byte[] arr, Function<Object, Object> readResolve)
      throws IOException {
    return read(ByteBuffer.wrap(arr), readResolve);
  }

  /**
   * Read object written down by {@link #write} from a byte buffer.
   *
   * @param buf the stored bytes
   * @param readResolve either {@code null} or function to call for each object being stored to
   *     provide a replacement
   * @return the read object
   * @throws java.io.IOException when an I/O problem happens
   */
  public static Reference<?> read(ByteBuffer buf, Function<Object, Object> readResolve)
      throws IOException {
    return PerInputImpl.readObject(buf, readResolve);
  }

  /**
   * Writes down an object into an array of bytes. Use {@link #read} method to convert the array
   * back to object.
   *
   * @param obj the object to persist
   * @param writeReplace {@code null} or a function that allows to convert each object before
   *     storing it down
   * @return the array of bytes
   * @throws IOException when an I/O problem happens
   */
  public static byte[] write(Object obj, Function<Object, Object> writeReplace) throws IOException {
    return PerGenerator.writeObject(obj, writeReplace);
  }

  /**
   * Reference to an object. Either created directly or obtained from {@link Input#readReference}
   * method.
   *
   * <p>When reading, the Reference is read lazily, only on demand. When writing, the Reference is
   * deferred towards the end, allowing to handle circular references inside of the serialized
   * structure.
   *
   * @param <T> expected type of the referenced object
   * @see Input#readReference
   */
  public abstract static sealed class Reference<T> permits PerBufferReference, PerMemoryReference {
    Reference() {}

    /**
     * Reference to {@code null} object.
     *
     * @param <T> the type of the reference
     * @return reference to {@code null}
     */
    @SuppressWarnings("unchecked")
    public static final <T> Reference<T> none() {
      return (Reference<T>) PerMemoryReference.NULL;
    }

    /**
     * Extract object from the reference. Multiple calls to this method may return the same or
     * another instance of object depending on the type of the reference and <em>laziness
     * policy</em>.
     *
     * @param <V> the type of the object to expect
     * @param expectedType the expected clazz of the object
     * @return the referenced object
     * @throws ClassCastException if the object isn't of the expected type
     */
    public <V> V get(Class<V> expectedType) {
      var value =
          switch (this) {
            case PerMemoryReference m -> m.value();
            case PerBufferReference<T> b -> {
              try {
                yield b.readObject(expectedType);
              } catch (IOException e) {
                throw raise(RuntimeException.class, e);
              }
            }
          };
      return expectedType.cast(value);
    }

    /**
     * Creates a reference to existing object. Behaves like {@code of(obj, false)} when the {@code
     * Reference} is being written into the stream.
     *
     * @param <V> the type of the object
     * @param obj the object to "reference"
     * @return reference pointing to the provided object
     */
    public static <V> Reference<V> of(V obj) {
      return of(obj, false);
    }

    /**
     * Creates a reference to existing object and specifies how to persist it. The {@code
     * deferWrite} value controls the order of serialization:
     *
     * <ul>
     *   <li>if {@code false} then the {@code obj} is stored in the stream immediatelly and only
     *       then the {@link Persistance.Output#writeObject(Object)} returns - <b>more effective</b>
     *   <li>if {@code true} then the {@link Persistance.Output#writeObject(Object)} makes <em>a
     *       note</em> to persist also {@code obj}, but returns almost immediatelly - useful to deal
     *       with <b>cycles in the references</b>
     * </ul>
     *
     * @param <V> the type of the object
     * @param obj the object to "reference"
     * @param deferWrite {@code false} or {@code true} as described in the method description
     * @return reference pointing to the provided object
     */
    public static <V> Reference<V> of(V obj, boolean deferWrite) {
      return deferWrite ? new PerMemoryReference.Deferred<>(obj) : new PerMemoryReference<>(obj);
    }

    abstract boolean isDeferredWrite();
  }
}
