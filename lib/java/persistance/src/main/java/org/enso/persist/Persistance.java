package org.enso.persist;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.function.Function;

import static org.enso.persist.PerUtils.raise;

/** Central persistance class. Use static {@link
 * Persistance#write write} method to turn a graph of JVM objects into a {@code byte[]}.
 *
 * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="write"}
 *
 * <p>Use sibling static {@link Persistance#read readO} method to read the byte buffer back into
 * their memory representation.
 *
 * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="read"}
 *
 * <h2>Manual Persistance</h2>
 *
 * Unlike typical Java serialization (which tries to make things automatic), this framework requires
 * one to implement the persistance manually. For each class that one wants to support, one has to
 * implement subclass {@link Persistance} and implement its {@link Persistance#writeObject} and
 * {@link Persistance#readObject} method.
 *
 * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="manual"}
 *
 * There is a semi-automatic way to generate such subclasses of {@link Persistance} via
 * the {@link Persistable @Persistable} annotation.
 * @param <T>
 */
public abstract class Persistance<T> {
  final Class<T> clazz;
  final boolean includingSubclasses;
  final int id;

  /**
   * Constructor for subclasses to register persistance for certain
   * {@code clazz}. Sample registration:
   *
   * <p>
   * {@snippet file="org/enso/persist/PersistanceTest.java" region="manual"}
   *
   * Each persistance requires unique ID. A stream created by
   * {@link #write(Object, Function<Object, Object>)} and read by
   * {@link #read(byte[], Function<Object, Object>)} contains a header derived
   * from the all the IDs present in the system. When versioning the protocol
   * and implementation:
   * <ul>
   * <li>when you change something really core in the Persitance itself -
   * change the header</li>
   * <li>when you add or remove a Persistance implementation the version
   * changes (computed from all the IDs)</li>
   * <li>when you change format of some Persitance.writeObject method - change
   * its ID</li>
   * </ul>
   *
   * @param clazz the class persistance is written for
   * @param includingSubclasses should the persistance also apply to
   * subclasses
   * @param id unique ID
   */
  protected Persistance(Class<T> clazz, boolean includingSubclasses, int id) {
    this.clazz = clazz;
    this.includingSubclasses = includingSubclasses;
    this.id = id;
  }

  protected abstract void writeObject(T obj, Output out) throws IOException;
  protected abstract T readObject(Input in) throws IOException, ClassNotFoundException;

  /** Prints the {@code clazz} and {@code id} values.
  */
  @Override
  public final String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Persistance{");
    sb.append("clazz=").append(clazz.getName());
    sb.append(", id=").append(id);
    sb.append('}');
    return sb.toString();
  }

  /** Extended output interface for {@link #writeObject(Object)} method.
   */
  public static interface Output extends DataOutput {
    /** Writes an object "inline" - as a value.
    *
    * @param <T> the type of the class
    * @param clazz the class to use to locate {@link Persistance} implementation
    * @param obj the object to write down
    * @throws IOException when an I/O problem happens
    */
    public abstract <T> void writeInline(Class<T> clazz, T obj) throws IOException;

    /** Writes an object as a "reference". Objects written by by this method
    * are shared - each {@code obj} is stored only once and referenced from
    * all the locations it is used.
    *
    * @param obj the object to write down
    * @throws IOException when an I/O problem happens
    */
    public abstract void writeObject(Object obj) throws IOException;
  }

  /** Extended input interface for the {@link #writeObject(T, Output)} method.
   */
  public static interface Input extends DataInput {
    /** Reads objects written down by {@link Output#writeInline}.
    *
    * @param <T> the type to read
    * @param clazz class that identifies {@link Persistance} to use for reading
    * @return the read in object
    * @throws IOException when an I/O problem happens
    */
    public abstract <T> T readInline(Class<T> clazz) throws IOException;

    /** Reads a reference to object written down by {@link Output#writeObject(Object)}.
     *
     * @return the read in object
     * @throws IOException when an I/O problem happens
     */
    public abstract Object readObject() throws IOException;

    /** *  Reads a reference to an object written down by {@link Output#writeObject(Object)}
     * but without reading the object itself. The object can then be obtained
     * <em>"later"</em> via the {@link Reference#get(Class)} method.
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

  /** Read object written down by {@link #write} from an array.
   *
   * <p>{@snippet file="org/enso/persist/PersistanceTest.java" region="read"}
   *
   * @param <T> expected type of object
   * @param arr the stored bytes
   * @param readResolve either {@code null} or function to call for each object being stored to provide a replacement
   * @return the read object
   * @throws java.io.IOException when an I/O problem happens
   */
  public static <T> Reference<T> read(byte[] arr, Function<Object, Object> readResolve) throws IOException {
    return PerInputImpl.readObject(arr, readResolve);
  }

  /** Writes down an object into an array of bytes. Use {@link #read} method to convert the
   * array back to object.
   *
   * @param obj the object to persist
   * @param writeReplace {@code null} or a function that allows to convert each object before storing it down
   * @return the array of bytes
   * @throws IOException when an I/O problem happens
   */
  public static byte[] write(Object obj, Function<Object, Object> writeReplace) throws IOException {
    return PerGenerator.writeObject(obj, writeReplace);
  }

  /** Reference to an object. Either created directly or obtained from {@link Input#readReference}
   * method.
   *
   * @see Input#readReference
   */
  public static sealed abstract class Reference<T> permits PerBufferReference, PerMemoryReference {
    Reference() {
    }
    /** Reference to {@code null} object.
     * @param <T> the type of the reference
     * @return reference to {@code null}
     */
    @SuppressWarnings("unchecked")
    public static final <T> Reference<T> none() {
      return (Reference<T>) PerMemoryReference.NULL;
    }

    /** Extract object from the reference. Multiple calls to this method
     * may return the same or another instance of object depending on the
     * type of the reference and <em>laziness policy</em>.
     *
     * @param <V> the type of the object to expect
     * @param expectedType the expected clazz of the object
     * @return the referenced object
     * @throws ClassCastException if the object isn't of the expected type
     */
    public <V> V get(Class<V> expectedType) {
      var value = switch (this) {
        case PerMemoryReference m -> m.value();
        case PerBufferReference<T> b -> b.readObject(expectedType);
      };
      return expectedType.cast(value);
    }

    /** Creates a reference to existing object.
     *
       * @param <V> the type of the object
       * @param obj the object to "reference"
       * @return reference pointing to the provided object
     */
    public static <V> Reference<V> of(V obj) {
      return new PerMemoryReference<>(obj);
    }
  }
}
