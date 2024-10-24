/**
 * Framework for persisting Java objects and reading them <em>"lazily"</em>. Use static {@link
 * Persistance#write write} method to turn a graph of JVM objects into a {@code byte[]}. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="write"}
 *
 * <p>Use sibling static {@link Persistance#read read} method to read the byte buffer back into
 * their memory representation. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="read"}
 *
 * <h2>Laziness</h2>
 *
 * The major benefit of this framework is the ability to read only a part of the stored graph that
 * is actually needed. One may {@linkplain Persistance.Input#readReference obtain a reference} to an
 * object and turn that {@link Persistance.Reference} into actual object <b>later</b>, when needed.
 *
 * <h2>Manual Persistance</h2>
 *
 * Unlike typical Java serialization (which tries to make things automatic), this framework requires
 * one to implement the persistance manually. For each class one wants to serde, one has to
 * implement a subclass of {@link Persistance} and implement its {@link Persistance#writeObject} and
 * {@link Persistance#readObject} methods. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="manual"}
 *
 * <h2>Semi-automatic Persistance</h2>
 *
 * Writing serialization and deserialization implementation manually is a boring, tedious and
 * errorprone process. That's why there is a {@link Persistable @Persistable} annotation and
 * associated annotation processor that generates the necessary {@link Persistance} subclass based
 * on the "richest" constructor in the class to be persisted. This approach seems to work well for
 * Java records and Scala case classes. <br>
 * {@snippet file="org/enso/persist/PersistanceTest.java" region="annotation"}
 */
package org.enso.persist;
