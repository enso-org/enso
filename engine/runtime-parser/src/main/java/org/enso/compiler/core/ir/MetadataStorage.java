package org.enso.compiler.core.ir;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import org.enso.compiler.core.CompilerStub;
import scala.Option;

/** Stores metadata for the various passes. */
public final class MetadataStorage {
  private Map<ProcessingPass, ProcessingPass.Metadata> metadata;

  public MetadataStorage() {
    this(Collections.emptyMap());
  }

  public MetadataStorage(Map<ProcessingPass, ProcessingPass.Metadata> init) {
    this.metadata = init;
  }

  /**
   * Adds a new metadata entity to the pass metadata, or updates it if it already exists for a given
   * pass.
   *
   * @param pass the pass to add the metadata for
   * @param newMeta the metadata to add for `pass`
   * @tparam K the concrete type of `pass`
   */
  public void update(ProcessingPass pass, ProcessingPass.Metadata newMeta) {
    var copy = copyMetaMap();
    copy.put(pass, newMeta);
    metadata = copy;
  }

  /**
   * Adds a metadata pair to the node metadata.
   *
   * <p>This will overwrite any entry whose key matches [[MetadataPair#pass]].
   *
   * @param <K> the concrete type of the pass
   * @param metadataPair the pair to add to the storage
   */
  public <K extends ProcessingPass> void update(MetadataPair<K> metadataPair) {
    update(metadataPair.pass(), metadataPair.metadata());
  }

  /**
   * Removes the metadata for the specified pass from the list.
   *
   * @param pass the pass to remove metadata for
   * @tparam K the concrete type of `pass`
   * @return the removed metadata for that pass, if it exists
   */
  public Option<ProcessingPass.Metadata> remove(ProcessingPass pass) {
    var prev = metadata.get(pass);
    if (prev == null) {
      return Option.empty();
    } else {
      var copy = copyMetaMap();
      copy.remove(pass);
      metadata = copy;
      return Option.apply(prev);
    }
  }

  /**
   * Gets the metadata for the specified pass.
   *
   * @param pass the pass to get the metadata for
   * @tparam K the concrete type of `pass`
   * @return the metadata for `pass`, if it exists
   */
  public Option<ProcessingPass.Metadata> get(ProcessingPass pass) {
    var prev = (ProcessingPass.Metadata) metadata.get(pass);
    return Option.apply(prev);
  }

  /**
   * Creates a deep copy of `this`.
   *
   * @return a deep copy of `this`
   */
  public MetadataStorage duplicate() {
    var map = new HashMap<ProcessingPass, ProcessingPass.Metadata>();
    for (var entry : this.metadata.entrySet()) {
      var key = entry.getKey();
      var meta = (ProcessingPass.Metadata) entry.getValue();
      var duplicated = meta.duplicate();
      if (duplicated.nonEmpty()) {
        map.put(key, duplicated.get());
      }
    }
    var res = new MetadataStorage(map);
    return res;
  }

  /**
   * Maps across the stored metadata, transforming it to an output list.
   *
   * @param <R> the resulting element of the list
   * @param fn the function to apply over the metadata
   * @return a list containing the results of transforming the metadata storage
   */
  public <R> List<R> map(BiFunction<ProcessingPass, ProcessingPass.Metadata, R> fn) {
    return metadata.entrySet().stream().map((en) -> fn.apply(en.getKey(), en.getValue())).toList();
  }

  /**
   * Prepares the metadata for serialization.
   *
   * <p>This operation takes place _in place_.
   *
   * <p>Metadata prepared for serialization should not contain any links that span more than one
   * module, or any other properties that are problematic when serialized.
   *
   * <p>Due to the type safety properties of [[org.enso.compiler.core.ir.MetadataStorage]], to allow
   * this conversion to work it must be type-refined to return `typeof this`. To that end, there is
   * no default definition for this method.
   *
   * @param compiler the Enso compiler
   */
  public final void prepareForSerialization(CompilerStub compiler) {
    var newMap =
        metadata.entrySet().stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    (en) -> {
                      var value = en.getValue();
                      var newVal = value.prepareForSerialization(compiler);
                      return newVal;
                    }));
    this.metadata.putAll(newMap);
  }

  /**
   * Restores metadata after it has been deserialized.
   *
   * <p>Due to the type safety properties of [[org.enso.compiler.core.ir.MetadataStorage]], to allow
   * this conversion to work it must be type-refined to return `typeof this`. To that end, there is
   * no default definition for this method.
   *
   * @param compiler the Enso compiler
   * @return `true` if restoration was successful, `false` otherwise
   */
  public boolean restoreFromSerialization(CompilerStub compiler) {
    var ok = new boolean[] {true};
    var newMap =
        metadata.entrySet().stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    (en) -> {
                      var value = en.getValue();
                      var newOption = value.restoreFromSerialization(compiler);
                      if (newOption.nonEmpty()) {
                        return newOption.get();
                      } else {
                        ok[0] = false;
                        return value;
                      }
                    }));
    this.metadata = newMap;
    return ok[0];
  }

  @Override
  public String toString() {
    var sb = new StringBuilder("MetadataStorage[");
    var names = new ArrayList<String>();
    for (var v : metadata.values()) {
      var m = (ProcessingPass.Metadata) v;
      names.add(m.metadataName());
    }
    Collections.sort(names);
    var sep = "";
    for (var n : names) {
      sb.append(sep);
      sb.append(n);
      sep = ", ";
    }
    sb.append("]");
    return sb.toString();
  }

  private static final Comparator<ProcessingPass> COMPARATOR =
      (p1, p2) -> {
        return p1.getClass().getName().compareTo(p2.getClass().getName());
      };

  private Map<ProcessingPass, ProcessingPass.Metadata> copyMetaMap() {
    var copy = new TreeMap<ProcessingPass, ProcessingPass.Metadata>(COMPARATOR);
    copy.putAll(metadata);
    return copy;
  }

  @Override
  public int hashCode() {
    int hash = 5;
    hash = 17 * hash + Objects.hashCode(this.metadata);
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj instanceof MetadataStorage other) {
      return Objects.equals(this.metadata, other.metadata);
    }
    return false;
  }

  public record MetadataPair<K extends ProcessingPass>(K pass, ProcessingPass.Metadata metadata) {}
}
