package org.enso.compiler.pass.analyse;

import java.util.NoSuchElementException;
import java.util.UUID;
import org.enso.compiler.core.ExternalID;
import scala.Option;
import scala.collection.immutable.Set;
import scala.collection.mutable.HashSet;
import scala.collection.mutable.LinkedHashSet;
import scala.collection.mutable.Map;
import scala.collection.mutable.Queue;

/**
 * Storage for dependency information. Pass metadata for {@link DataflowAnalysis}.
 *
 * <p>It maps from an expression to other expressions based on some relationship between them.
 */
public final class DependencyMapping {
  private final scala.collection.mutable.Map<DependencyInfo.Type, Set<DependencyInfo.Type>> mapping;

  @SuppressWarnings("unchecked")
  public static DependencyMapping.Builder newBuilder() {
    return new DependencyMapping.Builder(scala.collection.mutable.Map$.MODULE$.empty());
  }

  public static DependencyMapping.Builder newBuilder(DependencyMapping mapping) {
    return new DependencyMapping.Builder(mapping.mapping());
  }

  /**
   * Constructs new mapping.
   *
   * @param mapping storage for the direct mapping between program components
   */
  public DependencyMapping(Map<DependencyInfo.Type, Set<DependencyInfo.Type>> mapping) {
    this.mapping = mapping;
  }

  final Map<DependencyInfo.Type, Set<DependencyInfo.Type>> mapping() {
    return mapping;
  }

  /**
   * Obtains the program components _directly_ associated with a given node in the IR.
   *
   * <p>Please note that this does _not_ return the transitive closure of all associations with the
   * node.
   *
   * @param key the key to get the associated components of
   * @return the set of the components directly associated with `key`, if it exists
   */
  public /* used by tests only */ Option<Set<DependencyInfo.Type>> getDirect(
      DependencyInfo.Type key) {
    return mapping.get(key);
  }

  /**
   * Obtains the external identifiers of the _direct_ dependents of a given node in the IR.
   *
   * @param key the key to get the dependents of
   * @return the set of external identifiers for the direct dependencies of `key`, if they exist
   */
  @SuppressWarnings("unchecked")
  public /* used by tests only */ Option<Set<@ExternalID UUID>> getExternalDirect(
      DependencyInfo.Type key) {
    var res = getDirect(key).map(m -> m.flatMap(id -> id.externalId()));
    return (Option<Set<@ExternalID UUID>>) (Object) res;
  }

  /**
   * Safely gets the set of all program components associated with the provided key.
   *
   * <p>Please note that the result set contains not just the components that are directly
   * associated with the key, but all components associated with the key
   *
   * @param key the key to get the associations of
   * @return the set of all associations with `key`, if key exists
   */
  @SuppressWarnings("unchecked")
  public Option<? extends Set<DependencyInfo.Type>> get(DependencyInfo.Type key) {
    if (mapping.contains(key)) {
      var queue = new Queue<DependencyInfo.Type>(1);
      queue.addOne(key);

      var visited = new HashSet<>();
      var result = new LinkedHashSet();

      while (queue.nonEmpty()) {
        var elem = queue.dequeue();
        if (visited.contains(elem)) {
          continue;
        }
        visited.addOne(elem);
        var opt = mapping.get(elem);
        if (opt.nonEmpty()) {
          var deps = opt.get();
          queue.enqueueAll(deps);
          result.addAll(deps);
        }
      }

      var set = result.toSet();
      return Option.apply(set);
    } else {
      return Option.empty();
    }
  }

  /**
   * Safely gets the external identifiers for all program component associated with the provided
   * key.
   *
   * <p>Please note that the result set contains not just the components that are directly
   * associated with the key, but all associations with the key.
   *
   * @param key the key from which to get the external identifiers of its associated program
   *     components
   * @return the set of all external identifiers of program components associated with `key`, if it
   *     exists
   */
  @SuppressWarnings("unchecked")
  public final Option<Set<@ExternalID UUID>> getExternal(DependencyInfo.Type key) {
    var res = get(key).map(m -> m.flatMap(e -> e.externalId()));
    return (Option<Set<UUID>>) (Object) res;
  }

  /**
   * @return A deep copy of this dependency mapping
   */
  @SuppressWarnings("unchecked")
  final DependencyMapping deepCopy() {
    var tupples = this.mapping.toSeq();
    var map = scala.collection.mutable.Map$.MODULE$.apply(tupples);
    return new DependencyMapping((Map<DependencyInfo.Type, Set<DependencyInfo.Type>>) map);
  }

  /** Mutable builder to construct {@link DependencyMapping}. */
  public static final class Builder {
    private scala.collection.mutable.Map<DependencyInfo.Type, Set<DependencyInfo.Type>> mapping;

    private Builder(
        scala.collection.mutable.Map<DependencyInfo.Type, Set<DependencyInfo.Type>> mapping) {
      this.mapping = mapping;
    }

    public DependencyMapping build() {
      return new DependencyMapping(mapping);
    }

    /**
     * Executes an update on the association information. Used from scala as
     *
     * <pre>
     * dependencies(ids.head) = Set(ids(1), ids(2))
     * </pre>
     *
     * @param key the key to update the associations for
     * @param newDependents the updated associations for `key`
     */
    public
    /** only needed from tests */
    void update(DependencyInfo.Type key, Set<DependencyInfo.Type> newDependents) {
      mapping.put(key, newDependents);
    }

    /**
     * Updates the associations for the provided key, or creates them if they do not already exist.
     *
     * @param key the key to add or update associations for
     * @param newDependents the new associations information for `key`
     */
    public /* used by tests only */ void updateAt(
        DependencyInfo.Type key, Set<DependencyInfo.Type> newDependents) {
      if (mapping.contains(key)) {
        var set = mapping.apply(key);
        var both = set.$plus$plus(newDependents);
        mapping.put(key, both.toSet());
      } else {
        mapping.put(key, newDependents);
      }
    }

    /**
     * @return A deep copy of this dependency mapping
     */
    @SuppressWarnings("unchecked")
    DependencyMapping deepCopy() {
      var copy = this.mapping.toMap(null);
      var map = Map.from(copy);
      return new DependencyMapping((Map<DependencyInfo.Type, Set<DependencyInfo.Type>>) map);
    }

    public Option<? extends Set<DependencyInfo.Type>> get(DependencyInfo.Type key) {
      return build().get(key);
    }

    /**
     * Returns the set of all program component associated with the provided key.
     *
     * <p>Please note that the result set contains not just the _direct_ associations with the key,
     * but also the _indirect_ associations with the key.
     *
     * @param key the key to get the associated components of
     * @return the set of all components associated with `key`
     * @throws NoSuchElementException when `key` does not exist in the dependencies mapping
     */
    public /* only used from tests */ Set<DependencyInfo.Type> apply(DependencyInfo.Type key)
        throws NoSuchElementException {
      if (mapping.contains(key)) {
        var opt = get(key);
        if (opt.isDefined()) {
          return opt.get();
        } else {
          throw new NoSuchElementException();
        }
      } else {
        throw new NoSuchElementException();
      }
    }

    /**
     * Combines two dependency information containers.
     *
     * @param that the other container to combine with `this`
     * @return the result of combining `this` and `that`
     */
    public /* used by tests only */ final DependencyMapping.Builder combine(
        DependencyMapping that) {
      var combinedModule = new DependencyMapping.Builder(this.mapping);

      that.mapping.foreach(
          tupple -> {
            var key = tupple._1();
            var value = tupple._2();
            if (combinedModule.mapping.contains(key)) {
              var xs = combinedModule.mapping.get(key);
              var both = value.$plus$plus(xs.get());
              combinedModule.mapping.put(key, both.toSet());
            } else {
              combinedModule.mapping.put(key, value);
            }
            return null;
          });

      return combinedModule;
    }
  }
}
