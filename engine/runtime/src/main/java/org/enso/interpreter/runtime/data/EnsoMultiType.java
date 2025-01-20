package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import java.util.HashSet;
import java.util.concurrent.ConcurrentHashMap;
import org.enso.interpreter.runtime.EnsoContext;

/**
 * Internal representation of {@code Type[]} that supports identity comparision with {@code ==} to
 * support inline caching of {@link EnsoMultiValue}. This is a separate and hidden concept from
 * {@link Type} which is used thru out the Enso codebase.
 *
 * <p>Think twice before opening this type to public!
 */
final class EnsoMultiType {
  private static final ConcurrentHashMap<EnsoMultiType, EnsoMultiType> ALL_TYPES =
      new ConcurrentHashMap<>();

  @CompilerDirectives.CompilationFinal(dimensions = 1)
  private final Type[] types;

  private EnsoMultiType(Type[] types) {
    this.types = types;
  }

  @CompilerDirectives.TruffleBoundary
  static EnsoMultiType findOrCreateSlow(Type[] types, int from, int to) {
    var mt = new EnsoMultiType(Arrays.copyOfRange(types, from, to));
    return ALL_TYPES.computeIfAbsent(mt, java.util.function.Function.identity());
  }

  final int typesLength() {
    return types.length;
  }

  final Type firstType() {
    return types[0];
  }

  private int find(EnsoContext ctx, Type type) {
    for (var i = 0; i < types.length; i++) {
      for (var t : types[i].allTypes(ctx)) {
        if (t == type) {
          return i;
        }
      }
    }
    return -1;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public int hashCode() {
    int hash = 7;
    hash = 89 * hash + Arrays.deepHashCode(this.types);
    return hash;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final EnsoMultiType other = (EnsoMultiType) obj;
    return Arrays.deepEquals(this.types, other.types);
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return "MultiType{" + "types=" + Arrays.toString(types) + '}';
  }

  @CompilerDirectives.TruffleBoundary
  final boolean hasIntersectionWith(EnsoMultiType other) {
    var my = new HashSet<>(Arrays.asList(types));
    var their = Arrays.asList(other.types);
    my.removeAll(their);
    return my.size() < types.length;
  }

  @GenerateUncached
  abstract static class FindIndexNode extends Node {
    private static final String INLINE_CACHE_LIMIT = "5";

    abstract int executeFindIndex(Type type, EnsoMultiType mt);

    @NeverDefault
    public static FindIndexNode create() {
      return EnsoMultiTypeFactory.FindIndexNodeGen.create();
    }

    @NeverDefault
    public static FindIndexNode getUncached() {
      return EnsoMultiTypeFactory.FindIndexNodeGen.getUncached();
    }

    @Specialization(
        guards = {"type == cachedType", "mt == cachedMt"},
        limit = INLINE_CACHE_LIMIT)
    int findsCachedIndexOfAType(
        Type type,
        EnsoMultiType mt,
        @Cached("type") Type cachedType,
        @Cached("mt") EnsoMultiType cachedMt,
        @Cached(allowUncached = true, value = "findsAnIndexOfAType(type, mt)") int cachedIndex) {
      return cachedIndex;
    }

    @Specialization(replaces = "findsCachedIndexOfAType")
    int findsAnIndexOfAType(Type type, EnsoMultiType mt) {
      var ctx = EnsoContext.get(this);
      var index = mt.find(ctx, type);
      return index;
    }
  }

  @GenerateUncached
  abstract static class AllTypesWith extends Node {
    private static final String INLINE_CACHE_LIMIT = "5";

    @NeverDefault
    static AllTypesWith getUncached() {
      return EnsoMultiTypeFactory.AllTypesWithNodeGen.getUncached();
    }

    @NeverDefault
    static AllTypesWith create() {
      return EnsoMultiTypeFactory.AllTypesWithNodeGen.create();
    }

    /**
     * Don't modify the return value. It maybe cached!
     *
     * @param first first set of types
     * @param second second set of types
     * @param moveToFirst what type index to move to first
     * @return union of both types
     */
    abstract Type[] executeAllTypes(EnsoMultiType first, EnsoMultiType second, int moveToFirst);

    @Specialization(
        limit = INLINE_CACHE_LIMIT,
        guards = {
          "self == cachedSelf",
          "nextOrNull == cachedNextOrNull",
          "moveToFirst == cachedMovedToFirst",
        })
    Type[] optimizeForTypes(
        EnsoMultiType self,
        EnsoMultiType nextOrNull,
        int moveToFirst,
        @Cached("self") EnsoMultiType cachedSelf,
        @Cached("nextOrNull") EnsoMultiType cachedNextOrNull,
        @Cached("moveToFirst") int cachedMovedToFirst,
        @Cached("slowlyComputeTypes(self, nextOrNull, moveToFirst)") Type[] result) {
      return result;
    }

    @Specialization(replaces = "optimizeForTypes")
    Type[] slowlyComputeTypes(EnsoMultiType self, EnsoMultiType nextOrNull, int moveToFirst) {
      Type[] concat;
      if (nextOrNull == null || nextOrNull.types.length == 0) {
        concat = self.types.clone();
      } else {
        var next = nextOrNull;
        concat = new Type[self.types.length + next.types.length];
        System.arraycopy(self.types, 0, concat, 0, self.types.length);
        System.arraycopy(next.types, 0, concat, self.types.length, next.types.length);
      }
      if (moveToFirst != 0) {
        var first = concat[0];
        concat[0] = concat[moveToFirst];
        concat[moveToFirst] = first;
      }
      return concat;
    }
  }
}
