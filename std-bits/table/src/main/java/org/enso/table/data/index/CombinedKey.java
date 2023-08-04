package org.enso.table.data.index;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CombinedKey<KeyTypeA extends MultiValueKey, KeyTypeB extends MultiValueKey> implements MultiValueKey {
    private KeyTypeA keyA;

    private KeyTypeB keyB;

    private final int hashCodeValue;

    public CombinedKey(KeyTypeA keyA, KeyTypeB keyB) {
        this.keyA = keyA;
        this.keyB = keyB;

        this.hashCodeValue = calculateHashCode();
    }

    public KeyTypeA getKeyA() { return keyA; }

    public KeyTypeB getKeyB() { return keyB; }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CombinedKey that)) return false;
        return this.keyA.equals(that.keyA) && this.keyB.equals(that.keyB);
    }

    public int hashCode() {
        return this.hashCodeValue;
    }

    private int calculateHashCode() {
        return ((31 + keyA.hashCode()) * 31) + keyB.hashCode();
    }

    public boolean hasFloatValues() {
        return keyA.hasFloatValues() && keyB.hasFloatValues();
    }

    public List<Integer> floatColumnPositions() {
        var positionsA = keyA.floatColumnPositions();
        var positionsB = keyB.floatColumnPositions();
        return Stream.concat(positionsA.stream(), positionsB.stream())
            .collect(Collectors.toList());
    }
}