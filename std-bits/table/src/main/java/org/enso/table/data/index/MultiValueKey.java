package org.enso.table.data.index;

import java.util.List;

public interface MultiValueKey {
    abstract public boolean hasFloatValues();

    abstract public List<Integer> floatColumnPositions();
}