package org.enso.base;

import java.util.HashMap;
import java.util.Map;

public class Replacer_Cache {
    // Map from replacement string to replacement vector.
    private static Map<String, Object> cache = new HashMap<String, Object>();

    private static final int lruSize = 5;

    // Circular buffer containing the most recent cache keys.
    private static final String lru[] = new String[lruSize];

    // Index into the circular buffer.
    private static int nextSlot = 0;

    public static void put(String key, Object value) {
        if (lru[nextSlot] != null) {
            cache.remove(lru[nextSlot]);
        }
        lru[nextSlot] = key;
        nextSlot = (nextSlot + 1) % lruSize;

        cache.put(key, value);
    }

    public static Object get(String key) {
        return cache.get(key);
    }

    public static int getLruArraySize() {
        return lruSize;
    }

    public static int getLruCacheSize() {
        return cache.size();
    }
}