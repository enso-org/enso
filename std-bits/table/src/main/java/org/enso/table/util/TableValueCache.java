package org.enso.table.util;

import org.graalvm.polyglot.Value;
import org.slf4j.Logger;

import java.util.function.Function;

public class TableValueCache {
    private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(TableValueCache.class);

    private static LeastRecentlyUsedCache<String, Value> _cache;

    private static LeastRecentlyUsedCache<String, Value> cache() {
        if (_cache == null) {
            _cache = new LeastRecentlyUsedCache<>(100);
        }
        return _cache;
    }

    public static Value getOrCompute(String key, Function<String, Value> compute) {
        var cache = cache();
        if (cache.containsKey(key)) {
            LOGGER.debug("Cache hit for key: {}", key);
            return cache.get(key);
        }

        LOGGER.debug("Cache miss for key: {}", key);
        var value = compute.apply(key);
        cache.put(key, value);
        return value;
    }
}
