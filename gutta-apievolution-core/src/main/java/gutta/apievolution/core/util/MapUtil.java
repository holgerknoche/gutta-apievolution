package gutta.apievolution.core.util;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Utility functions for maps.
 */
public class MapUtil {
    
    public static <A, B, C> Map<A, C> composeMaps(Map<A, B> map1, Function<B, C> map2) {
        Map<A, C> composedMap = new HashMap<>(map1.size());

        for (Map.Entry<A, B> entry : map1.entrySet()) {
            C value = map2.apply(entry.getValue());

            if (value != null) {
                composedMap.put(entry.getKey(), value);
            }
        }

        return composedMap;
    }

    public static <A, B> Map<B, A> invertMap(Map<A, B> map, Consumer<B> onConflict) {
        Map<B, A> invertedMap = new HashMap<>(map.size());

        for (Map.Entry<A, B> entry : map.entrySet()) {
            A existingValue = invertedMap.put(entry.getValue(), entry.getKey());

            if (existingValue != null) {
                onConflict.accept(entry.getValue());
            }
        }

        return invertedMap;
    } 
    
    private MapUtil() {
        // Private constructor
    }

}
