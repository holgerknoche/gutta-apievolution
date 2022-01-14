package gutta.apievolution.core.apimodel;

import java.util.Map;
import java.util.stream.Stream;

/**
 * Abstract supertype for type lookup based on a map.
 * @param <S> The source UDT type in this lookup
 * @param <T> The target UDT type in this lookup
 */
public abstract class MapBackedTypeLookup<S, T> extends TypeLookup<S, T> {

    private final Map<S, T> udtLookup;

    /**
     * Creates a new type lookup using the given lookup map for user-defined types.
     * @param udtLookup A lookup map for user-defined types
     */
    protected MapBackedTypeLookup(Map<S, T> udtLookup) {
        this.udtLookup = udtLookup;
    }

    /**
     * Returns a stream of all UDT mappings in this type lookup.
     * @return see above
     */
    protected Stream<Map.Entry<S, T>> udtLookupStream() {
        return this.udtLookup.entrySet().stream();
    }

    @Override
    protected T mapUserDefinedType(S type) {
        return this.udtLookup.get(type);
    }

}
