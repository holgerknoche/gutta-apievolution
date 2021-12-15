package gutta.apievolution.core.apimodel;

import java.util.Map;
import java.util.stream.Stream;

/**
 * Abstract superclass for type lookups which support mapping types between different API definitions. This class may be
 * subclassed for resolving specific types, such as provider or consumer types.
 * @param <U> The concrete type of user-defined types that is managed in this type lookup
 */
public abstract class TypeLookup<U> {

    private final Map<U, U> udtLookup;

    /**
     * Creates a new type lookup using the given lookup map for user-defined types.
     * @param udtLookup A lookup map for user-defined types
     */
    public TypeLookup(Map<U, U> udtLookup) {
        this.udtLookup = udtLookup;
    }

    /**
     * Looks up the equivalent of the given type in this type lookup.
     * @param <T> The desired metatype of the result
     * @param inType The type to determine the equivalent for
     * @return see above
     */
    @SuppressWarnings("unchecked")
    public <T extends Type> T lookupType(T inType) {
        if (this.isUserDefinedType(inType)) {
            //noinspection SuspiciousMethodCalls
            return (T) this.udtLookup.get(inType);
        } else if (inType instanceof ListType) {
            return (T) this.convertListType((ListType) inType);
        } else {
            return inType;
        }
    }

    /**
     * Returns whether the given type is a user-defined type.
     * @param type The type to check
     * @return see above
     */
    protected abstract boolean isUserDefinedType(Type type);

    /**
     * Returns a stream of all UDT mappings in this type lookup.
     * @return see above
     */
    protected Stream<Map.Entry<U, U>> udtLookupStream() {
        return this.udtLookup.entrySet().stream();
    }

    private ListType convertListType(ListType inType) {
        if (inType.isBounded()) {
            return ListType.bounded(inType.getElementType(), inType.getBound());
        } else {
            return ListType.unbounded(inType.getElementType());
        }
    }

}
