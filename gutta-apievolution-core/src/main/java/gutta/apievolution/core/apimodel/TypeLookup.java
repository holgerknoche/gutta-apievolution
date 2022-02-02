package gutta.apievolution.core.apimodel;

/**
 * Abstract superclass for type lookups which support mapping types between different API definitions. This class may be
 * subclassed for resolving specific types, such as provider or consumer types.
 * @param <S> The concrete type of source user-defined types that is managed in this type lookup
 * @param <T> The concrete type of target user-defined types
 */
public abstract class TypeLookup<S, T> {

    /**
     * Looks up the equivalent of the given type in this type lookup.
     * @param <X> The desired metatype of the result
     * @param inType The type to determine the equivalent for
     * @return see above
     */
    @SuppressWarnings("unchecked")
    public <X extends Type> X lookupType(Type inType) {
        if (this.isUserDefinedType(inType)) {
            //noinspection SuspiciousMethodCalls
            return (X) this.mapUserDefinedType((S) inType);
        } else if (inType instanceof ListType) {
            return (X) this.convertListType((ListType) inType);
        } else {
            return (X) inType;
        }
    }

    /**
     * Returns whether the given type is a user-defined type.
     * @param type The type to check
     * @return see above
     */
    protected abstract boolean isUserDefinedType(Type type);

    /**
     * Maps the given user-defined type to its match, if it exists.
     * @param type The user-defined type to map
     * @return The matching type or {@code null}
     */
    protected abstract T mapUserDefinedType(S type);

    private ListType convertListType(ListType inType) {
        if (inType.isBounded()) {
            return ListType.bounded(inType.getElementType(), inType.getBound());
        } else {
            return ListType.unbounded(inType.getElementType());
        }
    }

}
