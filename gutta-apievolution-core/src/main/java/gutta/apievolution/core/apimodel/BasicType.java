package gutta.apievolution.core.apimodel;

/**
 * Interface for basic types, i.e., types that cannot be changed by the user.
 */
public interface BasicType extends Type {

    /**
     * Returns whether this type is a basic type.
     * @return see above
     */
    default boolean isBasicType() {
        return true;
    }

    /**
     * Returns whether this type is an atomic type.
     * @return see above
     */
    default boolean isAtomicType() {
        return false;
    }

    /**
     * Returns whether this type is a bounded type.
     * @return see above.
     */
    default boolean isBoundedType() {
        return false;
    }

}
