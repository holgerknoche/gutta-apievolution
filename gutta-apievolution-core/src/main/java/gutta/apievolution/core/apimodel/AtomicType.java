package gutta.apievolution.core.apimodel;

/**
 * Atomic types are basic types that cannot be parameterized (such as int types of fixed width).
 */
public enum AtomicType implements BasicType {
    /**
     * Atomic type representing "32-bit signed integer".
     */
    INT_32,
    /**
     * Atomic type representing "64-bit signed integer".
     */
    INT_64;

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleAtomicType(this);
    }
}
