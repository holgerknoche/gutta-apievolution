package gutta.apievolution.core.apimodel;

/**
 * Atomic types are basic types that cannot be parameterized (such as int types of fixed width).
 */
public enum AtomicType implements BasicType {
    /**
     * Atomic type representing "32-bit signed integer".
     */
    INT_32("int32"),
    /**
     * Atomic type representing "64-bit signed integer".
     */
    INT_64("int64");

    private final String representation;

    AtomicType(String representation) {
        this.representation = representation;
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleAtomicType(this);
    }

    @Override
    public String toString() {
        return this.representation;
    }
}
