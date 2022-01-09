package gutta.apievolution.core.apimodel;

/**
 * This type represents the unbounded variant of the {@link StringType}.
 */
public class UnboundedStringType extends StringType {

    private static final UnboundedStringType INSTANCE = new UnboundedStringType();

    static UnboundedStringType instance() {
        return INSTANCE;
    }

    private UnboundedStringType() {
        // Empty Singleton Constructor
    }

    @Override
    public int hashCode() {
        return 1234;
    }

    @Override
    public boolean equals(Object that) {
        return (this == that);
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleUnboundedStringType(this);
    }

    @Override
    public String toString() {
        return "string";
    }
}
