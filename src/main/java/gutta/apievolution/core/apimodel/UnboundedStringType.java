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

}
