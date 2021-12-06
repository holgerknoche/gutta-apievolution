package gutta.apievolution.core.apimodel;

/**
 * String types represent character strings, which may be bounded.
 */
public abstract class StringType implements BoundedType {

    /**
     * Creates an unbounded string type.
     * @return The string type
     */
    public static StringType unbounded() {
        return UnboundedStringType.instance();
    }

    /**
     * Creates a bounded string type.
     * @param bound The bound of the string type
     * @return The string type
     */
    public static StringType bounded(int bound) {
        return new BoundedStringType(bound);
    }

}
