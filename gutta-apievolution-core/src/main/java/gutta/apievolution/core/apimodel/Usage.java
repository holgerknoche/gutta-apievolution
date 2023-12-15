package gutta.apievolution.core.apimodel;

/**
 * Enumeration of usage types of an element within an API definition.
 */
public enum Usage {
    
    /**
     * The element is not used at all.
     */
    NONE,
    /**
     * The element is only used for input.
     */
    INPUT_ONLY,
    /**
     * The element is only used for output.
     */
    OUTPUT_ONLY,
    /**
     * The element is used for both in- and output.
     */
    IN_OUT;

    /**
     * Denotes whether this usage type includes the given one.
     *
     * @param usage The usage type to compare against
     * @return {@code True} iff this type includes the given one
     */
    public boolean includes(Usage usage) {
        if (this == usage) {
            return true;
        }

        switch (this) {
        case NONE:
            return false;

        case INPUT_ONLY:
        case OUTPUT_ONLY:
            return (usage == IN_OUT);

        case IN_OUT:
            return true;

        default:
            throw new IllegalArgumentException("Values " + this + " and " + usage + " are not comparable.");
        }
    }

    /**
     * Computes the least upper bound of this usage and the given one.
     *
     * @param usage The usage type to compare against
     * @return see above
     */
    public Usage lubOfThisAnd(Usage usage) {
        switch (this) {
        case NONE:
            return usage;

        case INPUT_ONLY:
        case OUTPUT_ONLY:
            return (usage == NONE) ? this : IN_OUT;

        case IN_OUT:
            return this;

        default:
            throw new IllegalArgumentException("No valid upper bound of " + this + " and " + usage + ".");
        }
    }

}
