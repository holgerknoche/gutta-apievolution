package gutta.apievolution.core.apimodel;

/**
 * Optionality represents to which degree a given element (such as a parameter or a field) is optional.
 */
public enum Optionality {
    /**
     * The respective element is fully optional.
     */
    OPTIONAL,
    /**
     * The respective element is optional for input, put required for output.
     */
    OPT_IN,
    /**
     * The respective element is always mandatory.
     */
    MANDATORY;

    /**
     * Returns whether one of two optionality values is stricter than the other.
     * @param o1 The first value to compare
     * @param o2 The second ("other") value to compare
     * @return a {@link Comparable}-like comparison value
     */
    public static int moreStrict(Optionality o1, Optionality o2) {
        return Integer.compare(o2.ordinal(), o1.ordinal());
    }

    /**
     * Returns whether one of two optionality values is more permissive than the other.
     * @param o1 The first value to compare
     * @param o2 The second ("other") value to compare
     * @return a {@link Comparable}-like comparison value
     */
    public static int morePermissive(Optionality o1, Optionality o2) {
        return Integer.compare(o1.ordinal(), o2.ordinal());
    }

}
