package gutta.apievolution.core.apimodel;

/**
 * Optionality represents to which degree a given element (such as a parameter
 * or a field) is optional.
 */
public enum Optionality {
    /**
     * The respective element is fully optional.
     */
    OPTIONAL("optional"),
    /**
     * The respective element is optional for input, put required for output.
     */
    OPT_IN("optin"),
    /**
     * The respective element is always mandatory.
     */
    MANDATORY("mandatory");

    private final String representation;

    Optionality(String representation) {
        this.representation = representation;
    }

    /**
     * Returns whether one of two optionality values is stricter than the other.
     *
     * @param o1 The first value to compare
     * @param o2 The second ("other") value to compare
     * @return a {@link Comparable}-like comparison value
     */
    public static int moreStrict(Optionality o1, Optionality o2) {
        return Integer.compare(o2.ordinal(), o1.ordinal());
    }

    /**
     * Returns whether one of two optionality values is more permissive than the
     * other.
     *
     * @param o1 The first value to compare
     * @param o2 The second ("other") value to compare
     * @return a {@link Comparable}-like comparison value
     */
    public static int morePermissive(Optionality o1, Optionality o2) {
        return Integer.compare(o1.ordinal(), o2.ordinal());
    }

    /**
     * Returns the maximum (in terms of permissiveness, i.e. the more permissive) of
     * two optionalities
     *
     * @param o1 The first value to compare
     * @param o2 The second value to compare
     * @return The more permissive optionality
     */
    public static Optionality max(Optionality o1, Optionality o2) {
        return (o2.isMorePermissiveThan(o1)) ? o2 : o1;
    }

    /**
     * Returns whether this optionality value is more permissive than the given one.
     *
     * @param that The optionality value to compare against
     * @return {@code True}, iff this value is more permissive than the given one
     */
    public boolean isMorePermissiveThan(Optionality that) {
        return (morePermissive(this, that) < 0);
    }

    @Override
    public String toString() {
        return this.representation;
    }

}
