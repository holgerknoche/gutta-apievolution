package gutta.apievolution.core.apimodel;

import java.util.Objects;

/**
 * Annotations allow to embed additional information within an API model, such
 * as, for instance, specific names of types for particular programming
 * languages.
 */
public class Annotation {

    private final String name;

    private final String value;

    /**
     * Creates a new annotation with the given annotation type name.
     *
     * @param name  The annotation's name
     * @param value The annotation's value
     */
    public Annotation(final String name, final String value) {
        this.name = name;
        this.value = value;
    }

    /**
     * Returns the name of this annotation.
     *
     * @return see above
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns the value of this annotation.
     *
     * @return see above
     */
    public String getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.name, this.value);
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof Annotation) {
            return this.stateEquals((Annotation) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(Annotation that) {
        return this.name.equals(that.name) && this.value.equals(that.value);
    }

}
