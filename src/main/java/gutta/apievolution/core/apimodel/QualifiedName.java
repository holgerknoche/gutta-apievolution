package gutta.apievolution.core.apimodel;

import java.security.cert.PolicyQualifierInfo;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 * Qualified names consist of (potentially) multiple parts. Syntactically, these parts are separated by dots, for
 * instance, "a.b.c".
 */
public class QualifiedName {

    private final List<String> parts;

    /**
     * Creates a new qualified name from its syntactical representation.
     * @param name The name string to create an object from
     * @return The qualified name object
     */
    public static QualifiedName of(String name) {
        if (name == null || name.isEmpty()) {
            throw new IllegalArgumentException("Empty names are not supported.");
        }

        return new QualifiedName(Arrays.asList(name.split("\\.")));
    }

    /**
     * Creates a qualified name from the given list of parts.
     * @param parts The parts of the qualified name
     */
    public QualifiedName(final List<String> parts) {
        if (parts == null) {
            throw new NullPointerException();
        }

        this.parts = parts;
    }

    @Override
    public int hashCode() {
        return this.parts.hashCode();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        } else if (other instanceof QualifiedName) {
            return this.equals((QualifiedName) other);
        } else {
            return false;
        }
    }

    private boolean equals(QualifiedName other) {
        return (this.parts.equals(other.parts));
    }

    @Override
    public String toString() {
        Iterator<String> parts = this.parts.iterator();

        StringBuilder builder = new StringBuilder();
        while (parts.hasNext()) {
            builder.append(parts.next());

            if (parts.hasNext()) {
                builder.append('.');
            }
        }

        return builder.toString();
    }

}
