package gutta.apievolution.javacodegen;

import java.util.Set;

/**
 * Representation of a Java exception for code generation.
 */
public class JavaException
        extends JavaRecordLikeType {

    private JavaException superType;

    JavaException(String packageName, String name) {
        super(packageName, name);
    }

    /**
     * Returns the supertype of this exception, if present.
     * 
     * @return The supertype or {@code null}
     */
    public JavaException getSuperType() {
        return this.superType;
    }

    void setSuperTypes(Set<JavaException> superTypes) {
        if (superTypes.isEmpty()) {
            this.superType = null;
        } else if (superTypes.size() == 1) {
            this.superType = superTypes.iterator().next();
        } else {
            throw new UnsupportedOperationException(
                    "More than one supertype on an exception is not supported.");
        }
    }

    @Override
    <R> R accept(JavaUserDefinedTypeVisitor<R> visitor) {
        return visitor.handleJavaException(this);
    }
}
