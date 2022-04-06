package gutta.apievolution.javacodegen;

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
     * Returns the supertype of this interface, if present.
     * 
     * @return The supertype or {@code null}
     */
    public JavaException getSuperType() {
        return this.superType;
    }

    void setSuperType(JavaException superType) {
        this.superType = superType;
    }

    @Override
    <R> R accept(JavaUserDefinedTypeVisitor<R> visitor) {
        return visitor.handleJavaException(this);
    }
}
