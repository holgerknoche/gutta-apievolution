package gutta.apievolution.javacodegen;

/**
 * Representation of a Java interface for code generation.
 *
 * <p/> <b>Note:</b> This class must be public for Velocity code generation to work.
 */
public class JavaInterface extends JavaRecordLikeType {

    private JavaInterface superType;

    JavaInterface(String packageName, String name) {
        super(packageName, name);
    }

    /**
     * Returns the supertype of this interface, if present.
     * @return The supertype or {@code null}
     */
    public JavaInterface getSuperType() {
        return this.superType;
    }

    void setSuperType(JavaInterface superType) {
        this.superType = superType;
    }

    @Override
    <R> R accept(JavaUserDefinedTypeVisitor<R> visitor) {
        return visitor.handleJavaInterface(this);
    }

}
