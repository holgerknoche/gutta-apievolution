package gutta.apievolution.javacodegen;

/**
 * Abstract superclass for user-defined types for Java code generation.
 */
public abstract class JavaUserDefinedType
        extends JavaModelElement
        implements JavaType {

    /**
     * Creates a new Java user-defined type.
     * 
     * @param packageName The type's package name
     * @param name        The type's name
     */
    protected JavaUserDefinedType(String packageName, String name) {
        super(packageName, name);
    }

    @Override
    public String getFullyQualifiedName() {
        return this.packageName + "." + this.name;
    }

    abstract <R> R accept(JavaUserDefinedTypeVisitor<R> visitor);

}
