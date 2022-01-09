package gutta.apievolution.javacodegen;

/**
 * Abstract superclass for user-defined types for Java code generation.
 */
public abstract class JavaUserDefinedType implements JavaType {

    public final String packageName;

    public final String name;

    JavaUserDefinedType(String packageName, String name) {
        this.packageName = packageName;
        this.name = name;
    }

    /**
     * Returns this type's package name.
     * @return see above
     */
    public String getPackageName() {
        return this.packageName;
    }

    /**
     * Returns this type's name.
     * @return see above
     */
    public String getName() {
        return this.name;
    }

    @Override
    public String getFullyQualifiedName() {
        return this.packageName + "." + this.name;
    }

}
