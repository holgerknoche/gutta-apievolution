package gutta.apievolution.javacodegen;

public abstract class JavaUserDefinedType implements JavaType {

    public final String packageName;

    public final String name;

    JavaUserDefinedType(String packageName, String name) {
        this.packageName = packageName;
        this.name = name;
    }

    public String getPackageName() {
        return this.packageName;
    }

    public String getName() {
        return this.name;
    }

    @Override
    public String getFullyQualifiedName() {
        return this.packageName + "." + this.name;
    }

}
