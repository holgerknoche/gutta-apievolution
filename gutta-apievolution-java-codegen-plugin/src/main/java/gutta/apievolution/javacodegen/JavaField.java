package gutta.apievolution.javacodegen;

public class JavaField {

    public final String name;

    public final JavaType type;

    JavaField(String name, JavaType type) {
        this.name = name;
        this.type = type;
    }

    public String getName() {
        return this.name;
    }

    public String getCapitalizedName() {
        return Character.toUpperCase(this.name.charAt(0)) + this.name.substring(1);
    }

    public JavaType getType() {
        return this.type;
    }

}
