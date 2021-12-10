package gutta.apievolution.javacodegen;

/**
 * Representation of a Java property for code generation.
 *
 * <p/> <b>Note:</b> This class must be public for Velocity code generation to work.
 */
public class JavaProperty {

    private final String name;

    private final JavaType type;

    JavaProperty(String name, JavaType type) {
        this.name = name;
        this.type = type;
    }

    /**
     * Returns this property's name.
     * @return see above
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns this property's name with capitalized first letter.
     * @return see above
     */
    public String getCapitalizedName() {
        return Character.toUpperCase(this.name.charAt(0)) + this.name.substring(1);
    }

    /**
     * Returns this property's type.
     * @return see above
     */
    public JavaType getType() {
        return this.type;
    }

}
