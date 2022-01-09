package gutta.apievolution.javacodegen;

/**
 * Representation of a Java enumeration member for code generation.
 *
 * <p/> <b>Note:</b> This class must be public for Velocity code generation to work.
 */
public class JavaEnumMember {

    private final String name;

    JavaEnumMember(String name) {
        this.name = name;
    }

    /**
     * Returns this enum member's name.
     * @return see above
     */
    public String getName() {
        return this.name;
    }

}
