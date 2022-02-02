package gutta.apievolution.javacodegen;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Representation of a Java interface for code generation.
 *
 * <p/> <b>Note:</b> This class must be public for Velocity code generation to work.
 */
public class JavaInterface extends JavaUserDefinedType {

    private final List<JavaProperty> properties = new ArrayList<>();

    private final Set<String> fieldNames = new HashSet<>();

    private JavaInterface superType;

    JavaInterface(String packageName, String name) {
        super(packageName, name);
    }

    /**
     * Returns the properties for which accessors are required in this interface.
     * @return see above
     */
    public List<JavaProperty> getProperties() {
        return this.properties;
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

    void addField(JavaProperty field) {
        if (!this.fieldNames.contains(field.getName())) {
            this.properties.add(field);
            this.fieldNames.add(field.getName());
        }
    }

}
