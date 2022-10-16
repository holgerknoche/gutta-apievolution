package gutta.apievolution.javacodegen;

import java.util.Collections;
import java.util.Set;

/**
 * Representation of a Java interface for code generation.
 *
 * <p/>
 * <b>Note:</b> This class must be public for Velocity code generation to work.
 */
public class JavaInterface
        extends JavaRecordLikeType {

    private Set<JavaInterface> superTypes;

    JavaInterface(String packageName, String name) {
        super(packageName, name);
    }

    /**
     * Returns the supertypes of this interface, if any.
     * 
     * @return The (possible empty) set of super types
     */
    public Set<JavaInterface> getSuperTypes() {
        return this.superTypes;
    }

    void setSuperTypes(Set<JavaInterface> superTypes) {
        this.superTypes = (superTypes == null) ? Collections.emptySet() : superTypes;
    }

    @Override
    <R> R accept(JavaUserDefinedTypeVisitor<R> visitor) {
        return visitor.handleJavaInterface(this);
    }

}
