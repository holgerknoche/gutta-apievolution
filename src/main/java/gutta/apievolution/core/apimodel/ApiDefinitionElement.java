package gutta.apievolution.core.apimodel;

import java.util.Optional;

/**
 * Abstract superclass for all elements (such as user-defined types) that can be contained within an API definition.
 */
public abstract class ApiDefinitionElement {

    private final String publicName;

    private final String internalName;

    /**
     * Creates a new API definition element from the given data.
     * @param publicName The element's public name
     * @param internalName The element's internal name, if any. Otherwise, the public name is assumed
     */
    protected ApiDefinitionElement(final String publicName, final Optional<String> internalName) {
        this.publicName = publicName;
        this.internalName = internalName.orElse(publicName);
    }

    /**
     * Returns this element's public name.
     * @return see above
     */
    public String getPublicName() {
        return this.publicName;
    }

    /**
     * Returns this element's internal name.
     * @return see above
     */
    public String getInternalName() {
        return this.internalName;
    }

    @Override
    public String toString() {
        if (this.getInternalName().equals(this.getPublicName())) {
            return this.getPublicName();
        } else {
            return this.getPublicName() + "(" + this.getInternalName() + ")";
        }
    }

}
