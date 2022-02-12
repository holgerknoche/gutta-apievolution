package gutta.apievolution.core.apimodel;

import java.util.Objects;
import java.util.Optional;

/**
 * Abstract superclass for all elements (such as user-defined types) that can be specified within an API definition.
 */
abstract class AbstractApiDefinitionElement {

    private final String publicName;

    private final String internalName;

    /**
     * Creates a new API definition element from the given data.
     * @param publicName The element's public name
     * @param internalName The element's internal name, if any. Otherwise, the public name is assumed
     */
    protected AbstractApiDefinitionElement(final String publicName, final Optional<String> internalName) {
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

    /**
     * Returns the internal name as an optional, which is only present if the internal name differs from the public
     * name.
     * @return see above
     */
    public Optional<String> getOptionalInternalName() {
        return (this.publicName.equals(this.internalName)) ? Optional.empty() : Optional.of(this.internalName);
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        return Objects.hash(this.publicName, this.internalName);
    }

    boolean stateEquals(AbstractApiDefinitionElement that) {
        return this.publicName.equals(that.publicName) &&
                this.internalName.equals(that.internalName);
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
