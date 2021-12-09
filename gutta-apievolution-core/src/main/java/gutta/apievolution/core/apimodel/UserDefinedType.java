package gutta.apievolution.core.apimodel;

import java.util.Optional;

/**
 * As the name implies, user-defined types (UDTs) are types that are given by the user. Examples of such types are
 * record and enumeration types.
 *
 * @param <A> The concrete type of the API definition this UDT resides in
 */
public abstract class UserDefinedType<A extends ApiDefinition<A>> extends AbstractApiDefinitionElement implements Type {

    private final int typeId;

    private final A owner;

    /**
     * Creates a new UDT from the given data.
     * @param publicName The UDT's public name
     * @param internalName The UDT's internal name, if any (otherwise, the public name is assumed)
     * @param typeId The UDT's type id
     * @param owner The API definition owning this UDT
     */
    protected UserDefinedType(final String publicName, final Optional<String> internalName, final int typeId,
                              final A owner) {
        super(publicName, internalName);

        this.owner = owner;
        this.typeId = typeId;
    }

    /**
     * Returns this UDT's type id.
     * @return see above
     */
    public int getTypeId() {
        return this.typeId;
    }

    /**
     * Returns the API definition that owns this UDT.
     * @return see above
     */
    public A getOwner() {
        return this.owner;
    }

    @Override
    public int hashCode() {
        // Owner is excluded as to avoid cycles
        return super.hashCode() + this.typeId;
    }

    boolean stateEquals(UserDefinedType<A> that) {
        // Owner is excluded as to avoid cycles
        return super.stateEquals(that) &&
                this.typeId == that.typeId;
    }

}
