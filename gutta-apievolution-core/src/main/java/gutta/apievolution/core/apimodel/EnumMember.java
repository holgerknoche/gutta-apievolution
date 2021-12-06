package gutta.apievolution.core.apimodel;

import java.util.Optional;

/**
 * An enum member represents a particular value of an {@link EnumType}.
 * @param <E> The concrete type of the enum type
 * @param <M> The concrete type of enum member type (e.g., provider or consumer)
 */
public class EnumMember<E extends EnumType<?, E, M>, M extends EnumMember<E, M>> extends ApiDefinitionElement {

    private final E owner;

    /**
     * Creates a new enum member from the given data.
     * @param publicName The public name of the enum member
     * @param internalName The internal name of the enum member, if applicable. Otherwise, the public name is assumed
     * @param owner The enum type that owns this member
     */
    @SuppressWarnings("unchecked")
    protected EnumMember(final String publicName, final Optional<String> internalName, final E owner) {
        super(publicName, internalName);

        this.owner = owner;
        owner.addDeclaredMember((M) this);
    }

    /**
     * Returns the enum type that owns this member.
     * @return see above
     */
    public E getOwner() {
        return this.owner;
    }

}
