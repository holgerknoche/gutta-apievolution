package gutta.apievolution.core.apimodel;

import java.util.Optional;

/**
 * An enum member represents a particular value of an {@link EnumType}.
 * 
 * @param <E> The concrete type of the enum type
 * @param <M> The concrete type of enum member type (e.g., provider or consumer)
 */
public abstract class EnumMember<E extends EnumType<?, E, M>, M extends EnumMember<E, M>>
        extends AbstractApiDefinitionElement {

    private final E owner;

    /**
     * Creates a new enum member from the given data.
     * 
     * @param publicName   The public name of the enum member
     * @param internalName The internal name of the enum member, if applicable.
     *                     Otherwise, the public name is assumed
     * @param owner        The enum type that owns this member
     */
    @SuppressWarnings("unchecked")
    protected EnumMember(final String publicName, final Optional<String> internalName, final E owner) {
        super(publicName, internalName);

        this.owner = owner;
        owner.addDeclaredMember((M) this);
    }

    /**
     * Returns the enum type that owns this member.
     * 
     * @return see above
     */
    public E getOwner() {
        return this.owner;
    }

    @Override
    protected void assertMutability() {
        this.getOwner().assertMutability();
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        // Owner is excluded as to avoid cycles
        return super.hashCode();
    }

    /**
     * Compares this enum member's state against the state of the given member.
     * 
     * @param that The enum member to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(EnumMember<E, M> that) {
        // Owner is excluded as to avoid cycles
        return super.stateEquals(that);
    }

}
