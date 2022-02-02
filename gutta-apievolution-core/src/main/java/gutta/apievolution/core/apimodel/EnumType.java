package gutta.apievolution.core.apimodel;

import java.util.*;

/**
 * An enum type consists of a number of user-provided static values.
 * @param <A> The concrete type of the API definition that contains this enum type
 * @param <E> The concrete type of the enum type (e.g., provider or consumer)
 * @param <M> The concrete type of the enum members
 */
public abstract class EnumType<A extends ApiDefinition<A>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>>
        extends UserDefinedType<A> implements Iterable<M> {

    private final List<M> declaredMembers;

    private final Map<String, M> publicNameLookup;

    private final Map<String, M> internalNameLookup;

    /**
     * Creates a new enum type from the given data.
     * @param publicName The public name of the enum type
     * @param internalName The internal name of the enum type, if applicable. Otherwise, the public name is assumed
     * @param typeId The enum type's type id
     * @param owner The API definition that owns this enum type
     */
    protected EnumType(final String publicName, final Optional<String> internalName, final int typeId, final A owner) {
        super(publicName, internalName, typeId, owner);

        this.declaredMembers = new ArrayList<>();
        this.publicNameLookup = new HashMap<>();
        this.internalNameLookup = new HashMap<>();

        owner.addUserDefinedType(this);
    }

    /**
     * Returns the declared members of this enum type.
     * @return see above
     */
    public List<M> getDeclaredMembers() {
        return this.declaredMembers;
    }

    /**
     * Adds a member to this enum type.
     * @param member The member to add
     */
    protected void addDeclaredMember(final M member) {
        this.assertMutability();

        this.declaredMembers.add(member);
        this.publicNameLookup.put(member.getPublicName(), member);
        this.internalNameLookup.put(member.getInternalName(), member);
    }

    @Override
    public Iterator<M> iterator() {
        return this.getDeclaredMembers().iterator();
    }

    /**
     * Resolves the member based on its public name.
     * @param name The public name of the desired member
     * @return The resolved member, if it exists
     */
    public Optional<M> resolveMember(final String name) {
        return Optional.ofNullable(this.publicNameLookup.get(name));
    }

    /**
     * Finds a member based on its internal name.
     * @param internalName The internal name of the desired member
     * @return The member, if it exists
     */
    public Optional<M> findMemberByInternalName(String internalName) {
        return Optional.ofNullable(this.internalNameLookup.get(internalName));
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        return super.hashCode() + this.declaredMembers.hashCode();
    }

    /**
     * Compares this enum type's state against the state of the given member.
     * @param that The enum type to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(EnumType<A, E, M> that) {
        return super.stateEquals(that) &&
                this.declaredMembers.equals(that.declaredMembers);
    }

}
