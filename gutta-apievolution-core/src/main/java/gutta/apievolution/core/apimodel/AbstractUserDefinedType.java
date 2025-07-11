package gutta.apievolution.core.apimodel;

/**
 * As the name implies, user-defined types (UDTs) are types that are given by
 * the user. Examples of such types are record and enumeration types.
 *
 * @param <A> The concrete type of the API definition this UDT resides in
 */
public abstract class AbstractUserDefinedType<A extends ApiDefinition<A, ?>> extends AbstractApiDefinitionElement 
    implements UserDefinedType<A> {

    private final int typeId;

    private final A owner;

    private Usage usage = Usage.NONE;

    /**
     * Creates a new UDT from the given data.
     *
     * @param publicName   The UDT's public name
     * @param internalName The UDT's internal name, if any (if {@code null}, the public
     *                     name is assumed)
     * @param typeId       The UDT's type id
     * @param owner        The API definition owning this UDT
     */
    protected AbstractUserDefinedType(final String publicName, final String internalName, final int typeId,
            final A owner) {
        super(publicName, internalName);

        this.owner = owner;
        this.typeId = typeId;
    }
    
    /**
     * Returns this UDT's type id.
     *
     * @return see above
     */
    public int getTypeId() {
        return this.typeId;
    }

    /**
     * Returns the API definition that owns this UDT.
     *
     * @return see above
     */
    public A getOwner() {
        return this.owner;
    }

    /**
     * Returns the usage of this user-defined type.
     *
     * @return see above
     */
    public Usage getUsage() {
        return this.usage;
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        // Owner is excluded as to avoid cycles
        return super.hashCode() + this.typeId;
    }

    boolean stateEquals(AbstractUserDefinedType<A> that) {
        // Owner is excluded as to avoid cycles
        return super.stateEquals(that) && this.typeId == that.typeId;
    }

    /**
     * Asserts that this type is mutable, and throws an exception otherwise.
     */
    protected void assertMutability() {
        this.getOwner().assertMutability();
    }

    void registerUsage(Usage usage) {
        this.assertMutability();

        var newUsage = this.usage.lubOfThisAnd(usage);
        
        if (newUsage != this.usage) {
            this.usage = newUsage;
            this.propagateUsageChange(newUsage);
        }
    }
    
    /**
     * Propagates the given change in usage to the sub-elements of this type.
     * 
     * @param newUsage The new usage of this type to be propagated to the sub-elements
     */
    protected void propagateUsageChange(Usage newUsage) {
        // Do nothing by default
    }

}
