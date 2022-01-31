package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.ConcatenatedIterator;

import java.util.*;
import java.util.stream.Stream;

/**
 * A record type represents a user-defined record, i.e., a type that consists of fields.
 * @param <A> The concrete API definition type that owns this type
 * @param <R> The concrete record type (e.g., provider or consumer)
 * @param <F> The concrete field type for this record type
 */
public abstract class RecordType<A extends ApiDefinition<A>, R extends RecordType<A, R, F>, F extends Field<R, F>>
        extends UserDefinedType<A> implements Iterable<F> {

    private final boolean abstractFlag;

    private Optional<R> superType = Optional.empty();

    private final List<F> declaredFields;

    private final List<F> inheritedFields;

    private final Map<String, F> fieldLookup;

    private boolean subTypes;

    private Usage usage = Usage.NONE;

    /**
     * Creates a new record type from the given data.
     * @param publicName The public name of this record type
     * @param internalName The internal name of this record type, if applicable. Otherwise, the public name is assumed
     * @param typeId The type id of this record type
     * @param owner The API definition that owns this record type
     * @param abstractFlag A flag denoting whether this record type is abstract
     */
    protected RecordType(final String publicName, final Optional<String> internalName, final int typeId, final A owner,
                         final boolean abstractFlag) {
        this(publicName, internalName, typeId, owner, abstractFlag, Optional.empty());
    }

    /**
     * Creates a new record type from the given data.
     * @param publicName The public name of this record type
     * @param internalName The internal name of this record type, if applicable. Otherwise, the public name is assumed
     * @param typeId The type id of this record type
     * @param owner The API definition that owns this record type
     * @param abstractFlag A flag denoting whether this record type is abstract
     * @param superType An optional supertype for this record type
     */
    protected RecordType(final String publicName, final Optional<String> internalName, final int typeId, final A owner,
                      final boolean abstractFlag, final Optional<R> superType) {
        super(publicName, internalName, typeId, owner);

        this.declaredFields = new ArrayList<>();
        this.inheritedFields = new ArrayList<>();
        this.abstractFlag = abstractFlag;
        this.fieldLookup = new HashMap<>();

        owner.addUserDefinedType(this);

        superType.ifPresent(this::setSuperType);
    }

    /**
     * Returns the fields declared by this type.
     * @return see above
     */
    public List<F> getDeclaredFields() {
        return this.declaredFields;
    }

    /**
     * Returns all fields of this type, including inherited fields.
     * @return see above
     */
    public Stream<F> getFields() {
        return Stream.concat(
                this.inheritedFields.stream(),
                this.declaredFields.stream()
        );
    }

    /**
     * Sets the supertype for this record type. This method can only be called if no supertype
     * is currently assigned to this type.
     * @param superType The supertype to assign
     */
    @SuppressWarnings("unchecked")
    public void setSuperType(R superType) {
        this.assertMutability();

        if (this.superType.isPresent()) {
            throw new InvalidApiDefinitionException("There is already a supertype for " + this + ".");
        }

        this.superType = Optional.of(superType);
        superType.registerSubType((R) this);
    }

    /**
     * Adds a declared field to this record type.
     * @param field The field to add
     */
    protected void addDeclaredField(final F field) {
        this.assertMutability();

        this.declaredFields.add(field);
        this.fieldLookup.put(field.getPublicName(), field);
    }

    /**
     * Adds an inherited field to this record type.
     * @param field The field to add
     */
    protected void addInheritedField(final F field) {
        this.assertMutability();

        this.inheritedFields.add(field);
        this.fieldLookup.put(field.getPublicName(), field);
    }

    /**
     * Returns whether this record type is abstract.
     * @return see above
     */
    public boolean isAbstract() {
        return this.abstractFlag;
    }

    /**
     * Returns whether this record type has subtypes.
     * @return see above
     */
    public boolean hasSubTypes() {
        return this.subTypes;
    }

    /**
     * Returns whether this record type has a supertype.
     * @return see above
     */
    public boolean hasSuperType() {
        return this.superType.isPresent();
    }

    /**
     * Returns this record type's supertype.
     * @return see above
     */
    public Optional<R> getSuperType() {
        return this.superType;
    }

    /**
     * Resolves a field identified by its name within this record type.
     * @param name The name of the desired field
     * @return The resolved field, if it exists
     */
    public Optional<F> resolveField(final String name) {
        return Optional.ofNullable(this.fieldLookup.get(name));
    }

    void registerSubType(final R subType) {
        this.assertMutability();

        this.subTypes = true;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Iterator<F> iterator() {
        return new ConcatenatedIterator<>(this.inheritedFields, this.declaredFields);
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subtypes
        return super.hashCode() + Objects.hash(this.declaredFields, this.superType);
    }

    /**
     * Compares this record type's state against the state of the given member.
     * @param that The record type to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(RecordType<A, R, F> that) {
        return super.stateEquals(that) &&
                this.declaredFields.equals(that.declaredFields) &&
                this.subTypes == that.subTypes &&
                this.abstractFlag == that.abstractFlag &&
                this.superType.equals(that.superType);
    }

}
