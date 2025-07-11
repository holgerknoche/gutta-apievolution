package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.ConcatenatedIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Objects.requireNonNull;

/**
 * A record type represents a user-defined record, i.e., a type that consists of
 * fields.
 *
 * @param <A> The concrete API definition type that owns this type
 * @param <R> The concrete record type (e.g., provider or consumer)
 * @param <F> The concrete field type for this record type
 */
public abstract class RecordType<A extends ApiDefinition<A, ?>, R extends RecordType<A, R, F>, F extends Field<R, F>>
        extends AbstractUserDefinedType<A> implements Iterable<F> {

    private final boolean abstractFlag;

    private final RecordKind recordKind;

    private final Set<R> superTypes = new LinkedHashSet<>(); // We use a linked hash set for a predictable iteration order

    private final List<F> declaredFields;

    private final List<F> inheritedFields;

    private final Map<String, F> fieldLookup;

    private final Map<String, F> internalNameLookup;

    private final Set<R> subTypes;

    /**
     * Creates a new record type from the given data.
     *
     * @param publicName   The public name of this record type
     * @param internalName The internal name of this record type, if any. If
     *                     {@code null}, the public name is assumed
     * @param typeId       The type id of this record type
     * @param owner        The API definition that owns this record type
     * @param abstractFlag Denotes whether this type is abstract
     * @param recordKind   Denotes the kind of record to be created
     * @param superTypes   A (possibly empty) set of supertypes for this record type
     */
    protected RecordType(final String publicName, final String internalName, final int typeId, final A owner,
            final Abstract abstractFlag, RecordKind recordKind, final Set<R> superTypes) {
        super(publicName, internalName, typeId, owner);

        this.declaredFields = new ArrayList<>();
        this.inheritedFields = new ArrayList<>();
        this.abstractFlag = (requireNonNull(abstractFlag) == Abstract.YES);
        this.recordKind = requireNonNull(recordKind);
        this.fieldLookup = new HashMap<>();
        this.internalNameLookup = new HashMap<>();
        this.subTypes = new HashSet<>();

        owner.addUserDefinedType(this);

        if (superTypes != null) {
            superTypes.forEach(this::addSuperType);
        }
    }

    /**
     * Returns the fields declared by this type.
     *
     * @return see above
     */
    public List<F> getDeclaredFields() {
        return this.declaredFields;
    }

    /**
     * Returns all fields of this type, including inherited fields.
     *
     * @return see above
     */
    public Stream<F> getFields() {
        return Stream.concat(this.inheritedFields.stream(), this.declaredFields.stream());
    }

    /**
     * Adds a supertype for this record type.
     *
     * @param superType The supertype to add
     */
    @SuppressWarnings("unchecked")
    public void addSuperType(R superType) {
        this.assertMutability();

        if (this.isException() != superType.isException()) {
            throw new InvalidApiDefinitionException(
                    "The super type " + superType + " of " + this + " must be an exception.");
        }

        this.superTypes.add(superType);
        superType.registerSubType((R) this);
    }

    /**
     * Adds a declared field to this record type.
     *
     * @param field The field to add
     */
    protected void addDeclaredField(final F field) {
        this.assertMutability();

        this.declaredFields.add(field);
        this.fieldLookup.put(field.getPublicName(), field);
        this.internalNameLookup.put(field.getInternalName(), field);
    }

    /**
     * Adds an inherited field to this record type.
     *
     * @param field The field to add
     */
    protected void addInheritedField(final F field) {
        this.assertMutability();

        this.inheritedFields.add(field);
        this.fieldLookup.put(field.getPublicName(), field);
        this.internalNameLookup.put(field.getInternalName(), field);
    }

    /**
     * Returns whether this record type is abstract.
     *
     * @return see above
     */
    public boolean isAbstract() {
        return this.abstractFlag;
    }

    /**
     * Returns whether this record type is concrete.
     * 
     * @return see above
     */
    public boolean isConcrete() {
        return !(this.isAbstract());
    }
    
    @Override
    public boolean isRecord() {
        return true;
    }
    
    /**
     * Returns whether this record type is an exception.
     *
     * @return see above
     */
    public boolean isException() {
        return (this.recordKind == RecordKind.EXCEPTION);
    }

    /**
     * Returns the kind of this record type.
     * 
     * @return see above
     */
    public RecordKind getRecordKind() {
        return this.recordKind;
    }

    /**
     * Returns whether this record type has subtypes.
     *
     * @return see above
     */
    public boolean hasSubTypes() {
        return !(this.subTypes.isEmpty());
    }

    /**
     * Determines whether this record type is a supertype of the given type.
     * 
     * @param type The potential subtype
     * @return {@code True} if this type is a supertype of the given type, {@code false} otherwise
     */
    public boolean isSupertypeOf(RecordType<?, ?, ?> type) {
        return (this.equals(type)) || this.subTypes.contains(type);
    }
    
    /**
     * Returns the subtypes of this record type.
     * 
     * @return see above
     */
    public Set<R> getSubTypes() {
        return Collections.unmodifiableSet(this.subTypes);
    }
    
    /**
     * Returns the subtypes of this record type, optionally including this type.
     * 
     * @param inclusive Flag whether to include this type in the result
     * @return see above
     */
    @SuppressWarnings("unchecked")
    public Set<R> getSubTypes(Inclusive inclusive) {
        if (inclusive == Inclusive.YES) {
            Set<R> types = new HashSet<>(this.subTypes.size() + 1);
            types.add((R) this);
            types.addAll(this.subTypes);
            
            return types;
        } else {
            return this.getSubTypes();
        }
    }

    /**
     * Returns whether this record type has at least one supertype.
     *
     * @return see above
     */
    public boolean hasSuperTypes() {
        return !(this.superTypes.isEmpty());
    }

    /**
     * Returns this record type's supertypes.
     *
     * @return see above
     */
    public Set<R> getSuperTypes() {
        return this.superTypes;
    }

    /**
     * Resolves a field identified by its public name within this record type. Note
     * that in merged definitions, this mapping does not need to be unique, and only
     * one of the fields will be returned. In such definitions, use the internal
     * name instead.
     *
     * @param name The public name of the desired field
     * @return The resolved field, if it exists
     */
    public Optional<F> resolveField(final String name) {
        return Optional.ofNullable(this.fieldLookup.get(name));
    }

    /**
     * Resolves a field identified by its internal name within this record type.
     *
     * @param name The internal name of the desired field
     * @return The resolved field, if it exists
     */
    public Optional<F> resolveFieldByInternalName(String name) {
        return Optional.ofNullable(this.internalNameLookup.get(name));
    }

    void registerSubType(final R subType) {
        this.assertMutability();

        this.subTypes.add(subType);
    }

    @Override
    public Iterator<F> iterator() {
        return new ConcatenatedIterator<>(this.inheritedFields, this.declaredFields);
    }

    /**
     * Returns the set of all subtypes of this record type, including the type
     * itself, that match the given predicate.
     * 
     * @param selectionPredicate A predicate determining the types to be included in
     *                           the set
     * @return see above
     */
    public Set<R> collectAllSubtypes(Predicate<R> selectionPredicate) {
        Set<R> subTypes = new HashSet<>();

        this.collectAllSubtypesInternal(subTypes, selectionPredicate);

        return subTypes;
    }

    @SuppressWarnings("unchecked")
    void collectAllSubtypesInternal(Set<R> knownSubtypes, Predicate<R> selectionPredicate) {
        if (knownSubtypes.contains(this)) {
            return;
        }

        R currentType = (R) this;
        if (selectionPredicate.test(currentType)) {
            knownSubtypes.add(currentType);
        }

        this.subTypes.forEach(type -> type.collectAllSubtypesInternal(knownSubtypes, selectionPredicate));
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subtypes
        return this.getTypeId();
    }

    private Set<Integer> typeIdSet(Collection<R> types) {
        if (types.isEmpty()) {
            return Collections.emptySet();
        } else {
            return types.stream().map(RecordType::getTypeId).collect(Collectors.toSet());
        }
    }

    private Set<Integer> subTypeIds() {
        return this.typeIdSet(this.subTypes);
    }

    private Set<Integer> superTypeIds() {
        return this.typeIdSet(this.superTypes);
    }
    
    @Override
    protected void propagateUsageChange(Usage newUsage) {
        // Propagate the usage change to the supertypes
        this.getSuperTypes().forEach(superType -> superType.registerUsage(newUsage));
        
        // Propagate the usage change to the field types
        this.getFields().forEach(field -> {
            var fieldType = field.getType();
            
            if (fieldType instanceof AbstractUserDefinedType fieldUdt) {
                // If the field has a user-defined type, inform the type of the usage
                fieldUdt.registerUsage(newUsage);
            }
        });
    }
    
    /**
     * Compares this record type's state against the state of the given member.
     *
     * @param that The record type to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(RecordType<A, R, F> that) {
        // To avoid cycles, we only compare the type ids of the subtypes
        return super.stateEquals(that) && this.declaredFields.equals(that.declaredFields) &&
                this.subTypeIds().equals(that.subTypeIds()) && this.abstractFlag == that.abstractFlag &&
                this.superTypeIds().equals(that.superTypeIds());
    }

    @Override
    public final <X> X accept(TypeVisitor<X> visitor) {
        return visitor.handleRecordType(this);
    }

}
