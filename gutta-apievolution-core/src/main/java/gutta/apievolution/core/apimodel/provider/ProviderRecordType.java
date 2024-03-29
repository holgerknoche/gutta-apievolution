package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.RecordKind;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.util.EqualityUtil;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noDeclaredPredecessors;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

/**
 * Provider-specific implementation of a {@link RecordType}.
 */
public class ProviderRecordType extends RecordType<ProviderApiDefinition, ProviderRecordType, ProviderField>
        implements RevisionedElement<ProviderRecordType>, ProviderUserDefinedType {

    private final ProviderRecordType predecessor;

    private ProviderRecordType successor;

    /**
     * Creates a new record type from the given data.
     *
     * @param publicName   The record type's public name
     * @param internalName The record type's internal name, if any. If {@code null}, the public name is assumed
     * @param typeId       The record type's type id
     * @param owner        The API definition that owns this record type
     * @param abstractFlag Denotes whether this type is abstract
     * @param exception    Denotes whether this type is an ordinary record or an exception
     * @param superTypes   This type's supertypes, if any
     * @param predecessor  The type's predecessor, if any
     */
    ProviderRecordType(final String publicName, final String internalName, final int typeId, final ProviderApiDefinition owner, final Abstract abstractFlag,
            RecordKind recordKind, final Set<ProviderRecordType> superTypes, final ProviderRecordType predecessor) {
        super(publicName, internalName, typeId, owner, abstractFlag, recordKind, superTypes);

        this.predecessor = predecessor;
        this.successor = null;

        ifPresent(predecessor, recordType -> recordType.setSuccessor(this));
    }

    @Override
    public Optional<ProviderRecordType> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderRecordType> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    private void setSuccessor(final ProviderRecordType successor) {
        this.successor = successor;
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderRecordType(this);
    }

    // Element creators

    /**
     * Creates a new field in this record type.
     * 
     * @param publicName  The field's public name
     * @param type        The field's type
     * @param optionality The field's optionality
     * @return The created field
     */
    public ProviderField newField(String publicName, Type type, Optionality optionality) {
        return new ProviderField(publicName, noInternalName(), this, type, optionality, Inherited.NO, noDeclaredPredecessors(), noPredecessor());
    }

    /**
     * Creates a new field in this record type.
     * 
     * @param publicName   The field's public name
     * @param internalName The field's internal name
     * @param type         The field's type
     * @param optionality  The field's optionality
     * @param predecessor  The field's predecessor, if any
     * @return The created field
     */
    public ProviderField newField(String publicName, String internalName, Type type, Optionality optionality, ProviderField predecessor) {
        return new ProviderField(publicName, internalName, this, type, optionality, Inherited.NO, noDeclaredPredecessors(), predecessor);
    }

    /**
     * Creates a new field in this record type.
     * 
     * @param publicName           The field's public name
     * @param internalName         The field's internal name
     * @param type                 The field's type
     * @param optionality          The field's optionality
     * @param inherited            Denotes whether this field is inherited
     * @param declaredPredecessors The field's declared predecessors, if any
     * @param predecessor          The field's predecessor, if any
     * @return The created field
     */
    public ProviderField newField(String publicName, String internalName, Type type, Optionality optionality, Inherited inherited,
            List<ProviderField> declaredPredecessors, ProviderField predecessor) {
        return new ProviderField(publicName, internalName, this, type, optionality, inherited, declaredPredecessors, predecessor);
    }

    @Override
    public String toString() {
        return this.getInternalName() + "@" + this.getOwner().toString();
    }

    @Override
    public int hashCode() {
        // No predecessor / successor to avoid cycles
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    boolean stateEquals(ProviderRecordType that) {
        // No predecessor / successor to avoid cycles
        return super.stateEquals(that);
    }

}
