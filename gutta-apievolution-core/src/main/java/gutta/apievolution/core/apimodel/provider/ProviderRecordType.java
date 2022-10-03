package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.RecordType;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static gutta.apievolution.core.util.UtilityFunctions.*;

/**
 * Provider-specific implementation of a {@link RecordType}.
 */
public class ProviderRecordType extends RecordType<ProviderApiDefinition, ProviderRecordType, ProviderField>
        implements RevisionedElement<ProviderRecordType>, ProviderUserDefinedType {

    private final ProviderRecordType predecessor;

    private ProviderRecordType successor;
       
    public static ProviderRecordType createRecordType(String publicName, int typeId, ProviderApiDefinition owner) {
        return recordWithInternalName(publicName, null, typeId, owner);
    }
    
    public static ProviderRecordType abstractRecord(String publicName, int typeId, ProviderApiDefinition owner) {
        return new ProviderRecordType(publicName, null, typeId, owner, true, false, Collections.emptySet(), null);
    }
        
    public static ProviderRecordType recordWithoutSupertype(String publicName, String internalName, int typeId, ProviderApiDefinition owner,
            boolean abstractFlag, ProviderRecordType predecessor) {
        return new ProviderRecordType(publicName, internalName, typeId, owner, abstractFlag, false, Collections.emptySet(), predecessor);
    }
    
    public static ProviderRecordType recordWithSuperType(String publicName, int typeId, ProviderApiDefinition owner, ProviderRecordType superType) {
        return new ProviderRecordType(publicName, null, typeId, owner, false, false, Collections.singleton(superType), null);
    }
    
    public static ProviderRecordType recordWithInternalName(String publicName, String internalName, int typeId, ProviderApiDefinition owner) {
        return new ProviderRecordType(publicName, internalName, typeId, owner, false, false, Collections.emptySet(), null); 
    }
    
    public static ProviderRecordType recordWithoutInternalName(String publicName, int typeId, ProviderApiDefinition owner, boolean abstractFlag,
            ProviderRecordType superType, ProviderRecordType predecessor) {
        return new ProviderRecordType(publicName, null, typeId, owner, abstractFlag, false, Collections.singleton(superType), predecessor);
    }
    
    public static ProviderRecordType recordWithPredecessor(String publicName, int typeId, ProviderApiDefinition owner, ProviderRecordType predecessor) {
        return new ProviderRecordType(publicName, null, typeId, owner, false, false, null, predecessor);
    }
        
    public static ProviderRecordType withoutSuperTypeOrPredecessor(String publicName, String internalName, int typeId, ProviderApiDefinition owner,
            boolean abstractFlag, boolean exception) {
        return new ProviderRecordType(publicName, internalName, typeId, owner, abstractFlag, exception, Collections.emptySet(), null);
    }
    
    public static ProviderRecordType createExceptionType(String publicName, int typeId, ProviderApiDefinition owner) {
        return exceptionWithInternalName(publicName, null, typeId, owner);
    }
    
    public static ProviderRecordType exceptionWithInternalName(String publicName, String internalName, int typeId, ProviderApiDefinition owner) {
        return new ProviderRecordType(publicName, internalName, typeId, owner, false, true, Collections.emptySet(), null); 
    }
    
    public static ProviderRecordType exceptionWithPredecessor(String publicName, int typeId, ProviderApiDefinition owner, ProviderRecordType prededessor) {
        return new ProviderRecordType(publicName, null, typeId, owner, false, true, Collections.emptySet(), prededessor);
    }
    
    /**
     * Creates a new record type from the given data.
     *
     * @param publicName   The record type's public name
     * @param internalName The record type's internal name, if any. If {@code null}, the
     *                     public name is assumed
     * @param typeId       The record type's type id
     * @param owner        The API definition that owns this record type
     * @param abstractFlag Denotes whether this type is abstract
     * @param exception    Denotes whether this type is an exception
     * @param superTypes   This type's supertypes, if any
     * @param predecessor  The type's predecessor, if any
     */
    public ProviderRecordType(final String publicName, final String internalName, final int typeId,
            final ProviderApiDefinition owner, final boolean abstractFlag, boolean exception,
            final Set<ProviderRecordType> superTypes, final ProviderRecordType predecessor) {
        super(publicName, internalName, typeId, owner, abstractFlag, exception, superTypes);

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
        if (this == that) {
            return true;
        } else if (that instanceof ProviderRecordType) {
            return this.stateEquals((ProviderRecordType) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderRecordType that) {
        // No predecessor / successor to avoid cycles
        return super.stateEquals(that);
    }

}
