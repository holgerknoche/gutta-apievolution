package gutta.apievolution.core.apimodel.provider;

import static gutta.apievolution.core.apimodel.Conventions.*;
import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.RecordKind;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Provider-specific implementation of an {@link ApiDefinition}.
 */
public class ProviderApiDefinition extends ApiDefinition<ProviderApiDefinition, ProviderOperation>
        implements RevisionedElement<ProviderApiDefinition> {

    private final int revision;

    private final ProviderApiDefinition predecessor;

    private ProviderApiDefinition successor;

    public static ProviderApiDefinition create(String name, int revision) {
        return new ProviderApiDefinition(name, noAnnotations(), revision, noPredecessor());
    }
    
    /**
     * Creates a new provider API definition from the given data.
     *
     * @param name        The name of the API definition
     * @param annotations The annotations of this API definition
     * @param revision    The revision number of this API definition
     * @param predecessor The predecessor of this API definition, if any
     */
    public ProviderApiDefinition(final String name, final Set<Annotation> annotations, final int revision,
            final ProviderApiDefinition predecessor) {
        super(name, annotations);

        this.predecessor = predecessor;
        this.revision = revision;

        ifPresent(predecessor, definition -> definition.setSuccessor(this));
    }
    
    /**
     * Returns the revision number of this API definition.
     *
     * @return see above
     */
    public int getRevision() {
        return this.revision;
    }

    @Override
    public Optional<ProviderApiDefinition> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderApiDefinition> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    private void setSuccessor(final ProviderApiDefinition successor) {
        this.successor = successor;
    }

    /**
     * Performs the given action for each element of this API definition.
     *
     * @param action The action to perform
     */
    public void forEach(Consumer<ProviderApiDefinitionElement> action) {
        // Iterate over all user-defined types and operations
        this.getUserDefinedTypes().forEach(udt -> action.accept((ProviderApiDefinitionElement) udt));
        this.getOperations().forEach(operation -> action.accept((ProviderApiDefinitionElement) operation));
    }

    // Element creators
     
    public ProviderEnumType newEnumType(String publicName, int typeId) {
        return new ProviderEnumType(publicName, noInternalName(), typeId, this, noPredecessor());
    }
    
    public ProviderEnumType newEnumType(String publicName, String internalName, int typeId, ProviderEnumType predecessor) {
        return new ProviderEnumType(publicName, internalName, typeId, this, predecessor);
    }
    
    public ProviderRecordType newRecordType(String publicName, int typeId) {
        return this.newRecordType(publicName, noInternalName(), typeId, Abstract.NO, noSuperTypes(), noPredecessor());
    }
    
    public ProviderRecordType newRecordType(String publicName, String internalName, int typeId, ProviderRecordType predecessor) {
        return this.newRecordType(publicName, internalName, typeId, Abstract.NO, noSuperTypes(), predecessor);
    }
    
    public ProviderRecordType newRecordType(String publicName, String internalName, int typeId, Abstract abstractFlag,
            Set<ProviderRecordType> superTypes, ProviderRecordType predecessor) {
        return new ProviderRecordType(publicName, internalName, typeId, this, abstractFlag, RecordKind.RECORD,
                superTypes, predecessor);
    }
    
    public ProviderRecordType newExceptionType(String publicName, int typeId) {
        return this.newExceptionType(publicName, noInternalName(), typeId, noPredecessor());
    }
    
    public ProviderRecordType newExceptionType(String publicName, String internalName, int typeId,
            ProviderRecordType predecessor) {
        return this.newExceptionType(publicName, internalName, typeId, Abstract.NO, noSuperTypes(), predecessor);
    }
    
    public ProviderRecordType newExceptionType(String publicName, String internalName, int typeId, Abstract abstractFlag,
            Set<ProviderRecordType> superTypes, ProviderRecordType predecessor) {
        return new ProviderRecordType(publicName, internalName, typeId, this, abstractFlag, RecordKind.EXCEPTION,
                superTypes, predecessor);
    }
        
    @Override
    public int hashCode() {
        return super.hashCode() + this.revision;
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ProviderApiDefinition) {
            return this.stateEquals((ProviderApiDefinition) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderApiDefinition that) {
        return super.stateEquals(that) && this.revision == that.revision;
    }

    @Override
    public String toString() {
        return "revision " + this.revision;
    }

    @Override
    protected void propagateInheritedFields() {
        List<ProviderRecordType> recordTypes = this.getUserDefinedTypes().stream()
                .filter(ProviderRecordType.class::isInstance).map(ProviderRecordType.class::cast)
                .collect(Collectors.toList());

        ProviderInheritedFieldPropagator propagator = new ProviderInheritedFieldPropagator();
        propagator.propagateFieldsFor(recordTypes);
    }
}
