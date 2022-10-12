package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.RecordKind;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;
import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

/**
 * Provider-specific implementation of an {@link ApiDefinition}.
 */
public class ProviderApiDefinition extends ApiDefinition<ProviderApiDefinition, ProviderOperation>
        implements RevisionedElement<ProviderApiDefinition> {

    private final int revision;

    private final ProviderApiDefinition predecessor;

    private ProviderApiDefinition successor;

    /**
     * Creates a new API definition from the given data.
     * 
     * @param name     The definition's name
     * @param revision The revision number
     * @return The created revision
     */
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
        this(QualifiedName.of(name), annotations, revision, predecessor);
    }

    /**
     * Creates a new provider API definition from the given data.
     * 
     * @param name        The name of the API definition
     * @param annotations The annotations of this API definition
     * @param revision    The revision number of this API definition
     * @param predecessor The predecessor of this API definition, if any
     */
    public ProviderApiDefinition(QualifiedName name, final Set<Annotation> annotations, final int revision,
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

    /**
     * Creates a new enum type in this definition.
     * 
     * @param publicName The type's public name
     * @param typeId     The type's id
     * @return The created enum type
     */
    public ProviderEnumType newEnumType(String publicName, int typeId) {
        return new ProviderEnumType(publicName, noInternalName(), typeId, this, noPredecessor());
    }

    /**
     * Creates a new enum type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param predecessor  The type's predecessor, if any
     * @return The created enum type
     */
    public ProviderEnumType newEnumType(String publicName, String internalName, int typeId,
            ProviderEnumType predecessor) {
        return new ProviderEnumType(publicName, internalName, typeId, this, predecessor);
    }

    /**
     * Creates a new record type in this definition.
     * 
     * @param publicName The type's public name
     * @param typeId     Thye type's id
     * @return The created record type
     */
    public ProviderRecordType newRecordType(String publicName, int typeId) {
        return this.newRecordType(publicName, noInternalName(), typeId, Abstract.NO, noSuperTypes(), noPredecessor());
    }

    /**
     * Creates a new record type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param predecessor  The type's predecessor, if any
     * @return The created record type
     */
    public ProviderRecordType newRecordType(String publicName, String internalName, int typeId,
            ProviderRecordType predecessor) {
        return this.newRecordType(publicName, internalName, typeId, Abstract.NO, noSuperTypes(), predecessor);
    }

    /**
     * Creates a new record type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param abstractFlag Denotes whether the type is abstract
     * @param superTypes   The super types of the type, if any
     * @param predecessor  The predecessor of the type, if any
     * @return The created record type
     */
    public ProviderRecordType newRecordType(String publicName, String internalName, int typeId, Abstract abstractFlag,
            Set<ProviderRecordType> superTypes, ProviderRecordType predecessor) {
        return this.newRecordOrExceptionType(publicName, internalName, typeId, abstractFlag, RecordKind.RECORD,
                superTypes, predecessor);
    }

    /**
     * Creates a new exception type in this definition.
     * 
     * @param publicName The type's public name
     * @param typeId     The type's id
     * @return The created exception type
     */
    public ProviderRecordType newExceptionType(String publicName, int typeId) {
        return this.newExceptionType(publicName, noInternalName(), typeId, noPredecessor());
    }

    /**
     * Creates a new exception type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param predecessor  The type's predecessor, if any
     * @return The created exception type
     */
    public ProviderRecordType newExceptionType(String publicName, String internalName, int typeId,
            ProviderRecordType predecessor) {
        return this.newExceptionType(publicName, internalName, typeId, Abstract.NO, noSuperTypes(), predecessor);
    }

    /**
     * Creates a new exception type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param abstractFlag Denotes whether the type is abstract
     * @param superTypes   The type's super types, if any
     * @param predecessor  The type's predecessor, if any
     * @return The created exception type
     */
    public ProviderRecordType newExceptionType(String publicName, String internalName, int typeId,
            Abstract abstractFlag, Set<ProviderRecordType> superTypes, ProviderRecordType predecessor) {
        return this.newRecordOrExceptionType(publicName, internalName, typeId, abstractFlag, RecordKind.EXCEPTION,
                superTypes, predecessor);
    }

    /**
     * Creates a new record or exception type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param abstractFlag Denotes whether the type is abstract
     * @param recordKind   Denotes the type of record to create (record or
     *                     exception)
     * @param superTypes   The type's super types, if any
     * @param predecessor  The type's predecessor, if any
     * @return The created type
     */
    public ProviderRecordType newRecordOrExceptionType(String publicName, String internalName, int typeId,
            Abstract abstractFlag, RecordKind recordKind, Set<ProviderRecordType> superTypes,
            ProviderRecordType predecessor) {
        return new ProviderRecordType(publicName, internalName, typeId, this, abstractFlag, recordKind, superTypes,
                predecessor);
    }

    /**
     * Creates a new operation in this definition.
     * 
     * @param publicName    The operation's public name
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     * @return The created operation
     */
    public ProviderOperation newOperation(String publicName, ProviderRecordType returnType,
            ProviderRecordType parameterType) {
        return this.newOperation(publicName, noInternalName(), returnType, parameterType, noPredecessor());
    }

    /**
     * Creates a new operation in this definition.
     * 
     * @param publicName    The operation's public name
     * @param internalName  The operation's internal name
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     * @param predecessor   The operation's predecessor, if any
     * @return The created operation
     */
    public ProviderOperation newOperation(String publicName, String internalName, ProviderRecordType returnType,
            ProviderRecordType parameterType, ProviderOperation predecessor) {
        return this.newOperation(noAnnotations(), publicName, internalName, returnType, parameterType, predecessor);
    }

    /**
     * Creates a new operation in this definition.
     * 
     * @param annotations   The annotations on the operation
     * @param publicName    The operation's public name
     * @param internalName  The operation's internal name
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     * @param predecessor   The operation's predecessor, if any
     * @return The created operation
     */
    public ProviderOperation newOperation(Set<Annotation> annotations, String publicName, String internalName,
            ProviderRecordType returnType, ProviderRecordType parameterType, ProviderOperation predecessor) {
        return new ProviderOperation(annotations, publicName, internalName, this, returnType, parameterType,
                predecessor);
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
