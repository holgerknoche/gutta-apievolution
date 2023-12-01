package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.RecordKind;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;

/**
 * Consumer-specific implementation of an {@link ApiDefinition}.
 */
public class ConsumerApiDefinition extends ApiDefinition<ConsumerApiDefinition, ConsumerOperation> {

    private final String referencedApiName;

    private final int referencedRevision;

    /**
     * Creates an API definition from the given data.
     *
     * @param name               The API definition's name
     * @param annotations        The annotations on this API definition, if any
     * @param referencedApiName  The name of the referenced provider API
     * @param referencedRevision The referenced revision number
     */
    public ConsumerApiDefinition(String name, Set<Annotation> annotations, String referencedApiName,
            int referencedRevision) {
        super(name, annotations);

        this.referencedApiName = referencedApiName;
        this.referencedRevision = referencedRevision;
    }

    /**
     * Creates a new customer API definition from the given data.
     * 
     * @param name               The API definition's name
     * @param annotations        The annotations on this API definition, if any
     * @param referencedApiName  The name of the referenced provider API
     * @param referencedRevision The referenced revision number
     */
    public ConsumerApiDefinition(QualifiedName name, Set<Annotation> annotations, String referencedApiName,
            int referencedRevision) {
        super(name, annotations);

        this.referencedApiName = referencedApiName;
        this.referencedRevision = referencedRevision;
    }

    /**
     * Returns the name of the referenced provider API.
     * 
     * @return see above
     */
    public String getReferencedApiName() {
        return this.referencedApiName;
    }

    /**
     * Returns the referenced revision number.
     *
     * @return see above
     */
    public int getReferencedRevision() {
        return this.referencedRevision;
    }

    // Element creators

    /**
     * Creates a new enum type in this definition.
     * 
     * @param publicName The type's public name
     * @param typeId     The type's id
     * @return The created enum type
     */
    public ConsumerEnumType newEnumType(String publicName, int typeId) {
        return this.newEnumType(publicName, noInternalName(), typeId);
    }

    /**
     * Creates a new enum type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @return The created enum type
     */
    public ConsumerEnumType newEnumType(String publicName, String internalName, int typeId) {
        return new ConsumerEnumType(publicName, internalName, typeId, this);
    }

    /**
     * Creates a new record type in this definition.
     * 
     * @param publicName The type's public name
     * @param typeId     The type's id
     * @return The created record type
     */
    public ConsumerRecordType newRecordType(String publicName, int typeId) {
        return this.newRecordType(publicName, noInternalName(), typeId, Abstract.NO, noSuperTypes());
    }

    /**
     * Creates a new record type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param abstractFlag Denotes whether this type is abstract
     * @param superTypes   The super types of the record
     * @return The created record type
     */
    public ConsumerRecordType newRecordType(String publicName, String internalName, int typeId, Abstract abstractFlag,
            Set<ConsumerRecordType> superTypes) {
        return this.newRecordOrExceptionType(publicName, internalName, typeId, abstractFlag, RecordKind.RECORD,
                superTypes);
    }

    /**
     * Creates a new exception type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param abstractFlag Denotes whether this type is abstract
     * @param superTypes   The super types of the exception
     * @return The created exception type
     */
    public ConsumerRecordType newExceptionType(String publicName, String internalName, int typeId,
            Abstract abstractFlag, Set<ConsumerRecordType> superTypes) {
        return this.newRecordOrExceptionType(publicName, internalName, typeId, abstractFlag, RecordKind.EXCEPTION,
                superTypes);
    }

    /**
     * Creates a new record or exception type in this definition.
     * 
     * @param publicName   The type's public name
     * @param internalName The type's internal name
     * @param typeId       The type's id
     * @param abstractFlag Denotes whether this type is abstract
     * @param recordKind   The type of record (record or exception) to create
     * @param superTypes   The super types of the type
     * @return The created type
     */
    public ConsumerRecordType newRecordOrExceptionType(String publicName, String internalName, int typeId,
            Abstract abstractFlag, RecordKind recordKind, Set<ConsumerRecordType> superTypes) {
        return new ConsumerRecordType(publicName, internalName, typeId, this, abstractFlag, recordKind, superTypes);
    }

    /**
     * Creates a new operation in this definition.
     * 
     * @param publicName    The operation's public name
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     * @return The created operation
     */
    public ConsumerOperation newOperation(String publicName, ConsumerRecordType returnType,
            ConsumerRecordType parameterType) {
        return this.newOperation(publicName, noInternalName(), returnType, parameterType);
    }

    /**
     * Creates a new operation in this definition.
     * 
     * @param publicName    The operation's public name
     * @param internalName  The operation's internal name
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     * @return The created operation
     */
    public ConsumerOperation newOperation(String publicName, String internalName, ConsumerRecordType returnType,
            ConsumerRecordType parameterType) {
        return new ConsumerOperation(publicName, internalName, this, returnType, parameterType);
    }

    @Override
    public int hashCode() {
        return super.hashCode() + this.referencedRevision;
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ConsumerApiDefinition) {
            return this.stateEquals((ConsumerApiDefinition) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerApiDefinition that) {
        return super.stateEquals(that) && this.referencedRevision == that.referencedRevision;
    }

    @Override
    protected void propagateInheritedFields() {
        List<ConsumerRecordType> recordTypes = this.getUserDefinedTypes().stream()
                .filter(ConsumerRecordType.class::isInstance).map(ConsumerRecordType.class::cast)
                .collect(Collectors.toList());

        ConsumerInheritedFieldPropagator propagator = new ConsumerInheritedFieldPropagator();
        propagator.propagateFieldsFor(recordTypes);
    }

}
