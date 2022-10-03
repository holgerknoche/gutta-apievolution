package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.RecordKind;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Consumer-specific implementation of an {@link ApiDefinition}.
 */
public class ConsumerApiDefinition extends ApiDefinition<ConsumerApiDefinition, ConsumerOperation> {

    private final int referencedRevision;

    public static ConsumerApiDefinition create(String name, int referencedRevision) {
        return new ConsumerApiDefinition(name, Collections.emptySet(), referencedRevision);
    }
        
    /**
     * Creates an API definition from the given data.
     *
     * @param name               The API definition's name
     * @param annotations        The annotations on this API definition, if any
     * @param referencedRevision The referenced revision number
     */
    public ConsumerApiDefinition(String name, Set<Annotation> annotations, int referencedRevision) {
        super(name, annotations);
        
        this.referencedRevision = referencedRevision;
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
        
    public ConsumerEnumType newEnumType(String publicName, String internalName, int typeId) {
        return new ConsumerEnumType(publicName, internalName, typeId, this);
    }
    
    public ConsumerRecordType newRecordType(String publicName, String internalName, int typeId,
            Abstract abstractFlag, Set<ConsumerRecordType> superTypes) {
        return new ConsumerRecordType(publicName, internalName, typeId, this, abstractFlag, RecordKind.RECORD, superTypes);
    }
    
    public ConsumerRecordType newExceptionType(String publicName, String internalName, int typeId,
            Abstract abstractFlag, Set<ConsumerRecordType> superTypes) {
        return new ConsumerRecordType(publicName, internalName, typeId, this, abstractFlag, RecordKind.EXCEPTION, superTypes);
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
