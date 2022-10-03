package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Operation;

/**
 * Consumer-specific implementation of an {@link Operation}.
 */
public class ConsumerOperation extends Operation<ConsumerApiDefinition, ConsumerOperation, ConsumerRecordType>
        implements ConsumerApiDefinitionElement {

    public static ConsumerOperation create(String publicName, ConsumerApiDefinition owner, ConsumerRecordType returnType, ConsumerRecordType parameterType) {
        return new ConsumerOperation(publicName, null, owner, returnType, parameterType);
    }
    
    public static ConsumerOperation withInternalName(String publicName, String internalName, ConsumerApiDefinition owner, ConsumerRecordType returnType, ConsumerRecordType parameterType) {
        return new ConsumerOperation(publicName, internalName, owner, returnType, parameterType);
    }
    
    /**
     * Creates a new service operation from the given data.
     *
     * @param publicName    The operation's public name
     * @param internalName  The operation's internal name, if any. If {@code null} the
     *                      public name is assumed
     * @param owner         The API definition that owns this operation
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     */
    public ConsumerOperation(final String publicName, final String internalName,
            final ConsumerApiDefinition owner, ConsumerRecordType returnType, ConsumerRecordType parameterType) {
        super(publicName, internalName, owner, returnType, parameterType);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ConsumerOperation) {
            return this.stateEquals((ConsumerOperation) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerOperation that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerOperation(this);
    }

}
