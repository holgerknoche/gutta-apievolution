package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Operation;

import java.util.Optional;

/**
 * Consumer-specific implementation of an {@link Operation}.
 */
public class ConsumerOperation extends Operation<ConsumerApiDefinition, ConsumerOperation, ConsumerRecordType>
        implements ConsumerApiDefinitionElement {

    /**
     * Creates a new service operation from the given data.
     *
     * @param publicName    The operation's public name
     * @param internalName  The operation's internal name, if any. Otherwise, the
     *                      public name is assumed
     * @param owner         The API definition that owns this operation
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     */
    public ConsumerOperation(final String publicName, final Optional<String> internalName,
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
