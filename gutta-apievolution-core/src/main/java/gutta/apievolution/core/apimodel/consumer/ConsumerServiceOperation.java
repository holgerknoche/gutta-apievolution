package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.ServiceOperation;

import java.util.Optional;

/**
 * Consumer-specific implementation of a {@link ServiceOperation}.
 */
public class ConsumerServiceOperation extends ServiceOperation<ConsumerService, ConsumerServiceOperation,
        ConsumerRecordType> implements ConsumerApiDefinitionElement {

    /**
     * Creates a new service operation from the given data.
     * @param publicName The operation's public name
     * @param internalName The operation's internal name, if any. Otherwise, the public name is assumed
     * @param owner The service that owns this operation
     * @param returnType The operation's return type
     * @param parameterType The operation's parameter type
     */
    public ConsumerServiceOperation(final String publicName, final Optional<String> internalName,
                                    final ConsumerService owner, ConsumerRecordType returnType,
                                    ConsumerRecordType parameterType) {
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
        } else if (that instanceof ConsumerServiceOperation) {
            return this.stateEquals((ConsumerServiceOperation) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerServiceOperation that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerServiceOperation(this);
    }

}
