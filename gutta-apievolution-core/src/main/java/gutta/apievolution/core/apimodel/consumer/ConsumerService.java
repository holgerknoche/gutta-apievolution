package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Service;

import java.util.Optional;

/**
 * Consumer-specific implementation of a {@link Service}.
 */
public class ConsumerService extends Service<ConsumerApiDefinition, ConsumerService, ConsumerServiceOperation,
        ConsumerRecordType> implements ConsumerApiDefinitionElement {

    /**
     * Creates a new service from the given data.
     * @param publicName The service's public name
     * @param internalName The service's internal name, if any. Otherwise, the public name is assumed
     * @param owner The API definition that owns this service
     */
    public ConsumerService(final String publicName, final Optional<String> internalName,
                           final ConsumerApiDefinition owner) {
        super(publicName, internalName, owner);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ConsumerService) {
            return this.stateEquals((ConsumerService) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerService that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerService(this);
    }

}
