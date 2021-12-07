package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.ServiceOperation;

import java.util.Optional;

/**
 * Consumer-specific implementation of a {@link ServiceOperation}.
 */
public class ConsumerServiceOperation extends ServiceOperation<ConsumerService, ConsumerServiceOperation> {

    /**
     * Creates a new service operation from the given data.
     * @param publicName The operation's public name
     * @param internalName The operation's internal name, if any. Otherwise, the public name is assumed
     * @param owner The service that owns this operation
     */
    public ConsumerServiceOperation(final String publicName, final Optional<String> internalName,
                                    final ConsumerService owner) {
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
        } else if (that instanceof ConsumerServiceOperation) {
            return this.stateEquals((ConsumerServiceOperation) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerServiceOperation that) {
        return super.stateEquals(that);
    }

}
