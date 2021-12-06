package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Service;

import java.util.Optional;

/**
 * Consumer-specific implementation of a {@link Service}.
 */
public class ConsumerService extends Service<ConsumerApiDefinition, ConsumerService, ConsumerServiceOperation> {

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

}
