package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;

import java.util.Collections;

/**
 * This class contains methods for creating common test fixtures. 
 */
class TestFixtures {
    
    private static final String DEFAULT_PROVIDER_API_NAME = "default";
    
    /**
     * Creates a new consumer API definition from the given data. A default name for the provider API is assumed.
     * 
     * @param name               The name of the definition
     * @param referencedRevision The referenced provider revision number
     * @return The newly created definition
     */
    static ConsumerApiDefinition createConsumerApiDefinition(String name, int referencedRevision) {
        return new ConsumerApiDefinition(name, Collections.emptySet(), DEFAULT_PROVIDER_API_NAME, referencedRevision);
    }

}
