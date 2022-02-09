package gutta.apievolution.repository;

import gutta.apievolution.core.resolution.DefinitionResolution;

/**
 * Interface for classes that create a concrete representation of an API mapping represented by a definition resolution.
 */
public interface ApiMappingRepresentationCreator {

    /**
     * Creates the consumer-side mapping representation of the given resolution.
     * @param resolution The definition resolution to work with
     * @return The consumer-side mapping in an appropriate representation
     */
    byte[] createConsumerSideMapping(DefinitionResolution resolution);

    /**
     * Creates the provider-side mapping representation of the given resolution.
     * @param resolution The definition resolution to work with
     * @return The provider-side mapping in an appropriate representation
     */
    byte[] createProviderSideMapping(DefinitionResolution resolution);

    /**
     * Creates a full mapping representation of the given resolution, i.e., a representation of a mapping from the
     * client-internal to the provider-internal representation. This can be useful for in-process communication in
     * order to save the additional mapping step.
     * @param resolution The definition resolution to work with
     * @return The mapping in an appropriate representation
     */
    byte[] createFullMapping(DefinitionResolution resolution);

}
