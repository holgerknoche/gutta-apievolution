package gutta.apievolution.repository;

/**
 * Enumeration of the potential API mapping types.
 */
public enum ApiMappingType {
    /**
     * Represents a consumer-side mapping, i.e., a mapping between public and
     * consumer-internal representation.
     */
    CONSUMER,
    /**
     * Represents a provider-side mapping, i.e., a mapping between public and
     * provider-internal representation.
     */
    PROVIDER,
    /**
     * Represents a full mapping, i.e., a mapping between provider-internal and
     * consumer-internal representation.
     */
    FULL;
}
