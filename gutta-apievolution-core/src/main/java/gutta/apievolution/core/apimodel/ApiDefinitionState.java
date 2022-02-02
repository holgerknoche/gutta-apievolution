package gutta.apievolution.core.apimodel;

/**
 * Enumeration of states of an API definition.
 */
enum ApiDefinitionState {
    /**
     * Denotes that the API definition is still under construction and may be modified. In this state, the definition
     * may be incomplete or inconsistent.
     */
    UNDER_CONSTRUCTION,
    /**
     * Denotes that the API definition is finalized and may no longer be modified. In this state, the model is
     * guaranteed to be complete and consistent.
     */
    FINALIZED;
}
