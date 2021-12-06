package gutta.apievolution.core.apimodel.provider;

/**
 * Interface for all elements that can be part of a provider API definition.
 */
public interface ProviderApiDefinitionElement {

    /**
     * Accepts a given visitor for provider API definition elements.
     * @param visitor The visitor to accept
     * @param <R> The result type of the visitor's operation
     * @return The result of the visitor's operation, if any
     */
    <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor);

}
