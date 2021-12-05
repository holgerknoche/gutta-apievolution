package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Service;

import java.util.Optional;

/**
 * Provider-specific implementation of a {@link Service}.
 */
public class ProviderService extends Service<ProviderApiDefinition, ProviderService, ProviderServiceOperation>
        implements RevisionedElement<ProviderService>, ProviderApiDefinitionElement {

    private final Optional<ProviderService> predecessor;

    private Optional<ProviderService> successor;

    /**
     * Creates a new service from the given data.
     * @param publicName The service's public name
     * @param internalName The service's internal name, if any. Otherwise, the public name is assumed
     * @param owner The API definition that owns this service
     * @param predecessor The service's predecessor, if any
     */
    public ProviderService(final String publicName, final Optional<String> internalName,
                           final ProviderApiDefinition owner, final Optional<ProviderService> predecessor) {
        super(publicName, internalName, owner);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        predecessor.ifPresent(service -> service.setSuccessor(this));
    }

    @Override
    public Optional<ProviderService> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderService> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderService successor) {
        this.successor = Optional.of(successor);
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderService(this);
    }

}
