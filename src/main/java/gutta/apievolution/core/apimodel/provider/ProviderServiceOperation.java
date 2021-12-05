package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.ServiceOperation;

import java.util.Optional;

/**
 * Provider-specific implementation of a {@link ServiceOperation}.
 */
public class ProviderServiceOperation extends ServiceOperation<ProviderService, ProviderServiceOperation>
        implements RevisionedElement<ProviderServiceOperation>, ProviderApiDefinitionElement {

    private final Optional<ProviderServiceOperation> predecessor;

    private Optional<ProviderServiceOperation> successor;

    /**
     * Creates a new service operation from the given data.
     * @param publicName The operation's public name
     * @param internalName The operation's internal name, if any. Otherwise, the public name is assumed
     * @param owner The service that owns this operation
     * @param predecessor The operation's predecessor, if any
     */
    public ProviderServiceOperation(final String publicName, final Optional<String> internalName,
                                    final ProviderService owner, final Optional<ProviderServiceOperation> predecessor) {
        super(publicName, internalName, owner);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        if (predecessor.isPresent()) {
            predecessor.get().setSuccessor(this);
        }
    }

    @Override
    public Optional<ProviderServiceOperation> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderServiceOperation> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderServiceOperation successor) {
        this.successor = Optional.of(successor);
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderServiceOperation(this);
    }

}
