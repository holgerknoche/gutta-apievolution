package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.ServiceOperation;

import java.util.Optional;

/**
 * Provider-specific implementation of a {@link ServiceOperation}.
 */
public class ProviderServiceOperation extends ServiceOperation<ProviderService, ProviderServiceOperation,
        ProviderRecordType> implements RevisionedElement<ProviderServiceOperation>, ProviderApiDefinitionElement {

    private final Optional<ProviderServiceOperation> predecessor;

    private Optional<ProviderServiceOperation> successor;

    /**
     * Creates a new service operation from the given data.
     * @param publicName The operation's public name
     * @param internalName The operation's internal name, if any. Otherwise, the public name is assumed
     * @param owner The service that owns this operation
     * @param returnType The operation's return type
     * @param parameterType The operation's parameter type
     * @param predecessor The operation's predecessor, if any
     */
    public ProviderServiceOperation(final String publicName, final Optional<String> internalName,
                                    final ProviderService owner, final ProviderRecordType returnType,
                                    final ProviderRecordType parameterType,
                                    final Optional<ProviderServiceOperation> predecessor) {
        super(publicName, internalName, owner, returnType, parameterType);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        predecessor.ifPresent(operation -> operation.setSuccessor(this));
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

    @Override
    public int hashCode() {
        // No predecessors or successors to avoid cycles
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ProviderServiceOperation) {
            return this.stateEquals((ProviderServiceOperation) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderServiceOperation that) {
        // No predecessors or successors to avoid cycles
        return super.stateEquals(that);
    }

}
