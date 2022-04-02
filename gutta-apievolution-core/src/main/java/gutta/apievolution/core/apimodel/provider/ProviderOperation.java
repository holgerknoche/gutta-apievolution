package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.Operation;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;

/**
 * Provider-specific implementation of an {@link Operation}.
 */
public class ProviderOperation extends Operation<ProviderApiDefinition, ProviderOperation,
        ProviderRecordType> implements RevisionedElement<ProviderOperation>, ProviderApiDefinitionElement {

    private final Optional<ProviderOperation> predecessor;

    private Optional<ProviderOperation> successor;

    /**
     * Creates a new service operation from the given data.
     * @param publicName The operation's public name
     * @param internalName The operation's internal name, if any. Otherwise, the public name is assumed
     * @param owner The service that owns this operation
     * @param returnType The operation's return type
     * @param parameterType The operation's parameter type
     * @param predecessor The operation's predecessor, if any
     */
    public ProviderOperation(final String publicName, final Optional<String> internalName,
            final ProviderApiDefinition owner, final ProviderRecordType returnType,
            final ProviderRecordType parameterType,
            final Optional<ProviderOperation> predecessor) {
        this(Collections.emptySet(), publicName, internalName, owner, returnType, parameterType, predecessor);
    }
    
    /**
     * Creates a new service operation from the given data.
     * @param annotations The annotations on this operation
     * @param publicName The operation's public name
     * @param internalName The operation's internal name, if any. Otherwise, the public name is assumed
     * @param owner The service that owns this operation
     * @param returnType The operation's return type
     * @param parameterType The operation's parameter type
     * @param predecessor The operation's predecessor, if any
     */
    public ProviderOperation(Set<Annotation> annotations, final String publicName, 
            final Optional<String> internalName,
            final ProviderApiDefinition owner, final ProviderRecordType returnType,
            final ProviderRecordType parameterType,
            final Optional<ProviderOperation> predecessor) {
        super(annotations, publicName, internalName, owner, returnType, parameterType);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        predecessor.ifPresent(operation -> operation.setSuccessor(this));
    }

    @Override
    public Optional<ProviderOperation> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderOperation> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderOperation successor) {
        this.successor = Optional.of(successor);
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderOperation(this);
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
        } else if (that instanceof ProviderOperation) {
            return this.stateEquals((ProviderOperation) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderOperation that) {
        // No predecessors or successors to avoid cycles
        return super.stateEquals(that);
    }

}
