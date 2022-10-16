package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.Operation;

import java.util.Optional;
import java.util.Set;

import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

/**
 * Provider-specific implementation of an {@link Operation}.
 */
public class ProviderOperation extends Operation<ProviderApiDefinition, ProviderOperation, ProviderRecordType>
        implements RevisionedElement<ProviderOperation>, ProviderApiDefinitionElement {

    private final ProviderOperation predecessor;

    private ProviderOperation successor;
    
    /**
     * Creates a new service operation from the given data.
     *
     * @param annotations   The annotations on this operation
     * @param publicName    The operation's public name
     * @param internalName  The operation's internal name, if any. If {@code null} the
     *                      public name is assumed
     * @param owner         The service that owns this operation
     * @param returnType    The operation's return type
     * @param parameterType The operation's parameter type
     * @param predecessor   The operation's predecessor, if any
     */
    ProviderOperation(Set<Annotation> annotations, final String publicName, final String internalName,
            final ProviderApiDefinition owner, final ProviderRecordType returnType,
            final ProviderRecordType parameterType, final ProviderOperation predecessor) {
        super(annotations, publicName, internalName, owner, returnType, parameterType);

        this.predecessor = predecessor;
        this.successor = null;

        ifPresent(predecessor, operation -> operation.setSuccessor(this));
    }

    @Override
    public Optional<ProviderOperation> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderOperation> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    private void setSuccessor(final ProviderOperation successor) {
        this.successor = successor;
    }

    @Override
    protected void addAnnotation(Annotation annotation) {
        super.addAnnotation(annotation);
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
