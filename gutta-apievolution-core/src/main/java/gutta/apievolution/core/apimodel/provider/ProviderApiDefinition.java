package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.QualifiedName;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Provider-specific implementation of an {@link ApiDefinition}.
 */
public class ProviderApiDefinition extends ApiDefinition<ProviderApiDefinition>
        implements RevisionedElement<ProviderApiDefinition> {

    private final int revision;

    private final Optional<ProviderApiDefinition> predecessor;

    private Optional<ProviderApiDefinition> successor = Optional.empty();

    /**
     * Creates a new provider API definition from the given data.
     * @param name The name of the API definition
     * @param annotations The annotations of this API definition
     * @param revision The revision number of this API definition
     * @param predecessor The predecessor of this API definition
     */
    public ProviderApiDefinition(final QualifiedName name, final Set<Annotation> annotations,
                                 final int revision,
                                 final Optional<ProviderApiDefinition> predecessor) {
        super(name, annotations);

        this.predecessor = predecessor;
        this.revision = revision;

        predecessor.ifPresent(definition -> definition.setSuccessor(this));
    }

    /**
     * Returns the revision number of this API definition.
     * @return see above
     */
    public int getRevision() {
        return this.revision;
    }

    @Override
    public Optional<ProviderApiDefinition> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderApiDefinition> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderApiDefinition successor) {
        this.successor = Optional.of(successor);
    }

    /**
     * Performs the given action for each element of this API definition.
     * @param action The action to perform
     */
    public void forEach(Consumer<ProviderApiDefinitionElement> action) {
        // Iterate over all user-defined types and services
        this.getUserDefinedTypes().forEach(udt -> action.accept((ProviderApiDefinitionElement) udt));
        this.getServices().forEach(service -> action.accept((ProviderApiDefinitionElement) service));
    }

    @Override
    public int hashCode() {
        return super.hashCode() + this.revision;
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ProviderApiDefinition) {
            return this.stateEquals((ProviderApiDefinition) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderApiDefinition that) {
        return super.stateEquals(that) &&
                this.revision == that.revision;
    }

    @Override
    public String toString() {
        return "revision " + this.revision;
    }

    @Override
    protected void propagateInheritedFields() {
        List<ProviderRecordType> recordTypes = this.getUserDefinedTypes().stream()
                .filter(ProviderRecordType.class::isInstance)
                .map(ProviderRecordType.class::cast)
                .collect(Collectors.toList());

        ProviderInheritedFieldPropagator propagator = new ProviderInheritedFieldPropagator();
        propagator.propagateFieldsFor(recordTypes);
    }
}
