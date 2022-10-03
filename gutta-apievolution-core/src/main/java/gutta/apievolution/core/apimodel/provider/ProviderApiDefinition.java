package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.QualifiedName;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

/**
 * Provider-specific implementation of an {@link ApiDefinition}.
 */
public class ProviderApiDefinition extends ApiDefinition<ProviderApiDefinition, ProviderOperation>
        implements RevisionedElement<ProviderApiDefinition> {

    private final int revision;

    private final ProviderApiDefinition predecessor;

    private ProviderApiDefinition successor;
    
    public static ProviderApiDefinition create(String name, int revision) {
        return new ProviderApiDefinition(name, Collections.emptySet(), revision, null);
    }
    
    public static ProviderApiDefinition withPredecessor(String name, int revision, ProviderApiDefinition predecessor) {
        return new ProviderApiDefinition(name, Collections.emptySet(), revision, predecessor);
    }
    
    /**
     * Creates a new provider API definition from the given data.
     *
     * @param name        The name of the API definition
     * @param annotations The annotations of this API definition
     * @param revision    The revision number of this API definition
     * @param predecessor The predecessor of this API definition, if any
     */
    public ProviderApiDefinition(final QualifiedName name, final Set<Annotation> annotations, final int revision,
            final ProviderApiDefinition predecessor) {
        super(name, annotations);

        this.predecessor = predecessor;
        this.revision = revision;

        ifPresent(predecessor, definition -> definition.setSuccessor(this));
    }

    /**
     * Creates a new provider API definition from the given data.
     * 
     * @param name The name of the API definition
     * @param annotations The annotations of this API definition
     * @param revision The revision number of this API definition
     * @param predecessor The predecessor of this API definition
     */
    public ProviderApiDefinition(String name, Set<Annotation> annotations, int revision, 
            ProviderApiDefinition predecessor) {
        this(QualifiedName.of(name), annotations, revision, predecessor);
    }
    
    /**
     * Returns the revision number of this API definition.
     *
     * @return see above
     */
    public int getRevision() {
        return this.revision;
    }

    @Override
    public Optional<ProviderApiDefinition> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderApiDefinition> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    private void setSuccessor(final ProviderApiDefinition successor) {
        this.successor = successor;
    }

    /**
     * Performs the given action for each element of this API definition.
     *
     * @param action The action to perform
     */
    public void forEach(Consumer<ProviderApiDefinitionElement> action) {
        // Iterate over all user-defined types and operations
        this.getUserDefinedTypes().forEach(udt -> action.accept((ProviderApiDefinitionElement) udt));
        this.getOperations().forEach(operation -> action.accept((ProviderApiDefinitionElement) operation));
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
        return super.stateEquals(that) && this.revision == that.revision;
    }

    @Override
    public String toString() {
        return "revision " + this.revision;
    }

    @Override
    protected void propagateInheritedFields() {
        List<ProviderRecordType> recordTypes = this.getUserDefinedTypes().stream()
                .filter(ProviderRecordType.class::isInstance).map(ProviderRecordType.class::cast)
                .collect(Collectors.toList());

        ProviderInheritedFieldPropagator propagator = new ProviderInheritedFieldPropagator();
        propagator.propagateFieldsFor(recordTypes);
    }
}
