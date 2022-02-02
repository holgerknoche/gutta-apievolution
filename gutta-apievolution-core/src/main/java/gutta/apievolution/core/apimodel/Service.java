package gutta.apievolution.core.apimodel;

import java.util.*;

/**
 * A Service represents a group of coherent service operations.
 * @param <A> The concrete type of the API definition this service belongs to
 * @param <S> The concrete service type of this service (e.g., provider or consumer)
 * @param <O> The concrete service operation type
 */
public abstract class Service<A extends ApiDefinition<A>, S extends Service<A, S, O, R>,
        O extends ServiceOperation<S, O, R>, R extends RecordType<A, R, ?>> extends AbstractApiDefinitionElement
        implements Iterable<O> {

    private final A owner;

    private final List<O> declaredOperations;

    private final Map<String, O> operationsLookup;

    /**
     * Creates a new service from the given data.
     * @param publicName The public name of the service
     * @param internalName The internal name of the service, if applicable. If no internal name is given, the public
     *                     name is assumed
     * @param owner The API definition that owns this service
     */
    protected Service(final String publicName, final Optional<String> internalName, final A owner) {
        super(publicName, internalName);

        this.owner = owner;
        this.declaredOperations = new ArrayList<>();
        this.operationsLookup = new HashMap<>();

        owner.addService(this);
    }

    /**
     * Returns the API definition that owns this service.
     * @return see above
     */
    public A getOwner() {
        return this.owner;
    }

    @Override
    public Iterator<O> iterator() {
        return this.declaredOperations.iterator();
    }

    private void assertMutability() {
        this.getOwner().assertMutability();
    }

    void addServiceOperation(final O operation) {
        this.assertMutability();

        this.declaredOperations.add(operation);
        this.operationsLookup.put(operation.getPublicName(), operation);
    }

    /**
     * Resolves a service operation by its name.
     * @param name The name of the desired service operation
     * @return The resolved service operation
     */
    public Optional<O> resolveServiceOperation(final String name) {
        return Optional.ofNullable(this.operationsLookup.get(name));
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        return super.hashCode() + this.declaredOperations.hashCode();
    }

    /**
     * Compares this service's state against the state of the given member.
     * @param that The service to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(Service<A, S, O, R> that) {
        return super.stateEquals(that) &&
                this.declaredOperations.equals(that.declaredOperations);
    }

}
