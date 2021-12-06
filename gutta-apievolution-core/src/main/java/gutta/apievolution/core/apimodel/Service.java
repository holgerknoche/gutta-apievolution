package gutta.apievolution.core.apimodel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A Service represents a group of coherent service operations.
 * @param <A> The concrete type of the API definition this service belongs to
 * @param <S> The concrete service type of this service (e.g., provider or consumer)
 * @param <O> The concrete service operation type
 */
public abstract class Service<A extends ApiDefinition<A>, S extends Service<A, S, O>, O extends ServiceOperation<S, O>>
        extends ApiDefinitionElement {

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

    void addServiceOperation(final O operation) {
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

}
