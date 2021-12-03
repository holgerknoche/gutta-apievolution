package gutta.apievolution.core.apimodel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * An API definition is the root element of the API model. It serves as the container for its elements, such as
 * user-defined types and services. <p/>
 *
 * API definitions come in two different forms: provider API definitions and consumer API definitions. Both are
 * similar, however, provider API definitions have histories, which consumer API definitions have not.  Therefore,
 * this package contains the (mostly abstract) supertypes, which are concretized in the consumer and provider packages.
 * In order to operate on the right concrete types, there is a high number of type parameters in these abstract types,
 * which are set appropriately in the respective subtypes.<p/>
 *
 * @param <A> The concrete API definition type (e.g., provider or consumer).
 */
public abstract class ApiDefinition<A extends ApiDefinition<A>> {

    private final QualifiedName name;

    private final List<Annotation> annotations;

    private final List<UserDefinedType<A>> userDefinedTypes;

    private final Map<String, UserDefinedType<A>> udtLookup;

    private final List<Service<A, ?, ?>> services;

    private final Map<String, Service<A, ?, ?>> serviceLookup;

    /**
     * Creates a new API definition from the given data.
     * @param name The name of the API definition
     * @param annotations Annotations of this API definition
     */
    protected ApiDefinition(final QualifiedName name, final List<Annotation> annotations) {
        this.name = name;
        this.annotations = (annotations == null) ? new ArrayList<>() : annotations;
        this.userDefinedTypes = new ArrayList<>();
        this.udtLookup = new HashMap<>();
        this.services = new ArrayList<>();
        this.serviceLookup = new HashMap<>();
    }

    /**
     * Returns the name of this API definition.
     * @return see above
     */
    public QualifiedName getName() {
        return this.name;
    }

    /**
     * Returns the annotations on this API definition.
     * @return see above
     */
    public List<Annotation> getAnnotations() {
        return this.annotations;
    }

    /**
     * Adds the given UDT to this API definition. Note that no UDT may be part of two different API definitions.
     * @param type The UDT to add
     */
    protected void addUserDefinedType(final UserDefinedType<A> type) {
        this.userDefinedTypes.add(type);
        this.udtLookup.put(type.getPublicName(), type);
    }

    /**
     * Adds the given service to this API definition. Note that no service may be part of two different API definitions.
     * @param service The service to add
     */
    protected void addService(final Service<A, ?, ?> service) {
        this.services.add(service);
        this.serviceLookup.put(service.getPublicName(), service);
    }

    /**
     * Returns the UDTs provided by this API definition.
     * @return see above
     */
    public List<UserDefinedType<A>> getUserDefinedTypes() {
        return this.userDefinedTypes;
    }

    /**
     * Resolves a name to an UDT provided by this API definition.
     * @param <T> The expected type of the UDT
     * @param name The name of the UDT to resolve
     * @return The resolved type, if it exists
     */
    @SuppressWarnings("unchecked")
    public <T extends UserDefinedType<A>> Optional<T> resolveUserDefinedType(final String name) {
        return (Optional<T>) Optional.ofNullable(this.udtLookup.get(name));
    }

    /**
     * Resolves a name to a service provided by this API definition.
     * @param <S> The expected type of the service
     * @param name The name of the service to resolve
     * @return The resolved service, if it exists
     */
    @SuppressWarnings("unchecked")
    public <S extends Service<A, ?, ?>> Optional<S> resolveService(final String name) {
        return (Optional<S>) Optional.ofNullable(this.serviceLookup.get(name));
    }

}
