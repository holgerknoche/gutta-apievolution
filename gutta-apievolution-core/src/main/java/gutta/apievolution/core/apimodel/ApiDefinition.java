package gutta.apievolution.core.apimodel;

import java.util.*;

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

    private ApiDefinitionState state = ApiDefinitionState.UNDER_CONSTRUCTION;

    private final QualifiedName name;

    private final Set<Annotation> annotations;

    private final List<UserDefinedType<A>> userDefinedTypes;

    private final Map<String, UserDefinedType<A>> udtPublicNameLookup;

    private final Map<String, UserDefinedType<A>> udtInternalNameLookup;

    private final List<Service<A, ?, ?, ?>> services;

    private final Map<String, Service<A, ?, ?, ?>> serviceLookup;

    /**
     * Creates a new API definition from the given data.
     * @param name The name of the API definition
     * @param annotations Annotations of this API definition
     */
    protected ApiDefinition(final QualifiedName name, final Set<Annotation> annotations) {
        this.name = name;
        this.annotations = (annotations == null) ? new HashSet<>() : annotations;
        this.userDefinedTypes = new ArrayList<>();
        this.udtPublicNameLookup = new HashMap<>();
        this.udtInternalNameLookup = new HashMap<>();
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
    public Set<Annotation> getAnnotations() {
        return this.annotations;
    }

    /**
     * Asserts that this API definition is currently mutable, and throws an exception otherwise.
     */
    protected void assertMutability() {
        if (this.state != ApiDefinitionState.UNDER_CONSTRUCTION) {
            throw new IllegalStateException("An attempt was made to change an immutable API definition.");
        }
    }

    /**
     * Adds the given UDT to this API definition. Note that no UDT may be part of two different API definitions.
     * @param type The UDT to add
     */
    protected void addUserDefinedType(final UserDefinedType<A> type) {
        this.assertMutability();

        this.userDefinedTypes.add(type);
        this.udtPublicNameLookup.put(type.getPublicName(), type);
        this.udtInternalNameLookup.put(type.getInternalName(), type);
    }

    /**
     * Adds the given service to this API definition. Note that no service may be part of two different API definitions.
     * @param service The service to add
     */
    protected void addService(final Service<A, ?, ?, ?> service) {
        this.assertMutability();

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
     * Returns the services provided by this API definition.
     * @return see above
     */
    protected List<Service<A, ?, ?, ?>> getServices() {
        return this.services;
    }

    /**
     * Resolves a public name to an UDT provided by this API definition.
     * @param <T> The expected type of the UDT
     * @param name The public name of the UDT to resolve
     * @return The resolved type, if it exists
     */
    @SuppressWarnings("unchecked")
    public <T extends UserDefinedType<A>> Optional<T> resolveUserDefinedType(final String name) {
        return (Optional<T>) Optional.ofNullable(this.udtPublicNameLookup.get(name));
    }

    /**
     * Finds an UDT in this API definition by its internal name.
     * @param <T> The expected type of the UDT
     * @param internalName The internal name of the UDT to find
     * @return The resolved type, if it exists
     */
    @SuppressWarnings("unchecked")
    public <T extends UserDefinedType<A>> Optional<T> findUDTByInternalName(String internalName) {
        return (Optional<T>) Optional.ofNullable(this.udtInternalNameLookup.get(internalName));
    }

    /**
     * Resolves a name to a service provided by this API definition.
     * @param <S> The expected type of the service
     * @param name The name of the service to resolve
     * @return The resolved service, if it exists
     */
    @SuppressWarnings("unchecked")
    public <S extends Service<A, ?, ?, ?>> Optional<S> resolveService(final String name) {
        return (Optional<S>) Optional.ofNullable(this.serviceLookup.get(name));
    }

    /**
     * Finalizes this definition, i.e., performs all necessary checks to ensure that the definition is complete and
     * consistent. After finalization, the definition is immutable.
     */
    public void finalizeDefinition() {
        // Propagate inherited fields to the subtypes
        this.propagateInheritedFields();

        // Perform specific finalization actions, if any
        this.performSpecificFinalizationActions();

        this.state = ApiDefinitionState.FINALIZED;
    }

    /**
     * Propagates inherited fields to subtypes as part of the finalization process.
     */
    protected abstract void propagateInheritedFields();

    /**
     * Performs specific finalization actions. This is essentially a template method for
     * provider and consumer specializations.
     */
    protected void performSpecificFinalizationActions() {
        // Do nothing by default
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        return Objects.hash(this.name, this.annotations, this.userDefinedTypes, this.services);
    }

    /**
     * Checks whether the given object's state matches this one's. This method is used as part of
     * {@link #equals(Object)}.
     * @param that The object to compare against
     * @return {@code True}, if the state matches, {@code false} otherwise
     */
    protected boolean stateEquals(ApiDefinition<A> that) {
        return (this.state == that.state) &&
                this.name.equals(that.name) &&
                this.annotations.equals(that.annotations) &&
                this.userDefinedTypes.equals(that.userDefinedTypes) &&
                this.services.equals(that.services);
    }

}
