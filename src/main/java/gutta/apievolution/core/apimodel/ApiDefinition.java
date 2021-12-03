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
	
	protected ApiDefinition(final QualifiedName name, final List<Annotation> annotations) {
		this.name = name;
		this.annotations = annotations;
		this.userDefinedTypes = new ArrayList<>();
		this.udtLookup = new HashMap<>();
		this.services = new ArrayList<>();
		this.serviceLookup = new HashMap<>();
	}
	
	public QualifiedName getName() {
		return this.name;
	}
	
	public List<Annotation> getAnnotations() {
		return this.annotations;
	}
	
	protected void addUserDefinedType(final UserDefinedType<A> type) {
		this.userDefinedTypes.add(type);
		this.udtLookup.put(type.getPublicName(), type);
	}
	
	protected void addService(final Service<A, ?, ?> service) {
		this.services.add(service);
		this.serviceLookup.put(service.getPublicName(), service);
	}
		
	public List<UserDefinedType<A>> getUserDefinedTypes() {
		return this.userDefinedTypes;
	}
		
	@SuppressWarnings("unchecked")
	public <T extends UserDefinedType<A>> Optional<T> resolveUserDefinedType(final String name) {
		return (Optional<T>) Optional.ofNullable(this.udtLookup.get(name));
	}

	@SuppressWarnings("unchecked")
	public <S extends Service<A, ?, ?>> Optional<S> resolveService(final String name) {
		return (Optional<S>) Optional.ofNullable(this.serviceLookup.get(name));
	}
	
}
