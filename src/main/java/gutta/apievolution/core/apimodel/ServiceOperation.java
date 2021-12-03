package gutta.apievolution.core.apimodel;

import java.util.Optional;

/**
 * A service operation represents a callable entity within a service. It may take parameters, return a value or throw
 * an exception. Restrictions may apply to special cases.
 *
 * @param <S> The concrete service type (e.g., provider or consumer) used by the operation type
 * @param <O> The concrete service operation type (e.g., provider or consumer)
 */
public abstract class ServiceOperation<S extends Service<?, S, O>, O extends ServiceOperation<S, O>> extends ApiDefinitionElement {

	private final S owner;
	
	@SuppressWarnings("unchecked")
	protected ServiceOperation(final String publicName, final Optional<String> internalName, final S owner) {
		super(publicName, internalName);
		
		this.owner = owner;
		owner.addServiceOperation((O) this);
	}

	/**
	 * Returns the service that owns this service operation.
	 * @return see above
	 */
	public S getOwner() {
		return this.owner;
	}

}
