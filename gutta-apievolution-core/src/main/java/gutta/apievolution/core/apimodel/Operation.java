package gutta.apievolution.core.apimodel;

import java.util.*;

/**
 * A service operation represents a callable entity within a service. It may
 * take parameters, return a value or throw an exception. Restrictions may apply
 * to special cases.
 *
 * @param <A> The concrete API definition type that owns this operation
 * @param <O> The concrete operation type (e.g., provider or consumer)
 * @param <R> The concrete record type
 */
public abstract class Operation<A extends ApiDefinition<A>, O extends Operation<A, O, R>, 
        R extends RecordType<A, R, ?>> extends AbstractApiDefinitionElement {

    private final A owner;

    private final R returnType;

    private final R parameterType;

    private final Set<R> thrownExceptions = new LinkedHashSet<>();

    /**
     * Creates a new service operation from the given data.
     * 
     * @param publicName   The public name of the service operation
     * @param internalName The internal name of the service operation, if
     *                     applicable. If no internal name is given, the public name
     *                     is assumed
     * @param owner        The API definition that owns this operation
     */
    @SuppressWarnings("unchecked")
    protected Operation(final String publicName, final Optional<String> internalName, final A owner, R returnType,
            R parameterType) {
        super(publicName, internalName);

        this.owner = owner;
        this.returnType = returnType;
        this.parameterType = parameterType;

        returnType.registerUsage(Usage.OUTPUT);
        parameterType.registerUsage(Usage.INPUT);

        owner.addOperation((O) this);
    }

    /**
     * Returns the service that owns this service operation.
     * 
     * @return see above
     */
    public A getOwner() {
        return this.owner;
    }

    /**
     * Returns the operation's parameter type.
     * 
     * @return see above
     */
    public R getParameterType() {
        return this.parameterType;
    }

    /**
     * Returns the operation's return type.
     * 
     * @return see above
     */
    public R getReturnType() {
        return this.returnType;
    }

    /**
     * Returns the exceptions thrown by this operation.
     * 
     * @return see above
     */
    public Set<R> getThrownExceptions() {
        return this.thrownExceptions;
    }

    /**
     * Adds a thrown exception to this service operation.
     * 
     * @param exceptionType The exception type to add
     */
    public void addThrownException(R exceptionType) {
        this.assertMutability();

        if (!exceptionType.isException()) {
            throw new InvalidApiDefinitionException(exceptionType + " is no exception type.");
        }

        this.thrownExceptions.add(exceptionType);
    }

    void assertMutability() {
        this.getOwner().assertMutability();
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        // No owner in the hash code as to avoid cycles
        return super.hashCode();
    }

    /**
     * Compares this service operation's state against the state of the given
     * member.
     * 
     * @param that The service operation to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(Operation<A, O, R> that) {
        // No owner as to avoid cycles
        return super.stateEquals(that) && this.parameterType.equals(that.parameterType) &&
                this.returnType.equals(that.returnType) && this.thrownExceptions.equals(that.thrownExceptions);
    }

}
