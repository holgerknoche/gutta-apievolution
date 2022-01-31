package gutta.apievolution.core.apimodel;

import java.util.Set;

/**
 * Abstract supertype for the provider and consumer inherited field propagation.
 * @param <R> The concrete record type to use
 * @param <F> The concrete field type to use
 */
public abstract class InheritedFieldPropagator<R extends RecordType<?, R, F>, F extends Field<R, F>> {

    private Set<R> processedTypes;

}
