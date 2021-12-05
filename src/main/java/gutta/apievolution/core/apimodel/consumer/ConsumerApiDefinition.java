package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.QualifiedName;

import java.util.List;

/**
 * Consumer-specific implementation of an {@link ApiDefinition}.
 */
public class ConsumerApiDefinition extends ApiDefinition<ConsumerApiDefinition> {

    private final int referencedRevision;

    /**
     * Creates an API definition from the given data.
     * @param name The API definition's name
     * @param annotations The annotations on this API definition, if any
     * @param referencedRevision The referenced revision number
     */
    public ConsumerApiDefinition(final QualifiedName name, final List<Annotation> annotations,
                                 final int referencedRevision) {
        super(name, annotations);

        this.referencedRevision = referencedRevision;
    }

    /**
     * Returns the referenced revision number.
     * @return see above
     */
    public int getReferencedRevision() {
        return this.referencedRevision;
    }

}
