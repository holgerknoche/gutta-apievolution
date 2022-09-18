package gutta.apievolution.core.apimodel;

import java.util.Collections;
import java.util.Set;

class TestApiDefinition extends ApiDefinition<TestApiDefinition, TestOperation> {

    public TestApiDefinition(String name) {
        this(QualifiedName.of(name), Collections.emptySet());
    }
    
    public TestApiDefinition(QualifiedName name, Set<Annotation> annotations) {
        super(name, annotations);
    }

    @Override
    protected void propagateInheritedFields() {
        // Do nothing
    }

}
