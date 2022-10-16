package gutta.apievolution.core.apimodel;

import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;

class TestApiDefinition extends ApiDefinition<TestApiDefinition, TestOperation> {

    public TestApiDefinition(String name) {
        this(name, noAnnotations());
    }
    
    public TestApiDefinition(String name, Set<Annotation> annotations) {
        super(name, annotations);
    }

    @Override
    protected void propagateInheritedFields() {
        // Do nothing
    }

}
