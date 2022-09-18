package gutta.apievolution.core.apimodel;

import java.util.Optional;
import java.util.Set;

class TestOperation extends Operation<TestApiDefinition, TestOperation, TestRecordType> {

    public TestOperation(Set<Annotation> annotations, String publicName, Optional<String> internalName,
            TestApiDefinition owner, TestRecordType returnType, TestRecordType parameterType) {
        
        super(annotations, publicName, internalName, owner, returnType, parameterType);
    }

}
