package gutta.apievolution.core.apimodel;

import java.util.Collections;
import java.util.Optional;

class TestOperation extends Operation<TestApiDefinition, TestOperation, TestRecordType> {

    public TestOperation(String publicName, TestApiDefinition owner, TestRecordType returnType, TestRecordType parameterType) {        
        super(Collections.emptySet(), publicName, Optional.empty(), owner, returnType, parameterType);
    }

}
