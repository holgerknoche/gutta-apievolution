package gutta.apievolution.core.apimodel;

import java.util.Collections;

class TestOperation extends Operation<TestApiDefinition, TestOperation, TestRecordType> {

    public TestOperation(String publicName, TestApiDefinition owner, TestRecordType returnType, TestRecordType parameterType) {        
        super(Collections.emptySet(), publicName, null, owner, returnType, parameterType);
    }

}
