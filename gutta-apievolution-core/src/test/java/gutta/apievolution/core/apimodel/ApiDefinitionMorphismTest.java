package gutta.apievolution.core.apimodel;

import static gutta.apievolution.core.util.MapUtil.mapOf;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import gutta.apievolution.core.util.CheckResult;
import org.junit.jupiter.api.Test;

import java.util.Map;

class ApiDefinitionMorphismTest {
    
    /**
     * Test case: An unmapped type of a field is detected and reported.
     */
    @Test
    void missingTypeMappingForField() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");
        
        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);
        
        TestField fieldA = new TestField("fieldA", recordTypeA1, recordTypeA2);
        
        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");
        
        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);
        
        TestField fieldB = new TestField("fieldB", recordTypeB1, recordTypeB2);
        
        // Create an API morphism from A to B, leaving type A2 unmapped
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA1, recordTypeB1));
        Map<TestField, TestField> fieldMap = mapOf(fieldA, fieldB);
        
        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(typeMap, fieldMap, emptyMap(), emptyMap());
        
        // Check the consistency of the morphism
        CheckResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList("Type 'TypeA2' of mapped field 'fieldA' is not mapped."), result.getMessages());
    }
    
    /**
     * Test case: An incompatible type mapping of a field is detected and reported.
     */
    @Test
    void incompatibleMappingForField() {
     // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");
        
        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);
        
        TestField fieldA = new TestField("fieldA", recordTypeA1, recordTypeA2);
        
        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");
        
        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);
        
        TestField fieldB = new TestField("fieldB", recordTypeB1, recordTypeB2);
        
        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        Map<TestField, TestField> fieldMap = mapOf(fieldA, fieldB);
        
        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(typeMap, fieldMap, emptyMap(), emptyMap());
        
        // Check the consistency of the morphism
        CheckResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList("Type 'TypeA2' of mapped field 'fieldA' is mapped to incompatible type 'TypeB1' instead of 'TypeB2'."), result.getMessages());
    }
    
    private static class TestApiDefinitionMorphism extends ApiDefinitionMorphism<TestApiDefinition, 
        TestApiDefinition, TestUserDefinedType, TestUserDefinedType, TestField, TestField, TestEnumMember, 
        TestEnumMember, TestOperation, TestOperation> {

        public TestApiDefinitionMorphism(TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap,
                Map<TestField, TestField> fieldMap, Map<TestEnumMember, TestEnumMember> memberMap,
                Map<TestOperation, TestOperation> operationMap) {
            
            super(typeMap, fieldMap, memberMap, operationMap);
        }
        
    }
    
}
