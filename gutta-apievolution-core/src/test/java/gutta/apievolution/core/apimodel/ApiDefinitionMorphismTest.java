package gutta.apievolution.core.apimodel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.validation.ValidationMessage;

class ApiDefinitionMorphismTest {

    /**
     * Test case: A valid morphism is accepted.
     */
    @Test
    void validMorphism() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        var fieldA = new TestField("fieldA", recordTypeA1, recordTypeA2);
        var operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        var fieldB = new TestField("fieldB", recordTypeB1, recordTypeB2);
        var operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(recordTypeA1, recordTypeB1, recordTypeA2, recordTypeB2));
        var fieldMap = Map.of(fieldA, fieldB);
        var operationMap = Map.of(operationA, operationB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, Map.of(), operationMap);

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertFalse(result.hasError());
        assertTrue(result.getMessages().isEmpty());
    }

    /**
     * Test case: An unmapped type of a field is detected and reported.
     */
    @Test
    void missingTypeMappingForField() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        var fieldA = new TestField("fieldA", recordTypeA1, recordTypeA2);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        var fieldB = new TestField("fieldB", recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B, leaving type A2 unmapped
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(recordTypeA1, recordTypeB1));
        var fieldMap = Map.of(fieldA, fieldB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, Map.of(), Map.of());

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Type 'TypeA2' of mapped field 'fieldA@TypeA1' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An incompatible type mapping of a field is detected and reported.
     */
    @Test
    void incompatibleTypeMappingForField() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA = new TestRecordType("TypeA", 0, definitionA);
        var recordTypeA1 = new TestRecordType("TypeA1", 1, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 2, definitionA);

        var fieldA = new TestField("fieldA", recordTypeA, recordTypeA2);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB = new TestRecordType("TypeB", 0, definitionB);
        var recordTypeB1 = new TestRecordType("TypeB1", 1, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 2, definitionB);

        var fieldB = new TestField("fieldB", recordTypeB, recordTypeB2);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(
                Map.of(recordTypeA, recordTypeB, recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        var fieldMap = Map.of(fieldA, fieldB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, Map.of(), Map.of());

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(
                List.of(ValidationMessage.error("Type 'TypeA2' of mapped field 'fieldA@TypeA' is mapped to incompatible type 'TypeB1' instead of 'TypeB2'.")),
                result.getMessages());
    }

    /**
     * Test case: A missing owner mapping of a field is detected and reported.
     */
    @Test
    void missingOwnerMappingForField() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA = new TestRecordType("TypeA", 0, definitionA);
        var recordTypeA1 = new TestRecordType("TypeA1", 1, definitionA);

        var fieldA = new TestField("fieldA", recordTypeA1, recordTypeA);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB = new TestRecordType("TypeB", 0, definitionB);
        var recordTypeB1 = new TestRecordType("TypeB1", 1, definitionB);

        var fieldB = new TestField("fieldB", recordTypeB1, recordTypeB);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(recordTypeA, recordTypeB));
        var fieldMap = Map.of(fieldA, fieldB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, Map.of(), Map.of());

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Record type 'TypeA1' containing field 'fieldA@TypeA1' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An incompatible owner mapping of a field is detected and reported.
     */
    @Test
    void incompatibleOwnerMappingForField() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA = new TestRecordType("TypeA", 0, definitionA);
        var recordTypeA1 = new TestRecordType("TypeA1", 1, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 2, definitionA);

        var fieldA = new TestField("fieldA", recordTypeA1, recordTypeA);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB = new TestRecordType("TypeB", 0, definitionB);
        var recordTypeB1 = new TestRecordType("TypeB1", 1, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 2, definitionB);

        var fieldB = new TestField("fieldB", recordTypeB1, recordTypeB);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(
                Map.of(recordTypeA, recordTypeB, recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        var fieldMap = Map.of(fieldA, fieldB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, Map.of(), Map.of());

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(
                List.of(
                        ValidationMessage.error("Record type 'TypeA1' containing field 'fieldA@TypeA1' is mapped to incompatible type 'TypeB2' instead of 'TypeB1'.")),
                result.getMessages());
    }

    /**
     * Test case: A missing owner mapping of an enum member is detected and reported.
     */
    @Test
    void missingOwnerMappingForEnumMember() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var enumTypeA = new TestEnumType("TypeA", 0, definitionA);

        var memberA = new TestEnumMember("memberA", enumTypeA);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var enumTypeB = new TestEnumType("TypeB", 0, definitionB);

        var memberB = new TestEnumMember("memberB", enumTypeB);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of());
        var memberMap = Map.of(memberA, memberB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, Map.of(), memberMap, Map.of());

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Enum type 'TypeA' containing member 'memberA' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An incompatible owner mapping of an enum member is detected and reported.
     */
    @Test
    void incompatibleOwnerMappingForEnumMember() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var enumTypeA = new TestEnumType("TypeA1", 0, definitionA);

        var memberA = new TestEnumMember("memberA", enumTypeA);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var enumTypeB1 = new TestEnumType("TypeB1", 0, definitionB);
        var enumTypeB2 = new TestEnumType("TypeB2", 1, definitionB);

        var memberB = new TestEnumMember("memberB", enumTypeB1);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(enumTypeA, enumTypeB2));
        var memberMap = Map.of(memberA, memberB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, Map.of(), memberMap, Map.of());

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(
                List.of(
                        ValidationMessage.error("Enum type 'TypeA1' containing member 'memberA' is mapped to incompatible type 'TypeB2' instead of 'TypeB1'.")),
                result.getMessages());
    }

    /**
     * Test case: A missing mapping of an operation's parameter type is detected and reported.
     */
    @Test
    void missingMappingForParameterType() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        var operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        var operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B, leaving type A2 unmapped
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(recordTypeA1, recordTypeB1));
        var operationMap = Map.of(operationA, operationB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, Map.of(), Map.of(), operationMap);

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Parameter type 'TypeA2' of mapped operation 'opA' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An invalid mapping of an operation's parameter and/or return types is detected and reported.
     */
    @Test
    void invalidMappingForParameterAndReturnType() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        var operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        var operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B with an incompatible type mapping
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        var operationMap = Map.of(operationA, operationB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, Map.of(), Map.of(), operationMap);

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Parameter type 'TypeA2' of mapped operation 'opA' is mapped to incompatible type 'TypeB1' instead of 'TypeB2'."),
                ValidationMessage.error("Return type 'TypeA1' of mapped operation 'opA' is mapped to incompatible type 'TypeB2' instead of 'TypeB1'.")), result.getMessages());
    }

    /**
     * Test case: A missing mapping of an operation's return type is detected and reported.
     */
    @Test
    void missingMappingForReturnType() {
        // Create definition A
        var definitionA = new TestApiDefinition("A");

        var recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        var recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        var operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        var definitionB = new TestApiDefinition("B");

        var recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        var recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        var operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B, leaving type A1 unmapped
        var typeMap = new TypeMap<TestUserDefinedType, TestUserDefinedType>(Map.of(recordTypeA2, recordTypeB2));
        var operationMap = Map.of(operationA, operationB);

        var morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, Map.of(), Map.of(), operationMap);

        // Check the consistency of the morphism
        var result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Return type 'TypeA1' of mapped operation 'opA' is not mapped.")), result.getMessages());
    }

    private static class TestApiDefinitionMorphism extends
            ApiDefinitionMorphism<TestApiDefinition, TestApiDefinition, TestUserDefinedType, TestUserDefinedType, TestField, TestField, TestEnumMember, TestEnumMember, TestOperation, TestOperation> {

        public TestApiDefinitionMorphism(TestApiDefinition sourceDefinition, TestApiDefinition targetDefinition,
                TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap, Map<TestField, TestField> fieldMap, Map<TestEnumMember, TestEnumMember> memberMap,
                Map<TestOperation, TestOperation> operationMap) {

            super(sourceDefinition, targetDefinition, typeMap, fieldMap, memberMap, operationMap);
        }

    }

}
