package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.validation.ValidationMessage;
import gutta.apievolution.core.validation.ValidationResult;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Map;

import static gutta.apievolution.core.util.MapUtil.mapOf;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiDefinitionMorphismTest {

    /**
     * Test case: A valid morphism is accepted.
     */
    @Test
    void validMorphism() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        TestField fieldA = new TestField("fieldA", recordTypeA1, recordTypeA2);
        TestOperation operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        TestField fieldB = new TestField("fieldB", recordTypeB1, recordTypeB2);
        TestOperation operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA1, recordTypeB1, recordTypeA2, recordTypeB2));
        Map<TestField, TestField> fieldMap = mapOf(fieldA, fieldB);
        Map<TestOperation, TestOperation> operationMap = mapOf(operationA, operationB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, emptyMap(), operationMap);

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertFalse(result.hasError());
        assertTrue(result.getMessages().isEmpty());
    }

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

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, emptyMap(), emptyMap());

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList(ValidationMessage.error("Type 'TypeA2' of mapped field 'fieldA@TypeA1' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An incompatible type mapping of a field is detected and reported.
     */
    @Test
    void incompatibleTypeMappingForField() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA = new TestRecordType("TypeA", 0, definitionA);
        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 1, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 2, definitionA);

        TestField fieldA = new TestField("fieldA", recordTypeA, recordTypeA2);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB = new TestRecordType("TypeB", 0, definitionB);
        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 1, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 2, definitionB);

        TestField fieldB = new TestField("fieldB", recordTypeB, recordTypeB2);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(
                mapOf(recordTypeA, recordTypeB, recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        Map<TestField, TestField> fieldMap = mapOf(fieldA, fieldB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, emptyMap(), emptyMap());

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(
                singletonList(ValidationMessage.error("Type 'TypeA2' of mapped field 'fieldA@TypeA' is mapped to incompatible type 'TypeB1' instead of 'TypeB2'.")),
                result.getMessages());
    }

    /**
     * Test case: A missing owner mapping of a field is detected and reported.
     */
    @Test
    void missingOwnerMappingForField() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA = new TestRecordType("TypeA", 0, definitionA);
        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 1, definitionA);

        TestField fieldA = new TestField("fieldA", recordTypeA1, recordTypeA);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB = new TestRecordType("TypeB", 0, definitionB);
        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 1, definitionB);

        TestField fieldB = new TestField("fieldB", recordTypeB1, recordTypeB);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA, recordTypeB));
        Map<TestField, TestField> fieldMap = mapOf(fieldA, fieldB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, emptyMap(), emptyMap());

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList(ValidationMessage.error("Record type 'TypeA1' containing field 'fieldA@TypeA1' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An incompatible owner mapping of a field is detected and reported.
     */
    @Test
    void incompatibleOwnerMappingForField() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA = new TestRecordType("TypeA", 0, definitionA);
        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 1, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 2, definitionA);

        TestField fieldA = new TestField("fieldA", recordTypeA1, recordTypeA);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB = new TestRecordType("TypeB", 0, definitionB);
        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 1, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 2, definitionB);

        TestField fieldB = new TestField("fieldB", recordTypeB1, recordTypeB);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(
                mapOf(recordTypeA, recordTypeB, recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        Map<TestField, TestField> fieldMap = mapOf(fieldA, fieldB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, fieldMap, emptyMap(), emptyMap());

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(
                singletonList(
                        ValidationMessage.error("Record type 'TypeA1' containing field 'fieldA@TypeA1' is mapped to incompatible type 'TypeB2' instead of 'TypeB1'.")),
                result.getMessages());
    }

    /**
     * Test case: A missing owner mapping of an enum member is detected and reported.
     */
    @Test
    void missingOwnerMappingForEnumMember() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestEnumType enumTypeA = new TestEnumType("TypeA", 0, definitionA);

        TestEnumMember memberA = new TestEnumMember("memberA", enumTypeA);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestEnumType enumTypeB = new TestEnumType("TypeB", 0, definitionB);

        TestEnumMember memberB = new TestEnumMember("memberB", enumTypeB);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(emptyMap());
        Map<TestEnumMember, TestEnumMember> memberMap = mapOf(memberA, memberB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, emptyMap(), memberMap, emptyMap());

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList(ValidationMessage.error("Enum type 'TypeA' containing member 'memberA' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An incompatible owner mapping of an enum member is detected and reported.
     */
    @Test
    void incompatibleOwnerMappingForEnumMember() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestEnumType enumTypeA = new TestEnumType("TypeA1", 0, definitionA);

        TestEnumMember memberA = new TestEnumMember("memberA", enumTypeA);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestEnumType enumTypeB1 = new TestEnumType("TypeB1", 0, definitionB);
        TestEnumType enumTypeB2 = new TestEnumType("TypeB2", 1, definitionB);

        TestEnumMember memberB = new TestEnumMember("memberB", enumTypeB1);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(enumTypeA, enumTypeB2));
        Map<TestEnumMember, TestEnumMember> memberMap = mapOf(memberA, memberB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, emptyMap(), memberMap, emptyMap());

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(
                singletonList(
                        ValidationMessage.error("Enum type 'TypeA1' containing member 'memberA' is mapped to incompatible type 'TypeB2' instead of 'TypeB1'.")),
                result.getMessages());
    }

    /**
     * Test case: A missing mapping of an operation's parameter type is detected and reported.
     */
    @Test
    void missingMappingForParameterType() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        TestOperation operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        TestOperation operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B, leaving type A2 unmapped
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA1, recordTypeB1));
        Map<TestOperation, TestOperation> operationMap = mapOf(operationA, operationB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, emptyMap(), emptyMap(), operationMap);

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList(ValidationMessage.error("Parameter type 'TypeA2' of mapped operation 'opA' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: An invalid mapping of an operation's parameter and/or return types is detected and reported.
     */
    @Test
    void invalidMappingForParameterAndReturnType() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        TestOperation operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        TestOperation operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B with an incompatible type mapping
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA1, recordTypeB2, recordTypeA2, recordTypeB1));
        Map<TestOperation, TestOperation> operationMap = mapOf(operationA, operationB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, emptyMap(), emptyMap(), operationMap);

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(Arrays.asList(ValidationMessage.error("Parameter type 'TypeA2' of mapped operation 'opA' is mapped to incompatible type 'TypeB1' instead of 'TypeB2'."),
                ValidationMessage.error("Return type 'TypeA1' of mapped operation 'opA' is mapped to incompatible type 'TypeB2' instead of 'TypeB1'.")), result.getMessages());
    }

    /**
     * Test case: A missing mapping of an operation's return type is detected and reported.
     */
    @Test
    void missingMappingForReturnType() {
        // Create definition A
        TestApiDefinition definitionA = new TestApiDefinition("A");

        TestRecordType recordTypeA1 = new TestRecordType("TypeA1", 0, definitionA);
        TestRecordType recordTypeA2 = new TestRecordType("TypeA2", 1, definitionA);

        TestOperation operationA = new TestOperation("opA", definitionA, recordTypeA1, recordTypeA2);

        // Create definition B
        TestApiDefinition definitionB = new TestApiDefinition("B");

        TestRecordType recordTypeB1 = new TestRecordType("TypeB1", 0, definitionB);
        TestRecordType recordTypeB2 = new TestRecordType("TypeB2", 1, definitionB);

        TestOperation operationB = new TestOperation("opB", definitionB, recordTypeB1, recordTypeB2);

        // Create an API morphism from A to B, leaving type A1 unmapped
        TypeMap<TestUserDefinedType, TestUserDefinedType> typeMap = new TypeMap<>(mapOf(recordTypeA2, recordTypeB2));
        Map<TestOperation, TestOperation> operationMap = mapOf(operationA, operationB);

        TestApiDefinitionMorphism morphism = new TestApiDefinitionMorphism(definitionA, definitionB, typeMap, emptyMap(), emptyMap(), operationMap);

        // Check the consistency of the morphism
        ValidationResult result = morphism.checkConsistency();

        assertTrue(result.hasError());
        assertEquals(singletonList(ValidationMessage.error("Return type 'TypeA1' of mapped operation 'opA' is not mapped.")), result.getMessages());
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
