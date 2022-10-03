package gutta.apievolution.core.apimodel.provider;

import static org.junit.jupiter.api.Assertions.assertEquals;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;

class ModelMergerTest {

    /**
     * Ensure that merging annotations works as expected.
     */
    @Test
    void testAnnotationMerging() {
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                new HashSet<>(Arrays.asList(new Annotation("a", "b"), new Annotation("b", "c"))), 0, null);

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                new HashSet<>(Arrays.asList(new Annotation("b", "x"), new Annotation("c", "d"))), 1, null);

        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedRevision = new ModelMerger().createMergedDefinition(revisionHistory);

        // We expect that annotations a to c exist, with the value for b from revision 2
        // as it is the "newer" revision
        Set<Annotation> expectedAnnotations = new HashSet<>(
                Arrays.asList(new Annotation("a", "b"), new Annotation("b", "x"), new Annotation("c", "d")));

        assertEquals(expectedAnnotations, mergedRevision.getAnnotations());
    }

    /**
     * Tests that merging the fields of a type across revisions works as expected.
     */
    @Test
    void testFieldMerging() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 1);

        ProviderRecordType testTypeV1 = ProviderRecordType.createRecordType("Test", 0, revision1);

        ProviderField unchangedFieldV1 = ProviderField.create("unchangedField", testTypeV1, StringType.unbounded(),
                Optionality.MANDATORY);
        ProviderField.create("typeChangeField", testTypeV1, AtomicType.INT_32, Optionality.MANDATORY);
        ProviderField.create("deletedField", testTypeV1, StringType.unbounded(), Optionality.MANDATORY);

        ProviderApiDefinition revision2 = ProviderApiDefinition.create("test", 2);

        ProviderRecordType testTypeV2 = ProviderRecordType.recordWithPredecessor("Test", 0, revision2, testTypeV1);

        ProviderField.withInternalName("typeChangeField", "newTypeChangeField", testTypeV2, AtomicType.INT_64,
                Optionality.MANDATORY);
        ProviderField.withPredecessor("unchangedField", null, testTypeV2, StringType.unbounded(), Optionality.MANDATORY,
                unchangedFieldV1);
        ProviderField.create("addedField", testTypeV2, StringType.unbounded(), Optionality.MANDATORY);

        // Merge the test revision history into a single definition
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        // Compare the created revision against the expected one. All optional fields
        // are opt-in because the type
        // is not used as output
        String expected = "api test [] {\n" + " record Test(Test) {\n" +
                "  optin typeChangeField(newTypeChangeField):int64\n" + // Must be opt-in due to the type change
                "  mandatory unchangedField(unchangedField):string\n" + // Must be mandatory as it does not change
                "  optin addedField(addedField):string\n" + // Must be opt-in because it is added
                "  optin typeChangeField(typeChangeField):int32\n" + // Must be opt-in due to type change
                "  optin deletedField(deletedField):string\n" + // Must be opt-in as it is deleted
                " }\n" + "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Tests that merging the members of an enumeration across revisions works as
     * expected.
     */
    @Test
    void testEnumMemberMerging() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 1);

        ProviderEnumType testEnumV1 = ProviderEnumType.create("Test", 0, revision1);

        ProviderEnumMember unchangedMember = ProviderEnumMember.create("UNCHANGED", testEnumV1);
        ProviderEnumMember.create("DELETED", testEnumV1);

        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 2, revision1);

        ProviderEnumType testEnumV2 = ProviderEnumType.withPredecessor("Test", null, 0, revision2,
                testEnumV1);

        ProviderEnumMember.withPredecessor("UNCHANGED", null, testEnumV2, unchangedMember);
        ProviderEnumMember.create("ADDED", testEnumV2);

        // Merge the test revision history into a single definition
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        // Compare the created revision against the expected one
        String expected = "api test [] {\n" + " enum Test(Test) {\n" + "  UNCHANGED(UNCHANGED)\n" + "  ADDED(ADDED)\n" +
                "  DELETED(DELETED)\n" + " }\n" + "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Tests the model merger's ability to perform a mapping merge, i.e., to provide
     * a map from a given revision of the history to the respective elements of the
     * merged revision.
     */
    @Test
    void testMappingMerge() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 1);

        ProviderRecordType testTypeV1 = ProviderRecordType.createRecordType("Test", 0, revision1);

        ProviderField unchangedFieldV1 = ProviderField.create("unchangedField", testTypeV1, StringType.unbounded(),
                Optionality.MANDATORY);
        ProviderField typeChangeFieldV1 = ProviderField.create("typeChangeField", testTypeV1, AtomicType.INT_32,
                Optionality.MANDATORY);
        ProviderField deletedFieldV1 = ProviderField.create("deletedField", testTypeV1, StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderEnumType testEnumV1 = ProviderEnumType.create("Test", 0, revision1);

        ProviderEnumMember unchangedMemberV1 = ProviderEnumMember.create("UNCHANGED", testEnumV1);
        ProviderEnumMember deletedMemberV1 = ProviderEnumMember.create("DELETED", testEnumV1);
        
        ProviderApiDefinition revision2 = ProviderApiDefinition.create("test", 2);

        ProviderRecordType testTypeV2 = new ProviderRecordType("Test", "TestInternal", 0, revision2, false, false,
                Collections.emptySet(), testTypeV1);

        ProviderField typeChangeFieldV2 = ProviderField.withInternalName("typeChangeField", "newTypeChangeField",
                testTypeV2, AtomicType.INT_64, Optionality.MANDATORY);
        ProviderField unchangedFieldV2 = new ProviderField("unchangedField", null, testTypeV2,
                StringType.unbounded(), Optionality.MANDATORY, false, Collections.singletonList(unchangedFieldV1),
                unchangedFieldV1);
        ProviderField addedFieldV2 = ProviderField.create("addedField", testTypeV2, StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderEnumType testEnumV2 = ProviderEnumType.withPredecessor("Test", null, 0, revision2, testEnumV1);

        ProviderEnumMember unchangedMemberV2 = ProviderEnumMember.withPredecessor("UNCHANGED", null, testEnumV2,
                unchangedMemberV1);
        ProviderEnumMember addedMemberV2 = ProviderEnumMember.create("ADDED", testEnumV2);

        // Merge the test revision history into a single definition
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);

        // Create and inspect a mapping from the first revision
        ToMergedModelMap map1 = new ModelMerger().createMergedDefinition(revisionHistory, revision1);

        // Inspect the mapped types
        ProviderRecordType mergedTestTypeV1 = map1.<ProviderRecordType>mapType(testTypeV1).get();
        assertEquals(testTypeV1.getPublicName(), mergedTestTypeV1.getPublicName());
        assertEquals(testTypeV2.getInternalName(), mergedTestTypeV1.getInternalName());

        ProviderEnumType mergedTestEnumV1 = map1.<ProviderEnumType>mapType(testEnumV1).get();
        assertEquals(testEnumV1.getPublicName(), mergedTestEnumV1.getPublicName());
        assertEquals(testEnumV1.getInternalName(), mergedTestEnumV1.getInternalName());

        // Inspect the mapped fields
        ProviderField mergedUnchangedFieldV1 = map1.mapField(unchangedFieldV1).get();
        assertEquals(unchangedFieldV1.getPublicName(), mergedUnchangedFieldV1.getPublicName());
        assertEquals(unchangedFieldV1.getInternalName(), mergedUnchangedFieldV1.getInternalName());
        assertEquals(unchangedFieldV1.getType(), map1.mapType(mergedUnchangedFieldV1.getType()).get());
        assertEquals(unchangedFieldV1.getOptionality(), mergedUnchangedFieldV1.getOptionality());

        ProviderField mergedTypeChangeFieldV1 = map1.mapField(typeChangeFieldV1).get();
        assertEquals(typeChangeFieldV1.getPublicName(), mergedTypeChangeFieldV1.getPublicName());
        assertEquals(typeChangeFieldV1.getInternalName(), mergedTypeChangeFieldV1.getInternalName());
        assertEquals(typeChangeFieldV1.getType(), map1.mapType(mergedTypeChangeFieldV1.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedTypeChangeFieldV1.getOptionality());

        ProviderField mergedDeletedFieldV1 = map1.mapField(deletedFieldV1).get();
        assertEquals(deletedFieldV1.getPublicName(), mergedDeletedFieldV1.getPublicName());
        assertEquals(deletedFieldV1.getInternalName(), mergedDeletedFieldV1.getInternalName());
        assertEquals(deletedFieldV1.getType(), map1.mapType(mergedDeletedFieldV1.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedDeletedFieldV1.getOptionality());

        // Inspect the mapped members
        ProviderEnumMember mergedUnchangedMemberV1 = map1.mapEnumMember(unchangedMemberV1).get();
        assertEquals(unchangedMemberV1.getPublicName(), mergedUnchangedMemberV1.getPublicName());
        assertEquals(unchangedMemberV1.getInternalName(), mergedUnchangedMemberV1.getInternalName());

        ProviderEnumMember mergedDeletedMemberV1 = map1.mapEnumMember(deletedMemberV1).get();
        assertEquals(deletedMemberV1.getPublicName(), mergedDeletedMemberV1.getPublicName());
        assertEquals(deletedMemberV1.getInternalName(), mergedDeletedMemberV1.getInternalName());

        // Create and inspect a mapping from the second revision
        ToMergedModelMap map2 = new ModelMerger().createMergedDefinition(revisionHistory, revision2);

        ProviderRecordType mergedTestTypeV2 = map2.<ProviderRecordType>mapType(testTypeV2).get();
        assertEquals(testTypeV2.getPublicName(), mergedTestTypeV2.getPublicName());
        assertEquals(testTypeV2.getInternalName(), mergedTestTypeV2.getInternalName());

        // Inspect the mapped fields
        ProviderField mergedUnchangedFieldV2 = map2.mapField(unchangedFieldV2).get();
        assertEquals(unchangedFieldV2.getPublicName(), mergedUnchangedFieldV2.getPublicName());
        assertEquals(unchangedFieldV2.getInternalName(), mergedUnchangedFieldV2.getInternalName());
        assertEquals(unchangedFieldV2.getType(), map1.mapType(mergedUnchangedFieldV2.getType()).get());
        assertEquals(unchangedFieldV2.getOptionality(), mergedUnchangedFieldV2.getOptionality());

        ProviderField mergedTypeChangeFieldV2 = map2.mapField(typeChangeFieldV2).get();
        assertEquals(typeChangeFieldV2.getPublicName(), mergedTypeChangeFieldV2.getPublicName());
        assertEquals(typeChangeFieldV2.getInternalName(), mergedTypeChangeFieldV2.getInternalName());
        assertEquals(typeChangeFieldV2.getType(), map1.mapType(mergedTypeChangeFieldV2.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedTypeChangeFieldV2.getOptionality());

        ProviderField mergedDeletedFieldV2 = map2.mapField(addedFieldV2).get();
        assertEquals(addedFieldV2.getPublicName(), mergedDeletedFieldV2.getPublicName());
        assertEquals(addedFieldV2.getInternalName(), mergedDeletedFieldV2.getInternalName());
        assertEquals(addedFieldV2.getType(), map2.mapType(mergedDeletedFieldV2.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedDeletedFieldV2.getOptionality());

        // Inspect the mapped members
        ProviderEnumMember mergedUnchangedMemberV2 = map2.mapEnumMember(unchangedMemberV2).get();
        assertEquals(unchangedMemberV2.getPublicName(), mergedUnchangedMemberV2.getPublicName());
        assertEquals(unchangedMemberV2.getInternalName(), mergedUnchangedMemberV2.getInternalName());

        ProviderEnumMember mergedDeletedMemberV2 = map2.mapEnumMember(addedMemberV2).get();
        assertEquals(addedMemberV2.getPublicName(), mergedDeletedMemberV2.getPublicName());
        assertEquals(addedMemberV2.getInternalName(), mergedDeletedMemberV2.getInternalName());
    }

    /**
     * Ensure that a normal evolution of a record type is not considered a type
     * change.
     */
    @Test
    void testNoTypeChangeOnRecordTypeEvolution() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType extendedTypeV1 = ProviderRecordType.createRecordType("ExtendedType", 0, revision1);

        ProviderField.create("field1", extendedTypeV1, StringType.bounded(10), Optionality.MANDATORY);

        ProviderRecordType usingTypeV1 = ProviderRecordType.createRecordType("UsingType", 1, revision1);

        ProviderField usingFieldV1 = ProviderField.create("usingField", usingTypeV1, extendedTypeV1,
                Optionality.MANDATORY);

        revision1.finalizeDefinition();

        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 1, revision1);

        ProviderRecordType extendedTypeV2 = ProviderRecordType.recordWithPredecessor("ExtendedType", 0,
                revision2, extendedTypeV1);

        // Type change on field 1
        ProviderField changedFieldV2 = ProviderField.withInternalName("field1", "fieldX", extendedTypeV2,
                StringType.bounded(20), Optionality.MANDATORY);

        ProviderRecordType usingTypeV2 = ProviderRecordType.recordWithPredecessor("UsingType", 1, revision2,
                usingTypeV1);

        ProviderField usingFieldV2 = ProviderField.withPredecessor("usingField", null, usingTypeV2, extendedTypeV2,
                Optionality.MANDATORY, usingFieldV1);

        revision2.finalizeDefinition();

        // Add a third revision without any changes as to have a merge of more than two
        // types (and thus the case that
        // the type of a field is compared against its pre-predecessor during the type
        // change check)
        ProviderApiDefinition revision3 = ProviderApiDefinition.withPredecessor("test", 2, revision2);

        ProviderRecordType extendedTypeV3 = ProviderRecordType.recordWithPredecessor("ExtendedType", 0, revision3,
                extendedTypeV2);

        ProviderField.withPredecessor("field1", "fieldX", extendedTypeV3, StringType.bounded(20),
                Optionality.MANDATORY, changedFieldV2);

        ProviderRecordType usingTypeV3 = ProviderRecordType.recordWithPredecessor("UsingType", 1, revision3,
                usingTypeV2);

        new ProviderField("usingField", null, usingTypeV3, extendedTypeV3, Optionality.MANDATORY, false,
                Collections.emptyList(), usingFieldV2);

        revision3.finalizeDefinition();

        // Merge the revision history
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        String expected = "api test [] {\n" + " record ExtendedType(ExtendedType) {\n" +
                "  optin field1(fieldX):string(20)\n" + "  optin field1(field1):string(10)\n" + " }\n" +
                " record UsingType(UsingType) {\n" + "  mandatory usingField(usingField):ExtendedType@revision 0\n" +
                " }\n" + "}\n";
        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);

        assertEquals(expected, actual);
    }

    /**
     * Test case: The newest internal name of an element is used in the merge.
     */
    @Test
    void newestInternalNameIsUsed() {
        // Build the first revision
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType recordTypeV1 = ProviderRecordType.recordWithInternalName("TypeA", "A1", 0, revision1);

        ProviderField providerFieldV1 = ProviderField.withInternalName("fieldA", "a1", recordTypeV1,
                StringType.unbounded(), Optionality.MANDATORY);

        ProviderEnumType enumTypeV1 = ProviderEnumType.withInternalName("EnumA", "E1", 1, revision1);

        ProviderEnumMember enumMemberV1 = ProviderEnumMember.withInternalName("MEMBER_A", "M1", enumTypeV1);

        revision1.finalizeDefinition();

        // Build the second revision
        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 1, revision1);

        ProviderRecordType recordTypeV2 = ProviderRecordType.recordWithoutSupertype("TypeA", "A2", 0, revision2, false,
                recordTypeV1);

        ProviderField.withPredecessor("fieldA", "a2", recordTypeV2, StringType.unbounded(), Optionality.MANDATORY,
                providerFieldV1);

        ProviderEnumType enumTypeV2 = ProviderEnumType.withPredecessor("EnumA", "E2", 1, revision2, enumTypeV1);

        ProviderEnumMember.withPredecessor("MEMBER_A", "M2", enumTypeV2, enumMemberV1);

        revision2.finalizeDefinition();

        // Merge the revisions and check the result
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        ProviderRecordType mergedRecordType = (ProviderRecordType) mergedDefinition.resolveUserDefinedType("TypeA")
                .orElseThrow(NoSuchElementException::new);
        ProviderField mergedField = mergedRecordType.resolveField("fieldA").orElseThrow(NoSuchElementException::new);

        assertEquals(1, mergedRecordType.getDeclaredFields().size());
        assertEquals("a2", mergedField.getInternalName());

        ProviderEnumType mergedEnumType = (ProviderEnumType) mergedDefinition.resolveUserDefinedType("EnumA")
                .orElseThrow(NoSuchElementException::new);
        ProviderEnumMember mergedMember = mergedEnumType.resolveMember("MEMBER_A")
                .orElseThrow(NoSuchElementException::new);

        assertEquals(1, mergedEnumType.getDeclaredMembers().size());
        assertEquals("M2", mergedMember.getInternalName());
    }

    /**
     * Test case: Inherited fields are handled correctly in the merge.
     */
    @Test
    void mergeWithInheritedFields() {
        // Build the first revision
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType superTypeV1 = ProviderRecordType.recordWithoutSupertype("SuperType", null, 0, revision1, true,
                null);

        ProviderField.create("inheritedField", superTypeV1, StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType subTypeAV1 = ProviderRecordType.recordWithSuperType("SubTypeA", 1, revision1, superTypeV1);

        revision1.finalizeDefinition();

        // Build the second revision
        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 1, revision1);

        ProviderRecordType superTypeV2 = ProviderRecordType.recordWithoutSupertype("SuperType", null, 0, revision2, true,
                superTypeV1);

        ProviderRecordType subTypeAV2 = ProviderRecordType.recordWithoutInternalName("SubTypeA", 1, revision2, false,
                superTypeV2, subTypeAV1);

        ProviderField.create("addedField", subTypeAV2, StringType.unbounded(), Optionality.MANDATORY);

        revision2.finalizeDefinition();

        // Merge the revisions and inspect the merged revision
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        String expected = "api test [] {\n" + " abstract record SuperType(SuperType) {\n" +
                "  optin inheritedField(inheritedField):string\n" + " }\n" +
                " record SubTypeA(SubTypeA) extends SuperType {\n" +
                "  inherited optin inheritedField(inheritedField):string\n" +
                "  optin addedField(addedField):string\n" + " }\n" + "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: When a field is restored to a previous state in a revision
     * history, the existing field can be reused in the merge (e.g., type change
     * from string to int and back should result in only two fields in the merge).
     */
    @Test
    void existingPredecessorReuse() {
        // Revision 1
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType recordTypeV1 = ProviderRecordType.createRecordType("TestType", 0, revision1);

        ProviderField.create("field", recordTypeV1, StringType.unbounded(), Optionality.MANDATORY);

        revision1.finalizeDefinition();

        // Revision 2: Change the field type to int32
        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 1, revision1);

        ProviderRecordType recordTypeV2 = ProviderRecordType.recordWithPredecessor("TestType", 0, revision2,
                recordTypeV1);

        // We need an internal name due to the type change
        ProviderField.withInternalName("field", "fieldInt", recordTypeV2, AtomicType.INT_32, Optionality.MANDATORY);

        revision2.finalizeDefinition();

        // Revision 3: Change the field type back to string
        ProviderApiDefinition revision3 = ProviderApiDefinition.withPredecessor("test", 2, revision2);

        ProviderRecordType recordTypeV3 = ProviderRecordType.recordWithPredecessor("TestType", 0, revision3,
                recordTypeV2);

        // No internal name, this should work as the field is reused
        ProviderField.create("field", recordTypeV3, StringType.unbounded(), Optionality.MANDATORY);

        revision3.finalizeDefinition();

        // Merge the revisions and inspect the merged revision
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        String expected = "api test [] {\n" + " record TestType(TestType) {\n" + "  optin field(field):string\n" +
                "  optin field(fieldInt):int32\n" + " }\n" + "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Merge a model with service operations and exceptions.
     */
    @Test
    void mergeModelWithServices() {
        // Revision 1
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType recordTypeV1 = ProviderRecordType.createRecordType("RecordType", 0, revision1);

        ProviderRecordType exceptionType1V1 = ProviderRecordType.createExceptionType("E1", 1, revision1);

        ProviderField.create("e1", exceptionType1V1, StringType.unbounded(), Optionality.MANDATORY);

        ProviderOperation operation1V1 = ProviderOperation.create("method1", revision1, recordTypeV1, recordTypeV1);

        operation1V1.addThrownException(exceptionType1V1);

        ProviderOperation.create("method2", revision1, recordTypeV1, recordTypeV1);

        revision1.finalizeDefinition();

        // Revision 2: Remove method2, add method3 and replace an exception on method 1
        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 1, revision1);

        ProviderRecordType recordTypeV2 = ProviderRecordType.recordWithPredecessor("RecordType", 0, revision2,
                recordTypeV1);

        ProviderRecordType exceptionType2V2 = ProviderRecordType.createExceptionType("E2", 2, revision2);

        ProviderField exceptionField2V2 = ProviderField.create("e2", exceptionType2V2, StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderOperation operation1V2 = ProviderOperation.withPredecessor("method1", revision2, recordTypeV2,
                recordTypeV2, operation1V1);

        operation1V2.addThrownException(exceptionType2V2);

        ProviderOperation operation3V2 = ProviderOperation.create("method3", revision2, recordTypeV2,
                recordTypeV2);

        revision2.finalizeDefinition();

        // Revision 3: Add an exception to method 3, exists mainly to detect errors with
        // respect to indirect
        // predecessors (which do not exist in revision history of length 2)
        ProviderApiDefinition revision3 = ProviderApiDefinition.create("test", 2);

        ProviderRecordType recordTypeV3 = ProviderRecordType.recordWithPredecessor("RecordType", 0, revision3,
                recordTypeV2);

        ProviderRecordType exceptionType2V3 = ProviderRecordType.exceptionWithPredecessor("E2", 2, revision3,
                exceptionType2V2);

        ProviderField.withPredecessor("e2", null, exceptionType2V3, StringType.unbounded(), Optionality.MANDATORY,
                exceptionField2V2);

        ProviderOperation operation1V3 = ProviderOperation.withPredecessor("method1", revision3, recordTypeV3,
                recordTypeV3, operation1V2);

        operation1V3.addThrownException(exceptionType2V3);

        ProviderOperation operation3V3 = ProviderOperation.withPredecessor("method3", revision3, recordTypeV3,
                recordTypeV3, operation3V2);

        operation3V3.addThrownException(exceptionType2V3);

        revision3.finalizeDefinition();

        // Create and merge the revision history
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        String expected = "api test [] {\n" + " record RecordType(RecordType) {\n" + " }\n" + " exception E2(E2) {\n" +
                "  optin e2(e2):string\n" + " }\n" + " exception E1(E1) {\n" + "  optin e1(e1):string\n" + " }\n" +
                " operation method1(method1) (RecordType@revision 0) : RecordType@revision 0 throws [E1@revision 0, E2@revision 0]\n" +
                " operation method3(method3) (RecordType@revision 0) : RecordType@revision 0 throws [E2@revision 0]\n" +
                " operation method2(method2) (RecordType@revision 0) : RecordType@revision 0\n" + "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

}
