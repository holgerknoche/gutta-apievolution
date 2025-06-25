package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ModelMergerTest {

    /**
     * Ensure that merging annotations works as expected.
     */
    @Test
    void testAnnotationMerging() {
        var revision1 = new ProviderApiDefinition("a.b", Set.of(new Annotation("a", "b"), new Annotation("b", "c")), 0, null);

        revision1.finalizeDefinition();
        
        var revision2 = new ProviderApiDefinition("a.b", Set.of(new Annotation("b", "x"), new Annotation("c", "d")), 1, null);        
        
        revision2.finalizeDefinition();
        
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedRevision = new ModelMerger().createMergedDefinition(revisionHistory);

        // We expect that annotations a to c exist, with the value for b from revision 2
        // as it is the "newer" revision
        var expectedAnnotations = Set.of(new Annotation("a", "b"), new Annotation("b", "x"), new Annotation("c", "d"));

        assertEquals(expectedAnnotations, mergedRevision.getAnnotations());
    }

    /**
     * Tests that merging the fields of a type across revisions works as expected.
     */
    @Test
    void testFieldMerging() {
        var revision1 = ProviderApiDefinition.create("test", 1);

        var testTypeV1 = revision1.newRecordType("Test", 0);
        var unchangedFieldV1 = testTypeV1.newField("unchangedField", StringType.unbounded(),
                Optionality.MANDATORY);
        testTypeV1.newField("typeChangeField", AtomicType.INT_32, Optionality.MANDATORY);
        testTypeV1.newField("deletedField", StringType.unbounded(), Optionality.MANDATORY);
        
        var operationV1 = revision1.newOperation("op", testTypeV1, testTypeV1);

        revision1.finalizeDefinition();
        
        var revision2 = ProviderApiDefinition.create("test", 2);

        var testTypeV2 = revision2.newRecordType("Test", noInternalName(), 0, testTypeV1);
        testTypeV2.newField("typeChangeField", "newTypeChangeField", AtomicType.INT_64, Optionality.MANDATORY,
                noPredecessor());
        testTypeV2.newField("unchangedField", noInternalName(), StringType.unbounded(), Optionality.MANDATORY,
                unchangedFieldV1);
        testTypeV2.newField("addedField", StringType.unbounded(), Optionality.MANDATORY);

        revision2.newOperation("op", noInternalName(), testTypeV2, testTypeV2, operationV1);
        
        revision2.finalizeDefinition();
        
        // Merge the test revision history into a single definition
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        // Compare the created revision against the expected one. All optional fields
        // are opt-in because the type
        // is not used as output
        var expected = """
api test [] {
 record Test(Test) {
  optin typeChangeField(newTypeChangeField):int64
  mandatory unchangedField(unchangedField):string
  optin addedField(addedField):string
  optin typeChangeField(typeChangeField):int32
  optin deletedField(deletedField):string
 }
 operation op(op) (Test@revision 0) : Test@revision 0
}
""";                
 
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Tests that merging the members of an enumeration across revisions works as
     * expected.
     */
    @Test
    void testEnumMemberMerging() {
        var revision1 = ProviderApiDefinition.create("test", 1);

        var testEnumV1 = revision1.newEnumType("Test", 0);
        var unchangedMember = testEnumV1.newEnumMember("UNCHANGED");
        testEnumV1.newEnumMember("DELETED");

        revision1.finalizeDefinition();
        
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 2, revision1);

        var testEnumV2 = revision2.newEnumType("Test", noInternalName(), 0, testEnumV1);
        testEnumV2.newEnumMember("UNCHANGED", noInternalName(), unchangedMember);
        testEnumV2.newEnumMember("ADDED");

        revision2.finalizeDefinition();
        
        // Merge the test revision history into a single definition
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        // Compare the created revision against the expected one
        var expected = """
api test [] {
 enum Test(Test) {
  UNCHANGED(UNCHANGED)
  ADDED(ADDED)
  DELETED(DELETED)
 }
}
""";
                
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Tests the model merger's ability to perform a mapping merge, i.e., to provide
     * a map from a given revision of the history to the respective elements of the
     * merged revision.
     */
    @Test
    void testMappingMerge() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 1);

        var testTypeV1 = revision1.newRecordType("Test", 0);
        var unchangedFieldV1 = testTypeV1.newField("unchangedField", StringType.unbounded(),
                Optionality.MANDATORY);
        var typeChangeFieldV1 = testTypeV1.newField("typeChangeField", AtomicType.INT_32,
                Optionality.MANDATORY);
        var deletedFieldV1 = testTypeV1.newField("deletedField", StringType.unbounded(),
                Optionality.MANDATORY);

        var testEnumV1 = revision1.newEnumType("Test", 0);
        var unchangedMemberV1 = testEnumV1.newEnumMember("UNCHANGED");
        var deletedMemberV1 = testEnumV1.newEnumMember("DELETED");

        // Single operation so that the record type is used as input
        var operationV1 = revision1.newOperation("op", testTypeV1, testTypeV1);
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 2);

        var testTypeV2 = revision2.newRecordType("Test", "TestInternal", 0, testTypeV1);
        var typeChangeFieldV2 = testTypeV2.newField("typeChangeField", "newTypeChangeField",
                AtomicType.INT_64, Optionality.MANDATORY, noPredecessor());
        var unchangedFieldV2 = testTypeV2.newField("unchangedField", noInternalName(), StringType.unbounded(),
                Optionality.MANDATORY, Inherited.NO, List.of(unchangedFieldV1), unchangedFieldV1);
        var addedFieldV2 = testTypeV2.newField("addedField", StringType.unbounded(), Optionality.MANDATORY);

        var testEnumV2 = revision2.newEnumType("Test", noInternalName(), 0, testEnumV1);

        var unchangedMemberV2 = testEnumV2.newEnumMember("UNCHANGED", noInternalName(), unchangedMemberV1);
        var addedMemberV2 = testEnumV2.newEnumMember("ADDED");

        revision2.newOperation("op", noInternalName(), testTypeV2, testTypeV2, operationV1); 
        
        revision2.finalizeDefinition();
        
        // Merge the test revision history into a single definition
        var revisionHistory = new RevisionHistory(revision1, revision2);

        // Create and inspect a mapping from the first revision
        var map1 = new ModelMerger().createMergedDefinition(revisionHistory, revision1).map;

        // Inspect the mapped types
        var mergedTestTypeV1 = map1.<ProviderRecordType>mapType(testTypeV1).get();
        assertEquals(testTypeV1.getPublicName(), mergedTestTypeV1.getPublicName());
        assertEquals(testTypeV2.getInternalName(), mergedTestTypeV1.getInternalName());

        var mergedTestEnumV1 = map1.<ProviderEnumType>mapType(testEnumV1).get();
        assertEquals(testEnumV1.getPublicName(), mergedTestEnumV1.getPublicName());
        assertEquals(testEnumV1.getInternalName(), mergedTestEnumV1.getInternalName());

        // Inspect the mapped fields
        var mergedUnchangedFieldV1 = map1.mapField(unchangedFieldV1).get();
        assertEquals(unchangedFieldV1.getPublicName(), mergedUnchangedFieldV1.getPublicName());
        assertEquals(unchangedFieldV1.getInternalName(), mergedUnchangedFieldV1.getInternalName());
        assertEquals(unchangedFieldV1.getType(), map1.mapType(mergedUnchangedFieldV1.getType()).get());
        assertEquals(unchangedFieldV1.getOptionality(), mergedUnchangedFieldV1.getOptionality());

        var mergedTypeChangeFieldV1 = map1.mapField(typeChangeFieldV1).get();
        assertEquals(typeChangeFieldV1.getPublicName(), mergedTypeChangeFieldV1.getPublicName());
        assertEquals(typeChangeFieldV1.getInternalName(), mergedTypeChangeFieldV1.getInternalName());
        assertEquals(typeChangeFieldV1.getType(), map1.mapType(mergedTypeChangeFieldV1.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedTypeChangeFieldV1.getOptionality());

        var mergedDeletedFieldV1 = map1.mapField(deletedFieldV1).get();
        assertEquals(deletedFieldV1.getPublicName(), mergedDeletedFieldV1.getPublicName());
        assertEquals(deletedFieldV1.getInternalName(), mergedDeletedFieldV1.getInternalName());
        assertEquals(deletedFieldV1.getType(), map1.mapType(mergedDeletedFieldV1.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedDeletedFieldV1.getOptionality());

        // Inspect the mapped members
        var mergedUnchangedMemberV1 = map1.mapEnumMember(unchangedMemberV1).get();
        assertEquals(unchangedMemberV1.getPublicName(), mergedUnchangedMemberV1.getPublicName());
        assertEquals(unchangedMemberV1.getInternalName(), mergedUnchangedMemberV1.getInternalName());

        var mergedDeletedMemberV1 = map1.mapEnumMember(deletedMemberV1).get();
        assertEquals(deletedMemberV1.getPublicName(), mergedDeletedMemberV1.getPublicName());
        assertEquals(deletedMemberV1.getInternalName(), mergedDeletedMemberV1.getInternalName());

        // Create and inspect a mapping from the second revision
        var map2 = new ModelMerger().createMergedDefinition(revisionHistory, revision2).map;

        var mergedTestTypeV2 = map2.<ProviderRecordType>mapType(testTypeV2).get();
        assertEquals(testTypeV2.getPublicName(), mergedTestTypeV2.getPublicName());
        assertEquals(testTypeV2.getInternalName(), mergedTestTypeV2.getInternalName());

        // Inspect the mapped fields
        var mergedUnchangedFieldV2 = map2.mapField(unchangedFieldV2).get();
        assertEquals(unchangedFieldV2.getPublicName(), mergedUnchangedFieldV2.getPublicName());
        assertEquals(unchangedFieldV2.getInternalName(), mergedUnchangedFieldV2.getInternalName());
        assertEquals(unchangedFieldV2.getType(), map1.mapType(mergedUnchangedFieldV2.getType()).get());
        assertEquals(unchangedFieldV2.getOptionality(), mergedUnchangedFieldV2.getOptionality());

        var mergedTypeChangeFieldV2 = map2.mapField(typeChangeFieldV2).get();
        assertEquals(typeChangeFieldV2.getPublicName(), mergedTypeChangeFieldV2.getPublicName());
        assertEquals(typeChangeFieldV2.getInternalName(), mergedTypeChangeFieldV2.getInternalName());
        assertEquals(typeChangeFieldV2.getType(), map1.mapType(mergedTypeChangeFieldV2.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedTypeChangeFieldV2.getOptionality());

        var mergedDeletedFieldV2 = map2.mapField(addedFieldV2).get();
        assertEquals(addedFieldV2.getPublicName(), mergedDeletedFieldV2.getPublicName());
        assertEquals(addedFieldV2.getInternalName(), mergedDeletedFieldV2.getInternalName());
        assertEquals(addedFieldV2.getType(), map2.mapType(mergedDeletedFieldV2.getType()).get());
        assertEquals(Optionality.OPT_IN, mergedDeletedFieldV2.getOptionality());

        // Inspect the mapped members
        var mergedUnchangedMemberV2 = map2.mapEnumMember(unchangedMemberV2).get();
        assertEquals(unchangedMemberV2.getPublicName(), mergedUnchangedMemberV2.getPublicName());
        assertEquals(unchangedMemberV2.getInternalName(), mergedUnchangedMemberV2.getInternalName());

        var mergedDeletedMemberV2 = map2.mapEnumMember(addedMemberV2).get();
        assertEquals(addedMemberV2.getPublicName(), mergedDeletedMemberV2.getPublicName());
        assertEquals(addedMemberV2.getInternalName(), mergedDeletedMemberV2.getInternalName());
    }

    /**
     * Ensure that a normal evolution of a record type is not considered a type
     * change.
     */
    @Test
    void testNoTypeChangeOnRecordTypeEvolution() {
        var revision1 = ProviderApiDefinition.create("test", 0);

        var extendedTypeV1 = revision1.newRecordType("ExtendedType", 0);
        extendedTypeV1.newField("field1", StringType.bounded(10), Optionality.MANDATORY);

        var usingTypeV1 = revision1.newRecordType("UsingType", 1);
        var usingFieldV1 = usingTypeV1.newField("usingField", extendedTypeV1, Optionality.MANDATORY);
        
        var operationV1 = revision1.newOperation("op", usingTypeV1, usingTypeV1);

        revision1.finalizeDefinition();

        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var extendedTypeV2 = revision2.newRecordType("ExtendedType", noInternalName(), 0,
                extendedTypeV1);

        // Type change on field 1
        var changedFieldV2 = extendedTypeV2.newField("field1", "fieldX", StringType.bounded(20),
                Optionality.MANDATORY, noPredecessor());

        var usingTypeV2 = revision2.newRecordType("UsingType", noInternalName(), 1, usingTypeV1);
        var usingFieldV2 = usingTypeV2.newField("usingField", noInternalName(), extendedTypeV2,
                Optionality.MANDATORY, usingFieldV1);

        var operationV2 = revision2.newOperation("op", noInternalName(), usingTypeV2, usingTypeV2, operationV1);
        
        revision2.finalizeDefinition();

        // Add a third revision without any changes as to have a merge of more than two
        // types (and thus the case that
        // the type of a field is compared against its pre-predecessor during the type
        // change check)
        var revision3 = new ProviderApiDefinition("test", noAnnotations(), 2, revision2);

        var extendedTypeV3 = revision3.newRecordType("ExtendedType", noInternalName(), 0,
                extendedTypeV2);
        extendedTypeV3.newField("field1", "fieldX", StringType.bounded(20), Optionality.MANDATORY, changedFieldV2);

        var usingTypeV3 = revision3.newRecordType("UsingType", noInternalName(), 1, usingTypeV2);
        usingTypeV3.newField("usingField", noInternalName(), extendedTypeV3, Optionality.MANDATORY, usingFieldV2);

        revision3.newOperation("op", noInternalName(), usingTypeV3, usingTypeV3, operationV2);
        
        revision3.finalizeDefinition();

        // Merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 record ExtendedType(ExtendedType) {
  optin field1(fieldX):string(20)
  optin field1(field1):string(10)
 }
 record UsingType(UsingType) {
  mandatory usingField(usingField):ExtendedType@revision 0
 }
 operation op(op) (UsingType@revision 0) : UsingType@revision 0
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);

        assertEquals(expected, actual);
    }

    /**
     * Test case: The newest internal name of an element is used in the merge.
     */
    @Test
    void newestInternalNameIsUsed() {
        // Build the first revision
        var revision1 = ProviderApiDefinition.create("test", 0);

        var recordTypeV1 = revision1.newRecordType("TypeA", "A1", 0, noPredecessor());

        var providerFieldV1 = recordTypeV1.newField("fieldA", "a1", StringType.unbounded(),
                Optionality.MANDATORY, noPredecessor());

        var enumTypeV1 = revision1.newEnumType("EnumA", "E1", 1, noPredecessor());

        var enumMemberV1 = enumTypeV1.newEnumMember("MEMBER_A", "M1", noPredecessor());

        revision1.finalizeDefinition();

        // Build the second revision
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var recordTypeV2 = revision2.newRecordType("TypeA", "A2", 0, recordTypeV1);

        recordTypeV2.newField("fieldA", "a2", StringType.unbounded(), Optionality.MANDATORY, providerFieldV1);

        var enumTypeV2 = revision2.newEnumType("EnumA", "E2", 1, enumTypeV1);

        enumTypeV2.newEnumMember("MEMBER_A", "M2", enumMemberV1);

        revision2.finalizeDefinition();

        // Merge the revisions and check the result
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var mergedRecordType = (ProviderRecordType) mergedDefinition.resolveUserDefinedType("TypeA")
                .orElseThrow(NoSuchElementException::new);
        var mergedField = mergedRecordType.resolveField("fieldA").orElseThrow(NoSuchElementException::new);

        assertEquals(1, mergedRecordType.getDeclaredFields().size());
        assertEquals("a2", mergedField.getInternalName());

        var mergedEnumType = (ProviderEnumType) mergedDefinition.resolveUserDefinedType("EnumA")
                .orElseThrow(NoSuchElementException::new);
        var mergedMember = mergedEnumType.resolveMember("MEMBER_A")
                .orElseThrow(NoSuchElementException::new);

        assertEquals(1, mergedEnumType.getDeclaredMembers().size());
        assertEquals("M2", mergedMember.getInternalName());
    }

    /**
     * Test case: Inherited fields are handled correctly in the merge (simple case).
     */
    @Test
    void mergeWithInheritedFields() {
        // Build the first revision
        var revision1 = ProviderApiDefinition.create("test", 0);

        var superTypeV1 = revision1.newRecordType("SuperType", noInternalName(), 0, Abstract.YES,
                noSuperTypes(), noPredecessor());

        superTypeV1.newField("inheritedField", StringType.unbounded(), Optionality.MANDATORY);

        var subTypeAV1 = revision1.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO, Set.of(superTypeV1), noPredecessor());
        
        var operationV1 = revision1.newOperation("op", subTypeAV1, subTypeAV1);

        revision1.finalizeDefinition();

        // Build the second revision
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var superTypeV2 = revision2.newRecordType("SuperType", noInternalName(), 0, Abstract.YES, noSuperTypes(), superTypeV1);

        var subTypeAV2 = revision2.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO, Set.of(superTypeV2), subTypeAV1);

        subTypeAV2.newField("addedField", StringType.unbounded(), Optionality.MANDATORY);

        revision2.newOperation("op", noInternalName(), subTypeAV2, subTypeAV2, operationV1);
        
        revision2.finalizeDefinition();

        // Merge the revisions and inspect the merged revision
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 abstract record SuperType(SuperType) {
  optin inheritedField(inheritedField):string
 }
 record SubTypeA(SubTypeA) extends SuperType {
  inherited optin inheritedField(inheritedField):string
  optin addedField(addedField):string
 }
 operation op(op) (SubTypeA@revision 0) : SubTypeA@revision 0
}
"""; 
                
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Inherited fields are handled correctly when a super type is
     * changed.
     */
    @Test
    void mergeWithChangeOfSupertype() {
        // Build the first revision
        var revision1 = ProviderApiDefinition.create("test", 0);

        var superTypeV1 = revision1.newRecordType("SuperType", noInternalName(), 0, Abstract.YES, noSuperTypes(), noPredecessor());
        superTypeV1.newField("inheritedField", StringType.unbounded(), Optionality.MANDATORY);

        var subTypeAV1 = revision1.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO, Set.of(superTypeV1), noPredecessor());

        // Operation so that the subtype is used as input
        var operationV1 = revision1.newOperation("op", subTypeAV1, subTypeAV1);
        
        revision1.finalizeDefinition();

        // Build the second revision
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        // The super type is not related to the previous super type
        var newSuperTypeV2 = revision2.newRecordType("NewSuperType", noInternalName(), 2, Abstract.YES, noSuperTypes(), noPredecessor());

        newSuperTypeV2.newField("anotherField", StringType.unbounded(), Optionality.MANDATORY);

        var subTypeAV2 = revision2.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO, Set.of(newSuperTypeV2), subTypeAV1);
        subTypeAV2.newField("addedField", StringType.unbounded(), Optionality.MANDATORY);

        revision2.newOperation("op", noInternalName(), subTypeAV2, subTypeAV2, operationV1);
        
        revision2.finalizeDefinition();

        // Merge the revisions and inspect the merged revision
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 abstract record NewSuperType(NewSuperType) {
  mandatory anotherField(anotherField):string
 }
 record SubTypeA(SubTypeA) extends NewSuperType, SuperType {
  inherited optin anotherField(anotherField):string
  inherited optin inheritedField(inheritedField):string
  optin addedField(addedField):string
 }
 abstract record SuperType(SuperType) {
  mandatory inheritedField(inheritedField):string
 }
 operation op(op) (SubTypeA@revision 0) : SubTypeA@revision 0
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
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
        var revision1 = ProviderApiDefinition.create("test", 0);

        var recordTypeV1 = revision1.newRecordType("TestType", 0);
        recordTypeV1.newField("field", StringType.unbounded(), Optionality.MANDATORY);

        var operationV1 = revision1.newOperation("op", recordTypeV1, recordTypeV1);
        
        revision1.finalizeDefinition();

        // Revision 2: Change the field type to int32
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var recordTypeV2 = revision2.newRecordType("TestType", noInternalName(), 0, recordTypeV1);
        // We need an internal name due to the type change
        recordTypeV2.newField("field", "fieldInt", AtomicType.INT_32, Optionality.MANDATORY, noPredecessor());

        var operationV2 = revision2.newOperation("op", noInternalName(), recordTypeV2, recordTypeV2, operationV1);
        
        revision2.finalizeDefinition();

        // Revision 3: Change the field type back to string
        var revision3 = new ProviderApiDefinition("test", noAnnotations(), 2, revision2);

        var recordTypeV3 = revision3.newRecordType("TestType", noInternalName(), 0, recordTypeV2);
        // No internal name, this should work as the field is reused
        recordTypeV3.newField("field", StringType.unbounded(), Optionality.MANDATORY);
        
        revision3.newOperation("op", noInternalName(), recordTypeV3, recordTypeV3, operationV2);

        revision3.finalizeDefinition();

        // Merge the revisions and inspect the merged revision
        var revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 record TestType(TestType) {
  optin field(field):string
  optin field(fieldInt):int32
 }
 operation op(op) (TestType@revision 0) : TestType@revision 0
}
""";                 

        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Merge a model with operations and exceptions.
     */
    @Test
    void mergeModelWithOperations() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var recordTypeV1 = revision1.newRecordType("RecordType", 0);

        var exceptionType1V1 = revision1.newExceptionType("E1", 1);
        exceptionType1V1.newField("e1", StringType.unbounded(), Optionality.MANDATORY);

        var operation1V1 = revision1.newOperation("method1", recordTypeV1, recordTypeV1);
        operation1V1.addThrownException(exceptionType1V1);

        revision1.newOperation("method2", recordTypeV1, recordTypeV1);

        revision1.finalizeDefinition();

        // Revision 2: Remove method2, add method3 and replace an exception on method 1
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var recordTypeV2 = revision2.newRecordType("RecordType", noInternalName(), 0, recordTypeV1);

        var exceptionType2V2 = revision2.newExceptionType("E2", 2);
        var exceptionField2V2 = exceptionType2V2.newField("e2", StringType.unbounded(), Optionality.MANDATORY);

        var operation1V2 = revision2.newOperation("method1", noInternalName(), recordTypeV2, recordTypeV2, operation1V1);
        operation1V2.addThrownException(exceptionType2V2);

        var operation3V2 = revision2.newOperation("method3", recordTypeV2, recordTypeV2);

        revision2.finalizeDefinition();

        // Revision 3: Add an exception to method 3, exists mainly to detect errors with
        // respect to indirect predecessors (which do not exist in revision history of length 2)
        var revision3 = new ProviderApiDefinition("test", noAnnotations(), 2, revision2);

        var recordTypeV3 = revision3.newRecordType("RecordType", noInternalName(), 0, recordTypeV2);

        var exceptionType2V3 = revision3.newExceptionType("E2", noInternalName(), 2, exceptionType2V2);
        exceptionType2V3.newField("e2", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, exceptionField2V2);

        var operation1V3 = revision3.newOperation("method1", noInternalName(), recordTypeV3, recordTypeV3, operation1V2);
        operation1V3.addThrownException(exceptionType2V3);

        var operation3V3 = revision3.newOperation("method3", noInternalName(), recordTypeV3, recordTypeV3, operation3V2);
        operation3V3.addThrownException(exceptionType2V3);

        revision3.finalizeDefinition();

        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 record RecordType(RecordType) {
 }
 exception E2(E2) {
  mandatory e2(e2):string
 }
 exception E1(E1) {
  mandatory e1(e1):string
 }
 operation method1(method1) (RecordType@revision 0) : RecordType@revision 0 throws [E1@revision 0, E2@revision 0]
 operation method3(method3) (RecordType@revision 0) : RecordType@revision 0 throws [E2@revision 0]
 operation method2(method2) (RecordType@revision 0) : RecordType@revision 0
}
"""; 
                        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
        
    /**
     * Test case: Pulling up a field into a supertype works as expected.
     */
    @Test
    void pullUpField() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);
        
        var recordTypeV1 = revision1.newRecordType("RecordType", 0);
        var fieldV1 = recordTypeV1.newField("field", AtomicType.INT_32, Optionality.MANDATORY);
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);
        
        var superTypeV2 = revision2.newRecordType("SuperType", 1);
        superTypeV2.newField("field", noInternalName(), AtomicType.INT_32, Optionality.MANDATORY, Inherited.NO, List.of(fieldV1), fieldV1);
        
        revision2.newRecordType("RecordType", noInternalName(), 0, Abstract.NO, Set.of(superTypeV2), recordTypeV1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record SuperType(SuperType) {
  mandatory field(field):int32
 }
 record RecordType(RecordType) extends SuperType {
  inherited mandatory field(field):int32
 }
}
""";
                        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * An unchanged operation is identified as such and results in a single operation in the merged model.
     */
    @Test
    void noTypeChangeOfOperation() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);
        
        var parameterTypeV1 = revision1.newRecordType("ParameterType", 0);
        var returnTypeV1 = revision1.newRecordType("ResultType", 1);
        
        var operationV1 = revision1.newOperation("operation", returnTypeV1, parameterTypeV1);
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);
        
        var parameterTypeV2 = revision2.newRecordType("ParameterType", noInternalName(), 0, parameterTypeV1);
        var returnTypeV2 = revision2.newRecordType("ResultType", noInternalName(), 1, returnTypeV1);
        
        revision2.newOperation("operation", noInternalName(), returnTypeV2, parameterTypeV2, operationV1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record ParameterType(ParameterType) {
 }
 record ResultType(ResultType) {
 }
 operation operation(operation) (ParameterType@revision 0) : ResultType@revision 0
}
"""; 
                        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Type change of an operation results in two different operations in the merged model.
     */
    @Test
    void typeChangeOfOperation() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);
        
        var recordTypeV1 = revision1.newRecordType("RecordType1", 0);
        recordTypeV1.newField("field1", AtomicType.INT_32, Optionality.MANDATORY);
        
        revision1.newOperation("operation", recordTypeV1, recordTypeV1);
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1);
        
        var recordTypeV2 = revision2.newRecordType("RecordType2", 1);
        recordTypeV2.newField("field2", AtomicType.INT_32, Optionality.MANDATORY);
        
        revision2.newOperation("operation", "operation2", recordTypeV2, recordTypeV2, noPredecessor());
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record RecordType2(RecordType2) {
  mandatory field2(field2):int32
 }
 record RecordType1(RecordType1) {
  mandatory field1(field1):int32
 }
 operation operation(operation2) (RecordType2@revision 0) : RecordType2@revision 0
 operation operation(operation) (RecordType1@revision 0) : RecordType1@revision 0
}
""";

        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);       
    }
    
    /**
     * Test case: The abstractness of types in the merged model is handled as expected.
     */
    @Test
    void abstractnessOfTypes() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);
        
        // Define one abstract and two concrete types
        var recordType1V1 = revision1.newRecordType("RecordType1", 0);
        var recordType2V1 = revision1.newRecordType("RecordType2", noInternalName(), 1, Abstract.YES, noSuperTypes(), noPredecessor());
        var recordType3V1 = revision1.newRecordType("RecordType3", 2);
        
        // Dummy operations to avoid warnings
        var op1V1 = revision1.newOperation("operation", recordType1V1, recordType2V1);
        var op2V1 = revision1.newOperation("operation2", recordType3V1, recordType3V1);
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1);
        
        // In this revision, one of the concrete types becomes abstract
        var recordType1V2 = revision2.newRecordType("RecordType1", noInternalName(), 0, Abstract.YES, noSuperTypes(), recordType1V1);
        var recordType2V2 = revision2.newRecordType("RecordType2", noInternalName(), 1, Abstract.YES, noSuperTypes(), recordType2V1);
        var recordType3V2 = revision2.newRecordType("RecordType3", noInternalName(), 2, recordType3V1);
        
        // Dummy operations to avoid warnings
        revision2.newOperation("operation", noInternalName(), recordType1V2, recordType2V2, op1V1);
        revision2.newOperation("operation2", noInternalName(), recordType3V2, recordType3V2, op2V1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 record RecordType1(RecordType1) {
 }
 abstract record RecordType2(RecordType2) {
 }
 record RecordType3(RecordType3) {
 }
 operation operation(operation) (RecordType2@revision 0) : RecordType1@revision 0
 operation operation2(operation2) (RecordType3@revision 0) : RecordType3@revision 0
}
""";

        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Test case: Adds a type to an inheritance hierarchy by setting the supertype of a type that previously had none.
     */
    @Test
    void addTypeToInheritanceHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", 10);
        
        var operationV1 = revision1.newOperation("op", typeXV1, typeXV1);

        revision1.finalizeDefinition();

        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1, revision1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        // Set TypeA as the supertype of TypeX in the second revision
        var typeXV2 = revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV2), typeXV1);

        revision2.newOperation("op", noInternalName(), typeXV2, typeXV2, operationV1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeX(TypeX) extends TypeA {
  inherited optin fieldA(fieldA):string
 }
 operation op(op) (TypeX@revision 0) : TypeX@revision 0
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Test case: Removes a type to an inheritance hierarchy by removing the supertype.
     */
    @Test
    void removeTypeFromInheritanceHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV1), noPredecessor());

        revision1.finalizeDefinition();

        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1, revision1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        // Remove the supertype from TypeX
        revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(), typeXV1);

        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeX(TypeX) extends TypeA {
  inherited mandatory fieldA(fieldA):string
 }
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Test case: Moves a given type from one type hierarchy to another by changing the supertype.
     */
    @Test
    void moveTypeToAnotherInheritanceHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeBV1 = revision1.newRecordType("TypeB", 1);
        var fieldBV1 = typeBV1.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV1), noPredecessor());
        
        var operationV1 = revision1.newOperation("op", typeXV1, typeXV1);

        revision1.finalizeDefinition();

        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1, revision1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        var typeBV2 = revision2.newRecordType("TypeB", noInternalName(), 1, typeBV1);
        typeBV2.newField("fieldB", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldBV1);

        // Set TypeA as the supertype of TypeX in the second revision
        var typeXV2 = revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeBV2), typeXV1);

        revision2.newOperation("op", noInternalName(), typeXV2, typeXV2, operationV1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        var expected = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeB(TypeB) {
  mandatory fieldB(fieldB):string
 }
 record TypeX(TypeX) extends TypeB, TypeA {
  inherited optin fieldB(fieldB):string
  inherited optin fieldA(fieldA):string
 }
 operation op(op) (TypeX@revision 0) : TypeX@revision 0
}
""";

        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Test case: Move a type up in the inheritance hierarchy by changing its supertype to a supertype of the previous supertype.
     */
    @Test
    void moveTypeUpInHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeBV1 = revision1.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Set.of(typeAV1), noPredecessor());
        var fieldBV1 = typeBV1.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeBV1), noPredecessor());
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1, revision1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        var typeBV2 = revision2.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Set.of(typeAV2), typeBV1);
        typeBV2.newField("fieldB", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldBV1);

        revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV2), typeXV1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeB(TypeB) extends TypeA {
  inherited mandatory fieldA(fieldA):string
  mandatory fieldB(fieldB):string
 }
 record TypeX(TypeX) extends TypeA, TypeB {
  inherited mandatory fieldA(fieldA):string
  inherited mandatory fieldB(fieldB):string
 }
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Test case: Move a type down in the inheritance hierarchy by changing its supertype to a subtype of the previous supertype.
     * Therefore, field B from type B is inherited with optionality opt-in, since it does not occur in all revisions.
     */
    @Test
    void moveTypeDownInHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeBV1 = revision1.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Set.of(typeAV1), noPredecessor());
        var fieldBV1 = typeBV1.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV1), noPredecessor());
        
        var operationV1 = revision1.newOperation("op", typeXV1, typeXV1);
        
        revision1.finalizeDefinition();
        
        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1, revision1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        var typeBV2 = revision2.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Set.of(typeAV2), typeBV1);
        typeBV2.newField("fieldB", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldBV1);

        var typeXV2 = revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeBV2), typeXV1);
        
        revision2.newOperation("op", noInternalName(), typeXV2, typeXV2, operationV1);
        
        revision2.finalizeDefinition();
        
        // Create and merge the revision history
        var revisionHistory = new RevisionHistory(revision1, revision2);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        
        var expected = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeB(TypeB) extends TypeA {
  inherited mandatory fieldA(fieldA):string
  mandatory fieldB(fieldB):string
 }
 record TypeX(TypeX) extends TypeB, TypeA {
  inherited mandatory fieldA(fieldA):string
  inherited optin fieldB(fieldB):string
 }
 operation op(op) (TypeX@revision 0) : TypeX@revision 0
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }
    
    /**
     * Test case: Pulling up a field from two types with different histories: In one type, the original field is present in all revisions, but not in the other.
     */
    @Test
    void pullUpFieldWithDifferentHistories() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);
        
        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);
        
        var typeBV1 = revision1.newRecordType("TypeB", 1);
        
        // Operations so that the types are also used for input
        var operation1V1 = revision1.newOperation("op1", typeAV1, typeAV1);
        var operation2V1 = revision1.newOperation("op2", typeBV1, typeBV1);
        
        revision1.finalizeDefinition();
        
        // Revision 2: Addition of field fieldB
        var revision2 = ProviderApiDefinition.create("test", 1, revision1);
        
        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        var fieldAV2 = typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);
        
        var typeBV2 = revision2.newRecordType("TypeB", noInternalName(), 1, typeBV1);
        var fieldBV2 = typeBV2.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);
                
        var operation1V2 = revision2.newOperation("op1", noInternalName(), typeAV2, typeAV2, operation1V1);
        var operation2V2 = revision2.newOperation("op2", noInternalName(), typeBV2, typeBV2, operation2V1);
        
        revision2.finalizeDefinition();
        
        // Revision 3: Pull-up of fields into a new type
        var revision3 = ProviderApiDefinition.create("test", 2, revision2);
        
        var superType = revision3.newRecordType("SuperType", 2);
        superType.newField("field", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, Inherited.NO, List.of(fieldAV2, fieldBV2), noPredecessor());
        
        var typeAV3 = revision3.newRecordType("TypeA", noInternalName(), 0, Abstract.NO, Set.of(superType), typeAV2);
        
        var typeBV3 = revision3.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Set.of(superType), typeBV2);
        
        revision3.newOperation("op1", noInternalName(), typeAV3, typeAV3, operation1V2);
        revision3.newOperation("op2", noInternalName(), typeBV3, typeBV3, operation2V2);
        
        revision3.finalizeDefinition();
        
        // Merge revisions
        var revisionHistory = new RevisionHistory(revision1, revision2, revision3);
        var mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
 
        // The field is mandatory in SuperType and TypeA, because it exists in all revisions of this type. It is only optin for TypeB because it does not exist 
        // in all revisions of this type
        var expected = """
api test [] {
 record SuperType(SuperType) {
  mandatory field(field):string
 }
 record TypeA(TypeA) extends SuperType {
  inherited mandatory field(field):string
 }
 record TypeB(TypeB) extends SuperType {
  inherited optin field(field):string
 }
 operation op1(op1) (TypeA@revision 0) : TypeA@revision 0
 operation op2(op2) (TypeB@revision 0) : TypeB@revision 0
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

}
