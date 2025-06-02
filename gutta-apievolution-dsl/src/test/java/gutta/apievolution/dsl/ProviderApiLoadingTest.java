package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinitionPrinter;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ProviderApiLoadingTest {

    /**
     * Test case: A simple model to test the overall parsing functionality.
     */
    @Test
    void testSimpleAPIDefinition() {
        // Build the expected API definition with all expected elements
        var expectedDefinition = ProviderApiDefinition.create("test.customer", 0);

        // Address type
        var addressType = expectedDefinition.newRecordType("Address", 0);

        addressType.newField("street", StringType.unbounded(), Optionality.MANDATORY);
        addressType.newField("number", StringType.unbounded(), Optionality.MANDATORY);
        addressType.newField("postalCode", NumericType.bounded(5, 0), Optionality.MANDATORY);
        addressType.newField("city", StringType.unbounded(), Optionality.MANDATORY);

        // Gender enum
        var genderEnum = expectedDefinition.newEnumType("Gender", 1);

        genderEnum.newEnumMember("MALE");
        genderEnum.newEnumMember("FEMALE");
        genderEnum.newEnumMember("THIRD");

        // Customer type
        var customerType = expectedDefinition.newRecordType("Customer", 2);

        customerType.newField("firstName", StringType.unbounded(), Optionality.MANDATORY);
        customerType.newField("lastName", StringType.unbounded(), Optionality.MANDATORY);
        customerType.newField("gender", genderEnum, Optionality.MANDATORY);
        customerType.newField("address", addressType, Optionality.MANDATORY);

        // Formatted address type
        var formattedAddressType = expectedDefinition.newRecordType("FormattedAddress", 3);

        formattedAddressType.newField("address", StringType.unbounded(), Optionality.MANDATORY);

        // Operations
        expectedDefinition.newOperation("save", customerType, customerType);
        expectedDefinition.newOperation("formatAddress", formattedAddressType, addressType);

        expectedDefinition.finalizeDefinition();

        // Load the API definition from a file
        var loadedDefinition = ProviderApiLoader.loadHistoryFromClasspath("apis/simple-model.api")
                .getRevision(0).orElseThrow(NoSuchElementException::new);

        // Compare the expected and loaded definition
        assertEquals(expectedDefinition, loadedDefinition);
    }

    /**
     * Test whether building a revision history works as expected, and if
     * predecessors are assigned correctly.
     */
    @Test
    void testRevisionHistoryBuilding() {
        // Define the first expected revision programmatically
        var expectedRevision1 = ProviderApiDefinition.create("test", 0);

        var unchangedTypeV1 = expectedRevision1.newRecordType("UnchangedType", 0);
        var unchangedTypeFieldV1 = unchangedTypeV1.newField("field", StringType.unbounded(), Optionality.MANDATORY);
        
        var changedTypeV1 = expectedRevision1.newRecordType("ChangedType", 1);
        var changedTypeFieldV1 = changedTypeV1.newField("field", StringType.unbounded(), Optionality.MANDATORY);

        var deletedType = expectedRevision1.newRecordType("DeletedType", 2);
        deletedType.newField("field", StringType.unbounded(), Optionality.MANDATORY);

        var testTypeV1 = expectedRevision1.newRecordType("TestType", 3);
        testTypeV1.newField("toRecord", StringType.unbounded(), Optionality.MANDATORY);        
        testTypeV1.newField("toSimple", deletedType, Optionality.MANDATORY);

        var changedTypeTestFieldV1 = testTypeV1.newField("changedType", changedTypeV1, Optionality.MANDATORY);
        var unchangedTypeTestFieldV1 = testTypeV1.newField("unchanged", unchangedTypeV1, Optionality.MANDATORY);

        testTypeV1.newField("basicField", AtomicType.INT_32, Optionality.MANDATORY);

        expectedRevision1.finalizeDefinition();

        // Define the second expected revision programmatically
        var expectedRevision2 = new ProviderApiDefinition("test", noAnnotations(), 1, expectedRevision1);

        var unchangedTypeV2 = expectedRevision2.newRecordType("UnchangedType", 0);
        unchangedTypeV2.newField("field", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, Inherited.NO, List.of(unchangedTypeFieldV1),
                unchangedTypeFieldV1);

        var changedTypeV2 = expectedRevision2.newRecordType("ChangedType", 1);
        changedTypeV2.newField("field", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, Inherited.NO, List.of(changedTypeFieldV1),
                changedTypeFieldV1);        
        changedTypeV2.newField("field2", StringType.unbounded(), Optionality.MANDATORY);

        var addedTypeV2 = expectedRevision2.newRecordType("AddedType", 2);
        addedTypeV2.newField("field", StringType.unbounded(), Optionality.MANDATORY);

        var testTypeV2 = expectedRevision2.newRecordType("TestType", 3);
        testTypeV2.newField("toRecord", "toRecord2", addedTypeV2, Optionality.MANDATORY, noPredecessor());
        testTypeV2.newField("toSimple", "toSimple2", StringType.unbounded(), Optionality.MANDATORY, noPredecessor());
        testTypeV2.newField("changedType", noInternalName(), changedTypeV2, Optionality.MANDATORY, Inherited.NO, List.of(changedTypeTestFieldV1),
                changedTypeTestFieldV1);
        testTypeV2.newField("unchanged", noInternalName(), unchangedTypeV2, Optionality.MANDATORY, Inherited.NO, List.of(unchangedTypeTestFieldV1),
                unchangedTypeTestFieldV1);
        testTypeV2.newField("basicField", "basicFieldChanged", AtomicType.INT_64, Optionality.MANDATORY, noPredecessor());

        expectedRevision2.finalizeDefinition();

        // Load and build the revision history from the given API definitions
        var history = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api");
        // Make sure that the history is consistent
        history.checkConsistency();

        // Retrieve the individual revisions from the history
        var revision1 = history.getRevision(0).orElseThrow(NoSuchElementException::new);
        var revision2 = history.getRevision(1).orElseThrow(NoSuchElementException::new);

        // Compare expected and actual revisions
        assertEquals(expectedRevision1, revision1);
        assertEquals(expectedRevision2, revision2);
    }

    /**
     * Test an API definition with forward references.
     */
    @Test
    void apiDefinitionWithForwardReferences() {
        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/revision-with-forward-reference.api");
        var apiDefinition = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);

        var expected = """
api test [] {
 record TypeA(TypeA) extends TypeB {
  mandatory field(field):TypeC
 }
 record TypeB(TypeB) {
 }
 enum TypeC(TypeC) {
  MEMBER(MEMBER)
 }
}
"""; 
 
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(apiDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test an API with supertypes.
     */
    @Test
    void apiDefinitionWithSupertypes() {
        var expectedDefinition = ProviderApiDefinition.create("test", 0);

        var typeA = expectedDefinition.newRecordType("TypeA", noInternalName(), 0, Abstract.YES, noSuperTypes(), noPredecessor());
        typeA.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeB = expectedDefinition.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Set.of(typeA), noPredecessor());
        typeB.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        var typeC = expectedDefinition.newRecordType("TypeC", noInternalName(), 2, Abstract.NO, Set.of(typeB), noPredecessor());
        typeC.newField("fieldC", StringType.unbounded(), Optionality.MANDATORY);

        expectedDefinition.finalizeDefinition();

        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/revision-with-supertypes.api");
        var apiDefinition = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);

        assertEquals(expectedDefinition, apiDefinition);
    }

    /**
     * Test case: Fields are pulled up along the inheritance hierarchy in a revision
     * history.
     */
    @Test
    void pullUpAttribute() {
        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/pull-up-attribute-1.api", "apis/pull-up-attribute-2.api");

        var revision1 = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        var revision2 = revisionHistory.getRevision(1).orElseThrow(NoSuchElementException::new);

        var expected1 = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeB(TypeB) {
  mandatory fieldB(fieldB):string
 }
 record TypeC(TypeC) {
  mandatory fieldC(fieldC):string
 }
}                
"""; 
                
        var expected2 = """
api test [] {
 record SuperType(SuperType) {
  mandatory field(field):string
 }
 record TypeA(TypeA) extends SuperType <- TypeA {
  inherited mandatory field(field):string <- fieldA
 }
 record TypeB(TypeB) extends SuperType <- TypeB {
  inherited mandatory field(field):string <- fieldB
 }
 record TypeC(TypeC) <- TypeC {
  mandatory fieldC(fieldC):string <- fieldC
 }
}
"""; 
         
        var printer = new ProviderApiDefinitionPrinter();

        var actual1 = printer.printApiDefinition(revision1);
        var actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: A field is pushed down along the inheritance hierarchy in a
     * revision history.
     */
    @Test
    void pushDownAttribute() {
        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/push-down-attribute-1.api", "apis/push-down-attribute-2.api");

        var revision1 = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        var revision2 = revisionHistory.getRevision(1).orElseThrow(NoSuchElementException::new);

        var expected1 = """
api test [] {
 record SuperType(SuperType) {
  mandatory fieldA(fieldA):string
 }
 record SubTypeA(SubTypeA) extends SuperType {
  inherited mandatory fieldA(fieldA):string
 }
 record SubTypeB(SubTypeB) extends SuperType {
  inherited mandatory fieldA(fieldA):string
 }
}
"""; 
 
        var expected2 = """
api test [] {
 record SuperType(SuperType) <- SuperType {
 }
 record SubTypeA(SubTypeA) extends SuperType <- SubTypeA {
  mandatory fieldA(fieldA):string <- fieldA
 }
 record SubTypeB(SubTypeB) extends SuperType <- SubTypeB {
  mandatory fieldB(fieldB):string <- fieldA
 }
}
""";                
         
        var printer = new ProviderApiDefinitionPrinter();

        var actual1 = printer.printApiDefinition(revision1);
        var actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);

        // Assert that the predecessor of the field in type B is actually the inherited field
        // from the first revision. This is checked explicitly as it is not shown in the string representation
        var recordType = (ProviderRecordType) revision2.resolveUserDefinedType("SubTypeB").orElseThrow(NoSuchElementException::new);
        var field = recordType.resolveField("fieldB").orElseThrow(NoSuchElementException::new);

        var predecessorType = recordType.getPredecessor().orElseThrow(NoSuchElementException::new);
        var predecessorField = field.getPredecessor().orElseThrow(NoSuchElementException::new);

        assertEquals(predecessorField.getOwner(), predecessorType);
        assertTrue(predecessorField.isInherited());
    }

    /**
     * Test case: Load an API revision without considering replacements.
     */
    @Test
    void loadRevisionIgnoringReplacements() {
        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath(true, "apis/push-down-attribute-2.api");
        var definition = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);

        var expected = """
api test [] {
 record SuperType(SuperType) {
 }
 record SubTypeA(SubTypeA) extends SuperType {
  mandatory fieldA(fieldA):string
 }
 record SubTypeB(SubTypeB) extends SuperType {
  mandatory fieldB(fieldB):string
 }
}
""";

        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(definition);
        assertEquals(expected, actual);
    }

    @Test
    void failOnEmptyString() {
        var exception = assertThrows(APIParseException.class, () -> ProviderApiLoader.loadFromString(0, "", false, Optional.empty()));
        assertTrue(exception.getMessage().contains("1:0: mismatched"));
    }

    @Test
    void failOnParseError() {
        // Missing delimiter in API definition
        var input = "api test {";

        var exception = assertThrows(APIParseException.class, () -> ProviderApiLoader.loadFromString(0, input, false, Optional.empty()));
        assertTrue(exception.getMessage().contains("1:10: mismatched"));
    }

    /**
     * Test case: API with exceptions.
     */
    @Test
    void apiWithExceptions() {
        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/revision-with-exceptions-1.api", "apis/revision-with-exceptions-2.api");
        var revision1 = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        var revision2 = revisionHistory.getRevision(1).orElseThrow(NoSuchElementException::new);

        var expected1 = """
api test [] {
 record Record(Record) {
 }
 exception E1(E1) {
  mandatory e1(e1):string
 }
 exception E2(E2) {
  mandatory e2(e2):string
 }
 operation op1(op1) (Record@revision 0) : Record@revision 0 throws [E1@revision 0, E2@revision 0]
}
"""; 
        var actual1 = new ProviderApiDefinitionPrinter().printApiDefinition(revision1);
        assertEquals(expected1, actual1);
       
        var expected2 = """
api test [] {
 record Record(Record) <- Record {
 }
 exception E1(E1) <- E1 {
  mandatory e1(e1):string <- e1
  mandatory e12(e12):string
 }
 exception E2(E2) <- E2 {
  mandatory e2(e2):string <- e2
  mandatory e12(e12):string
 }
 operation op1(op1) (Record@revision 1) : Record@revision 1 throws [E1@revision 1, E2@revision 1] <- op1
}
"""; 
        var actual2 = new ProviderApiDefinitionPrinter().printApiDefinition(revision2);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: Exceptions cannot be predecessors for records and vice versa.
     */
    @Test
    void failOnExecutionRecordPredecessor() {
        var revision1Def = "api test { exception Test {} }";
        var revision2Def = "api test { record Test {} }";

        // exception -> record
        var exception1 = assertThrows(APIResolutionException.class, () -> ProviderApiLoader.loadHistoryFromStrings(revision1Def, revision2Def));
        assertTrue(exception1.getMessage().contains("may not be a record"));

        // record -> exception
        var exception2 = assertThrows(APIResolutionException.class, () -> ProviderApiLoader.loadHistoryFromStrings(revision2Def, revision1Def));
        assertTrue(exception2.getMessage().contains("may not be a record"));
    }
    
    /**
     * Test case: API definition with bounded types.
     */
    @Test
    void boundedTypes() {
        var revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/bounded-types.api");
        var revision = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        
        var expected = """
api boundedTypes [] {
 enum TestEnum(TestEnum) {
  VALUE_A(VALUE_A)
 }
 record TestRecord2(TestRecord2) {
 }
 record TestRecord(TestRecord) {
  mandatory boundedString(boundedString):string(30)
  mandatory enumList(enumList):TestEnum[10]
  mandatory recordList(recordList):TestRecord2@revision 0[10]
 }
}
""";                

        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(revision);
        assertEquals(expected, actual);
    }

}
