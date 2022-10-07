package gutta.apievolution.dsl;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinitionPrinter;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;

class ProviderApiLoadingTest {

    /**
     * Test case: A simple model to test the overall parsing functionality.
     */
    @Test
    void testSimpleAPIDefinition() {
        // Build the expected API definition with all expected elements
        ProviderApiDefinition expectedDefinition = ProviderApiDefinition.create("test.customer", 0);

        // Address type
        ProviderRecordType addressType = expectedDefinition.newRecordType("Address", 0);

        addressType.newField("street", StringType.unbounded(), Optionality.MANDATORY);
        addressType.newField("number", StringType.unbounded(), Optionality.MANDATORY);
        addressType.newField("postalCode", NumericType.bounded(5, 0), Optionality.MANDATORY);
        addressType.newField("city", StringType.unbounded(), Optionality.MANDATORY);

        // Gender enum
        ProviderEnumType genderEnum = expectedDefinition.newEnumType("Gender", 1);

        genderEnum.newEnumMember("MALE");
        genderEnum.newEnumMember("FEMALE");
        genderEnum.newEnumMember("THIRD");

        // Customer type
        ProviderRecordType customerType = expectedDefinition.newRecordType("Customer", 2);

        customerType.newField("firstName", StringType.unbounded(), Optionality.MANDATORY);
        customerType.newField("lastName", StringType.unbounded(), Optionality.MANDATORY);
        customerType.newField("gender", genderEnum, Optionality.MANDATORY);
        customerType.newField("address", addressType, Optionality.MANDATORY);

        // Formatted address type
        ProviderRecordType formattedAddressType = expectedDefinition.newRecordType("FormattedAddress", 3);

        formattedAddressType.newField("address", StringType.unbounded(), Optionality.MANDATORY);

        // Operations
        expectedDefinition.newOperation("save", customerType, customerType);
        expectedDefinition.newOperation("formatAddress", formattedAddressType, addressType);

        expectedDefinition.finalizeDefinition();

        // Load the API definition from a file
        ProviderApiDefinition loadedDefinition = ProviderApiLoader.loadHistoryFromClasspath("apis/simple-model.api")
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
        ProviderApiDefinition expectedRevision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType unchangedTypeV1 = expectedRevision1.newRecordType("UnchangedType", 0);

        ProviderField unchangedTypeFieldV1 = unchangedTypeV1.newField("field", 
                StringType.unbounded(), Optionality.MANDATORY);
        
        ProviderRecordType changedTypeV1 = expectedRevision1.newRecordType("ChangedType", 1);

        ProviderField changedTypeFieldV1 = changedTypeV1.newField("field", StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType deletedType = expectedRevision1.newRecordType("DeletedType", 2);

        deletedType.newField("field", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType testTypeV1 = expectedRevision1.newRecordType("TestType", 3);

        ProviderField toRecordFieldV1 = testTypeV1.newField("toRecord", StringType.unbounded(),
                Optionality.MANDATORY);
        
        testTypeV1.newField("toSimple", deletedType, Optionality.MANDATORY);

        ProviderField changedTypeTestFieldV1 = testTypeV1.newField("changedType", changedTypeV1,
                Optionality.MANDATORY);

        ProviderField unchangedTypeTestFieldV1 = testTypeV1.newField("unchanged", unchangedTypeV1,
                Optionality.MANDATORY);

        testTypeV1.newField("basicField", AtomicType.INT_32, Optionality.MANDATORY);

        expectedRevision1.finalizeDefinition();

        // Define the second expected revision programmatically
        ProviderApiDefinition expectedRevision2 = new ProviderApiDefinition("test", noAnnotations(), 1,
                expectedRevision1);

        ProviderRecordType unchangedTypeV2 = expectedRevision2.newRecordType("UnchangedType", 0);

        ProviderField unchangedTypeFieldV2 = unchangedTypeV1.newField("field", noInternalName(),
                StringType.unbounded(), Optionality.MANDATORY, Inherited.NO,
                Arrays.asList(unchangedTypeFieldV1), unchangedTypeFieldV1);

        ProviderRecordType changedTypeV2 = expectedRevision2.newRecordType("ChangedType", 1);

        changedTypeV2.newField("field", noInternalName(), StringType.unbounded(), Optionality.MANDATORY,
                Inherited.NO, Arrays.asList(changedTypeFieldV1), changedTypeFieldV1);
        
        changedTypeV2.newField("field2", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType addedTypeV2 = expectedRevision2.newRecordType("AddedType", 2);

        addedTypeV2.newField("field", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType testTypeV2 = expectedRevision2.newRecordType("TestType", 3);

        testTypeV2.newField("toRecord", "toRecord2", addedTypeV2, Optionality.MANDATORY, noPredecessor());

        testTypeV2.newField("toSimple", "toSimple2", StringType.unbounded(), Optionality.MANDATORY,
                noPredecessor());

        testTypeV2.newField("changedType", noInternalName(), changedTypeV2, Optionality.MANDATORY, Inherited.NO,
                Arrays.asList(changedTypeTestFieldV1), changedTypeTestFieldV1);

        testTypeV2.newField("unchanged", noInternalName(), unchangedTypeV2, Optionality.MANDATORY, Inherited.NO,
                Arrays.asList(unchangedTypeTestFieldV1), unchangedTypeTestFieldV1);

        testTypeV2.newField("basicField", "basicFieldChanged", AtomicType.INT_64, Optionality.MANDATORY,
                noPredecessor());

        expectedRevision2.finalizeDefinition();

        // Load and build the revision history from the given API definitions
        RevisionHistory history = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api",
                "apis/provider-revision-2.api");
        // Make sure that the history is consistent
        history.checkConsistency();

        // Retrieve the individual revisions from the history
        ProviderApiDefinition revision1 = history.getRevision(0).orElseThrow(NoSuchElementException::new);
        ProviderApiDefinition revision2 = history.getRevision(1).orElseThrow(NoSuchElementException::new);

        // Compare expected and actual revisions
        assertEquals(expectedRevision1, revision1);
        assertEquals(expectedRevision2, revision2);
    }

    /**
     * Test an API definition with forward references.
     */
    @Test
    void apiDefinitionWithForwardReferences() {
        RevisionHistory revisionHistory = ProviderApiLoader
                .loadHistoryFromClasspath("apis/revision-with-forward-reference.api");
        ProviderApiDefinition apiDefinition = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);

        String expected = "api test [] {\n" + " record TypeA(TypeA) extends TypeB {\n" +
                "  mandatory field(field):TypeC\n" + " }\n" + " record TypeB(TypeB) {\n" + " }\n" +
                " enum TypeC(TypeC) {\n" + "  MEMBER(MEMBER)\n" + " }\n" + "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(apiDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test an API with supertypes.
     */
    @Test
    void apiDefinitionWithSupertypes() {
        ProviderApiDefinition expectedDefinition = ProviderApiDefinition.create("test", 0);

        ProviderRecordType typeA = expectedDefinition.newRecordType("TypeA", noInternalName(), 0, Abstract.YES,
                noSuperTypes(), noPredecessor());

        typeA.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType typeB = expectedDefinition.newRecordType("TypeB", noInternalName(), 1, Abstract.NO,
                Collections.singleton(typeA), noPredecessor());

        typeB.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType typeC = expectedDefinition.newRecordType("TypeC", noInternalName(), 2, Abstract.NO,
                Collections.singleton(typeB), noPredecessor());

        typeC.newField("fieldC", StringType.unbounded(), Optionality.MANDATORY);

        expectedDefinition.finalizeDefinition();

        RevisionHistory revisionHistory = ProviderApiLoader
                .loadHistoryFromClasspath("apis/revision-with-supertypes.api");
        ProviderApiDefinition apiDefinition = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);

        assertEquals(expectedDefinition, apiDefinition);
    }

    /**
     * Test case: Fields are pulled up along the inheritance hierarchy in a revision
     * history.
     */
    @Test
    void pullUpAttribute() {
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/pull-up-attribute-1.api",
                "apis/pull-up-attribute-2.api");

        ProviderApiDefinition revision1 = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        ProviderApiDefinition revision2 = revisionHistory.getRevision(1).orElseThrow(NoSuchElementException::new);
        ;

        String expected1 = "api test [] {\n" + " record TypeA(TypeA) {\n" + "  mandatory fieldA(fieldA):string\n" +
                " }\n" + " record TypeB(TypeB) {\n" + "  mandatory fieldB(fieldB):string\n" + " }\n" +
                " record TypeC(TypeC) {\n" + "  mandatory fieldC(fieldC):string\n" + " }\n" + "}\n";

        String expected2 = "api test [] {\n" + " record SuperType(SuperType) {\n" +
                "  mandatory field(field):string\n" + " }\n" + " record TypeA(TypeA) extends SuperType <- TypeA {\n" +
                "  inherited mandatory field(field):string <- fieldA\n" + " }\n" +
                " record TypeB(TypeB) extends SuperType <- TypeB {\n" +
                "  inherited mandatory field(field):string <- fieldB\n" + " }\n" + " record TypeC(TypeC) <- TypeC {\n" +
                "  mandatory fieldC(fieldC):string <- fieldC\n" + " }\n" + "}\n";

        ProviderApiDefinitionPrinter printer = new ProviderApiDefinitionPrinter();

        String actual1 = printer.printApiDefinition(revision1);
        String actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: A field is pushed down along the inheritance hierarchy in a
     * revision history.
     */
    @Test
    void pushDownAttribute() {
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/push-down-attribute-1.api",
                "apis/push-down-attribute-2.api");

        ProviderApiDefinition revision1 = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        ProviderApiDefinition revision2 = revisionHistory.getRevision(1).orElseThrow(NoSuchElementException::new);

        String expected1 = "api test [] {\n" + " record SuperType(SuperType) {\n" +
                "  mandatory fieldA(fieldA):string\n" + " }\n" + " record SubTypeA(SubTypeA) extends SuperType {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" + " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType {\n" + "  inherited mandatory fieldA(fieldA):string\n" +
                " }\n" + "}\n";

        String expected2 = "api test [] {\n" + " record SuperType(SuperType) <- SuperType {\n" + " }\n" +
                " record SubTypeA(SubTypeA) extends SuperType <- SubTypeA {\n" +
                "  mandatory fieldA(fieldA):string <- fieldA\n" + " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType <- SubTypeB {\n" +
                "  mandatory fieldB(fieldB):string <- fieldA\n" + " }\n" + "}\n";

        ProviderApiDefinitionPrinter printer = new ProviderApiDefinitionPrinter();

        String actual1 = printer.printApiDefinition(revision1);
        String actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);

        // Assert that the predecessor of the field in type B is actually the inherited
        // field
        // from the first revision. This is checked explicitly as it is not shown in the
        // string representation
        ProviderRecordType recordType = (ProviderRecordType) revision2.resolveUserDefinedType("SubTypeB")
                .orElseThrow(NoSuchElementException::new);
        ProviderField field = recordType.resolveField("fieldB").orElseThrow(NoSuchElementException::new);

        ProviderRecordType predecessorType = recordType.getPredecessor().orElseThrow(NoSuchElementException::new);
        ProviderField predecessorField = field.getPredecessor().orElseThrow(NoSuchElementException::new);

        assertEquals(predecessorField.getOwner(), predecessorType);
        assertTrue(predecessorField.isInherited());
    }

    /**
     * Test case: Load an API revision without considering replacements.
     */
    @Test
    void loadRevisionIgnoringReplacements() {
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath(true,
                "apis/push-down-attribute-2.api");
        ProviderApiDefinition definition = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);

        String expected = "api test [] {\n" + " record SuperType(SuperType) {\n" + " }\n" +
                " record SubTypeA(SubTypeA) extends SuperType {\n" + "  mandatory fieldA(fieldA):string\n" + " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType {\n" + "  mandatory fieldB(fieldB):string\n" + " }\n" +
                "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(definition);
        assertEquals(expected, actual);
    }

    @Test
    void failOnEmptyString() {
        APIParseException exception = assertThrows(APIParseException.class,
                () -> ProviderApiLoader.loadFromString(0, "", false, Optional.empty()));

        assertTrue(exception.getMessage().contains("1:0: mismatched"));
    }

    @Test
    void failOnParseError() {
        // Missing delimiter in API definition
        String input = "api test {";

        APIParseException exception = assertThrows(APIParseException.class,
                () -> ProviderApiLoader.loadFromString(0, input, false, Optional.empty()));

        assertTrue(exception.getMessage().contains("1:10: mismatched"));
    }

    /**
     * Test case: API with exceptions.
     */
    @Test
    void apiWithExceptions() {
        RevisionHistory revisionHistory = ProviderApiLoader
                .loadHistoryFromClasspath("apis/revision-with-exceptions-1.api", "apis/revision-with-exceptions-2.api");
        ProviderApiDefinition revision1 = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        ProviderApiDefinition revision2 = revisionHistory.getRevision(1).orElseThrow(NoSuchElementException::new);

        String expected1 = "api test [] {\n" + " record Record(Record) {\n" + " }\n" + " exception E1(E1) {\n" +
                "  mandatory e1(e1):string\n" + " }\n" + " exception E2(E2) {\n" + "  mandatory e2(e2):string\n" +
                " }\n" +
                " operation op1(op1) (Record@revision 0) : Record@revision 0 throws [E1@revision 0, E2@revision 0]\n" +
                "}\n";
        String actual1 = new ProviderApiDefinitionPrinter().printApiDefinition(revision1);
        assertEquals(expected1, actual1);

        String expected2 = "api test [] {\n" + " record Record(Record) <- Record {\n" + " }\n" +
                " exception E1(E1) <- E1 {\n" + "  mandatory e1(e1):string <- e1\n" + "  mandatory e12(e12):string\n" +
                " }\n" + " exception E2(E2) <- E2 {\n" + "  mandatory e2(e2):string <- e2\n" +
                "  mandatory e12(e12):string\n" + " }\n" +
                " operation op1(op1) (Record@revision 1) : Record@revision 1 throws [E1@revision 1, E2@revision 1] <- op1\n" +
                "}\n";
        String actual2 = new ProviderApiDefinitionPrinter().printApiDefinition(revision2);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: Exceptions cannot be predecessors for records and vice versa.
     */
    @Test
    void failOnExecutionRecordPredecessor() {
        String revision1Def = "api test { exception Test {} }";
        String revision2Def = "api test { record Test {} }";

        // exception -> record
        APIResolutionException exception1 = assertThrows(APIResolutionException.class,
                () -> ProviderApiLoader.loadHistoryFromStrings(revision1Def, revision2Def));
        assertTrue(exception1.getMessage().contains("may not be a record"));

        // record -> exception
        APIResolutionException exception2 = assertThrows(APIResolutionException.class,
                () -> ProviderApiLoader.loadHistoryFromStrings(revision2Def, revision1Def));
        assertTrue(exception2.getMessage().contains("may not be a record"));
    }
    
    /**
     * Test case: API definition with bounded types.
     */
    @Test
    void boundedTypes() {
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/bounded-types.api");
        ProviderApiDefinition revision = revisionHistory.getRevision(0).orElseThrow(NoSuchElementException::new);
        
        String expected = "api boundedTypes [] {\n" + 
                " enum TestEnum(TestEnum) {\n" + 
                "  VALUE_A(VALUE_A)\n" + 
                " }\n" + 
                " record TestRecord2(TestRecord2) {\n" + 
                " }\n" + 
                " record TestRecord(TestRecord) {\n" + 
                "  mandatory boundedString(boundedString):string(30)\n" + 
                "  mandatory enumList(enumList):TestEnum[10]\n" + 
                "  mandatory recordList(recordList):TestRecord2@revision 0[10]\n" + 
                " }\n" + 
                "}\n";
        
        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(revision);
        assertEquals(expected, actual);
    }

}
