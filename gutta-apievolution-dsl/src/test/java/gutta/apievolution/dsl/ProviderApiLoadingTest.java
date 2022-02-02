package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.provider.*;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ProviderApiLoadingTest {

    /**
     * Test case: A simple model to test the overall parsing functionality.
     */
    @Test
    void testSimpleAPIDefinition() {
        // Build the expected API definition with all expected elements
        ProviderApiDefinition expectedDefinition = new ProviderApiDefinition(QualifiedName.of("test.customer"),
                Collections.emptySet(),
                0,
                Optional.empty());


        // Address type
        ProviderRecordType addressType = new ProviderRecordType("Address",
                Optional.empty(),
                0,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("street",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("number",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("postalCode",
                Optional.empty(),
                addressType,
                NumericType.bounded(5,0),
                Optionality.MANDATORY);

        new ProviderField("city",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        // Gender enum
        ProviderEnumType genderEnum = new ProviderEnumType("Gender",
                Optional.empty(),
                1,
                expectedDefinition,
                Optional.empty());

        new ProviderEnumMember("MALE",
                Optional.empty(),
                genderEnum,
                Optional.empty());

        new ProviderEnumMember("FEMALE",
                Optional.empty(),
                genderEnum,
                Optional.empty());

        new ProviderEnumMember("THIRD",
                Optional.empty(),
                genderEnum,
                Optional.empty());

        // Customer type
        ProviderRecordType customerType = new ProviderRecordType("Customer",
                Optional.empty(),
                2,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("firstName",
                Optional.empty(),
                customerType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("lastName",
                Optional.empty(),
                customerType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("gender",
                Optional.empty(),
                customerType,
                genderEnum,
                Optionality.MANDATORY);

        new ProviderField("address",
                Optional.empty(),
                customerType,
                addressType,
                Optionality.MANDATORY);

        // Formatted address type
        ProviderRecordType formattedAddressType = new ProviderRecordType("FormattedAddress",
                Optional.empty(),
                3,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("address",
                Optional.empty(),
                formattedAddressType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        // Customer service
        ProviderService customerService = new ProviderService("CustomerService",
                Optional.empty(),
                expectedDefinition,
                Optional.empty());

        new ProviderServiceOperation("save",
                Optional.empty(),
                customerService,
                customerType,
                customerType,
                Optional.empty());

        new ProviderServiceOperation("formatAddress",
                Optional.empty(),
                customerService,
                formattedAddressType,
                addressType,
                Optional.empty());

        expectedDefinition.finalizeDefinition();

        // Load the API definition from a file
        ProviderApiDefinition loadedDefinition = ProviderApiLoader.loadHistoryFromClasspath(
                "apis/simple-model.api").getRevision(0).orElseThrow(NoSuchElementException::new);

        // Compare the expected and loaded definition
        assertEquals(expectedDefinition, loadedDefinition);
    }

    /**
     * Test whether building a revision history works as expected, and if predecessors are assigned correctly.
     */
    @Test
    void testRevisionHistoryBuilding() {
        // Define the first expected revision programmatically
        ProviderApiDefinition expectedRevision1 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType unchangedTypeV1 = new ProviderRecordType("UnchangedType",
                Optional.empty(),
                0,
                expectedRevision1,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField unchangedTypeFieldV1 = new ProviderField("field",
                Optional.empty(),
                unchangedTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType changedTypeV1 = new ProviderRecordType("ChangedType",
                Optional.empty(),
                1,
                expectedRevision1,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField changedTypeFieldV1 = new ProviderField("field",
                Optional.empty(),
                changedTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType deletedType = new ProviderRecordType("DeletedType",
                Optional.empty(),
                2,
                expectedRevision1,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("field",
                Optional.empty(),
                deletedType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType testTypeV1 = new ProviderRecordType("TestType",
                Optional.empty(),
                3,
                expectedRevision1,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField toRecordFieldV1 = new ProviderField("toRecord",
                Optional.empty(),
                testTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("toSimple",
                Optional.empty(),
                testTypeV1,
                deletedType,
                Optionality.MANDATORY);

        ProviderField changedTypeTestFieldV1 = new ProviderField("changedType",
                Optional.empty(),
                testTypeV1,
                changedTypeV1,
                Optionality.MANDATORY);

        ProviderField unchangedTypeTestFieldV1 = new ProviderField("unchanged",
                Optional.empty(),
                testTypeV1,
                unchangedTypeV1,
                Optionality.MANDATORY);

        new ProviderField("basicField",
                Optional.empty(),
                testTypeV1,
                AtomicType.INT_32,
                Optionality.MANDATORY);

        expectedRevision1.finalizeDefinition();

        // Define the second expected revision programmatically
        ProviderApiDefinition expectedRevision2 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                1,
                Optional.of(expectedRevision1));

        ProviderRecordType unchangedTypeV2 = new ProviderRecordType("UnchangedType",
                Optional.empty(),
                0,
                expectedRevision2,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField unchangedTypeFieldV2 = new ProviderField("field",
                Optional.empty(),
                unchangedTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                false,
                Arrays.asList(unchangedTypeFieldV1),
                Optional.of(unchangedTypeFieldV1));

        ProviderRecordType changedTypeV2 = new ProviderRecordType("ChangedType",
                Optional.empty(),
                1,
                expectedRevision2,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("field",
                Optional.empty(),
                changedTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                false,
                Arrays.asList(changedTypeFieldV1),
                Optional.of(changedTypeFieldV1));

        new ProviderField("field2",
                Optional.empty(),
                changedTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType addedTypeV2 = new ProviderRecordType("AddedType",
                Optional.empty(),
                2,
                expectedRevision2,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("field",
                Optional.empty(),
                addedTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType testTypeV2 = new ProviderRecordType("TestType",
                Optional.empty(),
                3,
                expectedRevision2,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("toRecord",
                Optional.of("toRecord2"),
                testTypeV2,
                addedTypeV2,
                Optionality.MANDATORY);

        new ProviderField("toSimple",
                Optional.of("toSimple2"),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("changedType",
                Optional.empty(),
                testTypeV2,
                changedTypeV2,
                Optionality.MANDATORY,
                false,
                Arrays.asList(changedTypeTestFieldV1),
                Optional.of(changedTypeTestFieldV1));

        new ProviderField("unchanged",
                Optional.empty(),
                testTypeV2,
                unchangedTypeV2,
                Optionality.MANDATORY,
                false,
                Arrays.asList(unchangedTypeTestFieldV1),
                Optional.of(unchangedTypeTestFieldV1));

        new ProviderField("basicField",
                Optional.of("basicFieldChanged"),
                testTypeV2,
                AtomicType.INT_64,
                Optionality.MANDATORY);

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
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath(
                "apis/revision-with-forward-reference.api");
        ProviderApiDefinition apiDefinition = revisionHistory.getRevision(0)
                .orElseThrow(NoSuchElementException::new);

        String expected = "api test [] {\n" +
                " record TypeA(TypeA) extends TypeB {\n" +
                "  mandatory field(field):TypeC\n" +
                " }\n" +
                " record TypeB(TypeB) {\n" +
                " }\n" +
                " enum TypeC(TypeC) {\n" +
                "  MEMBER(MEMBER)\n" +
                " }\n" +
                "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(apiDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Test an API with supertypes.
     */
    @Test
    void apiDefinitionWithSupertypes() {
        ProviderApiDefinition expectedDefinition = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType typeA = new ProviderRecordType("TypeA",
                Optional.empty(),
                0,
                expectedDefinition,
                true,
                Optional.empty());

        new ProviderField("fieldA",
                Optional.empty(),
                typeA,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeB = new ProviderRecordType("TypeB",
                Optional.empty(),
                1,
                expectedDefinition,
                false,
                Optional.of(typeA),
                Optional.empty());

        new ProviderField("fieldB",
                Optional.empty(),
                typeB,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeC = new ProviderRecordType("TypeC",
                Optional.empty(),
                2,
                expectedDefinition,
                false,
                Optional.of(typeB),
                Optional.empty());

        new ProviderField("fieldC",
                Optional.empty(),
                typeC,
                StringType.unbounded(),
                Optionality.MANDATORY);

        expectedDefinition.finalizeDefinition();

        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath(
                "apis/revision-with-supertypes.api");
        ProviderApiDefinition apiDefinition = revisionHistory.getRevision(0)
                .orElseThrow(NoSuchElementException::new);

        assertEquals(expectedDefinition, apiDefinition);
    }

    /**
     * Test case: Fields are pulled up along the inheritance hierarchy in a revision history.
     */
    @Test
    void pullUpAttribute() {
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath(
                "apis/pull-up-attribute-1.api",
                "apis/pull-up-attribute-2.api"
        );

        ProviderApiDefinition revision1 = revisionHistory.getRevision(0)
                .orElseThrow(NoSuchElementException::new);
        ProviderApiDefinition revision2 = revisionHistory.getRevision(1)
                .orElseThrow(NoSuchElementException::new);;

        String expected1 = "api test [] {\n" +
                " record TypeA(TypeA) {\n" +
                "  mandatory fieldA(fieldA):string\n" +
                " }\n" +
                " record TypeB(TypeB) {\n" +
                "  mandatory fieldB(fieldB):string\n" +
                " }\n" +
                " record TypeC(TypeC) {\n" +
                "  mandatory fieldC(fieldC):string\n" +
                " }\n" +
                "}\n";

        String expected2 = "api test [] {\n" +
                " record SuperType(SuperType) {\n" +
                "  mandatory field(field):string\n" +
                " }\n" +
                " record TypeA(TypeA) extends SuperType <- TypeA {\n" +
                "  inherited mandatory field(field):string <- fieldA\n" +
                " }\n" +
                " record TypeB(TypeB) extends SuperType <- TypeB {\n" +
                "  inherited mandatory field(field):string <- fieldB\n" +
                " }\n" +
                " record TypeC(TypeC) <- TypeC {\n" +
                "  mandatory fieldC(fieldC):string <- fieldC\n" +
                " }\n" +
                "}\n";

        ProviderApiDefinitionPrinter printer = new ProviderApiDefinitionPrinter();

        String actual1 = printer.printApiDefinition(revision1);
        String actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: A field is pushed down along the inheritance hierarchy in a revision history.
     */
    @Test
    void pushDownAttribute() {
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath(
                "apis/push-down-attribute-1.api",
                "apis/push-down-attribute-2.api"
        );

        ProviderApiDefinition revision1 = revisionHistory.getRevision(0)
                .orElseThrow(NoSuchElementException::new);
        ProviderApiDefinition revision2 = revisionHistory.getRevision(1)
                .orElseThrow(NoSuchElementException::new);

        String expected1 = "api test [] {\n" +
                " record SuperType(SuperType) {\n" +
                "  mandatory fieldA(fieldA):string\n" +
                " }\n" +
                " record SubTypeA(SubTypeA) extends SuperType {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" +
                " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" +
                " }\n" +
                "}\n";

        String expected2 = "api test [] {\n" +
                " record SuperType(SuperType) <- SuperType {\n" +
                " }\n" +
                " record SubTypeA(SubTypeA) extends SuperType <- SubTypeA {\n" +
                "  mandatory fieldA(fieldA):string <- fieldA\n" +
                " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType <- SubTypeB {\n" +
                "  mandatory fieldB(fieldB):string <- fieldA\n" +
                " }\n" +
                "}\n";

        ProviderApiDefinitionPrinter printer = new ProviderApiDefinitionPrinter();

        String actual1 = printer.printApiDefinition(revision1);
        String actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);

        // Assert that the predecessor of the field in type B is actually the inherited field
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

}
