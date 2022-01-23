package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.provider.*;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
                1,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("street",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("number",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("postalCode",
                Optional.empty(),
                addressType,
                NumericType.bounded(5,0),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("city",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

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
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("lastName",
                Optional.empty(),
                customerType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("gender",
                Optional.empty(),
                customerType,
                genderEnum,
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("address",
                Optional.empty(),
                customerType,
                addressType,
                Optionality.MANDATORY,
                Optional.empty()
        );

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
                Optionality.MANDATORY,
                Optional.empty()
        );

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
                Optionality.MANDATORY,
                Optional.empty());

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
                Optionality.MANDATORY,
                Optional.empty());

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
                Optionality.MANDATORY,
                Optional.empty());

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
                Optionality.MANDATORY,
                Optional.empty());

        new ProviderField("toSimple",
                Optional.empty(),
                testTypeV1,
                deletedType,
                Optionality.MANDATORY,
                Optional.empty());

        ProviderField changedTypeTestFieldV1 = new ProviderField("changedType",
                Optional.empty(),
                testTypeV1,
                changedTypeV1,
                Optionality.MANDATORY,
                Optional.empty());

        ProviderField unchangedTypeTestFieldV1 = new ProviderField("unchanged",
                Optional.empty(),
                testTypeV1,
                unchangedTypeV1,
                Optionality.MANDATORY,
                Optional.empty());

        new ProviderField("basicField",
                Optional.empty(),
                testTypeV1,
                AtomicType.INT_32,
                Optionality.MANDATORY,
                Optional.empty());

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
                Optional.of(changedTypeFieldV1));

        new ProviderField("field2",
                Optional.empty(),
                changedTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

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
                Optionality.MANDATORY,
                Optional.empty());

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
                Optionality.MANDATORY,
                Optional.empty());

        new ProviderField("toSimple",
                Optional.of("toSimple2"),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

        new ProviderField("changedType",
                Optional.empty(),
                testTypeV2,
                changedTypeV2,
                Optionality.MANDATORY,
                Optional.of(changedTypeTestFieldV1));

        new ProviderField("unchanged",
                Optional.empty(),
                testTypeV2,
                unchangedTypeV2,
                Optionality.MANDATORY,
                Optional.of(unchangedTypeTestFieldV1));

        new ProviderField("basicField",
                Optional.of("basicFieldChanged"),
                testTypeV2,
                AtomicType.INT_64,
                Optionality.MANDATORY,
                Optional.empty());

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


    }

}
