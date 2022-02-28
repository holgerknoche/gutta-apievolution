package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.core.apimodel.provider.*;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test cases for definition resolution.
 */
class DefinitionResolverTest {

    /**
     * Test case: Mapping on matching basic type fields.
     */
    @Test
    void matchingBasicTypeFields() {
        // Consumer API definition
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0);

        ConsumerRecordType consumerType = new ConsumerRecordType("TestType",
                Optional.empty(),
                0,
                consumerApi,
                false,
                Optional.empty());

        new ConsumerField("int32Field",
                Optional.empty(),
                consumerType,
                AtomicType.INT_32,
                Optionality.MANDATORY);

        new ConsumerField("int64Field",
                Optional.empty(),
                consumerType,
                AtomicType.INT_64,
                Optionality.MANDATORY);

        new ConsumerField("unboundedStringField",
                Optional.empty(),
                consumerType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ConsumerField("boundedStringField",
                Optional.empty(),
                consumerType,
                StringType.bounded(10),
                Optionality.MANDATORY);

        new ConsumerField("unboundedListField",
                Optional.empty(),
                consumerType,
                ListType.unbounded(StringType.unbounded()),
                Optionality.MANDATORY);

        new ConsumerField("boundedListField",
                Optional.empty(),
                consumerType,
                ListType.bounded(StringType.unbounded(), 10),
                Optionality.MANDATORY);

        new ConsumerField("numericField",
                Optional.empty(),
                consumerType,
                NumericType.bounded(5, 0),
                Optionality.MANDATORY);

        consumerApi.finalizeDefinition();

        // Provider API definition
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType providerType = new ProviderRecordType("TestType",
                Optional.empty(),
                0,
                providerApi,
                false,
                Optional.empty());

        new ProviderField("int32Field",
                Optional.empty(),
                providerType,
                AtomicType.INT_32,
                Optionality.MANDATORY);

        new ProviderField("int64Field",
                Optional.empty(),
                providerType,
                AtomicType.INT_64,
                Optionality.MANDATORY);

        new ProviderField("unboundedStringField",
                Optional.empty(),
                providerType,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("boundedStringField",
                Optional.empty(),
                providerType,
                StringType.bounded(10),
                Optionality.MANDATORY);

        new ProviderField("unboundedListField",
                Optional.empty(),
                providerType,
                ListType.unbounded(StringType.unbounded()),
                Optionality.MANDATORY);

        new ProviderField("boundedListField",
                Optional.empty(),
                providerType,
                ListType.bounded(StringType.unbounded(), 10),
                Optionality.MANDATORY);

        new ProviderField("numericField",
                Optional.empty(),
                providerType,
                NumericType.bounded(5, 0),
                Optionality.MANDATORY);

        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        String expected = "TestType -> TestType@revision 0\n" +
                " int32Field -> int32Field@TestType@revision 0\n" +
                " int64Field -> int64Field@TestType@revision 0\n" +
                " unboundedStringField -> unboundedStringField@TestType@revision 0\n" +
                " boundedStringField -> boundedStringField@TestType@revision 0\n" +
                " unboundedListField -> unboundedListField@TestType@revision 0\n" +
                " boundedListField -> boundedListField@TestType@revision 0\n" +
                " numericField -> numericField@TestType@revision 0\n";

        String actual = new DefinitionResolutionPrinter().printDefinitionResolution(resolution);

        assertEquals(expected, actual);
    }

    /**
     * Assert that a missing mapping of a mandatory field is appropriately reported.
     */
    @Test
    void testMissingMappingForMandatoryField() {
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0);

        ConsumerRecordType consumerType = new ConsumerRecordType("Test",
                Optional.empty(),
                1,
                consumerApi,
                false,
                Optional.empty());

        new ConsumerField("optionalField",
                Optional.empty(),
                consumerType,
                AtomicType.INT_32,
                Optionality.OPTIONAL,
                false);

        // Define a provider API with a mandatory field
        ProviderApiDefinition revision = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType recordType = new ProviderRecordType("Test",
                Optional.empty(),
                1,
                revision,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("mandatoryField",
                Optional.empty(),
                recordType,
                AtomicType.INT_32,
                Optionality.MANDATORY);

        new ProviderField("optionalField",
                Optional.empty(),
                recordType,
                AtomicType.INT_32,
                Optionality.OPTIONAL);

        // Resolve the consumer definition against the revision history
        RevisionHistory revisionHistory = new RevisionHistory(revision);
        Set<Integer> supportedRevision = new HashSet<>(Arrays.asList(0));

        DefinitionResolver resolver = new DefinitionResolver();
        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> resolver.resolveConsumerDefinition(revisionHistory, supportedRevision, consumerApi)
        );

        // Ensure that the exception has the right error message
        assertTrue(exception.getMessage().contains("is not mapped"));
    }

    @Test
    void testIncompatibleTypesInMapping() {
        // Provider revision
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType providerType = new ProviderRecordType("TestType",
                Optional.empty(),
                0,
                providerApi,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("testField",
                Optional.empty(),
                providerType,
                AtomicType.INT_32,
                Optionality.MANDATORY);

        // Consumer definition
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0);

        ConsumerRecordType consumerType = new ConsumerRecordType("TestType",
                Optional.empty(),
                0,
                consumerApi,
                false,
                Optional.empty());

        new ConsumerField("testField",
                Optional.empty(),
                consumerType,
                AtomicType.INT_64,
                Optionality.MANDATORY,
                false);

        //
        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0));
        DefinitionResolver resolver = new DefinitionResolver();

        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> resolver.resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi));

        assertTrue(exception.getMessage().contains("do not match"));
    }

    @Test
    void mapEnumMembers() {
        // Consumer API definition
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0);

        ConsumerEnumType consumerEnum = new ConsumerEnumType("TestEnum",
                Optional.empty(),
                0,
                consumerApi);

        new ConsumerEnumMember("MEMBER_A",
                Optional.empty(),
                consumerEnum);

        new ConsumerEnumMember("MEMBER_B",
                Optional.empty(),
                consumerEnum);

        consumerApi.finalizeDefinition();

        // Provider API definition
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderEnumType providerEnum = new ProviderEnumType("TestEnum",
                Optional.empty(),
                0,
                providerApi,
                Optional.empty());

        new ProviderEnumMember("MEMBER_A",
                Optional.empty(),
                providerEnum,
                Optional.empty());

        new ProviderEnumMember("MEMBER_B",
                Optional.empty(),
                providerEnum,
                Optional.empty());

        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        String expected = "TestEnum -> TestEnum\n" +
                " MEMBER_A -> MEMBER_A\n" +
                " MEMBER_B -> MEMBER_B\n";

        String actual = new DefinitionResolutionPrinter().printDefinitionResolution(resolution);

        assertEquals(expected, actual);
    }

    /**
     * Test case: Map services and operations.
     */
    @Test
    void mapServicesAndOperations() {
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0);

        ConsumerRecordType consumerRecord = new ConsumerRecordType("RecordType",
                Optional.of("ConsumerRecordType"),
                0,
                consumerApi,
                false);

        ConsumerRecordType consumerException = new ConsumerRecordType("ExceptionType",
                Optional.of("ConsumerExceptionType"),
                1,
                consumerApi,
                false,
                true,
                Optional.empty());

        ConsumerService consumerService = new ConsumerService("Service",
                Optional.of("ConsumerService"),
                consumerApi);

        ConsumerServiceOperation consumerOperation = new ConsumerServiceOperation("operation",
                Optional.of("consumerOperation"),
                consumerService,
                consumerRecord,
                consumerRecord);

        consumerOperation.addThrownException(consumerException);
        consumerApi.finalizeDefinition();

        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType providerRecord = new ProviderRecordType("RecordType",
                Optional.of("ProviderRecordType"),
                0,
                providerApi,
                false,
                Optional.empty());

        ProviderRecordType providerException = new ProviderRecordType("ExceptionType",
                Optional.of("ProviderExceptionType"),
                1,
                providerApi,
                false,
                true,
                Optional.empty(),
                Optional.empty());

        ProviderService providerService = new ProviderService("Service",
                Optional.of("ProviderService"),
                providerApi,
                Optional.empty());

        ProviderServiceOperation providerOperation = new ProviderServiceOperation("operation",
                Optional.of("providerOperation"),
                providerService,
                providerRecord,
                providerRecord,
                Optional.empty());

        providerOperation.addThrownException(providerException);
        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        // TODO Add assertions
    }

}
