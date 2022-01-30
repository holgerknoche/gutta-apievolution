package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.*;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for definition resolution.
 */
class DefinitionResolverTest {

    @Test
    void testSimpleResolution() {
        // Create the consumer revision
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("some.test"),
                Collections.emptySet(),
                1);

        ConsumerRecordType consumerType = new ConsumerRecordType("Test",
                Optional.empty(),
                0,
                consumerApi,
                false,
                Optional.empty());

        ConsumerField unchangedFieldConsumer = new ConsumerField("unchangedField",
                Optional.empty(),
                consumerType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                false);

        // Create the provider revision history
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                1,
                Optional.empty());

        ProviderRecordType testTypeV1 = new ProviderRecordType("Test",
                Optional.empty(),
                0,
                revision1,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField unchangedFieldV1 = new ProviderField("unchangedField",
                Optional.empty(),
                testTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderField typeChangeFieldV1 = new ProviderField("typeChangeField",
                Optional.empty(),
                testTypeV1,
                AtomicType.INT_32,
                Optionality.OPT_IN);

        ProviderField deletedFieldV1 = new ProviderField("deletedField",
                Optional.empty(),
                testTypeV1,
                StringType.unbounded(),
                Optionality.OPTIONAL);

        ProviderEnumType testEnumV1 = new ProviderEnumType("TestEnum",
                Optional.empty(),
                0,
                revision1,
                Optional.empty());

        ProviderEnumMember unchangedMemberV1 = new ProviderEnumMember("UNCHANGED",
                Optional.empty(),
                testEnumV1,
                Optional.empty());

        ProviderEnumMember deletedMemberV1 = new ProviderEnumMember("DELETED",
                Optional.empty(),
                testEnumV1,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                2,
                Optional.empty());

        ProviderRecordType testTypeV2 = new ProviderRecordType("Test",
                Optional.of("TestInternal"),
                0,
                revision2,
                false,
                Optional.empty(),
                Optional.of(testTypeV1));

        ProviderField typeChangeFieldV2 = new ProviderField("typeChangeField",
                Optional.of("newTypeChangeField"),
                testTypeV2,
                AtomicType.INT_64,
                Optionality.MANDATORY);

        ProviderField unchangedFieldV2 = new ProviderField("unchangedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                false,
                Arrays.asList(unchangedFieldV1),
                Optional.of(unchangedFieldV1));

        ProviderField addedFieldV2 = new ProviderField("addedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderEnumType testEnumV2 = new ProviderEnumType("TestEnum",
                Optional.empty(),
                0,
                revision2,
                Optional.of(testEnumV1));

        ProviderEnumMember unchangedMemberV2 = new ProviderEnumMember("UNCHANGED",
                Optional.empty(),
                testEnumV2,
                Optional.of(unchangedMemberV1));

        ProviderEnumMember addedMemberV2 = new ProviderEnumMember("ADDED",
                Optional.empty(),
                testEnumV2,
                Optional.empty());

        // Resolve the consumer API against the revision history
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));
        new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);
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

}
