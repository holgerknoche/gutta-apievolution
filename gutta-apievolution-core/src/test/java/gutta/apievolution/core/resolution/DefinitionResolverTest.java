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

/**
 * TODO
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
                Optionality.MANDATORY);

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
                Optionality.MANDATORY,
                Optional.empty());

        ProviderField typeChangeFieldV1 = new ProviderField("typeChangeField",
                Optional.empty(),
                testTypeV1,
                AtomicType.INT_32,
                Optionality.MANDATORY,
                Optional.empty());

        ProviderField deletedFieldV1 = new ProviderField("deletedField",
                Optional.empty(),
                testTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

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
                Optionality.MANDATORY,
                Optional.empty());

        ProviderField unchangedFieldV2 = new ProviderField("unchangedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.of(unchangedFieldV1));

        ProviderField addedFieldV2 = new ProviderField("addedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

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

}
