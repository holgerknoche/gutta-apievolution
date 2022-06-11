package gutta.apievolution.core.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumType;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

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
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0);

        ConsumerRecordType consumerType = new ConsumerRecordType("TestType", Optional.empty(), 0, consumerApi, false,
                Optional.empty());

        new ConsumerField("int32Field", Optional.empty(), consumerType, AtomicType.INT_32, Optionality.MANDATORY);

        new ConsumerField("int64Field", Optional.empty(), consumerType, AtomicType.INT_64, Optionality.MANDATORY);

        new ConsumerField("unboundedStringField", Optional.empty(), consumerType, StringType.unbounded(),
                Optionality.MANDATORY);

        new ConsumerField("boundedStringField", Optional.empty(), consumerType, StringType.bounded(10),
                Optionality.MANDATORY);

        new ConsumerField("unboundedListField", Optional.empty(), consumerType,
                ListType.unbounded(StringType.unbounded()), Optionality.MANDATORY);

        new ConsumerField("boundedListField", Optional.empty(), consumerType,
                ListType.bounded(StringType.unbounded(), 10), Optionality.MANDATORY);

        new ConsumerField("numericField", Optional.empty(), consumerType, NumericType.bounded(5, 0),
                Optionality.MANDATORY);

        consumerApi.finalizeDefinition();

        // Provider API definition
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0, Optional.empty());

        ProviderRecordType providerType = new ProviderRecordType("TestType", Optional.empty(), 0, providerApi, false,
                Optional.empty());

        new ProviderField("int32Field", Optional.empty(), providerType, AtomicType.INT_32, Optionality.MANDATORY);

        new ProviderField("int64Field", Optional.empty(), providerType, AtomicType.INT_64, Optionality.MANDATORY);

        new ProviderField("unboundedStringField", Optional.empty(), providerType, StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderField("boundedStringField", Optional.empty(), providerType, StringType.bounded(10),
                Optionality.MANDATORY);

        new ProviderField("unboundedListField", Optional.empty(), providerType,
                ListType.unbounded(StringType.unbounded()), Optionality.MANDATORY);

        new ProviderField("boundedListField", Optional.empty(), providerType,
                ListType.bounded(StringType.unbounded(), 10), Optionality.MANDATORY);

        new ProviderField("numericField", Optional.empty(), providerType, NumericType.bounded(5, 0),
                Optionality.MANDATORY);

        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        String expected = "TestType -> TestType@revision 0\n" + " int32Field -> int32Field@TestType@revision 0\n" +
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
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0);

        ConsumerRecordType consumerType = new ConsumerRecordType("Test", Optional.empty(), 1, consumerApi, false,
                Optional.empty());

        new ConsumerField("optionalField", Optional.empty(), consumerType, AtomicType.INT_32, Optionality.OPTIONAL,
                false);

        // Define a provider API with a mandatory field
        ProviderApiDefinition revision = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(), 0,
                Optional.empty());

        ProviderRecordType recordType = new ProviderRecordType("Test", Optional.empty(), 1, revision, false,
                Optional.empty(), Optional.empty());

        new ProviderField("mandatoryField", Optional.empty(), recordType, AtomicType.INT_32, Optionality.MANDATORY);

        new ProviderField("optionalField", Optional.empty(), recordType, AtomicType.INT_32, Optionality.OPTIONAL);

        // Resolve the consumer definition against the revision history
        RevisionHistory revisionHistory = new RevisionHistory(revision);
        Set<Integer> supportedRevision = new HashSet<>(Arrays.asList(0));

        DefinitionResolver resolver = new DefinitionResolver();
        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> resolver.resolveConsumerDefinition(revisionHistory, supportedRevision, consumerApi));

        // Ensure that the exception has the right error message
        assertTrue(exception.getMessage().contains("is not mapped"));
    }

    /**
     * Test case: A mapping of types with incompatible (base) types is detected.
     */
    @Test
    void testIncompatibleBaseTypesInMapping() {
        // Provider revision
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0, Optional.empty());

        ProviderRecordType providerType = new ProviderRecordType("TestType", Optional.empty(), 0, providerApi, false,
                Optional.empty(), Optional.empty());

        new ProviderField("testField", Optional.empty(), providerType, AtomicType.INT_32, Optionality.MANDATORY);

        // Consumer definition
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0);

        ConsumerRecordType consumerType = new ConsumerRecordType("TestType", Optional.empty(), 0, consumerApi, false,
                Optional.empty());

        new ConsumerField("testField", Optional.empty(), consumerType, AtomicType.INT_64, Optionality.MANDATORY, false);

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
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0);

        ConsumerEnumType consumerEnum = new ConsumerEnumType("TestEnum", Optional.empty(), 0, consumerApi);

        new ConsumerEnumMember("MEMBER_A", Optional.empty(), consumerEnum);

        new ConsumerEnumMember("MEMBER_B", Optional.empty(), consumerEnum);

        consumerApi.finalizeDefinition();

        // Provider API definition
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0, Optional.empty());

        ProviderEnumType providerEnum = new ProviderEnumType("TestEnum", Optional.empty(), 0, providerApi,
                Optional.empty());

        new ProviderEnumMember("MEMBER_A", Optional.empty(), providerEnum, Optional.empty());

        new ProviderEnumMember("MEMBER_B", Optional.empty(), providerEnum, Optional.empty());

        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        String expected = "TestEnum -> TestEnum\n" + " MEMBER_A -> MEMBER_A\n" + " MEMBER_B -> MEMBER_B\n";

        String actual = new DefinitionResolutionPrinter().printDefinitionResolution(resolution);

        assertEquals(expected, actual);
    }

    /**
     * Test case: Map services and operations.
     */
    @Test
    void mapServicesAndOperations() {
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0);

        ConsumerRecordType consumerRecord = new ConsumerRecordType("RecordType", Optional.of("ConsumerRecordType"), 0,
                consumerApi, false);

        ConsumerRecordType consumerException = new ConsumerRecordType("ExceptionType",
                Optional.of("ConsumerExceptionType"), 1, consumerApi, false, true, Optional.empty());

        ConsumerOperation consumerOperation = new ConsumerOperation("operation", Optional.of("consumerOperation"),
                consumerApi, consumerRecord, consumerRecord);

        consumerOperation.addThrownException(consumerException);
        consumerApi.finalizeDefinition();

        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(),
                0, Optional.empty());

        ProviderRecordType providerRecord = new ProviderRecordType("RecordType", Optional.of("ProviderRecordType"), 0,
                providerApi, false, Optional.empty());

        ProviderRecordType providerException = new ProviderRecordType("ExceptionType",
                Optional.of("ProviderExceptionType"), 1, providerApi, false, true, Optional.empty(), Optional.empty());

        ProviderOperation providerOperation = new ProviderOperation("operation", Optional.of("providerOperation"),
                providerApi, providerRecord, providerRecord, Optional.empty());

        providerOperation.addThrownException(providerException);
        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        String expectedResolution = "ExceptionType(ConsumerExceptionType) -> ProviderExceptionType@revision 0\n" +
                "RecordType(ConsumerRecordType) -> ProviderRecordType@revision 0\n" +
                "operation(consumerOperation) -> operation(providerOperation)\n";

        assertEquals(expectedResolution, new DefinitionResolutionPrinter().printDefinitionResolution(resolution));
    }

    /**
     * Test case: Map list fields.
     */
    @Test
    void mapListFields() {
        // Create a consumer API with an bounded and unbounded list field
        ConsumerApiDefinition consumerApi = new ConsumerApiDefinition(QualifiedName.of("test"), Collections.emptySet(), 0);
        ConsumerRecordType consumerRecordA = new ConsumerRecordType("A", Optional.empty(), 0, consumerApi, false);
        ConsumerRecordType consumerRecordB = new ConsumerRecordType("B", Optional.empty(), 1, consumerApi, false);
        new ConsumerField("boundedListField", Optional.empty(), consumerRecordB, ListType.bounded(consumerRecordA, 10), Optionality.MANDATORY);
        new ConsumerField("unboundedListField", Optional.empty(), consumerRecordB, ListType.unbounded(consumerRecordA), Optionality.MANDATORY);
        consumerApi.finalizeDefinition();
        
        // Create a matching provider API
        ProviderApiDefinition providerApi = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(), 0, Optional.empty());
        ProviderRecordType providerRecordA = new ProviderRecordType("A", Optional.empty(), 0, providerApi, false, Optional.empty());
        ProviderRecordType providerRecordB = new ProviderRecordType("B", Optional.empty(), 1, providerApi, false, Optional.empty());
        new ProviderField("boundedListField", Optional.empty(), providerRecordB, ListType.bounded(providerRecordA, 10), Optionality.MANDATORY);
        new ProviderField("unboundedListField", Optional.empty(), providerRecordB, ListType.unbounded(providerRecordA), Optionality.MANDATORY);
        providerApi.finalizeDefinition();
        
        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory,
                supportedRevisions, consumerApi);

        String expectedResolution = "A -> A@revision 0\n" +
                "B -> B@revision 0\n" +
                " boundedListField -> boundedListField@B@revision 0\n" +
                " unboundedListField -> unboundedListField@B@revision 0\n";

        assertEquals(expectedResolution, new DefinitionResolutionPrinter().printDefinitionResolution(resolution));
    }
 
    /**
     * Test case: A mapping of fields with incompatible record types is detected.
     */
    @Test
    void mapIncompatibleRecordTypes() {
        // Construct a provider API with a field of type "C".
        ProviderApiDefinition providerDefinition = new ProviderApiDefinition("test", Collections.emptySet(), 0, Optional.empty());
        
        ProviderRecordType providerTypeA = new ProviderRecordType("A", Optional.empty(), 0, providerDefinition, false, Optional.empty());
        ProviderRecordType providerTypeB = new ProviderRecordType("B", Optional.empty(), 1, providerDefinition, false, Optional.empty());
        new ProviderRecordType("C", Optional.empty(), 2, providerDefinition, false, Optional.empty());
        
        new ProviderField("field", Optional.empty(), providerTypeA, providerTypeB, Optionality.MANDATORY);
        
        providerDefinition.finalizeDefinition();
        
        // Construct a consumer API with a field of type "B".
        ConsumerApiDefinition consumerDefinition = new ConsumerApiDefinition("test", Collections.emptySet(), 0);
        
        ConsumerRecordType consumerTypeA = new ConsumerRecordType("A", Optional.empty(), 0, consumerDefinition, false);
        new ConsumerRecordType("B", Optional.empty(), 1, consumerDefinition, false);
        ConsumerRecordType consumerTypeC = new ConsumerRecordType("C", Optional.empty(), 2, consumerDefinition, false);
        
        new ConsumerField("field", Optional.empty(), consumerTypeA, consumerTypeC, Optionality.MANDATORY);
        
        consumerDefinition.finalizeDefinition();
        
        // Try to resolve the revisions against each other
        RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
        Set<Integer> supportedRevisions = Collections.singleton(0);
        
        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class, 
                () -> new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions,
                        consumerDefinition));
        
        assertTrue(exception.getMessage().contains("do not match"));
    }
        
    private DefinitionResolution runOptionalityTestForInputOnly(Optionality providerOptionality, Optionality consumerOptionality) {
        // Create a provider API definition with a type that is only used as input
        ProviderApiDefinition providerDefinition = new ProviderApiDefinition("test", Collections.emptySet(), 0, Optional.empty());
        
        ProviderRecordType providerInputType = new ProviderRecordType("A", Optional.empty(), 0, providerDefinition, false, Optional.empty());
        new ProviderField("field", Optional.empty(), providerInputType, AtomicType.INT_32, providerOptionality);
        
        ProviderRecordType providerOutputType = new ProviderRecordType("B", Optional.empty(), 1, providerDefinition, false, Optional.empty());
        
        new ProviderOperation("op", Optional.empty(), providerDefinition, providerOutputType, providerInputType, Optional.empty());
        
        providerDefinition.finalizeDefinition();
        
        // Create a consumer API with a matching type
        ConsumerApiDefinition consumerDefinition = new ConsumerApiDefinition("test", Collections.emptySet(), 0);
        
        ConsumerRecordType consumerInputType = new ConsumerRecordType("A", Optional.empty(), 0, consumerDefinition, false, Optional.empty());
        
        if (consumerOptionality != null) {
            // Do not create a field if no consumer optionality is given
            new ConsumerField("field", Optional.empty(), consumerInputType, AtomicType.INT_32, consumerOptionality);
        }
        
        ConsumerRecordType consumerOutputType = new ConsumerRecordType("B", Optional.empty(), 1, consumerDefinition, false, Optional.empty());
        
        new ConsumerOperation("op", Optional.empty(), consumerDefinition, consumerOutputType, consumerInputType);
        
        consumerDefinition.finalizeDefinition();
        
        // Try to resolve the consumer definition against the revision history
        RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
        Set<Integer> supportedRevisions = Collections.singleton(0);
        
        return new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerDefinition);
    }
    
    /**
     * Test case: Map a mandatory field that is used for input only in different consumer constructs.
     */
    @Test
    void mapMandatoryInputField() {
        DefinitionResolutionException exception;
        
        // The field must be mapped, so not mapping it must fail
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInputOnly(Optionality.MANDATORY, null));
        assertTrue(exception.getMessage().contains("Non-optional field") && exception.getMessage().contains("is not mapped"));
        
        // Mapping mandatory to mandatory must work
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.MANDATORY, Optionality.MANDATORY));
        // The field cannot be considered optional-for-input
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInputOnly(Optionality.MANDATORY, Optionality.OPT_IN));
        assertTrue(exception.getMessage().contains("are not compatible"));
        // Same for fully optional
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInputOnly(Optionality.MANDATORY, Optionality.OPTIONAL));
        assertTrue(exception.getMessage().contains("are not compatible"));
    }
    
    /**
     * Test case: Map an opt-in field that is used for input only in different consumer constructs.
     */
    @Test
    void mapOptInInputField() {
        // The field is opt-in, so it does not have to be mapped
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPT_IN, null));
        
        // The field may be considered mandatory by the consumer
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPT_IN, Optionality.MANDATORY));
        // The field can be considered optional-for-input
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPT_IN, Optionality.OPT_IN));
        // Same for fully optional
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPT_IN, Optionality.OPTIONAL));
    }
    
    /**
     * Test case: Map an opt-in field that is used for input only in different consumer constructs.
     */
    @Test
    void mapOptionalInputField() {
        // The field is optional, so it does not have to be mapped
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPTIONAL, null));
        
        // The field may be considered mandatory by the consumer
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPTIONAL, Optionality.MANDATORY));
        // The field can be considered optional-for-input
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPTIONAL, Optionality.OPT_IN));
        // Same for fully optional
        assertNotNull(this.runOptionalityTestForInputOnly(Optionality.OPTIONAL, Optionality.OPTIONAL));
    }
    
}
