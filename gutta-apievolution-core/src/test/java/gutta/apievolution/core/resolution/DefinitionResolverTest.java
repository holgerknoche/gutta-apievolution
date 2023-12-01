package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.Usage;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumType;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.validation.ValidationMessage;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import static java.util.Collections.*;

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
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerType = consumerApi.newRecordType("TestType", 0);

        consumerType.newField("int32Field", AtomicType.INT_32, Optionality.MANDATORY);
        consumerType.newField("int64Field", AtomicType.INT_64, Optionality.MANDATORY);
        consumerType.newField("unboundedStringField", StringType.unbounded(), Optionality.MANDATORY);
        consumerType.newField("boundedStringField", StringType.bounded(10), Optionality.MANDATORY);
        consumerType.newField("unboundedListField", ListType.unbounded(StringType.unbounded()), Optionality.MANDATORY);
        consumerType.newField("boundedListField", ListType.bounded(StringType.unbounded(), 10), Optionality.MANDATORY);
        consumerType.newField("numericField", NumericType.bounded(5, 0), Optionality.MANDATORY);

        consumerApi.finalizeDefinition();

        // Provider API definition
        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerType = providerApi.newRecordType("TestType", 0);

        providerType.newField("int32Field", AtomicType.INT_32, Optionality.MANDATORY);
        providerType.newField("int64Field", AtomicType.INT_64, Optionality.MANDATORY);
        providerType.newField("unboundedStringField", StringType.unbounded(), Optionality.MANDATORY);
        providerType.newField("boundedStringField", StringType.bounded(10), Optionality.MANDATORY);
        providerType.newField("unboundedListField", ListType.unbounded(StringType.unbounded()), Optionality.MANDATORY);
        providerType.newField("boundedListField", ListType.bounded(StringType.unbounded(), 10), Optionality.MANDATORY);
        providerType.newField("numericField", NumericType.bounded(5, 0), Optionality.MANDATORY);

        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);

        String expected = "TestType -> TestType@revision 0\n" + " int32Field -> int32Field@TestType@revision 0\n" +
                " int64Field -> int64Field@TestType@revision 0\n" + " unboundedStringField -> unboundedStringField@TestType@revision 0\n" +
                " boundedStringField -> boundedStringField@TestType@revision 0\n" + " unboundedListField -> unboundedListField@TestType@revision 0\n" +
                " boundedListField -> boundedListField@TestType@revision 0\n" + " numericField -> numericField@TestType@revision 0\n";

        String actual = new DefinitionResolutionPrinter().printDefinitionResolution(resolution);

        assertEquals(expected, actual);
    }

    /**
     * Assert that a missing mapping of a mandatory field is appropriately reported.
     */
    @Test
    void testMissingMappingForMandatoryField() {
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerType = consumerApi.newRecordType("Test", 1);

        consumerType.newField("optionalField", AtomicType.INT_32, Optionality.OPTIONAL);

        consumerApi.newOperation("operation", consumerType, consumerType);

        // Define a provider API with a mandatory field
        ProviderApiDefinition revision = ProviderApiDefinition.create("test", 0);

        ProviderRecordType recordType = revision.newRecordType("Test", 1);

        recordType.newField("mandatoryField", AtomicType.INT_32, Optionality.MANDATORY);
        recordType.newField("optionalField", AtomicType.INT_32, Optionality.OPTIONAL);

        revision.newOperation("operation", recordType, recordType);

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
        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerType = providerApi.newRecordType("TestType", 0);

        providerType.newField("testField", AtomicType.INT_32, Optionality.MANDATORY);

        // Consumer definition
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerType = consumerApi.newRecordType("TestType", 0);

        consumerType.newField("testField", AtomicType.INT_64, Optionality.MANDATORY);

        //
        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0));
        DefinitionResolver resolver = new DefinitionResolver();

        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> resolver.resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi));

        assertTrue(exception.getMessage().contains("is mapped to incompatible type"));
    }

    @Test
    void mapEnumMembers() {
        // Consumer API definition
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerEnumType consumerEnum = consumerApi.newEnumType("TestEnum", 0);

        consumerEnum.newEnumMember("MEMBER_A");
        consumerEnum.newEnumMember("MEMBER_B");

        consumerApi.finalizeDefinition();

        // Provider API definition
        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderEnumType providerEnum = providerApi.newEnumType("TestEnum", 0);

        providerEnum.newEnumMember("MEMBER_A");
        providerEnum.newEnumMember("MEMBER_B");

        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);

        String expected = "TestEnum -> TestEnum\n" + " MEMBER_A -> MEMBER_A\n" + " MEMBER_B -> MEMBER_B\n";

        String actual = new DefinitionResolutionPrinter().printDefinitionResolution(resolution);

        assertEquals(expected, actual);
    }

    /**
     * Test case: Map services and operations.
     */
    @Test
    void mapServicesAndOperations() {
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerRecord = consumerApi.newRecordType("RecordType", "ConsumerRecordType", 0, Abstract.NO, noSuperTypes());

        ConsumerRecordType consumerException = consumerApi.newExceptionType("ExceptionType", "ConsumerExceptionType", 1, Abstract.NO, noSuperTypes());

        ConsumerOperation consumerOperation = consumerApi.newOperation("operation", "consumerOperation", consumerRecord, consumerRecord);

        consumerOperation.addThrownException(consumerException);
        consumerApi.finalizeDefinition();

        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerRecord = providerApi.newRecordType("RecordType", "ProviderRecordType", 0, noPredecessor());

        ProviderRecordType providerException = providerApi.newExceptionType("ExceptionType", "ProviderExceptionType", 1, noPredecessor());

        ProviderOperation providerOperation = providerApi.newOperation("operation", "providerOperation", providerRecord, providerRecord, noPredecessor());

        providerOperation.addThrownException(providerException);
        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);

        String expectedResolution = "ExceptionType(ConsumerExceptionType) -> ProviderExceptionType@revision 0\n" +
                "RecordType(ConsumerRecordType) -> ProviderRecordType@revision 0\n" + "operation(consumerOperation) -> operation(providerOperation)\n";

        assertEquals(expectedResolution, new DefinitionResolutionPrinter().printDefinitionResolution(resolution));
    }

    /**
     * Test case: Map list fields.
     */
    @Test
    void mapListFields() {
        // Create a consumer API with an bounded and unbounded list field
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);
        ConsumerRecordType consumerRecordA = consumerApi.newRecordType("A", 0);
        ConsumerRecordType consumerRecordB = consumerApi.newRecordType("B", 1);
        consumerRecordB.newField("boundedListField", ListType.bounded(consumerRecordA, 10), Optionality.MANDATORY);
        consumerRecordB.newField("unboundedListField", ListType.unbounded(consumerRecordA), Optionality.MANDATORY);
        consumerApi.finalizeDefinition();

        // Create a matching provider API
        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);
        ProviderRecordType providerRecordA = providerApi.newRecordType("A", 0);
        ProviderRecordType providerRecordB = providerApi.newRecordType("B", 1);
        providerRecordB.newField("boundedListField", ListType.bounded(providerRecordA, 10), Optionality.MANDATORY);
        providerRecordB.newField("unboundedListField", ListType.unbounded(providerRecordA), Optionality.MANDATORY);
        providerApi.finalizeDefinition();

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);

        String expectedResolution = "A -> A@revision 0\n" + "B -> B@revision 0\n" + " boundedListField -> boundedListField@B@revision 0\n" +
                " unboundedListField -> unboundedListField@B@revision 0\n";

        assertEquals(expectedResolution, new DefinitionResolutionPrinter().printDefinitionResolution(resolution));
    }

    /**
     * Test case: A mapping of fields with incompatible record types is detected.
     */
    @Test
    void mapIncompatibleRecordTypes() {
        // Construct a provider API with a field of type "C".
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerTypeA = providerDefinition.newRecordType("A", 0);
        ProviderRecordType providerTypeB = providerDefinition.newRecordType("B", 1);
        providerDefinition.newRecordType("C", 2);

        providerTypeA.newField("field", providerTypeB, Optionality.MANDATORY);

        providerDefinition.finalizeDefinition();

        // Construct a consumer API with a field of type "B".
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerTypeA = consumerDefinition.newRecordType("A", 0);
        consumerDefinition.newRecordType("B", 1);
        ConsumerRecordType consumerTypeC = consumerDefinition.newRecordType("C", 2);

        consumerTypeA.newField("field", consumerTypeC, Optionality.MANDATORY);

        consumerDefinition.finalizeDefinition();

        // Try to resolve the revisions against each other
        RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerDefinition));

        assertTrue(exception.getMessage().contains("mapped to incompatible type"));
    }

    private DefinitionResolution runOptionalityTest(Optionality providerOptionality, Optionality consumerOptionality, Usage desiredUsage) {
        // Create a provider API definition with a type that is only used as input
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerTypeWithField = providerDefinition.newRecordType("A", 0);
        providerTypeWithField.newField("field", AtomicType.INT_32, providerOptionality);

        ProviderRecordType providerTypeNoField = providerDefinition.newRecordType("B", 1);

        ProviderRecordType providerInputType;
        ProviderRecordType providerOutputType;

        switch (desiredUsage) {
        case INPUT:
            providerInputType = providerTypeWithField;
            providerOutputType = providerTypeNoField;
            break;

        case OUTPUT:
            providerInputType = providerTypeNoField;
            providerOutputType = providerTypeWithField;
            break;

        default:
            providerInputType = providerTypeWithField;
            providerOutputType = providerTypeWithField;
            break;

        }

        providerDefinition.newOperation("op", providerOutputType, providerInputType);

        providerDefinition.finalizeDefinition();

        // Create a consumer API with a matching type
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerTypeWithField = consumerDefinition.newRecordType("A", 0);

        if (consumerOptionality != null) {
            // Do not create a field if no consumer optionality is given
            consumerTypeWithField.newField("field", AtomicType.INT_32, consumerOptionality);
        }

        ConsumerRecordType consumerTypeNoField = consumerDefinition.newRecordType("B", 1);

        ConsumerRecordType consumerInputType;
        ConsumerRecordType consumerOutputType;

        switch (desiredUsage) {
        case INPUT:
            consumerInputType = consumerTypeWithField;
            consumerOutputType = consumerTypeNoField;
            break;

        case OUTPUT:
            consumerInputType = consumerTypeNoField;
            consumerOutputType = consumerTypeWithField;
            break;

        default:
            consumerInputType = consumerTypeWithField;
            consumerOutputType = consumerTypeWithField;
            break;

        }

        consumerDefinition.newOperation("op", consumerOutputType, consumerInputType);

        consumerDefinition.finalizeDefinition();

        // Try to resolve the consumer definition against the revision history
        RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        return new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerDefinition);
    }

    private DefinitionResolution runOptionalityTestForInputOnly(Optionality providerOptionality, Optionality consumerOptionality) {
        return this.runOptionalityTest(providerOptionality, consumerOptionality, Usage.INPUT);
    }

    /**
     * Test case: Map a mandatory field that is used for input only in different consumer constructs.
     */
    @Test
    void mapMandatoryInputField() {
        DefinitionResolutionException exception;

        // The field must be mapped, so not mapping it must fail
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInputOnly(Optionality.MANDATORY, null));
        assertTrue(exception.getMessage().contains("Mandatory field") && exception.getMessage().contains("is not mapped"));

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
     * Test case: Map an optional field that is used for input only in different consumer constructs.
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

    private DefinitionResolution runOptionalityTestForOutputOnly(Optionality providerOptionality, Optionality consumerOptionality) {
        return this.runOptionalityTest(providerOptionality, consumerOptionality, Usage.OUTPUT);
    }

    /**
     * Test case: Map a mandatory field that is used for output only in different consumer constructs.
     */
    @Test
    void mapMandatoryOutputField() {
        // Output-only fields do not have to be mapped, even if they are marked as
        // mandatory by the provider
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.MANDATORY, null));

        // Mapping mandatory to mandatory must work
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.MANDATORY, Optionality.MANDATORY));
        // The field can be considered optional-for-input by the consumer, which has no
        // effect for output-only fields
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.MANDATORY, Optionality.OPT_IN));
        // Same for fully optional
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.MANDATORY, Optionality.OPTIONAL));
    }

    /**
     * Test case: Map an opt-in field that is used for output only in different consumer constructs.
     */
    @Test
    void mapOptInOutputField() {
        // Output-only fields do not have to be mapped
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.OPT_IN, null));

        // The field may be considered mandatory by the consumer
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.OPT_IN, Optionality.MANDATORY));
        // Same for optional-as-input
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.OPT_IN, Optionality.OPT_IN));
        // Same for fully optional
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.OPT_IN, Optionality.OPTIONAL));
    }

    /**
     * Test case: Map an optional field that is used for output only in different consumer constructs.
     */
    @Test
    void mapOptionalOutputField() {
        DefinitionResolutionException exception;

        // Output-only fields do not have to be mapped
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.OPTIONAL, null));

        // Optional fields can only be considered optional by the consumer
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForOutputOnly(Optionality.OPTIONAL, Optionality.MANDATORY));
        assertTrue(exception.getMessage().contains("are not compatible"));
        // Same for optional-for-input
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForOutputOnly(Optionality.OPTIONAL, Optionality.OPT_IN));
        assertTrue(exception.getMessage().contains("are not compatible"));
        // Optional must work
        assertNotNull(this.runOptionalityTestForOutputOnly(Optionality.OPTIONAL, Optionality.OPTIONAL));
    }

    private DefinitionResolution runOptionalityTestForInOut(Optionality providerOptionality, Optionality consumerOptionality) {
        return this.runOptionalityTest(providerOptionality, consumerOptionality, Usage.IN_OUT);
    }

    /**
     * Test case: Map a mandatory field that is used for both input and output in different consumer constructs.
     */
    @Test
    void mapMandatoryInOutField() {
        DefinitionResolutionException exception;

        // Mandatory fields must be mapped
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInOut(Optionality.MANDATORY, null));
        assertTrue(exception.getMessage().contains("Mandatory field") && exception.getMessage().contains("is not mapped"));

        // Mapping mandatory to mandatory must work
        assertNotNull(this.runOptionalityTestForInOut(Optionality.MANDATORY, Optionality.MANDATORY));
        // No optionality is allowed for mandatory in-out fields
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInOut(Optionality.MANDATORY, Optionality.OPT_IN));
        assertTrue(exception.getMessage().contains("are not compatible"));
        // Same for fully optional
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInOut(Optionality.MANDATORY, Optionality.OPTIONAL));
        assertTrue(exception.getMessage().contains("are not compatible"));
    }

    /**
     * Test case: Map an opt-in field that is used for both input and output in different consumer constructs.
     */
    @Test
    void mapOptInInOutField() {
        // Opt-in fields do not have to be mapped, since the consumer does not have to
        // provide it and is free to ignore it
        assertNotNull(this.runOptionalityTestForInOut(Optionality.OPT_IN, null));

        // The field may be considered mandatory by the consumer
        assertNotNull(this.runOptionalityTestForInOut(Optionality.OPT_IN, Optionality.MANDATORY));
        // Same for optional-as-input
        assertNotNull(this.runOptionalityTestForInOut(Optionality.OPT_IN, Optionality.OPT_IN));
        // Same for fully optional
        assertNotNull(this.runOptionalityTestForInOut(Optionality.OPT_IN, Optionality.OPTIONAL));
    }

    /**
     * Test case: Map an optional field that is used for both input and output in different consumer constructs.
     */
    @Test
    void mapOptionalInOutField() {
        DefinitionResolutionException exception;

        // Fully optional fields do not have to be mapped
        assertNotNull(this.runOptionalityTestForInOut(Optionality.OPTIONAL, null));

        // Optional fields can only be considered optional by the consumer
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInOut(Optionality.OPTIONAL, Optionality.MANDATORY));
        assertTrue(exception.getMessage().contains("are not compatible"));
        // Same for optional-for-input
        exception = assertThrows(DefinitionResolutionException.class, () -> this.runOptionalityTestForInOut(Optionality.OPTIONAL, Optionality.OPT_IN));
        assertTrue(exception.getMessage().contains("are not compatible"));
        // Optional must work
        assertNotNull(this.runOptionalityTestForInOut(Optionality.OPTIONAL, Optionality.OPTIONAL));
    }

    /**
     * Test case: The return type of a mapped provider operation is not mapped, which leads to an error.
     */
    @Test
    void unmappedReturnTypeOfMappedProviderOperation() {
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerResultType = consumerApi.newRecordType("ConsumerResult", 0);
        ConsumerRecordType consumerParameterType = consumerApi.newRecordType("Parameter", 1);

        consumerApi.newOperation("operation", consumerResultType, consumerParameterType);

        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerResultType = providerApi.newRecordType("ProviderResult", 0);
        ProviderRecordType providerParameterType = providerApi.newRecordType("Parameter", 1);

        providerApi.newOperation("operation", providerResultType, providerParameterType);

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi));
        assertTrue(exception.getMessage().contains("No matching type for ConsumerResult"));
    }

    /**
     * Test case: The parameter type of a mapped provider operation is not mapped, which leads to an error.
     */
    @Test
    void unmappedParameterTypeOfMappedProviderOperation() {
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerResultType = consumerApi.newRecordType("Result", 0);
        ConsumerRecordType consumerParameterType = consumerApi.newRecordType("ConsumerParameter", 1);

        consumerApi.newOperation("operation", consumerResultType, consumerParameterType);

        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerResultType = providerApi.newRecordType("Result", 0);
        ProviderRecordType providerParameterType = providerApi.newRecordType("ProviderParameter", 1);

        providerApi.newOperation("operation", providerResultType, providerParameterType);

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolutionException exception = assertThrows(DefinitionResolutionException.class,
                () -> new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi));
        assertTrue(exception.getMessage().contains("No matching type for ConsumerParameter"));
    }

    /**
     * Test case: An exception type of a mapped provider operation is not mapped, which leads to a warning.
     */
    @Test
    void unmappedExceptionTypeOfMappedProviderOperation() {
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);

        ConsumerRecordType consumerResultType = consumerApi.newRecordType("Result", 0);
        ConsumerRecordType consumerParameterType = consumerApi.newRecordType("Parameter", 1);

        consumerApi.newOperation("operation", consumerResultType, consumerParameterType);

        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);

        ProviderRecordType providerResultType = providerApi.newRecordType("Result", 0);
        ProviderRecordType providerParameterType = providerApi.newRecordType("Parameter", 1);

        ProviderRecordType providerExceptionType = providerApi.newExceptionType("ProviderException", 2);
        providerExceptionType.newField("exceptionField", AtomicType.INT_32, Optionality.MANDATORY);

        ProviderOperation providerOperation = providerApi.newOperation("operation", providerResultType, providerParameterType);
        providerOperation.addThrownException(providerExceptionType);

        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);

        List<ValidationMessage> validationMessages = resolution.getValidationMessages();
        assertEquals(singletonList(ValidationMessage.warning("Unmapped exception type 'ProviderException@revision 0' on operation 'operation'.")),
                validationMessages);
    }

    /**
     * Test case: A consumer type unrelated to any mapped operation results in a warning.
     */
    @Test
    void unrelatedRecordTypeInConsumerDefinition() {
        ConsumerApiDefinition consumerApi = TestFixtures.createConsumerApiDefinition("test", 0);        
        consumerApi.newRecordType("unrelated", 0);        
        consumerApi.finalizeDefinition();
        
        ProviderApiDefinition providerApi = ProviderApiDefinition.create("test", 0);
        providerApi.newRecordType("unrelated", 0);
        providerApi.finalizeDefinition();
        
        RevisionHistory revisionHistory = new RevisionHistory(providerApi);
        Set<Integer> supportedRevisions = Collections.singleton(0);

        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, supportedRevisions, consumerApi);

        List<ValidationMessage> validationMessages = resolution.getValidationMessages();
        assertEquals(singletonList(ValidationMessage.warning("Type 'unrelated' is not related to any operation.")), validationMessages);
    }

}
