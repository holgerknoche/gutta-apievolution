package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.Usage;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumType;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.consumer.ConsumerUserDefinedType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderUserDefinedType;
import gutta.apievolution.core.validation.ValidationMessage;
import gutta.apievolution.core.validation.ValidationResult;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.util.MapUtil.mapOf;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for map from the consumer revision to the provider revision.
 */
class ConsumerToProviderMapTest {
    
    /**
     * Test case: A consistent mapping is accepted.
     */
    @Test
    void consistentMapping() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        ConsumerField consumerField = consumerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        ConsumerEnumType consumerEnum = consumerDefinition.newEnumType("TestEnum", 1);
        ConsumerEnumMember consumerMember = consumerEnum.newEnumMember("TEST");
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("op", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", 0);
        ProviderField providerField = providerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        ProviderEnumType providerEnum = providerDefinition.newEnumType("TestEnum", 1);
        ProviderEnumMember providerMember = providerEnum.newEnumMember("TEST");
        
        ProviderOperation providerOperation = providerDefinition.newOperation("op", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                mapOf(consumerRecord, providerRecord, consumerEnum, providerEnum),
                mapOf(consumerField, providerField),
                mapOf(consumerMember, providerMember),
                mapOf(consumerOperation, providerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: At least one consumer record type is not mapped.
     */
    @Test
    void missingMappingForConsumerRecord() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        consumerDefinition.newRecordType("TestRecord", 0);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                emptyMap(), emptyMap(), emptyMap(), emptyMap());
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList(ValidationMessage.error("User-defined type 'TestRecord' is not mapped.")), result.getMessages());
    }

    /**
     * Test case: A missing mapping for an enum type is detected and reported.
     */
    @Test
    void missingMappingForConsumerEnum() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        consumerDefinition.newEnumType("TestEnum", 0);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                emptyMap(), emptyMap(), emptyMap(), emptyMap());
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList(ValidationMessage.error("User-defined type 'TestEnum' is not mapped.")), result.getMessages());
    }
    
    /**
     * A missing mapping for a field is detected and reported.
     */
    @Test
    void missingMappingForConsumerField() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        consumerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("operation", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", 0);
        
        ProviderOperation providerOperation = providerDefinition.newOperation("operation", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerRecord, providerRecord);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap, emptyMap(), emptyMap(), mapOf(consumerOperation, providerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList(ValidationMessage.error("Field 'field@TestRecord' is not mapped.")), result.getMessages());
    }
    
    /**
     * A missing mapping of an enum member is detected and reported.
     */
    @Test
    void missingMappingForConsumerEnumMember() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerEnumType consumerEnum = consumerDefinition.newEnumType("TestEnum", 0);
        consumerEnum.newEnumMember("TEST");
        
        // Establish connection to an operation
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("Record", 1);
        ConsumerField consumerField = consumerRecord.newField("field", consumerEnum, Optionality.MANDATORY);
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("operation", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderEnumType providerEnum = providerDefinition.newEnumType("TestEnum", 0);
        
        // Establish connection to an operation
        ProviderRecordType providerRecord = providerDefinition.newRecordType("Record", 1);
        ProviderField providerField = providerRecord.newField("field", providerEnum, Optionality.MANDATORY);
        ProviderOperation providerOperation = providerDefinition.newOperation("operation", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerEnum, providerEnum, consumerRecord, providerRecord);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap, mapOf(consumerField, providerField), emptyMap(), mapOf(consumerOperation, providerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList(ValidationMessage.error("Enum member 'TEST' is not mapped.")), result.getMessages());
    }
    
    /**
     * A missing mapping of an operation is detected an reported.
     */
    @Test
    void missingMappingForConsumerOperation() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        
        // Unmapped operation
        consumerDefinition.newOperation("unmapped", consumerRecord, consumerRecord);        
        
        // Mapped operation to avoid warnings due to unmapped types
        ConsumerOperation mappedConsumerOperation = consumerDefinition.newOperation("mapped", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", 0);
        
        ProviderOperation mappedProviderOperation = providerDefinition.newOperation("mapped", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerRecord, providerRecord);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap, emptyMap(), emptyMap(), mapOf(mappedConsumerOperation, mappedProviderOperation));
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList(ValidationMessage.error("Operation 'unmapped' is not mapped.")), result.getMessages());
    }
    
    /**
     * Test case: An inconsistent super type mapping is detected and reported.
     */
    @Test
    void inconsistentSuperType() {
     // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerSuperType = consumerDefinition.newRecordType("SuperType", 0);
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", noInternalName(), 1,
                Abstract.NO, Collections.singleton(consumerSuperType));
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("operation", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderRecordType providerSuperType = providerDefinition.newRecordType("SuperType", 0);
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", noInternalName(), 1,
                Abstract.NO, Collections.singleton(providerSuperType), noPredecessor());
        ProviderRecordType providerDummy = providerDefinition.newRecordType("Dummy", 2);
        
        ProviderOperation providerOperation = providerDefinition.newOperation("operation", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                mapOf(consumerRecord, providerRecord, consumerSuperType, providerDummy),
                emptyMap(),
                emptyMap(),
                mapOf(consumerOperation, providerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(asList(ValidationMessage.error("Mapped supertype 'Dummy@revision 0' of 'TestRecord' is not a supertype of 'TestRecord@revision 0'.")),
                result.getMessages());
    }
    
    /**
     * Test case: Mappings of an input field considered mandatory by the consumer.
     */
    @Test
    void mandatoryFieldForInput() {
        ValidationResult result;
        
        // When the consumer considers an input field mandatory, it can be of any optionality
        // on the provider side
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.MANDATORY, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.MANDATORY, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.MANDATORY, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: Mappings of an input field considered opt-in by the consumer.
     */
    @Test
    void optInFieldForInput() {
        ValidationResult result;
        
        // When the consumer considers an input field opt-in, it may not be mandatory on the
        // provider side
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.OPT_IN, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.OPT_IN, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.OPT_IN, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: Mappings of an input field considered optional by the consumer.
     */
    @Test
    void optionalFieldForInput() {
        ValidationResult result;
        
        // When the consumer considers an input field optional, it may not be mandatory on the
        // provider side
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.OPTIONAL, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.OPTIONAL, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT_ONLY, Optionality.OPTIONAL, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: Mappings of an output field considered mandatory by the consumer.
     */
    @Test
    void mandatoryFieldForOutput() {
        ValidationResult result;
        
        // When the consumer considers an output field mandatory, it may not be optional on
        // the provider side
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.MANDATORY, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.MANDATORY, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.MANDATORY, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    /**
     * Test case: Mappings of an output field considered opt-in by the consumer.
     */
    @Test
    void optInFieldForOutput() {
        ValidationResult result;
        
        // When the consumer considers an output field opt-in, it may not be optional on the
        // provider side
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.OPT_IN, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.OPT_IN, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.OPT_IN, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    /**
     * Test case: Mappings of an output field considered optional by the consumer.
     */
    @Test
    void optionalFieldForOutput() {
        ValidationResult result;
        
        // When the consumer considers an output field optional, it may be of any optionality
        // on the provider side
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.OPTIONAL, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.OPTIONAL, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT_ONLY, Optionality.OPTIONAL, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: Mappings of an in-out field considered mandatory by the consumer.
     */
    @Test
    void mandatoryFieldForInOut() {
        ValidationResult result;
        
        // When the consumer considers an in-out field mandatory, it must be at mandatory or
        // opt-in on the provider side
        result = runOptionalityTest(Usage.IN_OUT, Optionality.MANDATORY, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.MANDATORY, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.MANDATORY, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    /**
     * Test case: Mappings of an in-out field considered opt-in by the consumer.
     */
    @Test
    void optInFieldForInOut() {
        ValidationResult result;
        
        // When the consumer considers an in-out field opt-in, it must be opt-in on the
        // provider side
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPT_IN, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPT_IN, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPT_IN, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    /**
     * Test case: Mappings of an in-out field considered optional by the consumer.
     */
    @Test
    void optionalFieldForInOut() {
        ValidationResult result;
        
        // When the consumer considers an in-out field optional, it must be optional or opt-in
        // on the provider side
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPTIONAL, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPTIONAL, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPTIONAL, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: Inherited fields are mapped appropriately; in particular, the copies of inherited fields at receiving types do not
     * interfere with consistency checks.
     */
    @Test
    void inheritedFieldMapping() {
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerSuperType = consumerDefinition.newRecordType("SuperType", 0);
        ConsumerField consumerInheritedField = consumerSuperType.newField("inheritedField", AtomicType.INT_32, Optionality.MANDATORY);
        ConsumerRecordType consumerSubType = consumerDefinition.newRecordType("SubType", noInternalName(), 1, Abstract.NO, Collections.singleton(consumerSuperType));
        ConsumerField consumerSubField = consumerSubType.newField("field", AtomicType.INT_32, Optionality.MANDATORY);
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("operation", consumerSuperType, consumerSuperType);
        
        consumerDefinition.finalizeDefinition();
        
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType providerSuperType = providerDefinition.newRecordType("SuperType", 0);
        ProviderField providerInheritedField = providerSuperType.newField("inheritedField", AtomicType.INT_32, Optionality.MANDATORY);
        ProviderRecordType providerSubType = providerDefinition.newRecordType("SubType", noInternalName(), 1, Abstract.NO, Collections.singleton(providerSuperType), noPredecessor());
        ProviderField providerSubField = providerSubType.newField("field", AtomicType.INT_32, Optionality.MANDATORY);
        
        ProviderOperation providerOperation = providerDefinition.newOperation("operation", providerSuperType, providerSuperType);
        
        providerDefinition.finalizeDefinition();
        
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerSuperType, providerSuperType, consumerSubType, providerSubType);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap,
                mapOf(consumerInheritedField, providerInheritedField, consumerSubField, providerSubField),
                emptyMap(),
                mapOf(consumerOperation, providerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertFalse(result.hasError());               
    }
    
    private ValidationResult runOptionalityTest(Usage consumerUsage, Optionality consumerOptionality,
            Optionality providerOptionality) {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        ConsumerField consumerField = consumerRecord.newField("field", consumerRecord, consumerOptionality);
        
        ConsumerRecordType consumerDummy = consumerDefinition.newRecordType("Dummy", 1);
        
        ConsumerRecordType consumerParameterType;
        ConsumerRecordType consumerResultType;
        switch (consumerUsage) {
        case INPUT_ONLY:
            consumerParameterType = consumerRecord;
            consumerResultType = consumerDummy;
            break;
            
        case OUTPUT_ONLY:
            consumerParameterType = consumerDummy;
            consumerResultType = consumerRecord;
            break;
            
        case IN_OUT:
            consumerParameterType = consumerRecord;
            consumerResultType = consumerRecord;
            break;
        
        case NONE:
        default:
            consumerParameterType = consumerDummy;
            consumerResultType = consumerDummy;
            break;
        }
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("op", consumerResultType, consumerParameterType);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", 0);
        ProviderField providerField = providerRecord.newField("field", providerRecord, providerOptionality);
        
        ProviderRecordType providerDummy = providerDefinition.newRecordType("Dummy", 1);
        
        ProviderRecordType providerParameterType;
        ProviderRecordType providerResultType;
        switch (consumerUsage) {
        case INPUT_ONLY:
            providerParameterType = providerRecord;
            providerResultType = providerDummy;
            break;
            
        case OUTPUT_ONLY:
            providerParameterType = providerDummy;
            providerResultType = providerRecord;
            break;
            
        case IN_OUT:
            providerParameterType = providerRecord;
            providerResultType = providerRecord;
            break;
        
        case NONE:
        default:
            providerParameterType = providerDummy;
            providerResultType = providerDummy;
            break;
        }
        
        ProviderOperation providerOperation = providerDefinition.newOperation("op", providerResultType, providerParameterType);
        
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition,
                mapOf(consumerRecord, providerRecord, consumerDummy, providerDummy),
                mapOf(consumerField, providerField),
                emptyMap(),
                mapOf(consumerOperation, providerOperation));
        
        return map.checkConsistency();
    }
    
}
