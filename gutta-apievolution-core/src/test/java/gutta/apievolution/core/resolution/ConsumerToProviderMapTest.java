package gutta.apievolution.core.resolution;

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
import gutta.apievolution.core.util.CheckResult;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Map;

import static gutta.apievolution.core.util.MapUtil.mapOf;
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
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
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
        
        CheckResult result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: At least one consumer record type is not mapped.
     */
    @Test
    void missingMappingForConsumerRecord() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
        consumerDefinition.newRecordType("TestRecord", 0);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                emptyMap(), emptyMap(), emptyMap(), emptyMap());
        
        CheckResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList("User-defined type 'TestRecord' is not mapped."), result.getMessages());
    }

    @Test
    void missingMappingForConsumerEnum() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
        consumerDefinition.newEnumType("TestEnum", 0);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        providerDefinition.finalizeDefinition();
        
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                emptyMap(), emptyMap(), emptyMap(), emptyMap());
        
        CheckResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList("User-defined type 'TestEnum' is not mapped."), result.getMessages());
    }
    
    @Test
    void missingMappingForConsumerField() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        consumerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", 0);
        
        providerDefinition.finalizeDefinition();
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerRecord, providerRecord);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap, emptyMap(), emptyMap(), emptyMap());
        
        CheckResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList("Field 'field' is not mapped."), result.getMessages());
    }
    
    @Test
    void missingMappingForConsumerEnumMember() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
        ConsumerEnumType consumerEnum = consumerDefinition.newEnumType("TestEnum", 0);
        consumerEnum.newEnumMember("TEST");
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderEnumType providerEnum = providerDefinition.newEnumType("TestEnum", 0);
        
        providerDefinition.finalizeDefinition();
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerEnum, providerEnum);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap, emptyMap(), emptyMap(), emptyMap());
        
        CheckResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList("Enum member 'TEST' is not mapped."), result.getMessages());
    }
    
    @Test
    void missingMappingForConsumerOperation() {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        consumerDefinition.newOperation("op", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
       
        ProviderRecordType providerRecord = providerDefinition.newRecordType("TestRecord", 0);
        
        providerDefinition.finalizeDefinition();
        
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> typeMap = mapOf(consumerRecord, providerRecord);
        ConsumerToProviderMap map = new ConsumerToProviderMap(consumerDefinition, providerDefinition, 
                typeMap, emptyMap(), emptyMap(), emptyMap());
        
        CheckResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(Arrays.asList("Operation 'op' is not mapped."), result.getMessages());
    }
    
    @Test
    void mandatoryFieldForInput() {
        CheckResult result;
        
        // When the consumer considers an input field mandatory, it can be of any optionality
        // on the provider side
        result = runOptionalityTest(Usage.INPUT, Optionality.MANDATORY, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT, Optionality.MANDATORY, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT, Optionality.MANDATORY, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    @Test
    void optInFieldForInput() {
        CheckResult result;
        
        // When the consumer considers an input field opt-in, it may not be mandatory on the
        // provider side
        result = runOptionalityTest(Usage.INPUT, Optionality.OPT_IN, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.INPUT, Optionality.OPT_IN, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT, Optionality.OPT_IN, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    @Test
    void optionalFieldForInput() {
        CheckResult result;
        
        // When the consumer considers an input field optional, it may not be mandatory on the
        // provider side
        result = runOptionalityTest(Usage.INPUT, Optionality.OPTIONAL, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.INPUT, Optionality.OPTIONAL, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.INPUT, Optionality.OPTIONAL, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    @Test
    void mandatoryFieldForOutput() {
        CheckResult result;
        
        // When the consumer considers an output field mandatory, it may not be optional on
        // the provider side
        result = runOptionalityTest(Usage.OUTPUT, Optionality.MANDATORY, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT, Optionality.MANDATORY, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT, Optionality.MANDATORY, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    @Test
    void optInFieldForOutput() {
        CheckResult result;
        
        // When the consumer considers an output field opt-in, it may not be optional on the
        // provider side
        result = runOptionalityTest(Usage.OUTPUT, Optionality.OPT_IN, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT, Optionality.OPT_IN, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT, Optionality.OPT_IN, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    @Test
    void optionalFieldForOutput() {
        CheckResult result;
        
        // When the consumer considers an output field optional, it may be of any optionality
        // on the provider side
        result = runOptionalityTest(Usage.OUTPUT, Optionality.OPTIONAL, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT, Optionality.OPTIONAL, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.OUTPUT, Optionality.OPTIONAL, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    @Test
    void mandatoryFieldForInOut() {
        CheckResult result;
        
        // When the consumer considers an in-out field mandatory, it must be at mandatory or
        // opt-in on the provider side
        result = runOptionalityTest(Usage.IN_OUT, Optionality.MANDATORY, Optionality.MANDATORY);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.MANDATORY, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.MANDATORY, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    @Test
    void optInFieldForInOut() {
        CheckResult result;
        
        // When the consumer considers an in-out field opt-in, it must be opt-in on the
        // provider side
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPT_IN, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPT_IN, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPT_IN, Optionality.OPTIONAL);
        assertTrue(result.hasError());
    }
    
    @Test
    void optionalFieldForInOut() {
        CheckResult result;
        
        // When the consumer considers an in-out field optional, it must be optional or opt-in
        // on the provider side
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPTIONAL, Optionality.MANDATORY);
        assertTrue(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPTIONAL, Optionality.OPT_IN);
        assertFalse(result.hasError());
        result = runOptionalityTest(Usage.IN_OUT, Optionality.OPTIONAL, Optionality.OPTIONAL);
        assertFalse(result.hasError());
    }
    
    private CheckResult runOptionalityTest(Usage consumerUsage, Optionality consumerOptionality,
            Optionality providerOptionality) {
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = ConsumerApiDefinition.create("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("TestRecord", 0);
        ConsumerField consumerField = consumerRecord.newField("field", consumerRecord, consumerOptionality);
        
        ConsumerRecordType consumerDummy = consumerDefinition.newRecordType("Dummy", 1);
        
        ConsumerRecordType consumerParameterType;
        ConsumerRecordType consumerResultType;
        switch (consumerUsage) {
        case INPUT:
            consumerParameterType = consumerRecord;
            consumerResultType = consumerDummy;
            break;
            
        case OUTPUT:
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
        case INPUT:
            providerParameterType = providerRecord;
            providerResultType = providerDummy;
            break;
            
        case OUTPUT:
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
