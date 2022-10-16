package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
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
    
}
