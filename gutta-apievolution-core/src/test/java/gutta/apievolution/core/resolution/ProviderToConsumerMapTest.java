package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.TypeMap;
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
import gutta.apievolution.core.validation.ValidationMessage;
import gutta.apievolution.core.validation.ValidationResult;
import org.junit.jupiter.api.Test;

import static gutta.apievolution.core.util.MapUtil.mapOf;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for map from the provider revision to the consumer revision.
 */
class ProviderToConsumerMapTest {

    /**
     * Test case: A consistent mapping is accepted.
     */
    @Test
    void consistentMapping() {
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType providerRecord = providerDefinition.newRecordType("Test", 0);
        ProviderField providerField = providerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        ProviderEnumType providerEnum = providerDefinition.newEnumType("Enum", 1);
        ProviderEnumMember providerMember = providerEnum.newEnumMember("TEST");
        
        ProviderOperation providerOperation = providerDefinition.newOperation("op", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("Test", 0);
        ConsumerField consumerField = consumerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        ConsumerEnumType consumerEnum = consumerDefinition.newEnumType("Enum", 1);
        ConsumerEnumMember consumerMember = consumerEnum.newEnumMember("TEST");
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("op", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        ProviderToConsumerMap map = new ProviderToConsumerMap(providerDefinition, consumerDefinition,
                new TypeMap<>(mapOf(providerRecord, consumerRecord, providerEnum, consumerEnum)),
                mapOf(providerField, consumerField),
                mapOf(providerMember, consumerMember),
                mapOf(providerOperation, consumerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: A missing mapping of a field considered mandatory by the provider is detected
     * and reported.
     */
    @Test
    void missingMappingForMandatoryField() {
        // Create the provider definition
        ProviderApiDefinition providerDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType providerRecord = providerDefinition.newRecordType("Test", 0);
        providerRecord.newField("field", StringType.unbounded(), Optionality.MANDATORY);
        
        ProviderOperation providerOperation = providerDefinition.newOperation("operation", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        // Create the consumer definition
        ConsumerApiDefinition consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        ConsumerRecordType consumerRecord = consumerDefinition.newRecordType("Test", 0);
        
        ConsumerOperation consumerOperation = consumerDefinition.newOperation("operation", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        ProviderToConsumerMap map = new ProviderToConsumerMap(providerDefinition, consumerDefinition,
                new TypeMap<>(mapOf(providerRecord, consumerRecord)),
                emptyMap(),
                emptyMap(),
                mapOf(providerOperation, consumerOperation));
        
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(asList(ValidationMessage.error("Mandatory field field@Test@revision 0 is not mapped.")), result.getMessages());
    }
    
}
