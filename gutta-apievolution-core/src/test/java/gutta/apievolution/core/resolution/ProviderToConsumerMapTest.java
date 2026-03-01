package gutta.apievolution.core.resolution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.validation.ValidationMessage;
import gutta.apievolution.core.validation.ValidationResult;

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
        var providerDefinition = ProviderApiDefinition.create("test", 0);
        
        var providerRecord = providerDefinition.newRecordType("Test", 0);
        var providerField = providerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        var providerEnum = providerDefinition.newEnumType("Enum", 1);
        var providerMember = providerEnum.newEnumMember("TEST");
        
        var providerOperation = providerDefinition.newOperation("op", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        // Create the consumer definition
        var consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        var consumerRecord = consumerDefinition.newRecordType("Test", 0);
        var consumerField = consumerRecord.newField("field", StringType.unbounded(), Optionality.OPT_IN);
        
        var consumerEnum = consumerDefinition.newEnumType("Enum", 1);
        var consumerMember = consumerEnum.newEnumMember("TEST");
        
        var consumerOperation = consumerDefinition.newOperation("op", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        var map = new ProviderToConsumerMap(providerDefinition, consumerDefinition,
                new TypeMap<>(Map.of(providerRecord, consumerRecord, providerEnum, consumerEnum)),
                Map.of(providerField, consumerField),
                Map.of(providerMember, consumerMember),
                Map.of(providerOperation, consumerOperation));
        
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
        var providerDefinition = ProviderApiDefinition.create("test", 0);
        
        var providerRecord = providerDefinition.newRecordType("Test", 0);
        providerRecord.newField("field", StringType.unbounded(), Optionality.MANDATORY);
        
        var providerOperation = providerDefinition.newOperation("operation", providerRecord, providerRecord);
        
        providerDefinition.finalizeDefinition();
        
        // Create the consumer definition
        var consumerDefinition = TestFixtures.createConsumerApiDefinition("test", 0);
        
        var consumerRecord = consumerDefinition.newRecordType("Test", 0);
        
        var consumerOperation = consumerDefinition.newOperation("operation", consumerRecord, consumerRecord);
        
        consumerDefinition.finalizeDefinition();
        
        var map = new ProviderToConsumerMap(providerDefinition, consumerDefinition,
                new TypeMap<>(Map.of(providerRecord, consumerRecord)),
                Map.of(),
                Map.of(),
                Map.of(providerOperation, consumerOperation));
        
        var result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Mandatory field field@Test@revision 0 is not mapped.")), result.getMessages());
    }
    
}
