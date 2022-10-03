package gutta.apievolution.core.apimodel;

import static org.junit.jupiter.api.Assertions.assertEquals;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import org.junit.jupiter.api.Test;

/**
 * Test cases for API model creation.
 */
class ModelCreationTest {

    /**
     * This test asserts that the usage of types is calculated correctly.
     */
    @Test
    void testUsagesInModel() {
        // Create an API definition with four types, one used only for input, one only
        // for output, one for both and one
        // not at all
        ProviderApiDefinition apiDefinition = ProviderApiDefinition.create("test", 1);

        ProviderRecordType parameterType = ProviderRecordType.createRecordType("ParameterType", 0, apiDefinition);

        ProviderRecordType returnType = ProviderRecordType.createRecordType("ReturnType", 0, apiDefinition);

        ProviderRecordType inoutType = ProviderRecordType.createRecordType("InOutType", 0, apiDefinition);

        ProviderRecordType unusedType = ProviderRecordType.createRecordType("UnusedType", 0, apiDefinition);

        ProviderOperation.create("op1", apiDefinition, returnType, parameterType);

        ProviderOperation.create("op2", apiDefinition, inoutType, inoutType);

        // Assert that the usages on the types are set accordingly
        assertEquals(Usage.NONE, unusedType.getUsage());
        assertEquals(Usage.OUTPUT, returnType.getUsage());
        assertEquals(Usage.INPUT, parameterType.getUsage());
        assertEquals(Usage.IN_OUT, inoutType.getUsage());
    }

}
