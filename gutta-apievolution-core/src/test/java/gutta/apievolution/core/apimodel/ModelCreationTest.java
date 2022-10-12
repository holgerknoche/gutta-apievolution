package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

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

        ProviderRecordType parameterType = apiDefinition.newRecordType("ParameterType", 0);

        ProviderRecordType returnType = apiDefinition.newRecordType("ReturnType", 0);

        ProviderRecordType inoutType = apiDefinition.newRecordType("InOutType", 0);

        ProviderRecordType unusedType = apiDefinition.newRecordType("UnusedType", 0);

        apiDefinition.newOperation("op1", returnType, parameterType);

        apiDefinition.newOperation("op2", inoutType, inoutType);

        // Assert that the usages on the types are set accordingly
        assertEquals(Usage.NONE, unusedType.getUsage());
        assertEquals(Usage.OUTPUT, returnType.getUsage());
        assertEquals(Usage.INPUT, parameterType.getUsage());
        assertEquals(Usage.IN_OUT, inoutType.getUsage());
    }

}
