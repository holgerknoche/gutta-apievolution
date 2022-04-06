package gutta.apievolution.core.apimodel;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;

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
        ProviderApiDefinition apiDefinition = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(), 1, Optional.empty());

        ProviderRecordType parameterType = new ProviderRecordType("ParameterType", Optional.empty(), 0, apiDefinition,
                false, Optional.empty(), Optional.empty());

        ProviderRecordType returnType = new ProviderRecordType("ReturnType", Optional.empty(), 0, apiDefinition, false,
                Optional.empty(), Optional.empty());

        ProviderRecordType inoutType = new ProviderRecordType("InOutType", Optional.empty(), 0, apiDefinition, false,
                Optional.empty(), Optional.empty());

        ProviderRecordType unusedType = new ProviderRecordType("UnusedType", Optional.empty(), 0, apiDefinition, false,
                Optional.empty(), Optional.empty());

        new ProviderOperation("op1", Optional.empty(), apiDefinition, returnType, parameterType, Optional.empty());

        new ProviderOperation("op2", Optional.empty(), apiDefinition, inoutType, inoutType, Optional.empty());

        // Assert that the usages on the types are set accordingly
        assertEquals(Usage.NONE, unusedType.getUsage());
        assertEquals(Usage.OUTPUT, returnType.getUsage());
        assertEquals(Usage.INPUT, parameterType.getUsage());
        assertEquals(Usage.IN_OUT, inoutType.getUsage());
    }

}
