package gutta.apievolution.fixedformat;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.consumer.ConsumerEnum;
import gutta.apievolution.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.fixedformat.consumer.ConsumerResult;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.provider.ProviderParameter;
import gutta.apievolution.fixedformat.provider.ProviderResult;

class FixedFormatMappingTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    @Test
    void testFixedFormatConversation() {
    	// Generate the required API mapping scripts
        ConsumerApiDefinition consumerApi = ConsumerApiLoader.loadFromClasspath("apis/consumer-api.api", 0);
        RevisionHistory revisionHistory = ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api");
        
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, new HashSet<>(Arrays.asList(0, 1)), consumerApi);
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();
        ApiMappingScript consumerToProviderScript = scriptGenerator.generateMappingScript(resolution, MappingDirection.CONSUMER_TO_PRODUCER);
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(resolution, MappingDirection.PRODUCER_TO_CONSUMER);
        
        ConsumerParameter parameter = new ConsumerParameter()
                .testField("TestString")
                .testEnum(ConsumerEnum.VALUE_A)
                .testList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));
        
        FixedFormatMapper codec = new FixedFormatMapper();
        
        ByteBuffer consumerParameterBuffer = ByteBuffer.allocate(codec.determineMaxSizeOf(ConsumerParameter.class));
        FixedFormatData consumerParameterData = FixedFormatData.of(consumerParameterBuffer, CHARSET);
        codec.writeValue(parameter, consumerParameterData);
        
        // Convert customer parameter to provider parameter
        ByteBuffer providerParameterBuffer = ByteBuffer.allocate(108);

        consumerParameterBuffer.flip();
        consumerToProviderScript.mapType(1, consumerParameterBuffer, providerParameterBuffer);
        
        providerParameterBuffer.flip();
        FixedFormatData providerParameterData = FixedFormatData.of(providerParameterBuffer, CHARSET);
        ProviderParameter providerParameter = codec.readValue(providerParameterData, ProviderParameter.class);
        
        ProviderResult providerResult = this.providerOperation(providerParameter);
        
        ByteBuffer providerResultBuffer = ByteBuffer.allocate(codec.determineMaxSizeOf(ProviderResult.class));
        FixedFormatData providerResultData = FixedFormatData.of(providerResultBuffer, CHARSET);
        codec.writeValue(providerResult, providerResultData);
                        
        ByteBuffer consumerResultBuffer = ByteBuffer.allocate(codec.determineMaxSizeOf(ConsumerResult.class));
        
        providerResultBuffer.flip();
        providerToConsumerScript.mapType(2, providerResultBuffer, consumerResultBuffer);
        
        consumerResultBuffer.flip();
        FixedFormatData consumerResultData = FixedFormatData.of(consumerResultBuffer, CHARSET);
        ConsumerResult consumerResult = codec.readValue(consumerResultData, ConsumerResult.class);
        
        assertEquals("ret: TestString", consumerResult.getResultField());
        assertEquals(ConsumerEnum.VALUE_A, consumerResult.getResultEnum());
        assertEquals(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B), consumerResult.getResultList());
    }        
    
    private ProviderResult providerOperation(ProviderParameter parameter) {
        ProviderResult result = new ProviderResult();
        
        result.setResultEnum(parameter.getTestEnum());
        result.setRetField("ret: " + parameter.getFieldA());
        result.setResultList(parameter.getTestList());
        
        return result;
    }
    
}
