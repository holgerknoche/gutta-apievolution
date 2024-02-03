package gutta.apievolution.fixedformat.unrepresentablevalues;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.objectmapping.UnrepresentableValueException;
import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class UnrepresentableValuesTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    private static final ConsumerApiDefinition CONSUMER_API = ConsumerApiLoader.loadFromClasspath("apis/unrepresentablevalues/consumer-api.api",
            "test.provider", 0);

    private static final RevisionHistory PROVIDER_REVISION_HISTORY = ProviderApiLoader.loadHistoryFromClasspath(
            "apis/unrepresentablevalues/provider-revision-1.api", "apis/unrepresentablevalues/provider-revision-2.api"
            );

    private static final Set<Integer> SUPPORTED_REVISIONS = new HashSet<>(Arrays.asList(0, 1));
    
    private static final DefinitionResolution DEFINITION_RESOLUTION = new DefinitionResolver().resolveConsumerDefinition(PROVIDER_REVISION_HISTORY, SUPPORTED_REVISIONS, CONSUMER_API);
    
    @Test
    void unrepresentableSubtype() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();        
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);
        
        FixedFormatMapper mapper = new FixedFormatMapper();
        
        ProviderSubTypeB subTypeB = new ProviderSubTypeB();
        subTypeB.setSubValueB("Test");
        
        ByteBuffer providerBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(ProviderSuperType.class));
        FixedFormatData providerData = FixedFormatData.of(providerBuffer, CHARSET);
        mapper.writeValue(subTypeB, ProviderSuperType.class, providerData);
        
        ByteBuffer consumerBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(ConsumerSuperType.class));
        providerToConsumerScript.mapResultFor("op", providerBuffer, consumerBuffer);
        
        consumerBuffer.flip();
        
        FixedFormatData consumerData = FixedFormatData.of(consumerBuffer, CHARSET);
        UnrepresentableValueException exception = assertThrows(UnrepresentableValueException.class, () -> mapper.readValue(consumerData, ConsumerSuperType.class));
        
        assertTrue(exception.getMessage().contains("unrepresentable subtype"));
    }
    
    @Test
    void unrepresentableEnumMember() {
        ApiMappingScriptGenerator scriptGenerator = new ApiMappingScriptGenerator();        
        ApiMappingScript providerToConsumerScript = scriptGenerator.generateMappingScript(DEFINITION_RESOLUTION, MappingDirection.PROVIDER_TO_CONSUMER);
        
        FixedFormatMapper mapper = new FixedFormatMapper();
        
        ProviderSubTypeA subTypeA = new ProviderSubTypeA();
        subTypeA.setSubValueA(1);
        subTypeA.setEnumValue(ProviderEnumeration.VALUE_B);
        
        ByteBuffer providerBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(ProviderSuperType.class));
        FixedFormatData providerData = FixedFormatData.of(providerBuffer, CHARSET);
        mapper.writeValue(subTypeA, ProviderSuperType.class, providerData);
        
        ByteBuffer consumerBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(ConsumerSuperType.class));
        providerToConsumerScript.mapResultFor("op", providerBuffer, consumerBuffer);
        
        consumerBuffer.flip();
        
        FixedFormatData consumerData = FixedFormatData.of(consumerBuffer, CHARSET);
        RuntimeException exception = assertThrows(RuntimeException.class, () -> mapper.readValue(consumerData, ConsumerSuperType.class));
        UnrepresentableValueException cause = (UnrepresentableValueException) exception.getCause();
        
        assertTrue(cause.getMessage().contains("unrepresentable member"));
    }

}
