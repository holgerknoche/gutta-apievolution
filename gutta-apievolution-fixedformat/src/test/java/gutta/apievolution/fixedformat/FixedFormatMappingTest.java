package gutta.apievolution.fixedformat;

import static org.junit.jupiter.api.Assertions.assertEquals;

import gutta.apievolution.fixedformat.apimapping.CopyOperation;
import gutta.apievolution.fixedformat.apimapping.EnumMappingOperation;
import gutta.apievolution.fixedformat.apimapping.FieldMapping;
import gutta.apievolution.fixedformat.apimapping.ListMappingOperation;
import gutta.apievolution.fixedformat.apimapping.RecordMappingOperation;
import gutta.apievolution.fixedformat.apimapping.SkipOperation;
import gutta.apievolution.fixedformat.consumer.ConsumerEnum;
import gutta.apievolution.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.fixedformat.consumer.ConsumerResult;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.provider.ProviderParameter;
import gutta.apievolution.fixedformat.provider.ProviderResult;
import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

class FixedFormatMappingTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    @Test
    void testFixedFormatConversation() {
        ConsumerParameter parameter = new ConsumerParameter()
                .testField("TestString")
                .testEnum(ConsumerEnum.VALUE_A)
                .testList(Arrays.asList(ConsumerEnum.VALUE_A, ConsumerEnum.VALUE_B));
        
        FixedFormatMapper codec = new FixedFormatMapper();
        
        ByteBuffer consumerParameterBuffer = ByteBuffer.allocate(codec.determineMaxSizeOf(ConsumerParameter.class));
        FixedFormatData consumerParameterData = FixedFormatData.of(consumerParameterBuffer, CHARSET);
        codec.writeValue(parameter, consumerParameterData);

        // Manually specify the script
        RecordMappingOperation parameterMapping = new RecordMappingOperation(Arrays.asList(
                new FieldMapping(0, new CopyOperation(30)),
                new FieldMapping(0, new SkipOperation(30)),
                new FieldMapping(30, new EnumMappingOperation(new int[] {0, 1})),
                new FieldMapping(34, new ListMappingOperation(10, 4, 4, new EnumMappingOperation(new int[] {0, 1})))
                ));
        
        // Convert customer parameter to provider parameter
        ByteBuffer providerParameterBuffer = ByteBuffer.allocate(108);

        consumerParameterBuffer.flip();
        parameterMapping.apply(0, consumerParameterBuffer, providerParameterBuffer);
        
        providerParameterBuffer.flip();
        FixedFormatData providerParameterData = FixedFormatData.of(providerParameterBuffer, CHARSET);
        ProviderParameter providerParameter = codec.readValue(providerParameterData, ProviderParameter.class);
        
        ProviderResult providerResult = this.providerOperation(providerParameter);
        
        ByteBuffer providerResultBuffer = ByteBuffer.allocate(codec.determineMaxSizeOf(ProviderResult.class));
        FixedFormatData providerResultData = FixedFormatData.of(providerResultBuffer, CHARSET);
        codec.writeValue(providerResult, providerResultData);
        
        // Manually specify the script
        RecordMappingOperation resultMapping = new RecordMappingOperation(Arrays.asList(
                new FieldMapping(30, new EnumMappingOperation(new int[] {0, 1})),
                new FieldMapping(0, new CopyOperation(30)),
                new FieldMapping(34, new ListMappingOperation(10, 4, 4, new EnumMappingOperation(new int[] {0, 1})))
                ));
        
        ByteBuffer consumerResultBuffer = ByteBuffer.allocate(codec.determineMaxSizeOf(ConsumerResult.class));
        
        providerResultBuffer.flip();
        resultMapping.apply(0, providerResultBuffer, consumerResultBuffer);
        
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
