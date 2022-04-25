package gutta.apievolution.fixedformat;

import gutta.apievolution.fixedformat.apimapping.CopyOperation;
import gutta.apievolution.fixedformat.apimapping.EnumMappingOperation;
import gutta.apievolution.fixedformat.apimapping.FieldMapping;
import gutta.apievolution.fixedformat.apimapping.ListMappingOperation;
import gutta.apievolution.fixedformat.apimapping.RecordMappingOperation;
import gutta.apievolution.fixedformat.apimapping.SkipOperation;
import gutta.apievolution.fixedformat.consumer.ConsumerEnum;
import gutta.apievolution.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.provider.ProviderParameter;
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
        
        ByteBuffer requestBuffer = ByteBuffer.allocate(1024);
        FixedFormatData data = FixedFormatData.of(requestBuffer, CHARSET);
        FixedFormatMapper codec = new FixedFormatMapper();
                
        codec.writeValue(parameter, data);

        // Manually specify the script
        RecordMappingOperation parameterMapping = new RecordMappingOperation(Arrays.asList(
                new FieldMapping(0, new CopyOperation(30)),
                new FieldMapping(0, new SkipOperation(30)),
                new FieldMapping(30, new EnumMappingOperation(new int[] {0, 1})),
                new FieldMapping(34, new ListMappingOperation(10, 4, 4, new EnumMappingOperation(new int[] {0, 1})))
                ));
        
        // Convert customer parameter to provider parameter
        ByteBuffer providerParameterBuffer = ByteBuffer.allocate(108);

        requestBuffer.flip();
        parameterMapping.apply(0, requestBuffer, providerParameterBuffer);
        
        providerParameterBuffer.flip();
        FixedFormatData providerParameterData = FixedFormatData.of(providerParameterBuffer, CHARSET);
        ProviderParameter providerParameter = codec.readValue(providerParameterData, ProviderParameter.class);
        
        System.out.println(providerParameter);
    }        
    
}
