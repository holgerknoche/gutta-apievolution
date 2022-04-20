package gutta.apievolution.fixedformat;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript.MappingProcedure;
import gutta.apievolution.fixedformat.apimapping.CopyOperation;
import gutta.apievolution.fixedformat.apimapping.SkipOperation;
import gutta.apievolution.fixedformat.consumer.ConsumerEnum;
import gutta.apievolution.fixedformat.consumer.ConsumerParameter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
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
        MappingProcedure parameterProcedure = new MappingProcedure(Arrays.asList(
                new CopyOperation(0, 30),
                new SkipOperation(30),
                new CopyOperation(30, 4)
                ));
        
        // Convert customer parameter to provider parameter
        ByteBuffer providerParameterBuffer = ByteBuffer.allocate(104);

        requestBuffer.flip();
        parameterProcedure.apply(requestBuffer, providerParameterBuffer);
        
        System.out.println("Consumer Parameter: " + requestBuffer);
        System.out.println("Provider Parameter: " + providerParameterBuffer);
    }        
    
}
