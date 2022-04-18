package gutta.apievolution.fixedformat;

import gutta.apievolution.fixedformat.consumer.ConsumerEnum;
import gutta.apievolution.fixedformat.consumer.ConsumerParameter;
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
        
        System.out.println(requestBuffer);
    }        
    
}
