package gutta.apievolution.fixedformat;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

class FixedFormatMapperTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    /**
     * Test case: String serialization with padding.
     */
    @Test
    void serializeStringWithPadding() {
        String testValue = "TestTest";        
        byte[] expectedBytes = new byte[] {0x54, 0x65, 0x73, 0x74, 0x54, 0x65, 0x73, 0x74, 0x00, 0x00};
    
        this.stringSerializationTest(testValue, expectedBytes);
    }
    
    /**
     * Test case: String serialization with an exactly matching string (in terms of length).
     */
    @Test
    void serializeMatchingString() {
        String testValue = "TestTestTe";        
        byte[] expectedBytes = new byte[] {0x54, 0x65, 0x73, 0x74, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65};
    
        this.stringSerializationTest(testValue, expectedBytes);        
    }
    
    /**
     * Test case: String serialization with a string that needs truncation.
     */
    @Test
    void serializeTruncatedString() {
        String testValue = "TestTestTest";        
        byte[] expectedBytes = new byte[] {0x54, 0x65, 0x73, 0x74, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65};
    
        this.stringSerializationTest(testValue, expectedBytes);
    }
    
    private void stringSerializationTest(String string, byte[] expectedBytes) {
        ByteBuffer buffer = ByteBuffer.allocate(32);
        FixedFormatData data = FixedFormatData.of(buffer, CHARSET);
        
        FixedFormatMapper mapper = new FixedFormatMapper();

        StringOnlyStructure structure = new StringOnlyStructure().value(string);
        mapper.writeValue(structure, data);
        buffer.flip();
        
        assertEquals(expectedBytes.length, buffer.limit());
        
        byte[] writtenBytes = new byte[expectedBytes.length];
        buffer.get(writtenBytes);
        
        assertArrayEquals(expectedBytes, writtenBytes);
    }
    
    public static class StringOnlyStructure {
        
        @MaxLength(10)
        private String value;
        
        public String getValue() {
            return this.value;
        }
        
        public void setValue(String value) {
            this.value = value;
        }
        
        public StringOnlyStructure value(String value) {
            this.setValue(value);
            return this;
        }
        
    }
    
}
