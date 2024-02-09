package gutta.apievolution.fixedformat;

import gutta.apievolution.core.util.EqualityUtil;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;
import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FixedFormatMapperTest {

    private static final Charset CHARSET = StandardCharsets.ISO_8859_1;
    
    /**
     * Test case: String serialization with padding.
     */
    @Test
    void serializeStringWithPadding() {
        String testValue = "TestTest";        
        byte[] expectedBytes = new byte[] {0x01, 0x01, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65, 0x73, 0x74, 0x00, 0x00};
    
        this.stringSerializationDeserializationTest(testValue, expectedBytes);
    }
    
    /**
     * Test case: String serialization with an exactly matching string (in terms of length).
     */
    @Test
    void serializeMatchingString() {
        String testValue = "TestTestTe";        
        byte[] expectedBytes = new byte[] {0x01, 0x01, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65};
    
        this.stringSerializationDeserializationTest(testValue, expectedBytes);        
    }
    
    /**
     * Test case: String serialization with a string that needs truncation.
     */
    @Test
    void serializeTruncatedString() {
        String testValue = "TestTestTest";        
        byte[] expectedBytes = new byte[] {0x01, 0x01, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65, 0x73, 0x74, 0x54, 0x65};
    
        this.stringSerializationDeserializationTest(testValue, expectedBytes, "TestTestTe");
    }
    
    private void stringSerializationDeserializationTest(String value, byte[] expectedBytes) {
        this.stringSerializationDeserializationTest(value, expectedBytes, value);
    }
    
    private void stringSerializationDeserializationTest(String value, byte[] expectedBytes, String expectedValue) {
        ByteBuffer buffer = ByteBuffer.allocate(32);
        FixedFormatData data = FixedFormatData.of(buffer, CHARSET);
        
        FixedFormatMapper mapper = new FixedFormatMapper();

        StringOnlyStructure structure = new StringOnlyStructure().value(value);
        mapper.writeValue(structure, data);
        buffer.flip();
        
        assertEquals(expectedBytes.length, buffer.limit());
        
        byte[] writtenBytes = new byte[expectedBytes.length];
        buffer.get(writtenBytes);
        
        assertArrayEquals(expectedBytes, writtenBytes);
        
        // Read the value from the buffer
        buffer.position(0);
        
        StringOnlyStructure readStructure = mapper.readValue(data, StringOnlyStructure.class);
        assertEquals(expectedValue, readStructure.getValue());
    }
    
    /**
     * Test case: Serialization and deserialization of a non-empty list.
     */
    @Test
    void serializeAndDeserializeNonEmptyList() {        
        // Prepare the data to write
        StringOnlyStructure containedStructure = new StringOnlyStructure().value("Test");
        ListOnlyStructure writtenStructure = new ListOnlyStructure().values(Arrays.asList(containedStructure, containedStructure, containedStructure));
                
        byte[] expectedBytes = new byte[] {
                // Null indicator of the surrounding structure
                0x01,
                // Null indicator and element count of the list
                0x01, 0x00, 0x00, 0x00, 0x03,
                // Element 1
                0x01, 0x01, 0x54, 0x65, 0x73, 0x74, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 2
                0x01, 0x01, 0x54, 0x65, 0x73, 0x74, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 3
                0x01, 0x01, 0x54, 0x65, 0x73, 0x74, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 4 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 5 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        };
        
        this.listSerializationDeserializionTest(writtenStructure, expectedBytes);
    }
    
    /**
     * Test case: Serialization and deserialization of an empty list.
     */
    @Test
    void serializeAndDeserializeEmptyList() {        
        // Prepare the data to write
        ListOnlyStructure writtenStructure = new ListOnlyStructure().values(Collections.emptyList());
                
        byte[] expectedBytes = new byte[] {
                // Null indicator of the surrounding structure
                0x01,
                // Null indicator and element count of the list
                0x01, 0x00, 0x00, 0x00, 0x00,
                // Element 1 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 2 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 3 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 4 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 5 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        };
        
        this.listSerializationDeserializionTest(writtenStructure, expectedBytes);
    }
    
    /**
     * Test case: Serialization and deserialization of a null list.
     */
    @Test
    void serializeAndDeserializeNullList() {        
        // Prepare the data to write
        ListOnlyStructure writtenStructure = new ListOnlyStructure().values(null);
                
        byte[] expectedBytes = new byte[] {
                // Null indicator of the surrounding structure
                0x01,
                // Null indicator and element count of the list
                0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 1 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 2 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 3 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 4 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                // Element 5 (null)
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        };
        
        this.listSerializationDeserializionTest(writtenStructure, expectedBytes);
    }
    
    private void listSerializationDeserializionTest(ListOnlyStructure structure, byte[] expectedBytes) {
        // Allocate the buffer
        ByteBuffer buffer = ByteBuffer.allocate(1024);
        FixedFormatData data = FixedFormatData.of(buffer, CHARSET);
        
        // Write the value
        FixedFormatMapper mapper = new FixedFormatMapper();
        mapper.writeValue(structure, data);
        buffer.flip();

        byte[] actualBytes = new byte[buffer.limit()];
        buffer.get(actualBytes);
        
        assertArrayEquals(expectedBytes, actualBytes);
        
        // Read the value from the buffer
        buffer.position(0);
        
        ListOnlyStructure readStructure = mapper.readValue(data, ListOnlyStructure.class);
        assertEquals(structure, readStructure);
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
        
        @Override
        public int hashCode() {
            return Objects.hashCode(this.value);
        }
        
        @Override
        public boolean equals(Object that) {
            return EqualityUtil.equals(this, that, this::equalsInternal);
        }
        
        private boolean equalsInternal(StringOnlyStructure that) {
            return Objects.equals(this.value, that.value);
        }
        
    }
    
    public static class ListOnlyStructure {
        
        @MaxLength(5)
        private List<StringOnlyStructure> values;
                
        public List<StringOnlyStructure> getValues() {
            return this.values;
        }
        
        public void setValues(List<StringOnlyStructure> values) {
            this.values = values;
        }
        
        public ListOnlyStructure values(List<StringOnlyStructure> values) {
            this.setValues(values);
            return this;
        }
        
        @Override
        public int hashCode() {
            return Objects.hashCode(this.values);
        }
        
        @Override
        public boolean equals(Object that) {
            return EqualityUtil.equals(this, that, this::equalsInternal);
        }
        
        private boolean equalsInternal(ListOnlyStructure that) {
            return Objects.equals(this.values, that.values);
        }
        
    }
    
}
