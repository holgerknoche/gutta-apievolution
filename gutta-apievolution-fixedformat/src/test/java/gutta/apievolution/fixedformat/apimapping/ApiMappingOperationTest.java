package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;
import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static gutta.apievolution.fixedformat.objectmapping.Flags.IS_PRESENT;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiMappingOperationTest {
    
    /**
     * Test case: The copy operation works as expected.
     */
    @Test
    void copyOperation() {
        CopyOperation operation = new CopyOperation(10);
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(20);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(20);
        
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19});
        
        operation.apply(5, sourceDataBuffer, targetDataBuffer);
        
        // Assert that the target buffer is at the expected position
        assertEquals(10, targetDataBuffer.position());
        
        targetDataBuffer.flip();
        byte[] targetData = new byte[10];
        targetDataBuffer.get(targetData);
        
        // Assert that the target buffer contains the expected data
        assertArrayEquals(new byte[] {5, 6, 7, 8, 9, 10, 11, 12, 13, 14}, targetData);
    }
    
    /**
     * Test case: The enum mapping operation works as expected for an existing enum member.
     */
    @Test
    void enumMappingForExistingMember() {
        EnumTypeEntry entry = new EnumTypeEntry(0, 0, new int[] {2, 1, 0});
        EnumMappingOperation operation = new EnumMappingOperation(entry);
       
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(5);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(5);
        
        sourceDataBuffer
            .put(IS_PRESENT)
            .putInt(0)
            .flip();
        operation.apply(0, sourceDataBuffer, targetDataBuffer);
        
        targetDataBuffer.flip();
        assertEquals(IS_PRESENT, targetDataBuffer.get());
        assertEquals(2, targetDataBuffer.getInt());
    }
    
    /** 
     * Test case: The record mapping operation works as expected.
     */
    @Test
    void recordMappingOperation() {
        FieldMapping fieldMapping1 = new FieldMapping(0, new CopyOperation(10));
        FieldMapping fieldMapping2 = new FieldMapping(0, new SkipOperation(5));
        FieldMapping fieldMapping3 = new FieldMapping(10, new CopyOperation(1));
        
        RecordTypeEntry entry = new RecordTypeEntry(0, 0, 16, Arrays.asList(fieldMapping1, fieldMapping2, fieldMapping3));
        RecordMappingOperation operation = new RecordMappingOperation(entry);
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(12);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(17);
        
        sourceDataBuffer.put(new byte[] {IS_PRESENT, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
        sourceDataBuffer.flip();
        
        operation.apply(0, sourceDataBuffer, targetDataBuffer);
        
        targetDataBuffer.flip();
        byte[] targetData = new byte[17];
        targetDataBuffer.get(targetData);
        
        assertArrayEquals(new byte[] {IS_PRESENT, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 10}, targetData);
    }
    
    /**
     * Test case: The list mapping operation works as expected for fully utilized lists.
     */
    @Test
    void listMappingOperationWithFullUtilization() {
        ListMappingOperation operation = new ListMappingOperation(10, 1, 1, new CopyOperation(1));
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(15);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(15);
        
        sourceDataBuffer.put(IS_PRESENT);
        sourceDataBuffer.putInt(10);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        operation.apply(0, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(15, targetDataBuffer.position());
        
        targetDataBuffer.flip();
        byte flags = targetDataBuffer.get();
        int elementCount = targetDataBuffer.getInt();
        byte[] targetData = new byte[10];
        targetDataBuffer.get(targetData);
        
        assertEquals(IS_PRESENT, flags);
        assertEquals(10, elementCount);
        assertArrayEquals(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, targetData);
    }
    
    /**
     * Test case: The list mapping operation works as expected for partially utilized lists.
     */
    @Test
    void listMappingOperationWithPartialUtilization() {
        ListMappingOperation operation = new ListMappingOperation(10, 1, 1, new CopyOperation(1));
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(15);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(15);
        
        sourceDataBuffer.put(IS_PRESENT);
        sourceDataBuffer.putInt(4);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        operation.apply(0, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(15, targetDataBuffer.position());
        
        targetDataBuffer.flip();
        byte flags = targetDataBuffer.get();
        int elementCount = targetDataBuffer.getInt();
        byte[] targetData = new byte[10];
        targetDataBuffer.get(targetData);
        
        assertEquals(IS_PRESENT, flags);
        assertEquals(4, elementCount);
        assertArrayEquals(new byte[] {0, 1, 2, 3, 0, 0, 0, 0, 0, 0}, targetData);
    }
    
    /**
     * Test case: List mapping of a list with too many elements.
     */
    @Test
    void listMappingOperationWithOverfullList() {
        ListMappingOperation operation = new ListMappingOperation(5, 1, 1, new CopyOperation(1));
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(15);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(15);
        
        sourceDataBuffer.put(IS_PRESENT);
        sourceDataBuffer.putInt(10);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        IllegalStateException exception = assertThrows(IllegalStateException.class, 
                () -> operation.apply(0, sourceDataBuffer, targetDataBuffer));
        assertTrue(exception.getMessage().startsWith("Too many elements"));
    }
    
    /**
     * Test case: The skip operation works as expected.
     */
    @Test
    void skipOperation() {
        SkipOperation operation = new SkipOperation(10);
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(20);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(20);
        
        operation.apply(0, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(0, sourceDataBuffer.position());
        assertEquals(10, targetDataBuffer.position());
    }
    
    /**
     * Test case: The polymorphic record mapping operation works as expected.
     */
    @Test
    void polymorphicRecordMappingOperation() {
        FieldMapping fieldMappingType11 = new FieldMapping(0, new CopyOperation(5));
        FieldMapping fieldMappingType12 = new FieldMapping(5, new CopyOperation(5));
        
        RecordTypeEntry typeEntry1 = new RecordTypeEntry(0, 1, 10, Arrays.asList(fieldMappingType11, fieldMappingType12));
        
        FieldMapping fieldMappingType21 = new FieldMapping(5, new CopyOperation(5));
        FieldMapping fieldMappingType22 = new FieldMapping(0, new CopyOperation(5));
        
        RecordTypeEntry typeEntry2 = new RecordTypeEntry(1, 2, 10, Arrays.asList(fieldMappingType21, fieldMappingType22));
        
        List<PolymorphicRecordMapping> recordMappings = new ArrayList<>();
        recordMappings.add(new PolymorphicRecordMapping(1, 2, typeEntry1));
        recordMappings.add(new PolymorphicRecordMapping(2, 1, typeEntry2));
        
        PolymorphicRecordMappingOperation operation = new PolymorphicRecordMappingOperation(recordMappings);
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(15);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(15);
        
        sourceDataBuffer.put(IS_PRESENT);
        sourceDataBuffer.putInt(2);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        operation.apply(0, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(15, targetDataBuffer.position());
        
        targetDataBuffer.flip();
        byte flags = targetDataBuffer.get();
        int targetTypeId = targetDataBuffer.getInt();
        byte[] targetData = new byte[10];
        targetDataBuffer.get(targetData);
        
        assertEquals(IS_PRESENT, flags);
        assertEquals(1, targetTypeId);
        assertArrayEquals(new byte[] {5, 6, 7, 8, 9, 0, 1, 2, 3, 4}, targetData);
    }
    
}
