package gutta.apievolution.fixedformat.apimapping;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;

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
        
        operation.apply(5, null, sourceDataBuffer, targetDataBuffer);
        
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
        EnumMappingOperation operation = new EnumMappingOperation(0);
       
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(4);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(4);
        
        sourceDataBuffer.putInt(0).flip();
        operation.apply(0, new TestEntryResolver(entry), sourceDataBuffer, targetDataBuffer);
        
        targetDataBuffer.flip();
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
        
        RecordTypeEntry entry = new RecordTypeEntry(0, 0, Arrays.asList(fieldMapping1, fieldMapping2, fieldMapping3));
        RecordMappingOperation operation = new RecordMappingOperation(0);
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(11);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(16);
        
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
        sourceDataBuffer.flip();
        
        operation.apply(0, new TestEntryResolver(entry), sourceDataBuffer, targetDataBuffer);
        
        targetDataBuffer.flip();
        byte[] targetData = new byte[16];
        targetDataBuffer.get(targetData);
        
        assertArrayEquals(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 10}, targetData);
    }
    
    /**
     * Test case: The list mapping operation works as expected for fully utilized lists.
     */
    @Test
    void listMappingOperationWithFullUtilization() {
        ListMappingOperation operation = new ListMappingOperation(10, 1, 1, new CopyOperation(1));
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(14);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(14);
        
        sourceDataBuffer.putInt(10);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        operation.apply(0, null, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(14, targetDataBuffer.position());
        
        targetDataBuffer.flip();
        int elementCount = targetDataBuffer.getInt();
        byte[] targetData = new byte[10];
        targetDataBuffer.get(targetData);
        
        assertEquals(10, elementCount);
        assertArrayEquals(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, targetData);
    }
    
    /**
     * Test case: The list mapping operation works as expected for partially utilized lists.
     */
    @Test
    void listMappingOperationWithPartialUtilization() {
        ListMappingOperation operation = new ListMappingOperation(10, 1, 1, new CopyOperation(1));
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(14);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(14);
        
        sourceDataBuffer.putInt(4);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        operation.apply(0, null, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(14, targetDataBuffer.position());
        
        targetDataBuffer.flip();
        int elementCount = targetDataBuffer.getInt();
        byte[] targetData = new byte[10];
        targetDataBuffer.get(targetData);
        
        assertEquals(4, elementCount);
        assertArrayEquals(new byte[] {0, 1, 2, 3, 0, 0, 0, 0, 0, 0}, targetData);
    }
    
    /**
     * Test case: List mapping of a list with too many elements.
     */
    @Test
    void listMappingOperationWithOverfullList() {
        ListMappingOperation operation = new ListMappingOperation(5, 1, 1, new CopyOperation(1));
        
        ByteBuffer sourceDataBuffer = ByteBuffer.allocate(14);
        ByteBuffer targetDataBuffer = ByteBuffer.allocate(14);
        
        sourceDataBuffer.putInt(10);
        sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        sourceDataBuffer.flip();
        
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> operation.apply(0, null, sourceDataBuffer, targetDataBuffer));
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
        
        operation.apply(0, null, sourceDataBuffer, targetDataBuffer);
        
        assertEquals(0, sourceDataBuffer.position());
        assertEquals(10, targetDataBuffer.position());
    }
    
    /**
     * Test case: The polymorphic record mapping operation works as expected.
     */
    @Test
    void polymorphicRecordMappingOperation() {
    	FieldMapping fieldMappingType1_1 = new FieldMapping(0, new CopyOperation(5));
    	FieldMapping fieldMappingType1_2 = new FieldMapping(5, new CopyOperation(5));
    	
    	RecordTypeEntry typeEntry1 = new RecordTypeEntry(0, 1, Arrays.asList(fieldMappingType1_1, fieldMappingType1_2));
    	
    	FieldMapping fieldMappingType2_1 = new FieldMapping(5, new CopyOperation(5));
    	FieldMapping fieldMappingType2_2 = new FieldMapping(0, new CopyOperation(5));
    	
    	RecordTypeEntry typeEntry2 = new RecordTypeEntry(1, 2, Arrays.asList(fieldMappingType2_1, fieldMappingType2_2));
    	
    	TypeEntryResolver typeEntryResolver = new TestPolyEntryResolver(new TypeEntry[] {typeEntry1, typeEntry2});
    	
    	Map<Integer, PolymorphicRecordMapping> idToRecordMapping = new HashMap<>();
    	idToRecordMapping.put(1, new PolymorphicRecordMapping(1, 2, 0));
    	idToRecordMapping.put(2, new PolymorphicRecordMapping(2, 1, 1));
    	
    	PolymorphicRecordMappingOperation operation = new PolymorphicRecordMappingOperation(idToRecordMapping);
    	
    	ByteBuffer sourceDataBuffer = ByteBuffer.allocate(14);
    	ByteBuffer targetDataBuffer = ByteBuffer.allocate(14);
    	
    	sourceDataBuffer.putInt(2);
    	sourceDataBuffer.put(new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
    	sourceDataBuffer.flip();
    	
    	operation.apply(0, typeEntryResolver, sourceDataBuffer, targetDataBuffer);
    	
    	assertEquals(14, targetDataBuffer.position());
    	
    	targetDataBuffer.flip();
    	int targetTypeId = targetDataBuffer.getInt();
    	byte[] targetData = new byte[10];
    	targetDataBuffer.get(targetData);
    	
    	assertEquals(1, targetTypeId);
    	assertArrayEquals(new byte[] {5, 6, 7, 8, 9, 0, 1, 2, 3, 4}, targetData);
    }
    
    private static class TestEntryResolver implements TypeEntryResolver {
        
        private final TypeEntry entry;
        
        public TestEntryResolver(TypeEntry entry) {
            this.entry = entry;
        }
        
        @Override
        @SuppressWarnings("unchecked")
        public <T extends TypeEntry> T resolveEntry(int index) {
            return (T) this.entry;
        }
        
    }
    
    private static class TestPolyEntryResolver implements TypeEntryResolver {
    	
    	private final TypeEntry[] entries; 
    	
    	public TestPolyEntryResolver(TypeEntry[] entries) {
    		this.entries = entries;
    	}
    	
    	@Override
    	@SuppressWarnings("unchecked")
    	public <T extends TypeEntry> T resolveEntry(int index) {
    		return (T) this.entries[index];
    	}
    	
    }

}
