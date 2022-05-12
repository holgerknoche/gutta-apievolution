package gutta.apievolution.fixedformat.apimapping;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

public class ApiMappingScriptCodec {
	
	private static final byte OPCODE_COPY = 0x01;
	
	private static final byte OPCODE_SKIP = 0x02;
	
	private static final byte OPCODE_MAP_ENUM = 0x03;
	
	private static final byte OPCODE_MAP_RECORD = 0x04;
	
	private static final byte OPCODE_MAP_LIST = 0x05;
	
	private static final byte ENTRY_TYPE_ENUM = 0x01;
	
	private static final byte ENTRY_TYPE_RECORD = 0x02;
	
	public byte[] writeScript(ApiMappingScript script) {
		try (ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
			 DataOutputStream dataStream = new DataOutputStream(byteStream)) {

			// Write the actual script
			int[] offsets = this.writeScriptToStream(script, dataStream);

			// Fill the offset table at the beginning of the encoded script
			byte[] encodedScript = byteStream.toByteArray();
			ByteBuffer scriptBuffer = ByteBuffer.wrap(encodedScript);
			
			scriptBuffer.position(4);
			for (int offset : offsets) {
				scriptBuffer.putInt(offset);
			}
			
			return encodedScript;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	private int[] writeScriptToStream(ApiMappingScript script, DataOutputStream stream) throws IOException {
		int numberOfEntries = script.size(); 		
		int[] offsets = new int[numberOfEntries];
		
		// Prepare the offset table, but zero the offsets for the time being. They are added after they are known.
		stream.writeInt(numberOfEntries);
		for (int entry = 0; entry < numberOfEntries; entry++) {
			stream.writeInt(0);
		}
				
		TypeEntryWriter writer = new TypeEntryWriter(stream);		
		for (TypeEntry typeEntry : script) {
			offsets[typeEntry.getEntryIndex()] = stream.size();
			writer.writeEntry(typeEntry);
		}
		
		return offsets;
	}
	
	public ApiMappingScript readScript(byte[] encodedScript) {
	    ByteBuffer scriptBuffer = ByteBuffer.wrap(encodedScript);
	    
	    int numberOfEntries = scriptBuffer.getInt();
	    int[] offsets = new int[numberOfEntries + 1];
	    for (int entryIndex = 0; entryIndex < numberOfEntries; entryIndex++) {
	        offsets[entryIndex] = scriptBuffer.getInt();
	    }
	    offsets[numberOfEntries] = encodedScript.length;
	    
	    List<TypeEntry> typeEntries = new ArrayList<>(numberOfEntries);
	    for (int entryIndex = 0; entryIndex < numberOfEntries; entryIndex++) {
	        TypeEntry typeEntry = this.readTypeEntry(scriptBuffer, offsets[entryIndex + 1]);
	        typeEntries.add(typeEntry);
	    }
	   
	    return new ApiMappingScript(typeEntries);
	}
	
	private <T extends TypeEntry> T readTypeEntry(ByteBuffer byteBuffer, int endOffset) {
	    // TODO
	    return null;
	}
	
	private static class TypeEntryWriter implements TypeEntryVisitor<Void> {
		
		private final DataOutputStream dataStream;
		
		private final FieldWriter fieldWriter;
				
		public TypeEntryWriter(DataOutputStream dataStream) {
			this.dataStream = dataStream;
			this.fieldWriter = new FieldWriter(dataStream);
		}
		
		public void writeEntry(TypeEntry typeEntry) {
			typeEntry.accept(this);
		}
		
		@Override
		public Void handleEnumTypeEntry(EnumTypeEntry enumTypeEntry) {
			try {
				DataOutputStream outputStream = this.dataStream;
				int[] indexMap = enumTypeEntry.indexMap;
				
				outputStream.writeByte(ENTRY_TYPE_ENUM);
				
				// Write the index map
				outputStream.writeInt(indexMap.length);
				for (int targetIndex : indexMap) {
					outputStream.writeInt(targetIndex);
				}
				
				return null;
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		@Override
		public Void handleRecordTypeEntry(RecordTypeEntry recordTypeEntry) {
			try {
				DataOutputStream outputStream = this.dataStream;
				
				outputStream.writeByte(ENTRY_TYPE_RECORD);
				
				// Write the individual field mapping operations
				for (FieldMapping fieldMapping : recordTypeEntry) {
					this.fieldWriter.writeFieldMapping(fieldMapping);
				}
				
				return null;
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
	}
	
	private static class FieldWriter implements ApiMappingOperationVisitor<Void> {
		
		private final DataOutputStream dataStream;
						
		public FieldWriter(DataOutputStream dataStream) {			
			this.dataStream = dataStream;			
		}
	
		public void writeFieldMapping(FieldMapping fieldMapping) {
			try {
				this.dataStream.writeInt(fieldMapping.getOffset());
				this.writeOperation(fieldMapping.getMappingOperation());				
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		private void writeOperation(ApiMappingOperation operation) {
			operation.accept(this);
		}
				
		@Override
		public Void handleCopyOperation(CopyOperation copyOperation) {
			try {
				DataOutputStream outputStream = this.dataStream;
				
				outputStream.writeByte(OPCODE_COPY);
				outputStream.writeInt(copyOperation.length);
				
				return null;	
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		@Override
		public Void handleEnumMappingOperation(EnumMappingOperation enumMappingOperation) {
			try {
				DataOutputStream outputStream = this.dataStream;
				
				outputStream.writeByte(OPCODE_MAP_ENUM);
				outputStream.writeInt(enumMappingOperation.getEntryIndex());
				
				return null;	
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public Void handleListMappingOperation(ListMappingOperation listMappingOperation) {
			try {
				DataOutputStream outputStream = this.dataStream;
				
				outputStream.writeByte(OPCODE_MAP_LIST);
				outputStream.writeInt(listMappingOperation.maxElements);
				outputStream.writeInt(listMappingOperation.sourceElementSize);
				outputStream.writeInt(listMappingOperation.targetElementSize);

				this.writeOperation(listMappingOperation.elementMappingOperation);
				
				return null;	
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		@Override
		public Void handleRecordMappingOperation(RecordMappingOperation recordMappingOperation) {
			try {
				DataOutputStream outputStream = this.dataStream;
				
				outputStream.writeByte(OPCODE_MAP_RECORD);
				outputStream.writeInt(recordMappingOperation.getEntryIndex());
				
				return null;	
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		@Override
		public Void handleSkipOperation(SkipOperation skipOperation) {
			try {
				DataOutputStream outputStream = this.dataStream;
				
				outputStream.writeByte(OPCODE_SKIP);
				outputStream.writeInt(skipOperation.amount);
				
				return null;	
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
	}

}
