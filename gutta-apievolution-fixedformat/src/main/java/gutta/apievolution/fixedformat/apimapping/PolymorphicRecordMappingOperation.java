package gutta.apievolution.fixedformat.apimapping;

import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

class PolymorphicRecordMappingOperation implements ApiMappingOperation {

	private static final int TYPE_ID_SIZE = 4;
	
	private final Map<Integer, PolymorphicRecordMapping> idToRecordMapping;
	
	public PolymorphicRecordMappingOperation(Map<Integer, PolymorphicRecordMapping> idToRecordMapping) {
		this.idToRecordMapping = idToRecordMapping;
	}
	
	@Override
	public void apply(int sourceOffset, TypeEntryResolver typeEntryResolver, ByteBuffer source, ByteBuffer target) {
		int sourceTypeId = source.getInt();
		PolymorphicRecordMapping recordMapping = this.idToRecordMapping.get(sourceTypeId);
		
		if (recordMapping == null) {
			throw new IllegalArgumentException("No record mapping for id " + sourceTypeId + ".");
		}
		
		target.putInt(recordMapping.getTargetTypeId());
		
		RecordMappingOperation actualMappingOperation = new RecordMappingOperation(recordMapping.getEntryIndex());
		actualMappingOperation.apply(sourceOffset + TYPE_ID_SIZE, typeEntryResolver, source, target);
	}

	@Override
	public <R> R accept(ApiMappingOperationVisitor<R> visitor) {
		return visitor.handlePolymorphicRecordMappingOperation(this);
	}
	
	Collection<PolymorphicRecordMapping> getRecordMappings() {
		return Collections.unmodifiableCollection(this.idToRecordMapping.values());
	}
	
	static class PolymorphicRecordMapping {
		
		private final int sourceTypeId;
		
		private final int targetTypeId;
		
		private final int entryIndex;
		
		public PolymorphicRecordMapping(int sourceTypeId, int targetTypeId, int entryIndex) {
			this.sourceTypeId = sourceTypeId;
			this.targetTypeId = targetTypeId;
			this.entryIndex = entryIndex;
		}
		
		public int getSourceTypeId() {
			return this.sourceTypeId;
		}
		
		public int getTargetTypeId() {
			return this.targetTypeId;
		}
		
		public int getEntryIndex() {
			return this.entryIndex;
		}
		
	}	

}
