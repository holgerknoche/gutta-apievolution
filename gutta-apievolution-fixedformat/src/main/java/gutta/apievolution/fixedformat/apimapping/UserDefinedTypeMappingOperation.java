package gutta.apievolution.fixedformat.apimapping;

abstract class UserDefinedTypeMappingOperation implements ApiMappingOperation {
	
	private final int entryIndex;
	
	protected UserDefinedTypeMappingOperation(int entryIndex) {
		this.entryIndex = entryIndex;
	}
	
	public int getEntryIndex() {
		return this.entryIndex;
	}

}
