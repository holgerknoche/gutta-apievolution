package gutta.apievolution.fixedformat.apimapping;

abstract class UserDefinedTypeMappingOperation implements ApiMappingOperation {
	
	private final int typeId;
	
	protected UserDefinedTypeMappingOperation(int typeId) {
		this.typeId = typeId;
	}
	
	public int getTypeId() {
		return this.typeId;
	}

}
