package gutta.apievolution.fixedformat.apimapping;

class OperationEntry {

	private final int entryIndex;
		
	private final String name;
	
	private final ApiMappingOperation parameterMappingOperation;
	
	private final ApiMappingOperation resultMappingOperation;
	
	OperationEntry(int entryIndex, String name, ApiMappingOperation parameterMappingOperation, ApiMappingOperation resultMappingOperation) {
		this.entryIndex = entryIndex;
		this.name = name;
		this.parameterMappingOperation = parameterMappingOperation;
		this.resultMappingOperation = resultMappingOperation;
	}
	
	public int getEntryIndex() {
		return this.entryIndex;
	}
		
	public String getName() {
		return this.name;
	}
	
	public ApiMappingOperation getParameterMappingOperation() {
		return this.parameterMappingOperation;
	}
	
	public ApiMappingOperation getResultMappingOperation() {
		return this.resultMappingOperation;
	}
	
}
