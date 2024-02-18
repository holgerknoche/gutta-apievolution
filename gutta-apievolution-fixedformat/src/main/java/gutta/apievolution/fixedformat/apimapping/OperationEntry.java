package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

class OperationEntry {

    private final int entryIndex;

    private final String name;

    private final ApiMappingOperation parameterMappingOperation;

    private final ApiMappingOperation resultMappingOperation;

    OperationEntry(int entryIndex, String name, ApiMappingOperation parameterMappingOperation,
            ApiMappingOperation resultMappingOperation) {
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
    
    @Override
    public int hashCode() {
        return this.entryIndex;
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(OperationEntry that) {
        return (this.getEntryIndex() == that.getEntryIndex()) &&
               Objects.equals(this.getName(), that.getName()) &&
               Objects.equals(this.getParameterMappingOperation(),  that.getParameterMappingOperation()) &&
               Objects.equals(this.getResultMappingOperation(),  that.getResultMappingOperation());
    }

}
