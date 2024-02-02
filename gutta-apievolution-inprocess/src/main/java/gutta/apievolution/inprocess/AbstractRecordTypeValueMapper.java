package gutta.apievolution.inprocess;

import java.util.function.Supplier;

/**
 * Abstract superclass for mappers of record types, which provided common functionality.
 */
public abstract class AbstractRecordTypeValueMapper implements ValueMapper {
    
    protected final Supplier<?> onUnrepresentableValue;
    
    /**
     * Creates a new mapper for the given target type representation.
     * 
     * @param targetType The class representing the target type
     */
    protected AbstractRecordTypeValueMapper(Class<?> targetType) {
        this.onUnrepresentableValue = determineActionOnUnrepresentableValue(targetType);
    }
    
    private static Supplier<?> determineActionOnUnrepresentableValue(Class<?> targetType) {
        UnrepresentableValueSupplier supplier = UnrepresentableValueSupplier.findSupplierOnType(targetType);
        if (supplier != null) {
            return supplier;
        }
        
        return Defaults::onUnrepresentableValue;
    }
    
    @Override
    public final Object mapValue(Object value) {
        if (value == null) {
            return null;
        }
        
        if (this.isRepresentable(value)) {
            return this.mapRepresentableValue(value);
        } else {
            return this.onUnrepresentableValue.get();
        }
    }
    
    /**
     * Maps the given value, which is guaranteed to be representable (as ensured by {@link #isRepresentable(Object)} as well as not {@code null}.
     * 
     * @param value The value to map, never {@code null}
     * @return The mapped value
     */
    public abstract Object mapRepresentableValue(Object value);

}
