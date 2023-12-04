package gutta.apievolution.inprocess;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests for the class {@link EnumTypeValueMapper}.
 */
class EnumTypeValueMapperTest {

    @Test
    void regularValueMapping() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnum.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnum.VALUE_1, SourceEnum.VALUE_B, TargetEnum.VALUE_2));
        
        assertEquals(TargetEnum.VALUE_1, mapper.mapValue(SourceEnum.VALUE_A));
        assertEquals(TargetEnum.VALUE_2, mapper.mapValue(SourceEnum.VALUE_B));
    }

    @Test
    void unrepresentableValueWithDefault() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnumWithDefault.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnumWithDefault.VALUE_1, SourceEnum.VALUE_B, TargetEnumWithDefault.VALUE_2));
        
        assertEquals(TargetEnumWithDefault.UNREPRESENTABLE, mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }

    @Test
    void unrepresentableValueWithDefaultSupplier() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnumWithUnrepresentableValueSupplier.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnumWithUnrepresentableValueSupplier.VALUE_1, SourceEnum.VALUE_B, TargetEnumWithUnrepresentableValueSupplier.VALUE_2));
        
        assertEquals(TargetEnumWithUnrepresentableValueSupplier.UNREPRESENTABLE, mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }
    
    @Test
    void unrepresentableValueWithExceptionSupplier() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnumWithExceptionSupplier.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnumWithExceptionSupplier.VALUE_1, SourceEnum.VALUE_B, TargetEnumWithExceptionSupplier.VALUE_2));
        
        assertThrows(UnrepresentableValueException.class, () -> mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }
    
    @Test
    void unrepresentableValueWithDefaultBehavior() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnum.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnum.VALUE_1, SourceEnum.VALUE_B, TargetEnum.VALUE_2));
        
        assertNull(mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }

    private enum SourceEnum {
        VALUE_A, VALUE_B, UNMAPPED_VALUE
    }

    private enum TargetEnum {
        VALUE_1, VALUE_2
    }
    
    private enum TargetEnumWithDefault {
        VALUE_1, VALUE_2, @UnrepresentableValue UNREPRESENTABLE;
    }
    
    private enum TargetEnumWithUnrepresentableValueSupplier {
        VALUE_1, VALUE_2, UNREPRESENTABLE;
        
        @UnrepresentableValue
        public static TargetEnumWithUnrepresentableValueSupplier unrepresentableValue() {
            return UNREPRESENTABLE;
        }
        
    }
    
    private enum TargetEnumWithExceptionSupplier {
        VALUE_1, VALUE_2;
        
        @UnrepresentableValue
        public static TargetEnumWithUnrepresentableValueSupplier unrepresentableValue() {
            throw new UnrepresentableValueException();
        }
        
    }
    
    private static class UnrepresentableValueException extends RuntimeException {

        private static final long serialVersionUID = -1928151682893985532L;
        
    }

}
