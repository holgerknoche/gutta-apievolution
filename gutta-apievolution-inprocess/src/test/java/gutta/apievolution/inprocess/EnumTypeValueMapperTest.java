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

    /**
     * Test case: Mapping of a regular value works as expected.
     */
    @Test
    void mapRegularValue() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnum.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnum.VALUE_1, SourceEnum.VALUE_B, TargetEnum.VALUE_2));
        
        assertEquals(TargetEnum.VALUE_1, mapper.mapValue(SourceEnum.VALUE_A));
        assertEquals(TargetEnum.VALUE_2, mapper.mapValue(SourceEnum.VALUE_B));
    }
    
    /**
     * Test case: Mapping of {@code null} is always {@code null}.
     */
    @Test
    void mapNullValue() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnum.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnum.VALUE_1, SourceEnum.VALUE_B, TargetEnum.VALUE_2));
        
        assertNull(mapper.mapValue(null));
    }

    /**
     * Test case: The annotated member is used for unrepresentable values.
     */
    @Test
    void unrepresentableValueWithAnnotatedMember() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnumWithDefault.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnumWithDefault.VALUE_1, SourceEnum.VALUE_B, TargetEnumWithDefault.VALUE_2));
        
        assertEquals(TargetEnumWithDefault.UNREPRESENTABLE, mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }

    /**
     * Test case: An annotated supplier method is used for unrepresentable values.
     */
    @Test
    void unrepresentableValueWithDefaultSupplier() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnumWithUnrepresentableValueSupplier.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnumWithUnrepresentableValueSupplier.VALUE_1, SourceEnum.VALUE_B, TargetEnumWithUnrepresentableValueSupplier.VALUE_2));
        
        assertEquals(TargetEnumWithUnrepresentableValueSupplier.UNREPRESENTABLE, mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }
    
    /**
     * Test case: An exception thrown from a default value supplier method for unrepresentable types is passed to the invoker as-is. 
     */
    @Test
    void unrepresentableValueWithExceptionSupplier() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnumWithExceptionSupplier.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnumWithExceptionSupplier.VALUE_1, SourceEnum.VALUE_B, TargetEnumWithExceptionSupplier.VALUE_2));
        
        assertThrows(UnrepresentableValueException.class, () -> mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }
    
    /**
     * Test case: The default behavior is used for unrepresentable values on types with no specific behavior.
     */
    @Test
    void unrepresentableValueWithDefaultBehavior() {
        EnumTypeValueMapper mapper = new EnumTypeValueMapper(TargetEnum.class,
                ImmutableMap.of(SourceEnum.VALUE_A, TargetEnum.VALUE_1, SourceEnum.VALUE_B, TargetEnum.VALUE_2));
        
        assertNull(mapper.mapValue(SourceEnum.UNMAPPED_VALUE));
    }

    /**
     * Source enumeration for tests.
     */
    private enum SourceEnum {
        VALUE_A, VALUE_B, UNMAPPED_VALUE
    }

    /**
     * Target enumeration for tests without specified behavior for unrepresentable values.
     */
    private enum TargetEnum {
        VALUE_1, VALUE_2
    }
    
    /**
     * Target enumeration with an annotated member for unrepresentable values.
     */
    private enum TargetEnumWithDefault {
        VALUE_1, VALUE_2, @UnrepresentableValue UNREPRESENTABLE;
    }
    
    /**
     * Target enumeration with a supplier method that returns a default value.
     */
    private enum TargetEnumWithUnrepresentableValueSupplier {
        VALUE_1, VALUE_2, UNREPRESENTABLE;
        
        @UnrepresentableValue
        public static TargetEnumWithUnrepresentableValueSupplier unrepresentableValue() {
            return UNREPRESENTABLE;
        }
        
    }
    
    /**
     * Target enumeration with a supplier method that throws an exception.
     */
    private enum TargetEnumWithExceptionSupplier {
        VALUE_1, VALUE_2;
        
        @UnrepresentableValue
        public static TargetEnumWithUnrepresentableValueSupplier unrepresentableValue() {
            throw new UnrepresentableValueException();
        }
        
    }
    
    /**
     * Test exception for unrepresentable values.
     */
    private static class UnrepresentableValueException extends RuntimeException {

        private static final long serialVersionUID = -1928151682893985532L;
        
    }

}
