package gutta.apievolution.inprocess;

import org.junit.jupiter.api.Test;

import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for the class {@link AbstractRecordTypeValueMapper}.
 */
class AbstractRecordTypeValueMapperTest {

    /**
     * Test case: Mapping of a regular type works as expected.
     */
    @Test
    void mapRegularValue() {
        final String inputString = "TestString";
        
        AbstractRecordTypeValueMapper mapper = new TestRecordTypeValueMapper<>(TypeWithoutUnrepresentableValuesBehavior.class, (source) -> new TypeWithoutUnrepresentableValuesBehavior(source.toString()));
        
        TypeWithoutUnrepresentableValuesBehavior result = (TypeWithoutUnrepresentableValuesBehavior) mapper.mapValue(inputString);
        assertEquals(inputString, result.value);
    }
    
    /**
     * Test case: Mapping of {@code null} is always {@code null}.
     */
    @Test
    void mapNullValue() {
        AbstractRecordTypeValueMapper mapper = new TestRecordTypeValueMapper<>(TypeWithoutUnrepresentableValuesBehavior.class, (source) -> new TypeWithoutUnrepresentableValuesBehavior(source.toString()));
        
        assertNull(mapper.mapValue(null));
    }
    
    /**
     * Test case: The default behavior is used for unrepresentable values on types with no specific behavior.
     */
    @Test
    void unrepresentableValueWithDefaultBehavior() {
        AbstractRecordTypeValueMapper mapper = new TestRecordTypeValueMapper<>(TypeWithoutUnrepresentableValuesBehavior.class, (source) -> null);
        
        assertNull(mapper.mapValue("unrepresentable"));
    }
    
    /**
     * Test case: A default value supplier method for unrepresentable types works as expected. 
     */
    @Test
    void unrepresentableValueWithDefaultSupplier() {
        AbstractRecordTypeValueMapper mapper = new TestRecordTypeValueMapper<>(TypeWithDefaultValueForUnrepresentableValues.class, (source) -> null);
        
        TypeWithDefaultValueForUnrepresentableValues mappedValue = (TypeWithDefaultValueForUnrepresentableValues) mapper.mapValue("unrepresentable");
        assertTrue(mappedValue.isUnrepresentable());
    }
    
    /**
     * Test case: An exception thrown from a default value supplier method for unrepresentable types is passed to the invoker as-is. 
     */
    @Test
    void unrepresentableValueWithExceptionSupplier() {
        AbstractRecordTypeValueMapper mapper = new TestRecordTypeValueMapper<>(TypeWithExceptionOnUnrepresentableValue.class, (source) -> null);
        
        assertThrows(UnrepresentableValueException.class, () -> mapper.mapValue("unrepresentable"));
    }

    /**
     * Test implementation of a record type mapper.
     * 
     * @param <T> The target type of the mapper
     */
    private static class TestRecordTypeValueMapper<T> extends AbstractRecordTypeValueMapper {

        private final Function<Object, T> mapper;

        public TestRecordTypeValueMapper(Class<T> targetType, Function<Object, T> mapper) {
            super(targetType);

            this.mapper = mapper;
        }

        @Override
        public boolean isRepresentable(Object value) {
            return (this.mapper.apply(value) != null);
        }

        @Override
        protected Object mapRepresentableValue(Object value) {
            return this.mapper.apply(value);
        }

    }
    
    /**
     * Target type without specific behavior for unrepresentable types.
     */
    private static class TypeWithoutUnrepresentableValuesBehavior {
        
        public final String value;
        
        public TypeWithoutUnrepresentableValuesBehavior(String value) {
            this.value = value;
        }
        
    }
    
    /**
     * Target type with a default value for unrepresentable values.
     */
    private static class TypeWithDefaultValueForUnrepresentableValues {

        @UnrepresentableValue
        public static TypeWithDefaultValueForUnrepresentableValues unrepresentableValue() {
            return Unrepresentable.INSTANCE;
        }
        
        public boolean isUnrepresentable() {
            return false;
        }
        
        private static class Unrepresentable extends TypeWithDefaultValueForUnrepresentableValues {
            
            public static final Unrepresentable INSTANCE = new Unrepresentable();
            
            private Unrepresentable() {
                // Private constructor
            }
            
            @Override
            public boolean isUnrepresentable() {
                return true;
            }
            
        }
                
    }
    
    /**
     * Target type that throws an exception when an unrepresentable value is encountered.
     */
    private static class TypeWithExceptionOnUnrepresentableValue {

        @UnrepresentableValue
        public static TypeWithExceptionOnUnrepresentableValue unrepresentableValue() {
            throw new UnrepresentableValueException();
        }
                        
    }
    
    /**
     * Exception for unrepresentable values.
     */
    private static class UnrepresentableValueException extends RuntimeException {

        private static final long serialVersionUID = 8791847145401694717L;
        
    }

}
