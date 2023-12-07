package gutta.apievolution.inprocess.objectmapping;

import com.google.common.collect.ImmutableMap;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.ImplementedBy;
import gutta.apievolution.inprocess.InvalidApiException;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import static org.mockito.Mockito.*;

/**
 * Tests for mapping of record types using the object mapping strategy (see {@link RecordTypeValueMapper}).
 */
class RecordTypeValueMapperTest {

    /**
     * Test case: Mapping of a record type where the target type is a concrete class.
     * 
     * @throws NoSuchMethodException For formal reasons, not expected
     */
    @Test
    void mapSimpleType() throws NoSuchMethodException {
        Method targetAccessor = SimpleTargetType.class.getMethod("setTargetValue", Integer.class);
        FieldMapper fieldMapper = (value) -> ((SourceType) value).getIntValue();
        Map<Method, FieldMapper> fieldMappers = ImmutableMap.of(targetAccessor, fieldMapper);

        RecordType<?, ?, ?> recordType = mock(RecordType.class);
        when(recordType.isConcrete()).thenReturn(true);
        
        RecordTypeValueMapper mapper = new RecordTypeValueMapper(recordType, SimpleTargetType.class, fieldMappers);

        SourceType sourceValue = new SourceType();
        sourceValue.setIntValue(1234);

        SimpleTargetType targetValue = (SimpleTargetType) mapper.mapValue(sourceValue);
        assertEquals(1234, targetValue.getTargetValue());
    }

    /**
     * Test case: Mapping of a record type where the target type is an interface with an {@link ImplementedBy} annotation.
     * 
     * @throws NoSuchMethodException For formal reasons, not expected
     */
    @Test
    void mapSimpleTypeWithImplementor() throws NoSuchMethodException {
        Method targetAccessor = SimpleTargetType.class.getMethod("setTargetValue", Integer.class);
        FieldMapper fieldMapper = (value) -> ((SourceType) value).getIntValue();
        Map<Method, FieldMapper> fieldMappers = ImmutableMap.of(targetAccessor, fieldMapper);

        RecordType<?, ?, ?> recordType = mock(RecordType.class);
        when(recordType.isConcrete()).thenReturn(true);
        
        RecordTypeValueMapper mapper = new RecordTypeValueMapper(recordType, SimpleTargetTypeInterface.class, fieldMappers);

        SourceType sourceValue = new SourceType();
        sourceValue.setIntValue(1234);

        SimpleTargetTypeInterface targetValue = (SimpleTargetTypeInterface) mapper.mapValue(sourceValue);
        assertEquals(1234, targetValue.getTargetValue());
    }

    /**
     * Test case: An abstract target type without an {@link ImplementedBy} annotation results in an error.
     */
    @Test
    void missingImplementorOnInterface() {
        RecordType<?, ?, ?> recordType = mock(RecordType.class);
        when(recordType.isConcrete()).thenReturn(true);
        
        assertThrows(InvalidApiException.class, () -> new RecordTypeValueMapper(recordType, MissingImplementor.class, ImmutableMap.of()));
    }

    /**
     * Test case: Mapping of a specialized type (i.e., a type with a supertype). 
     * 
     * @throws NoSuchMethodException For formal reasons, not expected
     */
    @Test
    void specializedRecordMapping() throws NoSuchMethodException {
        Method targetAccessor1 = TargetSubType.class.getMethod("setTargetInheritedField", Integer.class);
        FieldMapper fieldMapper1 = (value) -> ((SourceSuperType) value).getInheritedField();
        Method targetAccessor2 = TargetSubType.class.getMethod("setTargetFieldA", Integer.class);
        FieldMapper fieldMapper2 = (value) -> ((SourceSubType) value).getFieldA();
        Map<Method, FieldMapper> fieldMappers = ImmutableMap.of(targetAccessor1, fieldMapper1, targetAccessor2, fieldMapper2);
        
        
        RecordType<?, ?, ?> recordType = mock(RecordType.class);
        when(recordType.isConcrete()).thenReturn(true);
        
        RecordTypeValueMapper mapper = new RecordTypeValueMapper(recordType, TargetSubType.class, fieldMappers);
        
        SourceSubType sourceValue = new SourceSubType();
        sourceValue.setFieldA(1234);
        sourceValue.setInheritedField(5678);
        
        TargetSubType targetValue = (TargetSubType) mapper.mapValue(sourceValue);
        assertEquals(1234, targetValue.getTargetFieldA());
        assertEquals(5678, targetValue.getTargetInheritedField());
    }

    /**
     * Simple source type.
     */
    private static class SourceType {

        private Integer intValue;

        public Integer getIntValue() {
            return this.intValue;
        }

        public void setIntValue(Integer intValue) {
            this.intValue = intValue;
        }

    }

    /**
     * Simple target type.
     */
    public static class SimpleTargetType implements SimpleTargetTypeInterface {

        private Integer targetValue;

        public Integer getTargetValue() {
            return this.targetValue;
        }

        public void setTargetValue(Integer targetValue) {
            this.targetValue = targetValue;
        }

    }

    /**
     * Simple target interface with an annotated implementor.
     */
    @ImplementedBy(SimpleTargetType.class)
    public interface SimpleTargetTypeInterface {

        public Integer getTargetValue();

    }

    /**
     * Target interface without an annotated implementor.
     */
    public interface MissingImplementor {

    }

    /**
     * Supertype of a source hierarchy.
     */
    public static abstract class SourceSuperType {

        private Integer inheritedField;

        public Integer getInheritedField() {
            return this.inheritedField;
        }

        public void setInheritedField(Integer inheritedField) {
            this.inheritedField = inheritedField;
        }

    }

    /**
     * Subtype of a source hierarchy.
     */
    public static class SourceSubType extends SourceSuperType {

        private Integer fieldA;

        public Integer getFieldA() {
            return this.fieldA;
        }

        public void setFieldA(Integer fieldA) {
            this.fieldA = fieldA;
        }

    }

    /**
     * Supertype of a target hierarchy.
     */
    public static abstract class TargetSuperType {

        private Integer targetInheritedField;

        public Integer getTargetInheritedField() {
            return this.targetInheritedField;
        }

        public void setTargetInheritedField(Integer targetInheritedField) {
            this.targetInheritedField = targetInheritedField;
        }

    }

    /**
     * Subtype of a target hierarchy.
     */
    public static class TargetSubType extends TargetSuperType {

        private Integer targetFieldA;

        public Integer getTargetFieldA() {
            return this.targetFieldA;
        }

        public void setTargetFieldA(Integer targetFieldA) {
            this.targetFieldA = targetFieldA;
        }

    }

}
