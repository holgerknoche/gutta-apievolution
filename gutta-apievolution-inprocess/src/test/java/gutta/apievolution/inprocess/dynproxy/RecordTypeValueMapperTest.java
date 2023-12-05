package gutta.apievolution.inprocess.dynproxy;

import com.google.common.collect.ImmutableMap;
import gutta.apievolution.inprocess.FieldMapper;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for mapping of record types using the dynamic proxies strategy (see {@link RecordTypeValueMapper}).
 */
class RecordTypeValueMapperTest {

    /**
     * Test case: Mapping of a record type where the target type is a concrete class.
     * 
     * @throws NoSuchMethodException For formal reasons, not expected
     */
    @Test
    void mapSimpleType() throws NoSuchMethodException {
        Method targetAccessor = SimpleTargetType.class.getMethod("getTargetValue");
        FieldMapper fieldMapper = (value) -> ((SourceType) value).getIntValue();
        Map<Method, FieldMapper> fieldMappers = ImmutableMap.of(targetAccessor, fieldMapper);

        RecordTypeValueMapper mapper = new RecordTypeValueMapper(SimpleTargetType.class, fieldMappers);

        SourceType sourceValue = new SourceType();
        sourceValue.setIntValue(1234);

        SimpleTargetType targetValue = (SimpleTargetType) mapper.mapValue(sourceValue);
        assertEquals(1234, targetValue.getTargetValue());
    }

    /**
     * Test case: Mapping of a specialized type (i.e., a type with a supertype). 
     * 
     * @throws NoSuchMethodException For formal reasons, not expected
     */
    @Test
    void specializedRecordMapping() throws NoSuchMethodException {
        Method targetAccessor1 = TargetSubType.class.getMethod("getTargetInheritedField");
        FieldMapper fieldMapper1 = (value) -> ((SourceSuperType) value).getInheritedField();
        Method targetAccessor2 = TargetSubType.class.getMethod("getTargetFieldA");
        FieldMapper fieldMapper2 = (value) -> ((SourceSubType) value).getFieldA();
        Map<Method, FieldMapper> fieldMappers = ImmutableMap.of(targetAccessor1, fieldMapper1, targetAccessor2, fieldMapper2);
        
        RecordTypeValueMapper mapper = new RecordTypeValueMapper(TargetSubType.class, fieldMappers);
        
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
    public interface SimpleTargetType {

        public Integer getTargetValue();

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
    public interface TargetSuperType {

        public Integer getTargetInheritedField();

    }

    /**
     * Subtype of a target hierarchy.
     */
    public interface TargetSubType extends TargetSuperType {

        public Integer getTargetFieldA();

    }

}
