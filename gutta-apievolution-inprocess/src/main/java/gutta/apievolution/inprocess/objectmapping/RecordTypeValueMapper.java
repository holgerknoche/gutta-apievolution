package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.inprocess.AbstractRecordTypeValueMapper;
import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.ImplementedBy;
import gutta.apievolution.inprocess.InvalidApiException;
import gutta.apievolution.inprocess.InvalidInvocationException;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

class RecordTypeValueMapper extends AbstractRecordTypeValueMapper {

    private ObjectCreator<?> recordCreator;

    private final List<FieldValueTransferrer> transferrers;

    public RecordTypeValueMapper(RecordType<?, ?, ?> type, Class<?> representingClass, Map<Method, FieldMapper> fieldMappers) {
        super(representingClass);

        this.recordCreator = creatorFor(type, representingClass);

        List<FieldValueTransferrer> transferrers = new ArrayList<>(fieldMappers.size());
        fieldMappers.forEach((accessor, mapper) -> transferrers.add(new FieldValueTransferrer(accessor, mapper)));

        this.transferrers = transferrers;
    }

    private static ObjectCreator<?> creatorFor(RecordType<?, ?, ?> type, Class<?> representingClass) {
        if (type.isConcrete()) {
            return new ConcreteObjectCreator<>(implementorOf(representingClass));
        } else {
            return new AbstractTypeObjectCreator<>(type);
        }
    }

    private static Class<?> implementorOf(Class<?> type) {
        if (Modifier.isAbstract(type.getModifiers())) {
            ImplementedBy implementorAnnotation = type.getAnnotation(ImplementedBy.class);
            if (implementorAnnotation == null) {
                throw new InvalidApiException("Unable to determine implementor class for '" + type + "'.");
            }

            return implementorAnnotation.value();
        } else {
            return type;
        }
    }

    @Override
    public Object mapRepresentableValue(Object value) {
        Object record = this.recordCreator.createObject();
        this.transferrers.forEach(transferrer -> transferrer.transferValue(value, record));

        return record;
    }

    private interface ObjectCreator<T> {

        public T createObject();

    }

    private static class ConcreteObjectCreator<T> implements ObjectCreator<T> {

        private final Constructor<T> constructor;

        public ConcreteObjectCreator(Class<T> createdType) {
            try {
                this.constructor = createdType.getConstructor();
            } catch (NoSuchMethodException | SecurityException e) {
                throw new InvalidApiException("No usable constructor on type '" + createdType + "'.", e);
            }
        }

        public T createObject() {
            try {
                return this.constructor.newInstance();
            } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
                throw new InvalidInvocationException("Error creating a record type instance.", e);
            }
        }

    }

    private static class AbstractTypeObjectCreator<T> implements ObjectCreator<T> {

        private final Type type;

        public AbstractTypeObjectCreator(Type type) {
            this.type = type;
        }

        @Override
        public T createObject() {
            throw new UnsupportedOperationException("Unable to create object for abstract type '" + this.type + "'.");
        }

    }

    private static class FieldValueTransferrer {

        private final MethodHandle targetHandle;

        private final FieldMapper fieldMapper;

        public FieldValueTransferrer(Method targetAccessor, FieldMapper fieldMapper) {
            this.targetHandle = findHandleFor(targetAccessor);
            this.fieldMapper = fieldMapper;
        }

        private static MethodHandle findHandleFor(Method accessor) {
            try {
                return MethodHandles.publicLookup().unreflect(accessor);
            } catch (IllegalAccessException e) {
                throw new InvalidApiException("Could not look up method handle for accessor '" + accessor + "',", e);
            }
        }

        public void transferValue(Object sourceObject, Object targetObject) {
            Object mappedValue = this.fieldMapper.mapField(sourceObject);
            try {
                this.targetHandle.invoke(targetObject, mappedValue);
            } catch (Throwable e) {
                throw new InvalidInvocationException("Error transferring a value.", e);
            }
        }

    }

}
