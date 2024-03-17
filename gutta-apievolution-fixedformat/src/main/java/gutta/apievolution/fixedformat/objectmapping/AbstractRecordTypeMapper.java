package gutta.apievolution.fixedformat.objectmapping;

abstract class AbstractRecordTypeMapper extends UserDefinedTypeMapper {
    
    protected static final int DISCRIMINATOR_SIZE = 4;
        
    protected int determineTypeIdFor(Object value) {
        Class<?> type = value.getClass();
        TypeId typeIdAnnotation = type.getAnnotation(TypeId.class);
        if (typeIdAnnotation == null) {
            throw new InvalidRepresentationElementException("Missing type id on type " + type + ".");
        }

        return typeIdAnnotation.value();
    }

}
