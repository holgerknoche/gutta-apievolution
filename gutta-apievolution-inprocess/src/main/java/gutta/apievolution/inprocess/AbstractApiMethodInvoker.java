package gutta.apievolution.inprocess;

public abstract class AbstractApiMethodInvoker implements ApiMethodInvoker {

    private TypeMappingStrategy typeMappingStrategy;
     
    protected AbstractApiMethodInvoker(TypeMappingStrategy typeMappingStrategy) {
        this.typeMappingStrategy = typeMappingStrategy;
    }
    
    @Override
    public Object invokeApiMethod(Object apiObject, Object parameterObject) {
        Object mappedParameterObject = this.mapObject(parameterObject);        
        Object resultObject = this.invokeMethod(apiObject, mappedParameterObject);        
        return this.mapObject(resultObject);
    }
    
    private Object mapObject(Object object) {
        if (object == null) {
            return null;
        }

        Class<?> sourceType = object.getClass();
        ValueMapper valueMapper = this.typeMappingStrategy.mapperFor(sourceType);
        if (valueMapper == null) {
            throw new InvalidApiException("No value mapper for type '" + sourceType + "'.");
        }
       
        return valueMapper.mapValue(object);
    }
        
    protected abstract Object invokeMethod(Object apiObject, Object mappedParameterObject);
    
}
