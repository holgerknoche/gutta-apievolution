package gutta.apievolution.inprocess;

/**
 * Abstract supertype for API method invokers that provides common functionality.
 */
public abstract class AbstractApiMethodInvoker implements ApiMethodInvoker {

    protected final TypeMappingStrategy typeMappingStrategy;

    /**
     * Creates a new invoker using the given type mapping strategy.
     * 
     * @param typeMappingStrategy The type mapping strategy to use
     */
    protected AbstractApiMethodInvoker(TypeMappingStrategy typeMappingStrategy) {
        this.typeMappingStrategy = typeMappingStrategy;
    }

    @Override
    public Object invokeApiMethod(Object apiObject, Object parameterObject) throws Exception {
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

    /**
     * Invokes the API method referenced by this invoker on the given API object.
     * 
     * @param apiObject             The API object to invoke the method on
     * @param mappedParameterObject The mapped parameter object to pass to the method
     * @return The result of the API method
     * @throws Exception If an exception occurs in the method
     */
    protected abstract Object invokeMethod(Object apiObject, Object mappedParameterObject) throws Exception;

}
