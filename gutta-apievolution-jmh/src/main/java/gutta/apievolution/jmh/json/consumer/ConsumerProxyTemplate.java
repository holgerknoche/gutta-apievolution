package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

import java.lang.reflect.ParameterizedType;

abstract class ConsumerProxyTemplate<R> extends ConsumerOperationProxy {
    
    private static final String API_ID = "";
    
    private static final int REVISION = 0;
    
    protected ConsumerProxyTemplate(ConsumerApiDefinition apiDefinition, String returnTypeName, RequestRouter requestRouter) {
        super(apiDefinition, "ConsumerParameter", returnTypeName, requestRouter);
        
        determineResultType(this.getClass());
    }
    
    protected Class<R> getResultClass() {
        return determineResultType(this.getClass());
    }
    
    protected abstract String getOperationName();
    
    public R invoke(ConsumerParameter parameter) {
        return this.invokeMethod(API_ID, REVISION, this.getOperationName(), parameter, this.getResultClass());
    }
    
    @SuppressWarnings({"rawtypes", "unchecked"})
    private static <T> Class<T> determineResultType(Class<? extends ConsumerProxyTemplate> subClass) {
        ParameterizedType superType = (ParameterizedType) subClass.getGenericSuperclass();
        
        return (Class<T>) superType.getActualTypeArguments()[0];
    }

}
