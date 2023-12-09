package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

abstract class ConsumerProxyTemplate<R> extends ConsumerOperationProxy {
    
    private static final String API_ID = "";
    
    private static final int REVISION = 0;
    
    protected ConsumerProxyTemplate(ConsumerApiDefinition apiDefinition, String returnTypeName, RequestRouter requestRouter) {
        super(apiDefinition, "ConsumerParameter", returnTypeName, requestRouter);
    }
    
    protected abstract Class<R> getResultClass();
    
    protected abstract String getOperationName();
    
    public R invoke(ConsumerParameter parameter) {
        return this.invokeMethod(API_ID, REVISION, this.getOperationName(), parameter, this.getResultClass());
    }

}
