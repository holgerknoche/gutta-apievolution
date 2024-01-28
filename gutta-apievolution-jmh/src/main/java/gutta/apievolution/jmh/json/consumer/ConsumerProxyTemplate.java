package gutta.apievolution.jmh.json.consumer;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.json.ConsumerOperationProxy;
import gutta.apievolution.json.RequestRouter;

import java.lang.reflect.ParameterizedType;

abstract class ConsumerProxyTemplate<R> extends ConsumerOperationProxy<ConsumerParameter, R> {

    private static final String API_ID = "";

    protected ConsumerProxyTemplate(ConsumerApiDefinition apiDefinition, String operationName, String returnTypeName, Class<R> returnTypeRepresentation,
            RequestRouter requestRouter) {
        super(apiDefinition, API_ID, operationName, "ConsumerParameter", returnTypeName, returnTypeRepresentation, requestRouter);

        determineResultType(this.getClass());
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static <T> Class<T> determineResultType(Class<? extends ConsumerProxyTemplate> subClass) {
        ParameterizedType superType = (ParameterizedType) subClass.getGenericSuperclass();

        return (Class<T>) superType.getActualTypeArguments()[0];
    }

}
