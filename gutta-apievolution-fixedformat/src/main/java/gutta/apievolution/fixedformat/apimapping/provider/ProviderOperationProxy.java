package gutta.apievolution.fixedformat.apimapping.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public abstract class ProviderOperationProxy<P, R> {

    private final String operationName;

    private final ApiMappingScript consumerToProviderScript;

    private final ApiMappingScript providerToConsumerScript;

    private final ByteBuffer providerParameterBuffer;

    private final FixedFormatData providerParameterData;

    private final ByteBuffer providerResultBuffer;

    private final FixedFormatData providerResultData;
    
    private final Class<P> parameterType;
    
    private final Class<R> resultType;
    
    private final FixedFormatMapper mapper;

    protected ProviderOperationProxy(String operationName, Class<P> parameterType, Class<R> resultType, ApiMappingScript consumerToProviderScript,
            ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper, Charset charset) {

        this.operationName = operationName;
        this.consumerToProviderScript = consumerToProviderScript;
        this.providerToConsumerScript = providerToConsumerScript;
        this.parameterType = parameterType;
        this.resultType = resultType;
        this.mapper = mapper;

        this.providerParameterBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(parameterType));
        this.providerParameterData = FixedFormatData.of(this.providerParameterBuffer, charset);

        this.providerResultBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(resultType));
        this.providerResultData = FixedFormatData.of(this.providerResultBuffer, charset);
    }

    public ByteBuffer invoke(ByteBuffer consumerParameterBuffer, ByteBuffer consumerResultBuffer) {
        // Map the parameter data provided by the consumer
        ByteBuffer parameterBuffer = this.providerParameterBuffer;        
        this.consumerToProviderScript.mapParameterFor(this.getOperationName(), consumerParameterBuffer, parameterBuffer);
        parameterBuffer.flip();
        P parameter = this.mapper.readValue(this.providerParameterData, this.parameterType);
        
        R result = this.invokeMethod(parameter);
        ByteBuffer resultBuffer = this.providerResultBuffer;
        this.mapper.writeValue(result, this.providerResultData);
        resultBuffer.flip();
        this.providerToConsumerScript.mapResultFor(this.getOperationName(), resultBuffer, consumerResultBuffer);
        
        consumerResultBuffer.flip();
        return consumerResultBuffer;
    }
    
    public String getOperationName() {
        return this.operationName;
    }

    protected abstract R invokeMethod(P parameter);

}
