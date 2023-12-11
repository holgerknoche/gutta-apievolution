package gutta.apievolution.fixedformat.apimapping.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 * A {@link ProviderOperationProxy} encapsulates a provider operation and transparently handles the required format conversion.
 * 
 * @param <P> The parameter type of the operation
 * @param <R> The result type of the operation
 */
public abstract class ProviderOperationProxy<P, R> {

    private final String operationName;

    private final ApiMappingScript consumerToProviderScript;

    private final ApiMappingScript providerToConsumerScript;

    private final Class<P> parameterType;

    private final Class<R> resultType;

    private final FixedFormatMapper mapper;
    
    private final Charset charset;

    /**
     * Creates a new proxy using the given data.
     * 
     * @param operationName            The name of the operation
     * @param parameterType            The parameter type of the operation
     * @param resultType               The result type of the operation
     * @param consumerToProviderScript The consumer-to-provider script to use for the parameter conversion
     * @param providerToConsumerScript The provider-to-consumer script to use for the result conversion
     * @param mapper                   The fixed-format data mapper to use
     * @param charset                  The charset to use
     */
    protected ProviderOperationProxy(String operationName, Class<P> parameterType, Class<R> resultType, ApiMappingScript consumerToProviderScript,
            ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper, Charset charset) {

        this.operationName = operationName;
        this.consumerToProviderScript = consumerToProviderScript;
        this.providerToConsumerScript = providerToConsumerScript;
        this.parameterType = parameterType;
        this.resultType = resultType;
        this.mapper = mapper;
        this.charset = charset;
    }

    /**
     * Invokes the representing operation using the given buffers.
     * 
     * @param consumerParameterBuffer The buffer containing the parameter data. It is expected that this buffer was {@link ByteBuffer#flip() flipped} before
     *                                invoking this method.
     * @param consumerResultBuffer    The buffer to store the result data in. This buffer will be {@link ByteBuffer#flip() flipped} before returning.
     * @return The result buffer
     */
    public ByteBuffer invoke(ByteBuffer consumerParameterBuffer, ByteBuffer consumerResultBuffer) {
        FixedFormatMapper formatMapper = this.mapper;
        
        // Map the parameter data provided by the consumer
        ByteBuffer parameterBuffer = ByteBuffer.allocate(formatMapper.determineMaxSizeOf(this.parameterType));        
        
        this.consumerToProviderScript.mapParameterFor(this.getOperationName(), consumerParameterBuffer, parameterBuffer);
        parameterBuffer.flip();
        
        FixedFormatData parameterData = FixedFormatData.of(parameterBuffer, this.charset);
        P parameter = this.mapper.readValue(parameterData, this.parameterType);

        R result = this.invokeOperation(parameter);
        
        ByteBuffer resultBuffer = ByteBuffer.allocate(formatMapper.determineMaxSizeOf(this.resultType));
        FixedFormatData resultData = FixedFormatData.of(resultBuffer, this.charset);
                
        this.mapper.writeValue(result, resultData);
        resultBuffer.flip();
        this.providerToConsumerScript.mapResultFor(this.getOperationName(), resultBuffer, consumerResultBuffer);

        consumerResultBuffer.flip();
        return consumerResultBuffer;
    }

    /**
     * Returns the name of the represented operation.
     * 
     * @return see above
     */
    public String getOperationName() {
        return this.operationName;
    }

    /**
     * Invokes the actual implementation of the operation.
     * 
     * @param parameter The parameter to pass to the operation.
     * @return The result of the operation
     */
    protected abstract R invokeOperation(P parameter);

}
