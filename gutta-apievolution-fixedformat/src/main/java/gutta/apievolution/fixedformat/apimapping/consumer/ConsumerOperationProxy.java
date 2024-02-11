package gutta.apievolution.fixedformat.apimapping.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 * A {@link ConsumerOperationProxy} encapsulates the format conversions and other technical details for invoking a provider operation with fixed-format data.
 * 
 * @param <P> The parameter type of the operation
 * @param <R> The result type of the operation
 */
public abstract class ConsumerOperationProxy<P, R> {

    private final RequestRouter router;

    private final FixedFormatMapper mapper;

    private final String operationName;

    private final Class<P> parameterType;
    
    private final Class<R> resultType;
    
    private final Charset charset;

    /**
     * Creates a new proxy with the given data.
     * 
     * @param operationName The name of the represented operation
     * @param parameterType The parameter type of the operation
     * @param resultType    The result type of the operation
     * @param router        The request router to use for finding the appropriate provider proxy
     * @param mapper        The fixed-format mapper to use
     * @param charset       The charset to use
     */
    protected ConsumerOperationProxy(String operationName, Class<P> parameterType, Class<R> resultType, RequestRouter router, FixedFormatMapper mapper,
            Charset charset) {
        this.operationName = operationName;
        this.router = router;
        this.mapper = mapper;
        this.parameterType = parameterType;
        this.resultType = resultType;
        this.charset = charset;
    }

    /**
     * Invokes the wrapped operation with the given parameter.
     * 
     * @param parameter The parameter to pass to the operation
     * @return The operation's result
     */
    public R invoke(P parameter) {
        FixedFormatMapper formatMapper = this.mapper;
        
        ByteBuffer parameterBuffer = ByteBuffer.allocate(formatMapper.determineMaxSizeOf(this.parameterType));
        FixedFormatData parameterData = FixedFormatData.of(parameterBuffer, this.charset);
        
        this.mapper.writeValue(parameter, this.parameterType, parameterData);
        parameterBuffer.flip();
        
        ByteBuffer resultBuffer = ByteBuffer.allocate(formatMapper.determineMaxSizeOf(this.resultType));

        this.router.routeRequest(this.operationName, parameterBuffer, resultBuffer);

        FixedFormatData resultData = FixedFormatData.of(resultBuffer, this.charset);
        return this.mapper.readValue(resultData, this.resultType);
    }

}
