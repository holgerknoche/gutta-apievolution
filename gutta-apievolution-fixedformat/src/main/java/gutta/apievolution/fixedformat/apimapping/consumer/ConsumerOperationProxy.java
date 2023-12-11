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

    private final Class<R> resultType;

    private final ByteBuffer parameterBuffer;

    private final FixedFormatData parameterData;

    private final ByteBuffer resultBuffer;

    private final FixedFormatData resultData;

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
        this.resultType = resultType;

        this.parameterBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(parameterType));
        this.parameterData = FixedFormatData.of(parameterBuffer, charset);
        this.resultBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(resultType));
        this.resultData = FixedFormatData.of(resultBuffer, charset);
    }

    /**
     * Invokes the wrapped operation with the given parameter.
     * 
     * @param parameter The parameter to pass to the operation
     * @return The operation's result
     */
    public R invoke(P parameter) {
        this.mapper.writeValue(parameter, this.parameterData);
        this.parameterBuffer.flip();

        this.router.routeRequest(this.operationName, this.parameterBuffer, this.resultBuffer);

        return this.mapper.readValue(this.resultData, this.resultType);
    }

}
