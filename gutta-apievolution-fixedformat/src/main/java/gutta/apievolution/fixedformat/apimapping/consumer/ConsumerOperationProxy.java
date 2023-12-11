package gutta.apievolution.fixedformat.apimapping.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatData;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public abstract class ConsumerOperationProxy<P, R> {

    private final RequestRouter router;
    
    private final FixedFormatMapper mapper;

    private final String operationName;
    
    private final Class<R> resultType;
    
    private final ByteBuffer parameterBuffer;
    
    private final FixedFormatData parameterData;
    
    private final ByteBuffer resultBuffer;
    
    private final FixedFormatData resultData;

    protected ConsumerOperationProxy(String operationName, Class<P> parameterType, Class<R> resultType, RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        this.operationName = operationName;
        this.router = router;
        this.mapper = mapper;
        this.resultType = resultType;
        
        this.parameterBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(parameterType));
        this.parameterData = FixedFormatData.of(parameterBuffer, charset);
        this.resultBuffer = ByteBuffer.allocate(mapper.determineMaxSizeOf(resultType));
        this.resultData = FixedFormatData.of(resultBuffer, charset);
    }
    
    public R invoke(P parameter) {
        this.mapper.writeValue(parameter, this.parameterData);
        this.parameterBuffer.flip();
        
        // Pass the data per some router to the provider method
        this.router.routeRequest(this.operationName, this.parameterBuffer, this.resultBuffer);
                
        return this.mapper.readValue(this.resultData, this.resultType);        
    }

}
