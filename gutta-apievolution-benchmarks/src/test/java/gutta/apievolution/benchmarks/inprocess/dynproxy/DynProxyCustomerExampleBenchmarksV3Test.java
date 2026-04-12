package gutta.apievolution.benchmarks.inprocess.dynproxy;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class DynProxyCustomerExampleBenchmarksV3Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        DynProxyCustomerExampleBenchmarksV3 benchmarks = new DynProxyCustomerExampleBenchmarksV3();
        
        benchmarks.invokeFromV3Client();        
    }

}
