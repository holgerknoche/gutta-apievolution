package gutta.apievolution.benchmarks.inprocess.dynproxy;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class DynProxyCustomerExampleBenchmarksV1Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        DynProxyCustomerExampleBenchmarksV1 benchmarks = new DynProxyCustomerExampleBenchmarksV1();
        
        benchmarks.invokeFromV1Client();        
    }

}
