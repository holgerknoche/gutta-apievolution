package gutta.apievolution.jmh.inprocess.dynproxy;

import org.junit.jupiter.api.Test;

import java.io.IOException;

class DynProxyCustomerExampleBenchmarksV1Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        DynProxyCustomerExampleBenchmarksV1 benchmarks = new DynProxyCustomerExampleBenchmarksV1();
        
        benchmarks.invokeFromV1Client_short();        
    }

}
