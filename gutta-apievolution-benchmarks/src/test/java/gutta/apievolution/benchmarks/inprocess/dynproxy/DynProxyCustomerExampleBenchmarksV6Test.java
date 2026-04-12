package gutta.apievolution.benchmarks.inprocess.dynproxy;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class DynProxyCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        DynProxyCustomerExampleBenchmarksV6 benchmarks = new DynProxyCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client();        
    }

}
