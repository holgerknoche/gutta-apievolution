package gutta.apievolution.jmh.inprocess.dynproxy;

import org.junit.jupiter.api.Test;

import java.io.IOException;

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
