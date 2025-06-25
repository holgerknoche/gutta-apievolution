package gutta.apievolution.benchmarks.inprocess.dynproxy;

import gutta.apievolution.benchmarks.inprocess.dynproxy.DynProxyCustomerExampleBenchmarksV3;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class DynProxyCustomerExampleBenchmarksV3Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        DynProxyCustomerExampleBenchmarksV3 benchmarks = new DynProxyCustomerExampleBenchmarksV3();
        
        benchmarks.invokeFromV3Client_short();        
    }

}
