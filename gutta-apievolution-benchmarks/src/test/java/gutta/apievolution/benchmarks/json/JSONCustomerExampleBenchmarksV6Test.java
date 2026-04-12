package gutta.apievolution.benchmarks.json;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class JSONCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        JSONCustomerExampleBenchmarksV6 benchmarks = new JSONCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client();        
    }

}
