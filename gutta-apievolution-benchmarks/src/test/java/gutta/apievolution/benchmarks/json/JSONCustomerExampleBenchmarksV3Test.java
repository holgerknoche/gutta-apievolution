package gutta.apievolution.benchmarks.json;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class JSONCustomerExampleBenchmarksV3Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        JSONCustomerExampleBenchmarksV3 benchmarks = new JSONCustomerExampleBenchmarksV3();
        
        benchmarks.invokeFromV3Client();        
    }

}
