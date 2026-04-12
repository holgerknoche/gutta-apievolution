package gutta.apievolution.benchmarks.json;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class JSONCustomerExampleBenchmarksV1Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        JSONCustomerExampleBenchmarksV1 benchmarks = new JSONCustomerExampleBenchmarksV1();
        
        benchmarks.invokeFromV1Client();        
    }

}
