package gutta.apievolution.benchmarks.json;

import gutta.apievolution.benchmarks.json.JSONCustomerExampleBenchmarksV6;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class JSONCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        JSONCustomerExampleBenchmarksV6 benchmarks = new JSONCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client_short();        
    }

}
