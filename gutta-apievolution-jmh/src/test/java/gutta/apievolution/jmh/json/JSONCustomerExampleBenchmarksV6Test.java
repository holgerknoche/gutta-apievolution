package gutta.apievolution.jmh.json;

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
