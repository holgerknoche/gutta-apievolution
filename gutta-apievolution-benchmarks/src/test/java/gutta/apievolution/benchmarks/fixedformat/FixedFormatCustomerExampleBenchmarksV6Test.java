package gutta.apievolution.benchmarks.fixedformat;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class FixedFormatCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        FixedFormatCustomerExampleBenchmarksV6 benchmarks = new FixedFormatCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client();        
    }

}
