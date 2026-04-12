package gutta.apievolution.benchmarks.fixedformat;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class FixedFormatCustomerExampleBenchmarksV1Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        FixedFormatCustomerExampleBenchmarksV1 benchmarks = new FixedFormatCustomerExampleBenchmarksV1();
        
        benchmarks.invokeFromV1Client();        
    }

}
