package gutta.apievolution.benchmarks.fixedformat;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class FixedFormatCustomerExampleBenchmarksV3Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        FixedFormatCustomerExampleBenchmarksV3 benchmarks = new FixedFormatCustomerExampleBenchmarksV3();
        
        benchmarks.invokeFromV3Client();        
    }

}
