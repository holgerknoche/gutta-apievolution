package gutta.apievolution.benchmarks.fixedformat;

import gutta.apievolution.benchmarks.fixedformat.FixedFormatCustomerExampleBenchmarksV3;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class FixedFormatCustomerExampleBenchmarksV3Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        FixedFormatCustomerExampleBenchmarksV3 benchmarks = new FixedFormatCustomerExampleBenchmarksV3();
        
        benchmarks.invokeFromV3Client_short();        
    }

}
