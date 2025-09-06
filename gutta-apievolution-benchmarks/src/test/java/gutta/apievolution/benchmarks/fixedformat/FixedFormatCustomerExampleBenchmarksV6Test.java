package gutta.apievolution.benchmarks.fixedformat;

import gutta.apievolution.benchmarks.fixedformat.FixedFormatCustomerExampleBenchmarksV6;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class FixedFormatCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        FixedFormatCustomerExampleBenchmarksV6 benchmarks = new FixedFormatCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client_short();        
    }

}
