package gutta.apievolution.jmh.fixedformat;

import org.junit.jupiter.api.Test;

import java.io.IOException;

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
