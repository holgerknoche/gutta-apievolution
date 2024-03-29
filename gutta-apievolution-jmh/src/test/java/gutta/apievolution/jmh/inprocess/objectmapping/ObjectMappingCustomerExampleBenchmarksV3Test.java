package gutta.apievolution.jmh.inprocess.objectmapping;

import org.junit.jupiter.api.Test;

import java.io.IOException;

class ObjectMappingCustomerExampleBenchmarksV3Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        ObjectMappingCustomerExampleBenchmarksV3 benchmarks = new ObjectMappingCustomerExampleBenchmarksV3();
        
        benchmarks.invokeFromV3Client_short();        
    }

}
