package gutta.apievolution.benchmarks.inprocess.objectmapping;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class ObjectMappingCustomerExampleBenchmarksV1Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        ObjectMappingCustomerExampleBenchmarksV1 benchmarks = new ObjectMappingCustomerExampleBenchmarksV1();
        
        benchmarks.invokeFromV1Client();        
    }

}
