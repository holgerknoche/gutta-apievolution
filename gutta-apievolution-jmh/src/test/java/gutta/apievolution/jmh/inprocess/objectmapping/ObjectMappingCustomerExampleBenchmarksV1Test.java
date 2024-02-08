package gutta.apievolution.jmh.inprocess.objectmapping;

import org.junit.jupiter.api.Test;

import java.io.IOException;

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
