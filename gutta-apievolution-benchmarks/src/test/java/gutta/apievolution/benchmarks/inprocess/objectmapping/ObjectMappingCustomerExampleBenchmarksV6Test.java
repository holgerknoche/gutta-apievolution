package gutta.apievolution.benchmarks.inprocess.objectmapping;

import java.io.IOException;

import org.junit.jupiter.api.Test;

class ObjectMappingCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        ObjectMappingCustomerExampleBenchmarksV6 benchmarks = new ObjectMappingCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client();        
    }

}
