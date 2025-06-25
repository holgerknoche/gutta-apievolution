package gutta.apievolution.jmh.inprocess.objectmapping;

import org.junit.jupiter.api.Test;

import java.io.IOException;

class ObjectMappingCustomerExampleBenchmarksV6Test {
    
    /**
     * Make sure that the benchmark methods are invokable without errors.
     * 
     * @throws IOException Not expected
     */
    @Test
    void invocationTests() {
        ObjectMappingCustomerExampleBenchmarksV6 benchmarks = new ObjectMappingCustomerExampleBenchmarksV6();
        
        benchmarks.invokeFromV6Client_short();        
    }

}
