package gutta.apievolution.benchmarks.inprocess.objectmapping;

import gutta.apievolution.benchmarks.inprocess.objectmapping.ObjectMappingCustomerExampleBenchmarksV1;
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
        
        benchmarks.invokeFromV1Client_short();        
    }

}
