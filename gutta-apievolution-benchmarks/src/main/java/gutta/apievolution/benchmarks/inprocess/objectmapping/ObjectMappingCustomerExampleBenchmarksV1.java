package gutta.apievolution.benchmarks.inprocess.objectmapping;

import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v1.Address;
import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v1.ConsumerApi;
import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v1.Customer;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class ObjectMappingCustomerExampleBenchmarksV1 extends CustomerExampleObjectMappingBenchmarkTemplate {

    private static final Customer CUSTOMER = createCustomer();
    
    private static Customer createCustomer() {
        Address address = new Address();
        address.setStreet("Test Street");
        address.setNumber(1234);
        address.setCity("Test City");
        address.setPostalCode(5678);

        Customer customer = new Customer();
        customer.setFirstName("Test");
        customer.setLastName("Test");
        customer.setGender(0);
        customer.setAddress(address);

        return customer;
    }
    
    private static final ConsumerApi CONSUMER_API = createApi(ConsumerApi.class, CONSUMER_API_V1);
    
    @Benchmark
    public void invokeFromV1Client_short() {
        CONSUMER_API.upsert(CUSTOMER);
    }
    
}
