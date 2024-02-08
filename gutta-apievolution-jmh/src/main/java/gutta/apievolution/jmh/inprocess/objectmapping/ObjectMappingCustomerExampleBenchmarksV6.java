package gutta.apievolution.jmh.inprocess.objectmapping;

import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6.ConsumerApi;
import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6.Customer;
import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6.Gender;
import gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6.StreetAddress;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

public class ObjectMappingCustomerExampleBenchmarksV6 extends CustomerExampleObjectMappingBenchmarkTemplate {

    private static final Customer CUSTOMER = createCustomer();
    
    private static Customer createCustomer() {
        StreetAddress address = new StreetAddress();
        address.setStreet("Test Street");
        address.setNumber(1234);
        address.setCity("Test City");
        address.setPostalCode(5678);

        Customer customer = new Customer();
        customer.setFirstName("Test");
        customer.setLastName("Test");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(Gender.THIRD);
        customer.setPrimaryAddress(address);

        return customer;
    }
    
    private static final ConsumerApi CONSUMER_API = createApi(ConsumerApi.class, CONSUMER_API_V6);
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeFromV6Client() {
        CONSUMER_API.upsert(CUSTOMER);
    }
    
}
