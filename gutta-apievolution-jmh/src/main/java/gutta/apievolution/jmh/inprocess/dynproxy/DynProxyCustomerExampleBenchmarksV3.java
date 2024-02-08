package gutta.apievolution.jmh.inprocess.dynproxy;

import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3.Address;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3.AddressImpl;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3.ConsumerApi;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3.Customer;
import gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v3.CustomerImpl;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;

import java.util.concurrent.TimeUnit;

public class DynProxyCustomerExampleBenchmarksV3 extends CustomerExampleDynProxyBenchmarkTemplate {

    private static final Customer CUSTOMER = createCustomer();
    
    private static Customer createCustomer() {
        Address address = new AddressImpl();
        address.setStreet("Test Street");
        address.setNumber(1234);
        address.setCity("Test City");
        address.setPostalCode(5678);

        Customer customer = new CustomerImpl();
        customer.setFirstName("Test");
        customer.setLastName("Test");
        customer.setDateOfBirth("2000-01-01");
        customer.setGender(0);
        customer.setPrimaryAddress(address);

        return customer;
    }
    
    private static final ConsumerApi CONSUMER_API = createApi(ConsumerApi.class, CONSUMER_API_V3);
    
    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    public void invokeFromV3Client() {
        CONSUMER_API.upsert(CUSTOMER);
    }
    
}
