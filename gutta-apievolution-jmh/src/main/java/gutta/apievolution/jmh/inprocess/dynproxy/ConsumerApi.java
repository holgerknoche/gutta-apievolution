package gutta.apievolution.jmh.inprocess.dynproxy;

public interface ConsumerApi {
    
    ConsumerResult10 testMethod10(ConsumerParameter parameter);
    
    ConsumerResult100 testMethod100(ConsumerParameter parameter);
    
    ConsumerResult500 testMethod500(ConsumerParameter parameter);

}
