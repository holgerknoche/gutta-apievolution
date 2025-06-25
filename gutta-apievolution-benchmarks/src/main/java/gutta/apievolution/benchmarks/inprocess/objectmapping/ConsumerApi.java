package gutta.apievolution.benchmarks.inprocess.objectmapping;

public interface ConsumerApi {
    
    EmptyConsumerResult testMethodEmpty(ConsumerParameter parameter);
    
    ConsumerResult10 testMethod10(ConsumerParameter parameter);
    
    ConsumerResult25 testMethod25(ConsumerParameter parameter);
    
    ConsumerResult50 testMethod50(ConsumerParameter parameter);
    
    ConsumerResult75 testMethod75(ConsumerParameter parameter);
    
    ConsumerResult100 testMethod100(ConsumerParameter parameter);
    
    ConsumerResult250 testMethod250(ConsumerParameter parameter);
    
    ConsumerResult500 testMethod500(ConsumerParameter parameter);

}
