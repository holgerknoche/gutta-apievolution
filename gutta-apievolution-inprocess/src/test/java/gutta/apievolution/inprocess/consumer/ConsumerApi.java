package gutta.apievolution.inprocess.consumer;

public interface ConsumerApi {
	
	ConsumerResult testOperation(ConsumerParameter parameter);
	
	ConsumerResult operationWithMappedException(ConsumerParameter parameter);

}
