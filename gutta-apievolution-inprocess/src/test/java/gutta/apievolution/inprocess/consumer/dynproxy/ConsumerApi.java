package gutta.apievolution.inprocess.consumer.dynproxy;

public interface ConsumerApi {
	
	ConsumerResult testOperation(ConsumerParameter parameter);
	
	ConsumerResult operationWithMappedException(ConsumerParameter parameter);

}
