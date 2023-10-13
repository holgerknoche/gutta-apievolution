package gutta.apievolution.inprocess.consumer;

import java.util.List;

public interface ConsumerResult {

	String getRetField();
	
	ConsumerEnum getResultEnum();
	
	List<ConsumerEnum> getResultList();
	
}
