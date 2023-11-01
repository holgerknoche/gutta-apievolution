package gutta.apievolution.inprocess.consumer;

import java.util.List;

public interface ConsumerResult {

	String getResultField();
	
	ConsumerEnum getResultEnum();
	
	List<ConsumerEnum> getResultList();
	
	ConsumerRecord getResultRecord();
	
}
