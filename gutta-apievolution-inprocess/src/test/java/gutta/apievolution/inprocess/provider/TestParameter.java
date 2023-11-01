package gutta.apievolution.inprocess.provider;

import java.util.List;

public interface TestParameter {

	String getFieldA();
	
	String getField2();
	
	ProviderEnum getTestEnum();
	
	List<ProviderEnum> getTestList();
	
	ProviderRecord getTestRecord();
	
}
