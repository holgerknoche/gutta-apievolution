package gutta.apievolution.inprocess.provider;

import java.util.List;

public interface TestParameter {

	String getField1();
	
	String getField2();
	
	ProviderEnum getTestEnum();
	
	List<ProviderEnum> getTestList();
	
}
