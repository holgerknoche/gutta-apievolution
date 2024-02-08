package gutta.apievolution.customerexample.inprocess.provider;

import gutta.apievolution.inprocess.ApiName;
import gutta.apievolution.inprocess.ProviderApiProvider;
import gutta.apievolution.inprocess.UnsupportedRevisionException;

@ApiName("customer.provider")
public class ApiProvider implements ProviderApiProvider {

	@Override
	public Object createApi(int revision) {
		switch (revision) {
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
			return new ProviderApi();
			
		default:
			throw new UnsupportedRevisionException(revision, new int[] {0, 1, 2, 3, 4, 5});
		}
	}

}
