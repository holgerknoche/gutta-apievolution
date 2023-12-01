package gutta.apievolution.inprocess.provider;

import gutta.apievolution.inprocess.ApiName;
import gutta.apievolution.inprocess.ProviderApiProvider;
import gutta.apievolution.inprocess.UnsupportedRevisionException;

@ApiName("test.provider")
public class ApiProvider implements ProviderApiProvider {

    @Override
    public Object createApi(int revision) {
        switch (revision) {
        case 0:
            return new ProviderApiV1();
        case 1:
            return new ProviderApiV2();

        default:
            throw new UnsupportedRevisionException(revision, new int[] { 0, 1 });
        }
    }

}
