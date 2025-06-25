package gutta.apievolution.benchmarks.inprocess.provider;

import gutta.apievolution.inprocess.ApiName;
import gutta.apievolution.inprocess.ProviderApiProvider;
import gutta.apievolution.inprocess.UnsupportedRevisionException;

@ApiName("test.provider")
public class ApiProvider implements ProviderApiProvider {

    private static final int[] SUPPORTED_REVISIONS = new int[] { 0 };

    @Override
    public Object createApi(int revision) {
        if (revision == 0) {
            return new ProviderApiV1();
        } else {
            throw new UnsupportedRevisionException(revision, SUPPORTED_REVISIONS);
        }
    }

}
