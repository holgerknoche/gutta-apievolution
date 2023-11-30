package gutta.apievolution.inprocess;

import java.util.ServiceLoader;

/**
 * Marker interface for provider API providers to make them discoverable using a {@link ServiceLoader}.
 */
public interface ProviderApiProvider {

    Object createApi(int revision);

}
