package gutta.apievolution.inprocess;

import java.util.ServiceLoader;

/**
 * Marker interface for provider API providers to make them discoverable using a {@link ServiceLoader}.
 */
public interface ProviderApiProvider {

    /**
     * Creates an API object representing the given revision.
     * 
     * @param revision The desired revision number
     * @return The API object representing the given revision number
     * @throws UnsupportedRevisionException If the desired revision number is not supported
     */
    Object createApi(int revision);

}
