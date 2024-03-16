package gutta.apievolution.dsl;

import java.io.InputStream;
import java.util.function.Supplier;

import static java.util.Objects.requireNonNull;

/**
 * A named input stream groups a regular {@link InputStream} with a meaningful
 * name to improve error messages. The underlying stream is closed when the
 * referencing named input stream is closed.
 */
public class NamedInputStream {

    private final String name;

    private final Supplier<InputStream> streamSupplier;

    /**
     * Creates a new input stream using the given data.
     * 
     * @param name The name associated with the input stream
     * @param streamSupplier A supplier for the underlying input stream, may not be {@code null}
     */
    public NamedInputStream(String name, Supplier<InputStream> streamSupplier) {
        this.name = name;
        this.streamSupplier = requireNonNull(streamSupplier);
    }

    /**
     * Returns the name associated with this input stream.
     * 
     * @return see above
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns the underlying input stream.
     * 
     * @return see above
     */
    public InputStream getStream() {
        return this.streamSupplier.get();
    }

}
