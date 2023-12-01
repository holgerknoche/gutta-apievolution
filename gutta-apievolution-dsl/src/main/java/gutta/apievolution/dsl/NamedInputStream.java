package gutta.apievolution.dsl;

import java.io.IOException;
import java.io.InputStream;

import static java.util.Objects.*;

/**
 * A named input stream groups a regular {@link InputStream} with a meaningful
 * name to improve error messages. The underlying stream is closed when the
 * referencing named input stream is closed.
 */
public class NamedInputStream implements AutoCloseable {

    private final String name;

    private final InputStream stream;

    /**
     * Creates a new input stream using the given data.
     * 
     * @param name The name associated with the input stream
     * @param stream The underlying input stream, may not be {@code null}
     */
    public NamedInputStream(String name, InputStream stream) {
        this.name = name;
        this.stream = requireNonNull(stream);
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
        return this.stream;
    }

    @Override
    public void close() throws IOException {
        this.stream.close();
    }

}
