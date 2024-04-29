package gutta.apievolution.tools.generator;

import java.io.IOException;
import java.io.Writer;

abstract class GeneratorMode {
	
    protected final Writer outputWriter;

    protected GeneratorMode(Writer outputWriter) {
        this.outputWriter = outputWriter;
    }

    protected void write(String output) {
        try {
            this.outputWriter.write(output);
        } catch (IOException e) {
            throw new RuntimeException("Error writing output line.", e);
        }
    }

    protected void writeLine(String line) {
        this.write(line + "\n");
    }

    public abstract void generateCode(int numberOfElements);

}
