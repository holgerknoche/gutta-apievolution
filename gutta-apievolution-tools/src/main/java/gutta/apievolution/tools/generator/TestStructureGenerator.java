package gutta.apievolution.tools.generator;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

public class TestStructureGenerator {

    public static void main(String[] arguments) throws IOException {
        String mode = arguments[0];
        int numberOfElements = Integer.parseInt(arguments[1]);

        try (Writer outputWriter = new OutputStreamWriter(System.out)) {
            GeneratorMode generatorMode = createGeneratorMode(mode, outputWriter);
            generatorMode.generateCode(numberOfElements);
        }
    }

    private static GeneratorMode createGeneratorMode(String mode, Writer outputWriter) {
        String normalizedMode = mode.toLowerCase();

        switch (normalizedMode) {
        case "interface":
            return new InterfaceGeneratorMode(outputWriter);

        case "record":
            return new RecordGeneratorMode(outputWriter);

        case "spec":
            return new SpecGeneratorMode(outputWriter);

        case "init":
            return new InitializationGeneratorMode(outputWriter);
            
        case "inspection":
            return new InspectionGeneratorMode(outputWriter);

        default:
            throw new IllegalArgumentException("Unsupported mode '" + mode + "'.");
        }
    }

    private static class RecordGeneratorMode extends GeneratorMode {

        public RecordGeneratorMode(Writer outputWriter) {
            super(outputWriter);
        }

        @Override
        public void generateCode(int numberOfElements) {
            this.writeLine("public class Test" + numberOfElements + " {");
            this.writeLine("");

            // Integer fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    private Integer intField" + count + ";");
            }

            this.writeLine("");

            // String fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    private String stringField" + count + ";");
            }

            this.writeLine("");

            // Accessors for integer fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    /**");
                this.writeLine("     * Returns the value of int field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @return see above");
                this.writeLine("     */");
                this.writeLine("    public Integer getIntField" + count + "() {");
                this.writeLine("        return this.intField" + count + ";");
                this.writeLine("    }");
                this.writeLine("");
                this.writeLine("    /**");
                this.writeLine("     * Sets the value of int field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @param value The value to set");
                this.writeLine("     */");
                this.writeLine("    public void setIntField" + count + "(Integer value) {");
                this.writeLine("        this.intField" + count + " = value;");
                this.writeLine("    }");
                this.writeLine("");
            }

            // Accessors for string fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    /**");
                this.writeLine("     * Returns the value of string field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @return see above");
                this.writeLine("     */");
                this.writeLine("    public String getStringField" + count + "() {");
                this.writeLine("        return this.stringField" + count + ";");
                this.writeLine("    }");
                this.writeLine("");
                this.writeLine("    /**");
                this.writeLine("     * Sets the value of string field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @param value The value to set");
                this.writeLine("     */");
                this.writeLine("    public void setStringField" + count + "(String value) {");
                this.writeLine("        this.stringField" + count + " = value;");
                this.writeLine("    }");
                this.writeLine("");
            }

            this.writeLine("}");
        }

    }

    private static class InterfaceGeneratorMode extends GeneratorMode {

        public InterfaceGeneratorMode(Writer outputWriter) {
            super(outputWriter);
        }

        @Override
        public void generateCode(int numberOfElements) {
            this.writeLine("public interface Test" + numberOfElements + " {");
            this.writeLine("");

            // Accessors for integer fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    /**");
                this.writeLine("     * Returns the value of int field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @return see above");
                this.writeLine("     */");
                this.writeLine("    Integer getIntField" + count + "();");
                this.writeLine("");
                this.writeLine("    /**");
                this.writeLine("     * Sets the value of int field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @param value The value to set");
                this.writeLine("     */");
                this.writeLine("    void setIntField" + count + "(Integer value);");
                this.writeLine("");
            }

            // Accessors for string fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    /**");
                this.writeLine("     * Returns the value of string field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @return see above");
                this.writeLine("     */");
                this.writeLine("    String getStringField" + count + "();");
                this.writeLine("");
                this.writeLine("    /**");
                this.writeLine("     * Sets the value of string field #" + count + ".");
                this.writeLine("     *");
                this.writeLine("     * @param value The value to set");
                this.writeLine("     */");
                this.writeLine("    void setStringField" + count + "(String value);");
                this.writeLine("");
            }

            this.writeLine("}");
        }

    }

    private static class SpecGeneratorMode extends GeneratorMode {

        public SpecGeneratorMode(Writer outputWriter) {
            super(outputWriter);
        }

        @Override
        public void generateCode(int numberOfElements) {
            this.writeLine("    record Test" + numberOfElements + " {");

            // Int fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("        int32 intField" + count);
            }

            this.writeLine("");

            // String fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("        string stringField" + count);
            }

            this.writeLine("    }");
        }

    }

    private static class InitializationGeneratorMode extends GeneratorMode {

        public InitializationGeneratorMode(Writer outputWriter) {
            super(outputWriter);
        }

        @Override
        public void generateCode(int numberOfElements) {
            // Int fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    result.setIntField" + count + "(" + count + ");");
            }

            this.writeLine("");

            // String fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    result.setStringField" + count + "(\"" + count + "\");");
            }
        }

    }

    private static class InspectionGeneratorMode extends GeneratorMode {

        public InspectionGeneratorMode(Writer outputWriter) {
            super(outputWriter);
        }

        @Override
        public void generateCode(int numberOfElements) {
            // Int fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    result.getIntField" + count + "();");
            }

            this.writeLine("");

            // String fields
            for (int count = 1; count <= numberOfElements; count++) {
                this.writeLine("    result.getStringField" + count + "();");
            }
        }

    }

}
