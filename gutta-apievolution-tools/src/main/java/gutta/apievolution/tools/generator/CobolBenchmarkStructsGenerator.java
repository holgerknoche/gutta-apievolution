package gutta.apievolution.tools.generator;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

public class CobolBenchmarkStructsGenerator {
	
	public static void main(String[] arguments) throws IOException {
		String mode = arguments[0];
		int numberOfElements = Integer.parseInt(arguments[1]);
		String outputFileName = arguments[2];
		
        try (Writer outputWriter = new FileWriter(outputFileName)) {
            GeneratorMode generatorMode = createGeneratorMode(mode, outputWriter);
            generatorMode.generateCode(numberOfElements);
        }
	}
	
    private static GeneratorMode createGeneratorMode(String mode, Writer outputWriter) {
        String normalizedMode = mode.toLowerCase();

        switch (normalizedMode) {
        case "structure":
        	return new StructureGeneratorMode(outputWriter);
        	
        case "initializer":
        	return new InitializationGeneratorMode(outputWriter);
        
        default:
            throw new IllegalArgumentException("Unsupported mode '" + mode + "'.");
        }
    }
    
    private static class StructureGeneratorMode extends GeneratorMode {
    	
    	public StructureGeneratorMode(Writer outputWriter) {
    		super(outputWriter);
    	}

		@Override
		public void generateCode(int numberOfElements) {
			this.writeLine("      * Test structure with " + numberOfElements + " elements"); 
			this.writeLine("           05 '*-'TEST-STRUCT-" + numberOfElements + ".");
			this.writeLine("             10 '*-'TEST-STRUCT-" + numberOfElements + "-FLAGS PIC 9 BINARY.");
			this.writeLine("                88 VALUE-ABSENT VALUE 0.");
			this.writeLine("                88 VALUE-PRESENT VALUE 1.");
			this.writeLine("                88 VALUE-UNREPRESENTABLE VALUE 2.");
			
			this.writeLine("");
			
			for (int elementIndex = 1; elementIndex <= numberOfElements; elementIndex++) {
				this.writeLine("             10 '*-'INT-FIELD-" + elementIndex + "-FLAGS PIC 9 BINARY.");
				this.writeLine("               88 VALUE-ABSENT VALUE 0.");
				this.writeLine("               88 VALUE-PRESENT VALUE 1.");
				this.writeLine("               88 VALUE-UNREPRESENTABLE VALUE 2.");
				this.writeLine("             10 '*-'INT-FIELD-" + elementIndex + " PIC S9(9) BINARY.");
				this.writeLine("");
			}
			
			for (int elementIndex = 1; elementIndex <= numberOfElements; elementIndex++) {
				this.writeLine("             10 '*-'STRING-FIELD-" + elementIndex + "-FLAGS PIC 9 BINARY.");
				this.writeLine("               88 VALUE-ABSENT VALUE 0.");
				this.writeLine("               88 VALUE-PRESENT VALUE 1.");
				this.writeLine("               88 VALUE-UNREPRESENTABLE VALUE 2.");
				this.writeLine("             10 '*-'STRING-FIELD-" + elementIndex + " PIC X(10).");
				this.writeLine("");
			}
		}    	
    }
    
    private static class InitializationGeneratorMode extends GeneratorMode {

		public InitializationGeneratorMode(Writer outputWriter) {
			super(outputWriter);
		}

		@Override
		public void generateCode(int numberOfElements) {
			final String structPrefix = "P" + numberOfElements; 
			
			this.writeLine("           SET VALUE-PRESENT IN " + structPrefix + "-TEST-STRUCT-" + numberOfElements + "-FLAGS");
			this.writeLine("            TO TRUE");
			
			this.writeLine("");
			
			for (int elementIndex = 1; elementIndex <= numberOfElements; elementIndex++) {
				final String currentFieldName = structPrefix + "-INT-FIELD-" + elementIndex;
				
				this.writeLine("           SET VALUE-PRESENT IN " + currentFieldName + "-FLAGS");
				this.writeLine("            TO TRUE");
				this.writeLine("           MOVE " + elementIndex);
				this.writeLine("             TO " + currentFieldName);
			}
			
			this.writeLine("");
			
			for (int elementIndex = 1; elementIndex <= numberOfElements; elementIndex++) {
				final String currentFieldName = structPrefix + "-STRING-FIELD-" + elementIndex;
				
				this.writeLine("           SET VALUE-PRESENT IN " + currentFieldName + "-FLAGS");
				this.writeLine("            TO TRUE");
				this.writeLine("           MOVE '" + elementIndex + "'");
				this.writeLine("             TO " + currentFieldName);
			}
		}    	
    }
	
}
