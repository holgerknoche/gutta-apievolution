package gutta.apievolution.fixedformat.apimapping;

public class ApiMappingScriptPrinter {
    
    public String printMappingScript(ApiMappingScript script) {
        StringBuilder scriptBuilder = new StringBuilder();
        
        int index = 0;
        Level1Printer printer = new Level1Printer(scriptBuilder);
        
        for (UserDefinedTypeMappingOperation operation : script) {
            scriptBuilder.append("entry ");
            scriptBuilder.append(index);
            scriptBuilder.append("\n");
            
            operation.accept(printer);
            index++;
        }
        
        return scriptBuilder.toString();
    }

    private static class Level1Printer implements ApiMappingOperationVisitor<Void> {
        
        private final StringBuilder scriptBuilder;
        
        private final Level2Printer subPrinter;
        
        public Level1Printer(StringBuilder scriptBuilder) {
            this.scriptBuilder = scriptBuilder;
            this.subPrinter = new Level2Printer(scriptBuilder);
        }
        
    }
    
    private static class Level2Printer implements ApiMappingOperationVisitor<Void> {
        
        private final StringBuilder scriptBuilder;
        
        public Level2Printer(StringBuilder scriptBuilder) {
            this.scriptBuilder = scriptBuilder;
        }
        
    }
    
}
