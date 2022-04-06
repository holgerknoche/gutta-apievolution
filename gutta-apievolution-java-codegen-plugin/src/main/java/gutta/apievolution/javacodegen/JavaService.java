package gutta.apievolution.javacodegen;

import java.util.ArrayList;
import java.util.List;

/**
 * Representation of a Java service.
 */
public class JavaService
        extends JavaModelElement {

    private final List<JavaServiceOperation> operations;

    JavaService(String packageName, String name) {
        super(packageName, name);
        this.operations = new ArrayList<>();
    }

    void addOperation(JavaServiceOperation operation) {
        this.operations.add(operation);
    }

    /**
     * Returns this service's operations.
     * 
     * @return see above
     */
    public List<JavaServiceOperation> getOperations() {
        return this.operations;
    }

}
