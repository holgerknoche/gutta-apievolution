package gutta.apievolution.javacodegen;

import java.util.List;

/**
 * Representation of a Java service.
 */
public class JavaService extends JavaModelElement {

    private List<JavaServiceOperation> operations;

    JavaService(String packageName, String name, List<JavaServiceOperation> operations) {
        super(packageName, name);
        this.operations = operations;
    }

    /**
     * Returns this service's operations.
     * @return see above
     */
    public List<JavaServiceOperation> getOperations() {
        return this.operations;
    }

}
