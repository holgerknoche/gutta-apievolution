package gutta.apievolution.javacodegen;

import java.util.List;

/**
 * Representation of a Java service operation.
 */
public class JavaServiceOperation {

    private final String name;

    private final JavaInterface resultType;

    private final JavaInterface parameterType;

    private final List<JavaException> thrownExceptions;

    JavaServiceOperation(String name, JavaInterface resultType, JavaInterface parameterType,
                         List<JavaException> thrownExceptions) {
        this.name = name;
        this.resultType = resultType;
        this.parameterType = parameterType;
        this.thrownExceptions = thrownExceptions;
    }

    /**
     * Returns this operation's name.
     * @return see above
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns this operation's result type.
     * @return see above
     */
    public JavaInterface getResultType() {
        return this.resultType;
    }

    /**
     * Returns this operation's parameter type.
     * @return see above
     */
    public JavaInterface getParameterType() {
        return this.parameterType;
    }

    /**
     * Returns the exceptions thrown by this operation.
     * @return see above
     */
    public List<JavaException> getThrownExceptions() {
        return this.thrownExceptions;
    }

}
