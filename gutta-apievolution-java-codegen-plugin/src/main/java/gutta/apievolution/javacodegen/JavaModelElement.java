package gutta.apievolution.javacodegen;

abstract class JavaModelElement {

    public final String packageName;

    public final String name;

    JavaModelElement(String packageName, String name) {
        this.packageName = packageName;
        this.name = name;
    }

    /**
     * Returns this type's package name.
     * 
     * @return see above
     */
    public String getPackageName() {
        return this.packageName;
    }

    /**
     * Returns this type's name.
     * 
     * @return see above
     */
    public String getName() {
        return this.name;
    }

}
