package gutta.apievolution.javacodegen;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

abstract class JavaRecordLikeType extends JavaUserDefinedType {

    private final List<JavaProperty> properties = new ArrayList<>();

    private final Set<String> fieldNames = new HashSet<>();

    JavaRecordLikeType(String packageName, String name) {
        super(packageName, name);
    }

    /**
     * Returns the properties for which accessors are required in this interface.
     * @return see above
     */
    public List<JavaProperty> getProperties() {
        return this.properties;
    }

    void addField(JavaProperty field) {
        if (!this.fieldNames.contains(field.getName())) {
            this.properties.add(field);
            this.fieldNames.add(field.getName());
        }
    }

}
