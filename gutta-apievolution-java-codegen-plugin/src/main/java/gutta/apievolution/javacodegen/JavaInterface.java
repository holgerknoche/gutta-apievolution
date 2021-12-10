package gutta.apievolution.javacodegen;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class JavaInterface extends JavaUserDefinedType {

    public final List<JavaField> fields = new ArrayList<>();

    private final Set<String> fieldNames = new HashSet<>();

    JavaInterface(String packageName, String name) {
        super(packageName, name);
    }

    public List<JavaField> getFields() {
        return this.fields;
    }

    void addField(JavaField field) {
        if (!this.fieldNames.contains(field.name)) {
            this.fields.add(field);
            this.fieldNames.add(field.name);
        }
    }

}
