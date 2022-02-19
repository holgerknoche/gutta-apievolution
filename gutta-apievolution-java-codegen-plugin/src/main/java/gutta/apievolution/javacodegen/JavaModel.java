package gutta.apievolution.javacodegen;

import java.util.Collection;
import java.util.List;

class JavaModel {

    public final Collection<JavaUserDefinedType> userDefinedTypes;

    public final List<JavaService> services;

    JavaModel(Collection<JavaUserDefinedType> userDefinedTypes, List<JavaService> services) {
        this.userDefinedTypes = userDefinedTypes;
        this.services = services;
    }

}
