package gutta.apievolution.core.apimodel;

import java.util.Collections;
import java.util.List;
import java.util.Set;

public class Conventions {
    
    public static String noInternalName() {
        return null;
    }
    
    public static Set<Annotation> noAnnotations() {
        return Collections.emptySet();
    }
    
    public static <T> T noPredecessor() {
        return null;
    }
    
    public static <R extends RecordType<?, R ,?>> Set<R> noSuperTypes() {
        return Collections.emptySet();
    }
    
    public static <F extends Field<?, F>> List<F> noDeclaredPredecessors() {
        return Collections.emptyList();
    }

}
