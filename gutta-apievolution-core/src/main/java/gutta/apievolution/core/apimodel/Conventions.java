package gutta.apievolution.core.apimodel;

import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * This class encodes certain conventions for representing particular
 * situations.
 */
public class Conventions {

    /**
     * Represents the fact that there is no internal name.
     * 
     * @return see above
     */
    public static String noInternalName() {
        return null;
    }

    /**
     * Represents the fact that there are not annotations.
     * 
     * @return see above
     */
    public static Set<Annotation> noAnnotations() {
        return Collections.emptySet();
    }

    /**
     * Represents the fact that there is no predecessor.
     * 
     * @param <T> The type of the predecessor
     * @return see above
     */
    public static <T> T noPredecessor() {
        return null;
    }

    /**
     * Represents the fact that there are no super types.
     * 
     * @param <R> The type of record type
     * @return see above
     */
    public static <R extends RecordType<?, R, ?>> Set<R> noSuperTypes() {
        return Collections.emptySet();
    }

    /**
     * Represents the fact that there are no declared predecessors.
     * 
     * @param <F> The type of field
     * @return see above
     */
    public static <F extends Field<?, F>> List<F> noDeclaredPredecessors() {
        return Collections.emptyList();
    }

}
