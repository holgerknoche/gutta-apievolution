package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Type;

import java.util.Optional;

/**
 * Provider-specific type tools, such as detecting type changes in revisions.
 */
public class ProviderTypeTools {

    /**
     * Determines whether the types of two successive fields represent a type change.
     * @param predecessorType The type of the predecessor field
     * @param successorType The type of the successor field
     * @return {@code True}, if the types are considered different, {@code false} otherwise
     */
    public static boolean isTypeChange(Type predecessorType, Type successorType) {
        if (successorType instanceof RevisionedElement) {
            // If the current type is revisioned, we must see if there is a predecessor of the successor type
            // matching the current type
            Optional<?> matchingPredecessor = ((RevisionedElement<?>) successorType).findFirstPredecessorMatching(
                    type -> type.equals(predecessorType)
            );

            // If no matching predecessor (or none at all) is present, we have a type change
            return !(matchingPredecessor.isPresent());
        } else {
            // Otherwise, the types can be compared immediately
            return !(successorType.equals(predecessorType));
        }
    }

}
