package gutta.apievolution.core.apimodel;

import java.util.HashSet;
import java.util.Set;

/**
 * Helper class to collect the user-defined types reachable from a given type.
 * 
 * @param <A> The concrete API definition type that is used
 */
class ReachableTypesCollector<A extends ApiDefinition<A, ?>> implements TypeVisitor<Void> {

    private Inclusive inclusive;
    
    private Set<UserDefinedType<A>> targetSet;
            
    public void collectReachableTypesOf(UserDefinedType<A> type, Inclusive inclusive, Set<UserDefinedType<A>> targetSet) {
        this.targetSet = targetSet;
        this.inclusive = inclusive;
        
        this.recurseOn(type);
    }
    
    private void recurseOn(Type type) {
        if (this.targetSet.contains(type)) {
            // Return immediately for types that have already been collected
            return;
        }
        
        type.accept(this);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public Void handleEnumType(EnumType<?, ?, ?> enumType) {
        if (this.inclusive == Inclusive.YES) {
            this.targetSet.add((UserDefinedType<A>) enumType);
        }
        
        return null;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public Void handleRecordType(RecordType<?, ?, ?> recordType) {                
        if (this.inclusive == Inclusive.YES) {
            this.targetSet.add((UserDefinedType<A>) recordType);
        }
        
        // Add all types reachable from the fields of this type 
        this.addTypesReachableFromFields(recordType);
        
        // Add all types reachable from supertypes or subtypes (transitively)
        this.addTypesReachableFromSupertypes(recordType, false);
        this.addTypesReachableFromSubtypes(recordType, false);
        
        return null;
    }
    
    private void addTypesReachableFromFields(RecordType<?, ?, ?> recordType) {
        for (Field<?, ?> field : recordType.getDeclaredFields()) {                        
            this.recurseOn(field.getType());
        }
    }
    
    @SuppressWarnings("unchecked")
    private void addTypesReachableFromSupertypes(RecordType<?, ?, ?> recordType, boolean inclusive) {
        if (inclusive) {
            this.targetSet.add((UserDefinedType<A>) recordType);
            this.addTypesReachableFromFields(recordType);
        }
        
        for (RecordType<?, ?, ?> superType : recordType.getSuperTypes()) {
            this.addTypesReachableFromSupertypes(superType, true);
        }
    }
    
    @SuppressWarnings("unchecked")
    private void addTypesReachableFromSubtypes(RecordType<?, ?, ?> recordType, boolean inclusive) {
        if (inclusive) {
            this.targetSet.add((UserDefinedType<A>) recordType);
            this.addTypesReachableFromFields(recordType);
        }
        
        for (RecordType<?, ?, ?> subType : recordType.getSubTypes()) {
            this.addTypesReachableFromSubtypes(subType, true);
        }
    }
    
    private Void addTypesReachableFromListType(ListType listType) {
        this.recurseOn(listType.getElementType());
        return null;
    }
    
    @Override
    public Void handleBoundedListType(BoundedListType boundedListType) {
        return this.addTypesReachableFromListType(boundedListType);
    }
    
    @Override
    public Void handleUnboundedListType(UnboundedListType unboundedListType) {
        return this.addTypesReachableFromListType(unboundedListType);
    }

}
