package gutta.apievolution.core.apimodel;

public interface UserDefinedType<A extends ApiDefinition<A, ?>> extends Type {

    int getTypeId();
    
    String getPublicName();
    
    String getInternalName();

    A getOwner();

}
