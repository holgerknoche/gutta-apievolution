package gutta.apievolution.json.provider;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;
import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeInfo(use = Id.NAME)
@JsonTypeName("ProviderSuperType")
@JsonSubTypes({@Type(ProviderSubTypeA.class), @Type(ProviderSubTypeB.class)})
public class ProviderSuperType {

}
