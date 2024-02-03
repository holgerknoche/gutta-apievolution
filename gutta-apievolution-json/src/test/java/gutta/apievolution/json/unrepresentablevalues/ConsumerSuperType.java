package gutta.apievolution.json.unrepresentablevalues;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;
import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeInfo(use = Id.NAME)
@JsonTypeName("SuperType")
@JsonSubTypes(@Type(ConsumerSubTypeA.class))
public class ConsumerSuperType {

}
