package gutta.apievolution.fixedformat.consumer;

import gutta.apievolution.fixedformat.objectmapping.SubTypes;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(4)
@SubTypes({ConsumerSubTypeA.class, ConsumerSubTypeB.class})
public class ConsumerSuperType {

}
