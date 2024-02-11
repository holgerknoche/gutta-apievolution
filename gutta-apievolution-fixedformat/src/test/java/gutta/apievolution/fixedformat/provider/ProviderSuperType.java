package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.SubTypes;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(4)
@SubTypes({ProviderSubTypeA.class, ProviderSubTypeB.class})
public class ProviderSuperType {

}
