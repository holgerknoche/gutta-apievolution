package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.MapBackedTypeLookup;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;

import java.util.Map;
import java.util.stream.Collectors;

class ProviderTypeLookup extends MapBackedTypeLookup<ProviderUserDefinedType, ProviderUserDefinedType> {

    ProviderTypeLookup(Map<ProviderUserDefinedType, ProviderUserDefinedType> udtLookup) {
        super(udtLookup);
    }

    @Override
    protected boolean isUserDefinedType(Type type) {
        return (type instanceof ProviderUserDefinedType);
    }

    /**
     * Returns a restriction of this type lookup to the given provider API definition.
     * @param definition The definition to restrict the type lookup to
     * @return The restricted type lookup
     */
    @SuppressWarnings("unchecked")
    public ProviderTypeLookup restrictTo(ProviderApiDefinition definition) {
        Map<ProviderUserDefinedType, ProviderUserDefinedType> restrictedUdtLookup = this.udtLookupStream()
                .filter(entry ->
                        ((UserDefinedType<ProviderApiDefinition>) entry.getKey()).getOwner().equals(definition))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        return new ProviderTypeLookup(restrictedUdtLookup);
    }

}
