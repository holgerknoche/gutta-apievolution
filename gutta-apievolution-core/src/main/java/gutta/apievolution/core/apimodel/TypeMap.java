package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.util.MapUtil;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This class represents a type mapping between two API definitions. Effectively, it extends a given
 * mapping on user-defined types to a full type mapping, i.e., it adds mappings for the base types and
 * list types derived from the user-defined types.
 */
public class TypeMap<S extends UserDefinedType<?>, T extends UserDefinedType<?>> {

    private final Map<S, T> udtMap;
    
    /**
     * Creates a new type mapping based on the given mapping on user-defined types.
     * @param udtMap The underlying mapping on user-defined types
     */
    public TypeMap(Map<S, T> udtMap) {
        this.udtMap = udtMap;
    }
    
    /**
     * Returns the source types of the underlying UDT map.
     * @return see above
     */
    public Set<S> sourceTypes() {
        return this.udtMap.keySet();
    }
    
    /**
     * Returns the entries of the underlying UDT map as a stream.
     * @return see above
     */
    public Stream<Entry<S, T>> entryStream() {
        return this.udtMap.entrySet().stream();
    }
    
    /**
     * Looks up the equivalent of the given type in this type lookup.
     *
     * @param <X>    The desired metatype of the result
     * @param inType The type to determine the equivalent for
     * @return see above
     */
    @SuppressWarnings("unchecked")
    public <X extends Type> X mapType(Type inType) {
        if (this.isUserDefinedType(inType)) {
            // noinspection SuspiciousMethodCalls
            return (X) this.mapUserDefinedType((S) inType);
        } else if (inType instanceof ListType) {
            return (X) this.mapListType((ListType) inType);
        } else {
            return (X) inType;
        }
    }
    
    /**
     * Returns whether the given type is a user-defined type.
     *
     * @param type The type to check
     * @return see above
     */
    protected final boolean isUserDefinedType(Type type) {
        return (type instanceof UserDefinedType);
    }

    /**
     * Maps the given user-defined type to its match, if it exists.
     *
     * @param type The user-defined type to map
     * @return The matching type or {@code null}
     */
    T mapUserDefinedType(S type) {
        return this.udtMap.get(type);
    }
    
    /**
     * Performs the given action for each key-value pair in the underlying UDT map.
     * @param action The action to perform
     */
    public void forEach(BiConsumer<? super S, ? super T> action) {
        this.udtMap.forEach(action);
    }
        
    private ListType mapListType(ListType inType) {
        if (inType.isBounded()) {
            return ListType.bounded(this.mapType(inType.getElementType()), inType.getBound());
        } else {
            return ListType.unbounded(this.mapType(inType.getElementType()));
        }
    }
    
    /**
     * Returns a restriction of this type lookup to the given provider API
     * definition.
     *
     * @param definition The definition to restrict the type lookup to
     * @return The restricted type lookup
     */
    @SuppressWarnings("unchecked")
    public TypeMap<S, T> restrictTo(ApiDefinition<?, ?> definition) {
        Map<S, T> restrictedUdtLookup = this.entryStream().filter(
                entry -> ((UserDefinedType<ProviderApiDefinition>) entry.getKey()).getOwner().equals(definition))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        return new TypeMap<>(restrictedUdtLookup);
    }
    
    /**
     * Composes this type map with the given one and returns the composed type map.
     * @param <X> The target type of user-defined type
     * @param udtMappingFunction The type map to compose this type map with
     * @return The composed type map
     */
    public <X extends UserDefinedType<?>> TypeMap<S, X> compose(Function<T, X> udtMappingFunction) {
        Map<S, X> composedUdtMap = this.udtMap.entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, entry -> udtMappingFunction.apply(entry.getValue())));

        return new TypeMap<>(composedUdtMap);
    }
    
    /**
     * Inverts this type map.
     * @param onConflict The action to perform on conflicts, i.e., when in inverse is not a function
     * @return The inverted type map
     */
    public TypeMap<T, S> invert(Consumer<T> onConflict) {
        Map<T, S> invertedUdtMap = MapUtil.invertMap(this.udtMap, onConflict);
        return new TypeMap<>(invertedUdtMap);
    }

}
