package gutta.apievolution.inprocess;

public interface TypeMappingStrategy {
    
    ValueMapper mapperFor(Class<?> type);       

}
