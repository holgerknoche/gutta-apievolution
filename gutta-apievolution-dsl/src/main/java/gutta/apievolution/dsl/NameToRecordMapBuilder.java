package gutta.apievolution.dsl;

import gutta.apievolution.dsl.parser.ApiRevisionBaseVisitor;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.HashMap;
import java.util.Map;

/**
 * Utility visitor to create a map from type names to record parse context. This map is useful
 * to ensure that supertypes are initialized before their subtypes.
 */
class NameToRecordMapBuilder extends ApiRevisionBaseVisitor<Void> {

    private Map<String, ApiRevisionParser.RecordTypeContext> identifierToRecord;

    public Map<String, ApiRevisionParser.RecordTypeContext> createMap(ApiRevisionParser.ApiDefinitionContext context) {

        this.identifierToRecord = new HashMap<>();
        context.accept(this);
        return this.identifierToRecord;
    }

    @Override
    public Void visitApiDefinition(ApiRevisionParser.ApiDefinitionContext context) {
        context.elements.forEach(element -> element.accept(this));
        return null;
    }

    @Override
    public Void visitRecordType(ApiRevisionParser.RecordTypeContext context) {
        this.identifierToRecord.put(context.name.getText(), context);
        return null;
    }
}
