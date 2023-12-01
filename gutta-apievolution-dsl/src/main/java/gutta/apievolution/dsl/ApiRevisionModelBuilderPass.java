package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Operation;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.dsl.parser.ApiRevisionBaseVisitor;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

abstract class ApiRevisionModelBuilderPass<A extends ApiDefinition<A, O>, R extends RecordType<A, R, F>,
        F extends Field<R, F>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>, O extends Operation<A, O, R>>
        extends ApiRevisionBaseVisitor<Void> {

    protected final String sourceName;
    
    protected ApiRevisionModelBuilderPass(String sourceName) {
        this.sourceName = sourceName;
    }
    
    private static String unquote(final String input) {
        int endIndex = (input.length() - 1);

        return input.substring(1, endIndex);
    }

    protected String identifierAsText(final ApiRevisionParser.IdentifierContext context) {
        if (context.id != null) {
            return context.id.getText();
        } else {
            return unquote(context.literal.getText());
        }
    }

    protected Optional<String> optionalIdentifierAsText(final ApiRevisionParser.IdentifierContext context) {
        if (context == null) {
            return Optional.empty();
        } else {
            return Optional.of(this.identifierAsText(context));
        }
    }

    protected String determineInternalName(final ApiRevisionParser.AsClauseContext context) {
        if (context == null) {
            return null;
        } else {
            return this.identifierAsText(context.aliasName);
        }
    }

    protected List<String> splitQualifiedName(final ApiRevisionParser.QualifiedNameContext context) {
        return context.parts.stream().map(this::identifierAsText).collect(Collectors.toList());
    }

    protected Set<Annotation> handleAnnotations(final List<ApiRevisionParser.AnnotationContext> annotationContexts) {
        Set<Annotation> annotations = new HashSet<>(annotationContexts.size());

        for (ApiRevisionParser.AnnotationContext annotationContext : annotationContexts) {
            String typeText = annotationContext.typeToken.getText();
            String valueText = unquote(annotationContext.value.getText());

            // Remove leading '@'
            String typeName = typeText.substring(1);
            Annotation annotation = new Annotation(typeName, valueText);
            annotations.add(annotation);
        }

        return annotations;
    }

    protected void registerNewRecordType(final R recordType) {
        // Do nothing by default
    }

    protected void registerNewField(final F field) {
        // Do nothing by default
    }

    protected void registerNewEnumType(final E enumType) {
        // Do nothing by default
    }

    protected void registerNewEnumMember(final M enumMember) {
        // Do nothing by default
    }

    protected void registerNewOperation(final O operation) {
        // Do nothing by default
    }

}
