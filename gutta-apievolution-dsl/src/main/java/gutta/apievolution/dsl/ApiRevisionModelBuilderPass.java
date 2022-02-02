package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.dsl.parser.ApiRevisionBaseVisitor;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

abstract class ApiRevisionModelBuilderPass<A extends ApiDefinition<A>, R extends RecordType<A, R, F>,
        F extends Field<R, F>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>,
        S extends Service<A, S, O, R>, O extends ServiceOperation<S, O, R>> extends ApiRevisionBaseVisitor<Void> {

    private static String unquote(final String input) {
        int endIndex = (input.length() - 2);

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

    protected Optional<String> determineInternalName(final ApiRevisionParser.AsClauseContext context) {
        if (context == null) {
            return Optional.empty();
        } else {
            return Optional.of(this.identifierAsText(context.aliasName));
        }
    }

    protected List<String> splitQualifiedName(final ApiRevisionParser.QualifiedNameContext context) {
        return context.parts.stream()
                .map(this::identifierAsText)
                .collect(Collectors.toList());
    }

    protected Set<Annotation> handleAnnotations(final List<ApiRevisionParser.AnnotationContext> annotationContexts) {
        Set<Annotation> annotations = new HashSet<>(annotationContexts.size());

        for (ApiRevisionParser.AnnotationContext annotationContext : annotationContexts) {
            String typeText = annotationContext.typeToken.getText();
            String valueText = annotationContext.value.getText();

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

    protected void registerNewService(final S service) {
        // Do nothing by default
    }

    protected void registerNewServiceOperation(final O serviceOperation) {
        // Do nothing by default
    }

}
