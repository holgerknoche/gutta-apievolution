package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Annotation;

import java.util.Iterator;

/**
 * Helper class to create a textual representation of an API definition. This is used to facilitate tests.
 */
public class ProviderApiDefinitionPrinter implements ProviderApiDefinitionElementVisitor<Void> {

    private StringBuilder outputBuilder;

    /**
     * Prints a given API definition into a string.
     * @param definition The definition to print
     * @return The string representation of the definition
     */
    public String printApiDefinition(ProviderApiDefinition definition) {
        StringBuilder builder = new StringBuilder();
        this.outputBuilder = builder;

        builder.append("api " + definition.getName().toString() + " ");
        this.printAnnotations(definition);
        builder.append(" {\n");

        definition.forEach(element -> element.accept(this));
        builder.append("}\n");

        return builder.toString();
    }

    private void printAnnotations(ProviderApiDefinition definition) {
        StringBuilder builder = this.outputBuilder;
        builder.append("[");

        Iterator<Annotation> annotations = definition.getAnnotations().iterator();
        while (annotations.hasNext()) {
            Annotation annotation = annotations.next();
            builder.append("@");
            builder.append(annotation.getName());
            builder.append("(");
            builder.append(annotation.getValue());
            builder.append(")");

            if (annotations.hasNext()) {
                builder.append(", ");
            }
        }

        builder.append("]");
    }

    @Override
    public Void handleProviderRecordType(ProviderRecordType recordType) {
        StringBuilder builder = this.outputBuilder;

        builder.append(" ");

        if (recordType.isAbstract()) {
            builder.append("abstract ");
        }

        builder.append("record ");
        builder.append(recordType.getPublicName());
        builder.append("(");
        builder.append(recordType.getInternalName());
        builder.append(")");

        recordType.getSuperType().ifPresent(type -> {
            builder.append(" extends ");
            builder.append(type.getPublicName());
        });

        recordType.getPredecessor().ifPresent(pred -> {
            builder.append(" <- ");
            builder.append(pred.getPublicName());
        });

        builder.append(" {\n");
        recordType.forEach(field -> field.accept(this));
        builder.append(" }\n");

        return null;
    }

    @Override
    public Void handleProviderField(ProviderField field) {
        StringBuilder builder = this.outputBuilder;

        builder.append("  ");

        if (field.isInherited()) {
            builder.append("inherited ");
        }

        builder.append(field.getOptionality());
        builder.append(" ");
        builder.append(field.getPublicName());
        builder.append("(");
        builder.append(field.getInternalName());
        builder.append(")");
        builder.append(":");
        builder.append(field.getType());

        field.getPredecessor().ifPresent(predecessor -> {
            builder.append(" <- ");
            builder.append(predecessor.getPublicName());
        });

        builder.append("\n");

        return null;
    }

    @Override
    public Void handleProviderEnumType(ProviderEnumType enumType) {
        StringBuilder builder = this.outputBuilder;

        builder.append(" enum ");
        builder.append(enumType.getPublicName());
        builder.append("(");
        builder.append(enumType.getInternalName());
        builder.append(")");

        enumType.getPredecessor().ifPresent(pred -> {
            builder.append(" <- ");
            builder.append(pred.getPublicName());
        });

        builder.append(" {\n");
        enumType.forEach(member -> member.accept(this));
        builder.append(" }\n");

        return null;
    }

    @Override
    public Void handleProviderEnumMember(ProviderEnumMember enumMember) {
        StringBuilder builder = this.outputBuilder;

        builder.append("  ");
        builder.append(enumMember.getPublicName());
        builder.append("(");
        builder.append(enumMember.getInternalName());
        builder.append(")\n");

        return null;
    }

    @Override
    public Void handleProviderService(ProviderService service) {
        StringBuilder builder = this.outputBuilder;

        builder.append(" service ");
        builder.append(service.getPublicName());
        builder.append("(");
        builder.append(service.getInternalName());
        builder.append(") {\n");

        service.forEach(operation -> operation.accept(this));

        builder.append(" }\n");
        return null;
    }

    @Override
    public Void handleProviderServiceOperation(ProviderServiceOperation serviceOperation) {
        StringBuilder builder = this.outputBuilder;

        builder.append("  ");
        builder.append(serviceOperation.getPublicName());
        builder.append("(");
        builder.append(serviceOperation.getInternalName());
        builder.append(")");

        builder.append(" (");
        builder.append(serviceOperation.getParameterType());
        builder.append(") : ");
        builder.append(serviceOperation.getReturnType());
        builder.append("\n");

        return null;
    }
}
