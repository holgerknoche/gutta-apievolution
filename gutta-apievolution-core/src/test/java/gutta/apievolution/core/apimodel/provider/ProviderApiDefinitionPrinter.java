package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Annotation;

import java.util.Iterator;

/**
 * Helper class to create a textual representation of an API definition. This is used to facilitate tests.
 */
class ProviderApiDefinitionPrinter implements ProviderApiDefinitionElementVisitor<Void> {

    private StringBuilder outputBuilder;

    String printApiDefinition(ProviderApiDefinition definition) {
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

        builder.append(" record " + recordType.getPublicName() + " {\n");
        recordType.forEach(field -> field.accept(this));
        builder.append(" }\n");

        return null;
    }

    @Override
    public Void handleProviderField(ProviderField field) {
        StringBuilder builder = this.outputBuilder;

        builder.append("  ");
        builder.append(field.getOptionality());
        builder.append(" ");
        builder.append(field.getPublicName());
        builder.append("(");
        builder.append(field.getInternalName());
        builder.append(")");
        builder.append(":");
        builder.append(field.getType());
        builder.append("\n");

        return null;
    }

    @Override
    public Void handleProviderEnumType(ProviderEnumType enumType) {
        StringBuilder builder = this.outputBuilder;

        builder.append(" enum " + enumType.getPublicName() + " {\n");
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
}
