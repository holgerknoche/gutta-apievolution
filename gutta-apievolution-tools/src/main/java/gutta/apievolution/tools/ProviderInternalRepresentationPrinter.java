package gutta.apievolution.tools;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import gutta.apievolution.core.apimodel.provider.ModelMerger;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinitionElementVisitor;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.NamedInputStream;
import gutta.apievolution.dsl.ProviderApiLoader;

public class ProviderInternalRepresentationPrinter implements ProviderApiDefinitionElementVisitor<Void> {

    public static void main(String[] arguments) {
        new ProviderInternalRepresentationPrinter().printProviderRepresentation(Arrays.asList(arguments));
    }

    public void printProviderRepresentation(List<String> apiNames) {
        RevisionHistory revisionHistory = this.loadRevisionHistory(apiNames);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        System.out.println("Revision '" + mergedDefinition.getName() + "':");
        mergedDefinition.forEach(element -> element.accept(this));
    }

    private RevisionHistory loadRevisionHistory(List<String> apiNames) {
        List<NamedInputStream> inputStreams = apiNames.stream().map(streamName -> new NamedInputStream(streamName, () -> this.openInputStream(streamName)))
                .collect(Collectors.toList());

        List<ProviderApiDefinition> providerApis = ProviderApiLoader.loadHistoryFromStreams(IntegerRange.unbounded(), false, inputStreams);
        return new RevisionHistory(providerApis);
    }

    InputStream openInputStream(String name) {
        try {
            return new FileInputStream(name);
        } catch (IOException e) {
            throw new IllegalArgumentException("Error opening API '" + name + "'.", e);
        }
    }

    @Override
    public Void handleProviderEnumType(ProviderEnumType enumType) {
        System.out.println(" - Enum type '" + enumType.getPublicName() + "' (internal name: '" + enumType.getInternalName() + "', type id: " +
                enumType.getTypeId() + "):");

        enumType.getDeclaredMembers().forEach(member -> member.accept(this));

        return null;
    }

    @Override
    public Void handleProviderEnumMember(ProviderEnumMember enumMember) {
        System.out.println("   - Enum member '" + enumMember.getPublicName() + "' (internal name: '" + enumMember.getInternalName() + ")");

        return null;
    }

    @Override
    public Void handleProviderRecordType(ProviderRecordType recordType) {
        System.out.println(" - Record type '" + recordType.getPublicName() + "' (internal name: '" + recordType.getInternalName() + "', type id: " +
                recordType.getTypeId() + "):");

        recordType.getFields().forEach(field -> field.accept(this));

        return null;
    }

    @Override
    public Void handleProviderField(ProviderField field) {
        System.out.println("   - Field '" + field.getPublicName() + "' (internal name: '" + field.getInternalName() + "'): " + field.getType());

        return null;
    }
    
    @Override
    public Void handleProviderOperation(ProviderOperation operation) {
    	System.out.println(" - Operation '" + operation.getPublicName() + "' (internal name: '" + operation.getInternalName() + "')");
    	
    	return null;
    }

}
