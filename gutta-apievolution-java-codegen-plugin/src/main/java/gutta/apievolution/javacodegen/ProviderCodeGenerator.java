package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.provider.ModelMerger;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.util.StringUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

/**
 * This class derives the actual code model and generates the provider Java
 * code.
 */
class ProviderCodeGenerator {

    /**
     * Generates the provider code for the given definitions into the given output
     * directory.
     * 
     * @param supportedDefinitions The definitions to generate the sources for. It
     *                             is expected that this list is ordered with
     *                             respect to revisions, esp. that the latest
     *                             revision is the last element of the list
     * @param outputDirectory      The directory to generate the sources into
     */
    public void generateCode(List<ProviderApiDefinition> supportedDefinitions, File outputDirectory) {
        JavaModel model = this.createCodeModel(new RevisionHistory(supportedDefinitions));
        this.generateSources(model, outputDirectory);
    }

    JavaModel createCodeModel(RevisionHistory revisionHistory) {
        if (revisionHistory.isEmpty()) {
            return new JavaModel(Collections.emptyList(), Collections.emptyList());
        }

        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        JavaModelBuilder javaModelBuilder = new JavaModelBuilder();
        return javaModelBuilder.buildModelForApi(mergedDefinition);
    }

    void generateSources(JavaModel model, File outputDirectory) {
        Properties properties = new Properties();
        properties.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
        properties.setProperty("classpath.resource.loader.class",
                "org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader");

        VelocityEngine velocityEngine = new VelocityEngine();
        velocityEngine.init(properties);

        for (JavaUserDefinedType classToGenerate : model.userDefinedTypes) {
            this.generateCodeFor(classToGenerate, velocityEngine, outputDirectory, this::generateCodeForUDT);
        }
        for (JavaService serviceToGenerate : model.services) {
            this.generateCodeFor(serviceToGenerate, velocityEngine, outputDirectory, this::generateCodeForService);
        }
    }

    private void generateCodeFor(JavaModelElement element, VelocityEngine engine, File outputDirectory,
            CodeGenerator codeGenerator) {

        String packagePath = element.packageName.replace('.', '/');
        File packageDirectory = new File(outputDirectory, packagePath);
        if (!packageDirectory.exists()) {
            packageDirectory.mkdirs();

            if (!packageDirectory.exists()) {
                throw new RuntimeException("Could not create directory " + packageDirectory + ".");
            }
        }

        String fileName = element.name + ".java";
        File outputFile = new File(packageDirectory, fileName);

        try (Writer writer = new FileWriter(outputFile)) {
            codeGenerator.generateCode(element, engine, writer);
        } catch (IOException e) {
            throw new RuntimeException("Unable to write file " + outputFile + ".");
        }
    }

    private void generateCodeForUDT(JavaModelElement element, VelocityEngine engine, Writer writer) {
        UDTCodeGenerator udtCodeGenerator = new UDTCodeGenerator(engine);
        udtCodeGenerator.generateCodeForUDT((JavaUserDefinedType) element, writer);
    }

    private void generateCodeForService(JavaModelElement service, VelocityEngine engine, Writer writer) {
        VelocityContext context = new VelocityContext();

        context.put("service", service);
        context.put("stringUtil", new StringUtils());

        engine.mergeTemplate("java/JavaServiceInterface.vt", "UTF-8", context, writer);
    }

    private static class UDTCodeGenerator implements JavaUserDefinedTypeVisitor<Void> {

        private final VelocityEngine velocityEngine;

        private final StringUtils stringUtils = new StringUtils();

        private Writer writer;

        public UDTCodeGenerator(VelocityEngine velocityEngine) {
            this.velocityEngine = velocityEngine;
        }

        public void generateCodeForUDT(JavaUserDefinedType type, Writer writer) {
            this.writer = writer;

            type.accept(this);
        }

        private Void generateCode(JavaUserDefinedType type, String templateName) {
            VelocityContext context = new VelocityContext();

            context.put("type", type);
            context.put("stringUtil", this.stringUtils);

            this.velocityEngine.mergeTemplate(templateName, "UTF-8", context, this.writer);
            return null;
        }

        @Override
        public Void handleJavaEnum(JavaEnum javaEnum) {
            return this.generateCode(javaEnum, "java/JavaEnum.vt");
        }

        @Override
        public Void handleJavaException(JavaException javaException) {
            return this.generateCode(javaException, "java/JavaException.vt");
        }

        @Override
        public Void handleJavaInterface(JavaInterface javaInterface) {
            return this.generateCode(javaInterface, "java/JavaInterface.vt");
        }

    }

    @FunctionalInterface
    private interface CodeGenerator {

        void generateCode(JavaModelElement element, VelocityEngine engine, Writer writer);

    }

}
