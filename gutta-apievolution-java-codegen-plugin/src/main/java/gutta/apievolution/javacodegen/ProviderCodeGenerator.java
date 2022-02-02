package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.provider.ModelMerger;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import org.apache.commons.lang3.StringUtils;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

/**
 * This class derives the actual code model and generates the provider Java code.
 */
class ProviderCodeGenerator {

    /**
     * Generates the provider code for the given definitions into the given output directory.
     * @param supportedDefinitions The definitions to generate the sources for. It is expected that this
     *                             list is ordered with respect to revisions, esp. that the latest revision
     *                             is the last element of the list
     * @param outputDirectory The directory to generate the sources into
     */
    public void generateCode(List<ProviderApiDefinition> supportedDefinitions, File outputDirectory) {
        Collection<JavaUserDefinedType> classesToGenerate = this.createCodeModel(
                new RevisionHistory(supportedDefinitions));
        this.generateSources(classesToGenerate, outputDirectory);
    }

    Collection<JavaUserDefinedType> createCodeModel(RevisionHistory revisionHistory) {
        if (revisionHistory.isEmpty()) {
            return Collections.emptyList();
        }

        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);
        JavaModelBuilder javaModelBuilder = new JavaModelBuilder();
        return javaModelBuilder.buildModelForApi(mergedDefinition);
    }

    void generateSources(Collection<JavaUserDefinedType> classesToGenerate, File outputDirectory) {
        Properties properties = new Properties();
        properties.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
        properties.setProperty("classpath.resource.loader.class",
                "org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader");

        VelocityEngine velocityEngine = new VelocityEngine();
        velocityEngine.init(properties);

        for (JavaUserDefinedType classToGenerate : classesToGenerate) {
            this.generateCodeFor(classToGenerate, velocityEngine, outputDirectory);
        }
    }

    private void generateCodeFor(JavaUserDefinedType classToGenerate, VelocityEngine engine, File outputDirectory) {
        String packagePath = classToGenerate.packageName.replace('.', '/');
        File packageDirectory = new File(outputDirectory, packagePath);
        if (!packageDirectory.exists()) {
            packageDirectory.mkdirs();

            if (!packageDirectory.exists()) {
                throw new RuntimeException("Could not create directory " + packageDirectory + ".");
            }
        }

        String fileName = classToGenerate.name + ".java";
        File outputFile = new File(packageDirectory, fileName);

        try (FileWriter writer = new FileWriter(outputFile)) {
            this.generateCodeForUDT(classToGenerate, engine, writer);
        } catch (IOException e) {
            throw new RuntimeException("Unable to write file " + outputFile + ".");
        }
    }

    private void generateCodeForUDT(JavaUserDefinedType udt, VelocityEngine engine, Writer writer) {
        if (udt instanceof JavaInterface) {
            this.generateInterface((JavaInterface) udt, engine, writer);
        } else if (udt instanceof JavaEnum) {
            this.generateEnum((JavaEnum) udt, engine, writer);
        } else {
            throw new RuntimeException("Unknown UDT type " + udt + ".");
        }
    }

    private void generateInterface(JavaInterface classToGenerate, VelocityEngine engine, Writer writer) {
        VelocityContext context = new VelocityContext();

        context.put("type", classToGenerate);
        context.put("stringUtil", new StringUtils()); // NOSONAR Must be instantiated to be used in the context

        engine.mergeTemplate("java/JavaInterface.vt", "UTF-8", context, writer);
    }

    private void generateEnum(JavaEnum enumToGenerate, VelocityEngine engine, Writer writer) {
        VelocityContext context = new VelocityContext();

        context.put("type", enumToGenerate);
        context.put("stringUtil", new StringUtils()); // NOSONAR Must be instantiated to be used in the context

        engine.mergeTemplate("java/JavaEnum.vt", "UTF-8", context, writer);
    }

}
