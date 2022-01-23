package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.ProviderApiLoader;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Mojo for generating provider code from a revision of API definitions.
 */
@Mojo(name = "generate-provider-code")
public class ProviderCodeGenerationMojo extends AbstractMojo {

    @Parameter(defaultValue = "${project}", readonly = true)
    MavenProject project;

    @Parameter
    File[] revisionFiles;

    @Parameter
    Integer[] supportedRevisions;

    @Parameter(defaultValue = "${project.basedir}/target/generated-sources")
    File outputPath;

    ProviderCodeGenerator providerCodeGenerator = new ProviderCodeGenerator();

    @Override
    public void execute() throws MojoFailureException {
        List<ProviderApiDefinition> definitions = this.loadApiDefinitions(revisionFiles);
        Set<Integer> supportedRevisionsSet = new HashSet<>(Arrays.asList(this.supportedRevisions));

        List<ProviderApiDefinition> supportedDefinitions = definitions.stream()
                .filter(definition -> supportedRevisionsSet.contains(definition.getRevision()))
                .collect(Collectors.toList());

        this.getLog().info("Generating code for " + supportedDefinitions.size() + " supported revisions.");
        this.getLog().info("Generated code will be stored in " + this.outputPath);
        this.providerCodeGenerator.generateCode(supportedDefinitions, this.outputPath);
    }

    private List<ProviderApiDefinition> loadApiDefinitions(File[] revisionFiles) throws MojoFailureException {
        if (revisionFiles != null) {
            return this.loadFromFiles(revisionFiles);
        } else {
            this.getLog().warn("No revision files specified, no code will be generated.");
            return Collections.emptyList();
        }
    }

    private List<ProviderApiDefinition> loadFromFiles(File[] revisionFiles) throws MojoFailureException {
        this.getLog().debug("Loading revisions from files:");
        for (File file : revisionFiles) {
            this.getLog().debug("- " + file.getName());
        }

        List<FileInputStream> streams = Stream.of(revisionFiles)
                .map(this::toInputStream)
                .collect(Collectors.toList());

        return ProviderApiLoader.loadHistoryFromStreams(IntegerRange.unbounded(), streams);
    }

    private FileInputStream toInputStream(File file) {
        try {
            return new FileInputStream(file);
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

}
