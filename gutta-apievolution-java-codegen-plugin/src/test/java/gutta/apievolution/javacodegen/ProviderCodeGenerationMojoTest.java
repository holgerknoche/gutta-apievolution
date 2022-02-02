package gutta.apievolution.javacodegen;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

public class ProviderCodeGenerationMojoTest {

    private static final String GOAL_NAME = "generate-provider-code";

    @Rule
    public MojoRule mojoRule = new MojoRule();

    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    private ProviderCodeGenerationMojo createMojo(String projectPath) {
        File baseDir = new File(AbstractMojoTestCase.getBasedir());
        File testResourcesDir = new File(baseDir, "/src/test/resources/");

        try {
            return (ProviderCodeGenerationMojo) this.mojoRule.lookupConfiguredMojo(new File(testResourcesDir,
                            projectPath), GOAL_NAME);
        } catch (Exception e) {
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            } else {
                throw new RuntimeException(e);
            }
        }
    }

    private Map<String, CompilationUnit> parseClasses(File outputDirectory) throws IOException {
        JavaParser javaParser = new JavaParser();

        Path outputPath = outputDirectory.toPath();
        List<Path> paths = Files.walk(outputPath)
                .filter(path -> path.getFileName().toString().endsWith("java"))
                .collect(Collectors.toList());

        Map<String, CompilationUnit> classMap = new HashMap<>();
        for (Path path : paths) {
            CompilationUnit compilationUnit = this.parseFile(javaParser, path);
            String cuName = path.getFileName().toString().replace(".java", "");
            classMap.put(cuName, compilationUnit);
        }

        return classMap;
    }

    private CompilationUnit parseFile(JavaParser javaParser, Path path) {
        File file = path.toFile();

        try {
            ParseResult<CompilationUnit> parseResult = javaParser.parse(file);
            if (parseResult.isSuccessful()) {
                return parseResult.getResult().get();
            } else {
                throw new RuntimeException("Parse error: " + parseResult.getProblems());
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void testSingleRevision() throws MojoFailureException, IOException {
        ProviderCodeGenerationMojo mojo = this.createMojo("single-revision-project");

        File outputDirectory = this.tempFolder.newFolder();
        mojo.outputPath = outputDirectory;

        // Generate provider code
        mojo.execute();

        // Parse the generated code for inspection
        Map<String, CompilationUnit> compilationUnits = this.parseClasses(outputDirectory);

        ClassOrInterfaceDeclaration addressType = compilationUnits.get("Address").getInterfaceByName("Address").get();
        List<String> actualAddressMethods = listMethods(addressType);

        List<String> expectedAddressMethods = Arrays.asList(
                "getCity():String",
                "getNumber():String",
                "getPostalCode():java.math.BigDecimal",
                "getStreet():String",
                "setCity(String):void",
                "setNumber(String):void",
                "setPostalCode(java.math.BigDecimal):void",
                "setStreet(String):void");
        assertEquals(expectedAddressMethods, actualAddressMethods);
    }

    private static List<String> listMethods(ClassOrInterfaceDeclaration declaration) {
        List<MethodDeclaration> methods = declaration.getMethods();
        List<String> methodSignatures = new ArrayList<>();

        for (MethodDeclaration method : methods) {
            methodSignatures.add(createMethodSignature(method));
        }

        Collections.sort(methodSignatures);
        return methodSignatures;
    }

    private static String createMethodSignature(MethodDeclaration method) {
        StringBuilder builder = new StringBuilder();

        builder.append(method.getName());
        builder.append('(');

        Iterator<Parameter> parameters = method.getParameters().iterator();
        while (parameters.hasNext()) {
            builder.append(parameters.next().getType());
            if (parameters.hasNext()) {
                builder.append(',');
            }
        }

        builder.append("):");
        builder.append(method.getType());

        return builder.toString();
    }

    @Test
    public void testProjectWithInheritance() throws MojoFailureException, IOException {
        ProviderCodeGenerationMojo mojo = this.createMojo("project-with-inheritance");

        File outputDirectory = this.tempFolder.newFolder();
        mojo.outputPath = outputDirectory;

        // Generate provider code
        mojo.execute();

        // Parse the generated code for inspection
        Map<String, CompilationUnit> compilationUnits = this.parseClasses(outputDirectory);

        ClassOrInterfaceDeclaration superType = compilationUnits.get("SuperType")
                .getInterfaceByName("SuperType").orElseThrow(NoSuchElementException::new);
        List<String> actualSuperTypeMethods = listMethods(superType);

        ClassOrInterfaceDeclaration subType = compilationUnits.get("SubType")
                .getInterfaceByName("SubType").orElseThrow(NoSuchElementException::new);
        List<String> actualSubTypeMethods = listMethods(subType);

        // Assert that the supertype reference is present
        assertEquals(1, subType.getExtendedTypes().size());
        assertEquals(superType.getName(), subType.getExtendedTypes(0).getName());

        // Check the generated methods
        List<String> expectedSuperTypeMethods = Arrays.asList(
                "getInheritedField():String",
                "setInheritedField(String):void"
        );
        List<String> expectedSubTypeMethods = Arrays.asList(
                "getNormalField():String",
                "setNormalField(String):void"
        );

        assertEquals(expectedSuperTypeMethods, actualSuperTypeMethods);
        assertEquals(expectedSubTypeMethods, actualSubTypeMethods);
    }

}
