package gutta.apievolution.benchmarks;

import org.apache.commons.math3.stat.StatUtils;
import org.openjdk.jmh.annotations.Benchmark;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class BenchmarkRunner {
    
    private static final String BENCHMARKS_FILE_NAME = "META-INF/benchmarks.txt";
    
    private static final int DEFAULT_WARMUP_ITERATIONS = 200000;
    
    private static final int DEFAULT_TIMED_ITERATIONS = 100000;
    
    private static final int REPORT_INTERVAL = 5000;
    
    private static final Logger LOGGER = LoggerFactory.getLogger(BenchmarkRunner.class);
    
    public static void main(String[] arguments) {
        var runner = new BenchmarkRunner();
        
        var configuration = runner.parseArguments(arguments);
        runner.runBenchmark(configuration);
    }
    
    private BenchmarkRunConfiguration parseArguments(String[] arguments) {
        var warmupIterations = DEFAULT_WARMUP_ITERATIONS;
        var timedIterations = DEFAULT_TIMED_ITERATIONS;
        String benchmarkName = null;
        
        var currentArgumentIndex = 0;
        while(currentArgumentIndex < arguments.length) {
            var currentArgument = arguments[currentArgumentIndex];
            
            switch (currentArgument) {
            case "-wi":
                warmupIterations = Math.max(0, Integer.parseInt(arguments[++currentArgumentIndex]));                
                break;
                
            case "-ti":
                timedIterations = Math.max(0, Integer.parseInt(arguments[++currentArgumentIndex]));
                break;
                
            default:
                benchmarkName = currentArgument;
                break;                
            }
            
            currentArgumentIndex++;
        }
        
        if (benchmarkName == null) {
            throw new BenchmarkException("No benchmark name specified.");
        }
        
        var availableBenchmarks = this.findAvailableBenchmarks();
        var benchmarkMethod = availableBenchmarks.get(benchmarkName);
        
        if (benchmarkMethod == null) {
            throw new BenchmarkException("No benchmark named '" + benchmarkName + "'.");
        }
        
        return new BenchmarkRunConfiguration(warmupIterations, timedIterations, benchmarkName, benchmarkMethod);
    }
    
    private Map<String, BenchmarkMethod> findAvailableBenchmarks() {
        try (var inputStream = this.getClass().getClassLoader().getResourceAsStream(BENCHMARKS_FILE_NAME)) {
            if (inputStream == null) {
                // No benchmarks defined
                return Map.of();
            }
            
            var benchmarkClasses = this.loadBenchmarkClasses(inputStream);
            return this.findBenchmarkMethods(benchmarkClasses);            
        } catch (IOException e) {
            throw new BenchmarkException("Error reading the available benchmark classes.", e);
        }
    }
    
    private List<Class<?>> loadBenchmarkClasses(InputStream inputStream) throws IOException {
        var benchmarkClasses = new ArrayList<Class<?>>();
        
        try (var reader = new BufferedReader(new InputStreamReader(inputStream))) {
            while (true) {
                var currentLine = reader.readLine();
                if (currentLine == null) {
                    break;
                }
                
                try {
                    var benchmarkClass = Class.forName(currentLine.trim());
                    benchmarkClasses.add(benchmarkClass);
                } catch (ClassNotFoundException e) {
                    LOGGER.warn("Benchmark class '{}' does not exist.", currentLine);
                }
            }   
        }
        
        return benchmarkClasses;
    }
    
    private Map<String, BenchmarkMethod> findBenchmarkMethods(List<Class<?>> benchmarkClasses) {
        var benchmarkMethods = new HashMap<String, BenchmarkMethod>();
        
        benchmarkClasses.forEach(benchmarkClass -> this.collectBenchmarkMethods(benchmarkClass, benchmarkMethods));
        
        return benchmarkMethods;
    }
    
    private void collectBenchmarkMethods(Class<?> benchmarkClass, Map<String, BenchmarkMethod> benchmarkMethods) {
        var className = benchmarkClass.getSimpleName();
        
        for (var method : benchmarkClass.getDeclaredMethods()) {            
            if (!method.isAnnotationPresent(Benchmark.class)) {
                continue;
            }
            
            var methodName = method.getName();
            var methodKey = className + "." + methodName;
            
            var benchmarkMethod = new BenchmarkMethod(benchmarkClass, method);
            benchmarkMethods.put(methodKey, benchmarkMethod);
        }
    }
    
    private void runBenchmark(BenchmarkRunConfiguration configuration) {
        LOGGER.info("Running benchmark '{}' with {} warmup iterations and {} timed iterations.",configuration.benchmarkName, configuration.warmupIterations,
                configuration.timedIterations);
        
        var benchmarkMethod = configuration.benchmarkMethod;
        
        // Run warmup iterations
        for (var iterationCount = 0; iterationCount < configuration.warmupIterations; iterationCount++) {
            benchmarkMethod.invoke();
            
            if (iterationCount > 0 && (iterationCount % REPORT_INTERVAL) == 0) {
                LOGGER.info("Warmup iteration {} completed.", iterationCount);
            }
        }
        
        // Run timed iterations
        var durationsMus = new double[configuration.timedIterations];
        for (var iterationCount = 0; iterationCount < configuration.timedIterations; iterationCount++) {
            var durationNs = benchmarkMethod.invoke();
            durationsMus[iterationCount] = (double) TimeUnit.NANOSECONDS.toMicros(durationNs);
            
            if (iterationCount > 0 && (iterationCount % REPORT_INTERVAL) == 0) {
                LOGGER.info("Timed iteration {} completed.", iterationCount);
            }            
        }
    
        var averageDurationMus = StatUtils.mean(durationsMus);
        var standardDevMus = Math.sqrt(StatUtils.variance(durationsMus, averageDurationMus));
        
        LOGGER.info("# Benchmark '{}': Average: {} µs, Std. Dev.: {} µs", configuration.benchmarkName, averageDurationMus, standardDevMus);        
    }
        
    private static class BenchmarkMethod {
        
        private final Class<?> benchmarkClass;
        
        private final Method benchmarkMethod;
        
        private Object benchmarkObject;
        
        public BenchmarkMethod(Class<?> benchmarkClass, Method benchmarkMethod) {
            this.benchmarkClass = benchmarkClass;
            this.benchmarkMethod = benchmarkMethod;
        }
        
        private void initialize() {
            try {
                var constructor = this.benchmarkClass.getConstructor();
                this.benchmarkObject = constructor.newInstance();
            } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException | InstantiationException e) {
                throw new BenchmarkException("Error initializing the benchmark object.", e);
            }
        }
        
        public long invoke() {
            if (this.benchmarkObject == null) {
                this.initialize();
            }
            
            try {
                var timeBefore = System.nanoTime();
                this.benchmarkMethod.invoke(this.benchmarkObject);
                var timeAfter = System.nanoTime();
                
                return (timeAfter - timeBefore);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new BenchmarkException("Error invoking the benchmark method.", e);
            }
        }
        
    }
    
    private static class BenchmarkRunConfiguration {
        
        private final int warmupIterations;
        
        private final int timedIterations;
        
        private final String benchmarkName;
        
        private final BenchmarkMethod benchmarkMethod;
        
        public BenchmarkRunConfiguration(int warmupIterations, int timedIterations, String benchmarkName, BenchmarkMethod benchmarkMethod) {
            this.warmupIterations = warmupIterations;
            this.timedIterations = timedIterations;
            this.benchmarkName = benchmarkName;
            this.benchmarkMethod = benchmarkMethod;
        }
                
    }
    
    private static class BenchmarkException extends RuntimeException {
        
        private static final long serialVersionUID = -5876187458260549618L;

        public BenchmarkException(String message) {
            super(message);
        }
        
        public BenchmarkException(String message, Throwable cause) {
            super(message, cause);
        }
        
    }

}
