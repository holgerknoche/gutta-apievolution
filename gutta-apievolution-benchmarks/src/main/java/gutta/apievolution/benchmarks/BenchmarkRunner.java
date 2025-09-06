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
import java.util.Collections;
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
        
        if (configuration.listOnly) {
            runner.listBenchmarks();
        } else {
            runner.runBenchmark(configuration);
        }
    }
    
    private BenchmarkRunConfiguration parseArguments(String[] arguments) {
        var listOnly = false;
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
                
            case "-l":
                listOnly = true;
                break;
                
            default:
                benchmarkName = currentArgument;
                break;                
            }
            
            currentArgumentIndex++;
        }
                
        if (listOnly) {
            return new BenchmarkRunConfiguration(true, 0, 0, benchmarkName, null);
        } else {
            var availableBenchmarks = this.findAvailableBenchmarks();
            var benchmarkMethod = availableBenchmarks.get(benchmarkName);
            
            if (benchmarkName == null) {
                throw new BenchmarkException("No benchmark name specified.");
            }
                   
                    
            if (benchmarkMethod == null) {
                throw new BenchmarkException("No benchmark named '" + benchmarkName + "'.");
            }
            
            return new BenchmarkRunConfiguration(listOnly, warmupIterations, timedIterations, benchmarkName, benchmarkMethod);
        }
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
                
                currentLine = currentLine.trim();                
                if (currentLine.isEmpty() || currentLine.startsWith("#")) {
                    continue;
                }
                
                try {
                    // Load the class without initializing it to avoid costly, but unnecessary static initializers
                    var benchmarkClass = Class.forName(currentLine, false, this.getClass().getClassLoader());
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
    
    private void listBenchmarks() {
        var availableBenchmarks = this.findAvailableBenchmarks();
        
        var benchmarkNames = new ArrayList<>(availableBenchmarks.keySet());
        Collections.sort(benchmarkNames);
        
        LOGGER.info("List of available benchmarks:");
        for (var benchmarkName : benchmarkNames) {
            LOGGER.info(benchmarkName);
        }
    }
    
    private void runBenchmark(BenchmarkRunConfiguration configuration) {
        LOGGER.info("Running benchmark '{}' with {} warmup iterations and {} timed iterations.",configuration.benchmarkName, configuration.warmupIterations,
                configuration.timedIterations);
        
        var benchmarkMethod = configuration.benchmarkMethod;
        
        // Run warmup iterations
        for (var iterationCount = 1; iterationCount <= configuration.warmupIterations; iterationCount++) {
            benchmarkMethod.invoke();
            
            if ((iterationCount % REPORT_INTERVAL) == 0) {
                LOGGER.info("Warmup iteration {} completed.", iterationCount);
            }
        }
        
        // Run timed iterations
        var durationsMus = new double[configuration.timedIterations];
        for (var iterationCount = 1; iterationCount <= configuration.timedIterations; iterationCount++) {
            var durationNs = benchmarkMethod.invoke();
            durationsMus[(iterationCount - 1)] = (double) TimeUnit.NANOSECONDS.toMicros(durationNs);
            
            if ((iterationCount % REPORT_INTERVAL) == 0) {
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
        
        private final boolean listOnly;
        
        private final int warmupIterations;
        
        private final int timedIterations;
        
        private final String benchmarkName;
        
        private final BenchmarkMethod benchmarkMethod;
        
        public BenchmarkRunConfiguration(boolean listOnly, int warmupIterations, int timedIterations, String benchmarkName, BenchmarkMethod benchmarkMethod) {
            this.listOnly = listOnly;
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
