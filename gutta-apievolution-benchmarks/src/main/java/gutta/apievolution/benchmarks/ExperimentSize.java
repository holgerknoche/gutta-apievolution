package gutta.apievolution.benchmarks;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to denote the size of an experiment (e.g., the number of converted fields).
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ExperimentSize {
	
	/**
	 * Returns the size of the experiment in appropriate units.
	 * 
	 * @return see above
	 */
	int value();

}
