package gutta.apievolution.core.apimodel;

/**
 * Annotations allow to embed additional information within an API model, such as, for instance, specific names of types
 * for particular programming languages.
 */
public class Annotation {
	
	private final String name;
	
	private final String value;

	/**
	 * Creates a new annotation with the given annotation type name.
	 * @param name The annotation's name
	 * @param value The annotation's value
	 */
	public Annotation(final String name, final String value) {
		this.name = name;
		this.value = value;
	}

	/**
	 * Returns the name of this annotation.
	 * @return see above
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Returns the value of this annotation.
	 * @return see above
	 */
	public String getValue() {
		return this.value;
	}

}
