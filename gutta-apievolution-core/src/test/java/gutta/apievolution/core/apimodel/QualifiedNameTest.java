package gutta.apievolution.core.apimodel;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for the class {@link QualifiedName}.
 */
class QualifiedNameTest {

    /**
     * Test cases of constructing a qualified name using the {@link QualifiedName#of(String)} method.
     */
    @Test
    void ofForSimpleNames() {
        QualifiedName name1 = QualifiedName.of("a.b.c");
        assertEquals(new QualifiedName(Arrays.asList("a", "b", "c")), name1);
    }

    /**
     * Ensure that no empty qualified name can be constructed.
     */
    @Test
    void failOnEmptyName() {
        assertThrows(IllegalArgumentException.class, () -> QualifiedName.of(""));
        assertThrows(IllegalArgumentException.class, () -> QualifiedName.of(null));
    }

}
