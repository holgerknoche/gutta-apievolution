package gutta.apievolution.core.apimodel;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class QualifiedNameTest {

    @Test
    void ofForSimpleNames() {
        QualifiedName name1 = QualifiedName.of("a.b.c");
        assertEquals(new QualifiedName(Arrays.asList("a", "b", "c")), name1);

    }
}
