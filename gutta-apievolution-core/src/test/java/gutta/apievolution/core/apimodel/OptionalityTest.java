package gutta.apievolution.core.apimodel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import static gutta.apievolution.core.apimodel.Optionality.*;

/**
 * Test cases for the {@link Optionality} enumeration.
 */
class OptionalityTest {
    
    /**
     * Test case: The comparisons with respect to permissiveness work as expected.
     */
    @Test
    void permissivenessComparison() {
        assertEquals(0, morePermissive(MANDATORY, MANDATORY));
        assertEquals(-1, morePermissive(MANDATORY, OPT_IN));
        assertEquals(-1, morePermissive(MANDATORY, OPTIONAL));
        
        assertEquals(1, morePermissive(OPT_IN, MANDATORY));
        assertEquals(0, morePermissive(OPT_IN, OPT_IN));
        assertEquals(-1, morePermissive(OPT_IN, OPTIONAL));
        
        assertEquals(1, morePermissive(OPTIONAL, MANDATORY));
        assertEquals(1, morePermissive(OPTIONAL, OPT_IN));
        assertEquals(0, morePermissive(OPTIONAL, OPTIONAL));
    }
    
    /**
     * Test case: The comparisons with respect to restrictiveness work as expected.
     */
    @Test
    void restrictivenessComparison() {
        assertEquals(0, moreRestrictive(MANDATORY, MANDATORY));
        assertEquals(1, moreRestrictive(MANDATORY, OPT_IN));
        assertEquals(1, moreRestrictive(MANDATORY, OPTIONAL));
        
        assertEquals(-1, moreRestrictive(OPT_IN, MANDATORY));
        assertEquals(0, moreRestrictive(OPT_IN, OPT_IN));
        assertEquals(1, moreRestrictive(OPT_IN, OPTIONAL));
        
        assertEquals(-1, moreRestrictive(OPTIONAL, MANDATORY));
        assertEquals(-1, moreRestrictive(OPTIONAL, OPT_IN));
        assertEquals(0, moreRestrictive(OPTIONAL, OPTIONAL));
    }

    /**
     * Test case: The isMorePermissive operation works as expected.
     */
    @Test
    void isMorePermissiveComparison() {
        assertFalse(MANDATORY.isMorePermissiveThan(MANDATORY));
        assertFalse(MANDATORY.isMorePermissiveThan(OPT_IN));
        assertFalse(MANDATORY.isMorePermissiveThan(OPTIONAL));
        
        assertTrue(OPT_IN.isMorePermissiveThan(MANDATORY));
        assertFalse(OPT_IN.isMorePermissiveThan(OPT_IN));
        assertFalse(OPT_IN.isMorePermissiveThan(OPTIONAL));
        
        assertTrue(OPTIONAL.isMorePermissiveThan(MANDATORY));
        assertTrue(OPTIONAL.isMorePermissiveThan(OPT_IN));
        assertFalse(OPTIONAL.isMorePermissiveThan(OPTIONAL));
    }
    
    /**
     * Test case: The max function in terms of permissiveness works as expected.
     */
    @Test
    void maxPermissiveness() {
        assertEquals(MANDATORY, max(MANDATORY, MANDATORY));
        assertEquals(OPT_IN, max(MANDATORY, OPT_IN));
        assertEquals(OPTIONAL, max(MANDATORY, OPTIONAL));
        
        assertEquals(OPT_IN, max(OPT_IN, MANDATORY));
        assertEquals(OPT_IN, max(OPT_IN, OPT_IN));
        assertEquals(OPTIONAL, max(OPT_IN, OPTIONAL));
        
        assertEquals(OPTIONAL, max(OPTIONAL, MANDATORY));
        assertEquals(OPTIONAL, max(OPTIONAL, OPT_IN));
        assertEquals(OPTIONAL, max(OPTIONAL, OPTIONAL));
    }
    
}
