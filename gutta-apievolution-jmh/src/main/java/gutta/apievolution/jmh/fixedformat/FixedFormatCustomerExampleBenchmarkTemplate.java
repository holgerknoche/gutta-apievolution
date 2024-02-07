package gutta.apievolution.jmh.fixedformat;

import gutta.apievolution.jmh.CustomerExampleBenchmarkTemplate;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

abstract class FixedFormatCustomerExampleBenchmarkTemplate extends CustomerExampleBenchmarkTemplate {

    protected static final Charset CHARSET = StandardCharsets.ISO_8859_1;    

}
