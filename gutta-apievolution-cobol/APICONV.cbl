123456*Conversion invocation from COBOL

       IDENTIFICATION DIVISION.
       PROGRAM-ID. APICONV.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONSTANTS.
           05 NUMBER-OF-ITERATIONS PIC S9(9) BINARY
              VALUE 10000000.
       
       01 TIMINGS.
           05 START-TIME-MS PIC S9(18) BINARY.
           05 END-TIME-MS PIC S9(18) BINARY.
           05 DURATION-MS PIC S9(18) BINARY.
           
           05 DURATION-DISP PIC 9(18).

       01 INDEXES.
           05 I-1 PIC S9(9) BINARY.

       01 MAPPER-PARAMETERS.
           05 CONSUMER-SCRIPT-NAME PIC X(30).
           05 PROVIDER-SCRIPT-NAME PIC X(30).
           
           05 OPERATION-INDEX PIC S9(9) BINARY.
           05 MAPPING-DIRECTION PIC S9(9) BINARY.
               88 CONSUMER-TO-PROVIDER VALUE 0.
               88 PROVIDER-TO-CONSUMER VALUE 1.
               
           05 MAPPING-TYPE PIC S9(9) BINARY.
               88 PARAMETER-MAPPING VALUE 0.
               88 RESULT-MAPPING VALUE 1.
      
      * Structures for Customer, version 1
       01 CUSTOMER-V1-IN.
           COPY CUSTOMR1 REPLACING '*-' BY CS1I-.
           
       01 CUSTOMER-V1-OUT.
           COPY CUSTOMR1 REPLACING '*-' BY CS1O-.
           
      * Structures for Customer, version 3
       01 CUSTOMER-V3-IN.
           COPY CUSTOMR3 REPLACING '*-' BY CS3I-.
           
       01 CUSTOMER-V3-OUT.
           COPY CUSTOMR3 REPLACING '*-' BY CS3O-.

      * Structures for Customer, version 6
       01 CUSTOMER-V6-IN.
           COPY CUSTOMR6 REPLACING '*-' BY CS6I-.
           
       01 CUSTOMER-V6-OUT.
           COPY CUSTOMR6 REPLACING '*-' BY CS6O-.

      * Structures for Customer, provider view
       01 CUSTOMER-PROVIDER-IN.
           COPY CUSTOMRP REPLACING '*-' BY CSPI-.
           
       01 CUSTOMER-PROVIDER-OUT.
           COPY CUSTOMRP REPLACING '*-' BY CSPO-.

      * Structures for size 10
       01 CONSUMER-STRUCT-0.
           COPY STRCT000 REPLACING '*-' BY C0-.
       
       01 PROVIDER-STRUCT-0.
           COPY STRCT000 REPLACING '*-' BY P0-.

      * Structures for size 10
       01 CONSUMER-STRUCT-10.
           COPY STRCT010 REPLACING '*-' BY C10-.
       
       01 PROVIDER-STRUCT-10.
           COPY STRCT010 REPLACING '*-' BY P10-.
       
      * Structures for size 25
       01 CONSUMER-STRUCT-25.
           COPY STRCT025 REPLACING '*-' BY C25-.
       
       01 PROVIDER-STRUCT-25.
           COPY STRCT025 REPLACING '*-' BY P25-.

      * Structures for size 50
       01 CONSUMER-STRUCT-50.
           COPY STRCT050 REPLACING '*-' BY C50-.
       
       01 PROVIDER-STRUCT-50.
           COPY STRCT050 REPLACING '*-' BY P50-.

      * Structures for size 75
       01 CONSUMER-STRUCT-75.
           COPY STRCT075 REPLACING '*-' BY C75-.
       
       01 PROVIDER-STRUCT-75.
           COPY STRCT075 REPLACING '*-' BY P75-.
           
      * Structures for size 100
       01 CONSUMER-STRUCT-100.
           COPY STRCT100 REPLACING '*-' BY C100-.
       
       01 PROVIDER-STRUCT-100.
           COPY STRCT100 REPLACING '*-' BY P100-.

      * Structures for size 250
       01 CONSUMER-STRUCT-250.
           COPY STRCT250 REPLACING '*-' BY C250-.
       
       01 PROVIDER-STRUCT-250.
           COPY STRCT250 REPLACING '*-' BY P250-.

      * Structures for size 500
       01 CONSUMER-STRUCT-500.
           COPY STRCT500 REPLACING '*-' BY C500-.
       
       01 PROVIDER-STRUCT-500.
           COPY STRCT500 REPLACING '*-' BY P500-.
       
       PROCEDURE DIVISION.
      *    Initial call to load the modules and make the other functions
      *    available
           CALL 'timer'
           CALL 'apimapper'      
           
           DISPLAY 'Running customer benchmarks...' 
              UPON CONSOLE
              
           PERFORM RUN-BENCHMARK-V1
           PERFORM RUN-BENCHMARK-V3
           PERFORM RUN-BENCHMARK-V6
           
           DISPLAY 'Running size benchmarks...'
             UPON CONSOLE
           
           PERFORM LOAD-SIZE-BENCHMARK-SCRIPTS
           PERFORM RUN-BENCHMARK-SIZE-0
           PERFORM RUN-BENCHMARK-SIZE-10
           PERFORM RUN-BENCHMARK-SIZE-25
           PERFORM RUN-BENCHMARK-SIZE-50
           PERFORM RUN-BENCHMARK-SIZE-75
           PERFORM RUN-BENCHMARK-SIZE-100
           PERFORM RUN-BENCHMARK-SIZE-250
           PERFORM RUN-BENCHMARK-SIZE-500
           PERFORM UNLOAD-SCRIPTS
                                             
           GOBACK.
      
      * ---
      * Run invocation benchmark v1
       RUN-BENCHMARK-V1 SECTION.
           PERFORM LOAD-SCRIPTS-V1
           PERFORM INIT-INPUT-DATA-V1
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM PERFORM-CONVERSION-V1 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark v1: ' DURATION-MS 'ms' UPON CONSOLE
           
           PERFORM UNLOAD-SCRIPTS
                      
           EXIT.
      
      * ---
      * Load scripts for invocation benchmark v1
       LOAD-SCRIPTS-V1 SECTION.
           MOVE 'consumer-script-v1.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v1.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
           
           EXIT.
           
      * ---
      * Initialize input data for benchmark v1
       INIT-INPUT-DATA-V1 SECTION.
           SET VALUE-PRESENT IN CS1I-CUSTOMER-FLAGS
            TO TRUE           
           
           SET VALUE-PRESENT IN CS1I-FIRST-NAME-FLAGS
            TO TRUE
           MOVE 'Test'
             TO CS1I-FIRST-NAME
           SET VALUE-PRESENT IN CS1I-LAST-NAME-FLAGS
            TO TRUE
           MOVE 'Tester'
             TO CS1I-LAST-NAME
           SET VALUE-PRESENT IN CS1I-GENDER-FLAGS
            TO TRUE
           MOVE 1
             TO CS1I-GENDER
           
           SET VALUE-PRESENT IN CS1I-ADDRESS-FLAGS
            TO TRUE
           
           SET VALUE-PRESENT IN CS1I-STREET-FLAGS
            TO TRUE
           MOVE 'Test Street'
             TO CS1I-STREET
           SET VALUE-PRESENT IN CS1I-NUMBER-FLAGS
             TO TRUE
           MOVE 17
             TO CS1I-NUMBER
           SET VALUE-PRESENT IN CS1I-POSTAL-CODE-FLAGS
            TO TRUE
           MOVE 12345
             TO CS1I-POSTAL-CODE
           SET VALUE-PRESENT IN CS1I-CITY-FLAGS
            TO TRUE
           MOVE 'Test City'
             TO CS1I-CITY
           
           EXIT.
           
      * ---
      * Perform conversion for benchmark v1
       PERFORM-CONVERSION-V1 SECTION.
           MOVE 0 TO OPERATION-INDEX
           SET CONSUMER-TO-PROVIDER TO TRUE
           SET PARAMETER-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CUSTOMER-V1-IN
             BY REFERENCE CUSTOMER-PROVIDER-IN
             
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CUSTOMER-PROVIDER-IN
             BY REFERENCE CUSTOMER-V1-OUT           
           
           EXIT.
           
      * ---
      * Run invocation benchmark v3
       RUN-BENCHMARK-V3 SECTION.
           PERFORM LOAD-SCRIPTS-V3
           PERFORM INIT-INPUT-DATA-V3
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM PERFORM-CONVERSION-V3 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark v3: ' DURATION-MS 'ms' UPON CONSOLE
           
           PERFORM UNLOAD-SCRIPTS
           
           EXIT.
      
      * ---
      * Load scripts for invocation benchmark v3
       LOAD-SCRIPTS-V3 SECTION.
           MOVE 'consumer-script-v3.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v3.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
           
           EXIT.
           
      * ---
      * Initialize input data for benchmark v3
       INIT-INPUT-DATA-V3 SECTION.
           SET VALUE-PRESENT IN CS3I-CUSTOMER-FLAGS
            TO TRUE           
           
           SET VALUE-PRESENT IN CS3I-FIRST-NAME-FLAGS
            TO TRUE
           MOVE 'Test'
             TO CS3I-FIRST-NAME
           SET VALUE-PRESENT IN CS3I-LAST-NAME-FLAGS
            TO TRUE
           MOVE 'Tester'
             TO CS3I-LAST-NAME
           SET VALUE-PRESENT IN CS3I-DATE-OF-BIRTH-FLAGS
            TO TRUE
           MOVE '01.01.2000'
             TO CS3I-DATE-OF-BIRTH             
           SET VALUE-PRESENT IN CS3I-GENDER-FLAGS
            TO TRUE
           MOVE 1
             TO CS3I-GENDER

      *    Primary addresses
           SET VALUE-PRESENT IN CS3I-PRIMARY-ADDRESS-FLAGS
            TO TRUE
           
           SET VALUE-PRESENT IN CS3I-STREET-FLAGS
                             IN CS3I-PRIMARY-ADDRESS
            TO TRUE
           MOVE 'Test Street'
             TO CS3I-STREET IN CS3I-PRIMARY-ADDRESS
           SET VALUE-PRESENT IN CS3I-NUMBER-FLAGS
                             IN CS3I-PRIMARY-ADDRESS
             TO TRUE
           MOVE 17
             TO CS3I-NUMBER IN CS3I-PRIMARY-ADDRESS
           SET VALUE-PRESENT IN CS3I-POSTAL-CODE-FLAGS
                             IN CS3I-PRIMARY-ADDRESS
            TO TRUE
           MOVE 12345
             TO CS3I-POSTAL-CODE IN CS3I-PRIMARY-ADDRESS
           SET VALUE-PRESENT IN CS3I-CITY-FLAGS
                             IN CS3I-PRIMARY-ADDRESS
            TO TRUE
           MOVE 'Test City'
             TO CS3I-CITY IN CS3I-PRIMARY-ADDRESS
             
      *    Secondary addresses
           SET VALUE-PRESENT IN CS3I-SEC-ADDR-LST-FLAGS
            TO TRUE
           MOVE 2
             TO CS3I-SEC-ADDRESS-COUNT
           
           PERFORM VARYING I-1 FROM 1 BY 1
                   UNTIL I-1 > 2
                   
             SET VALUE-PRESENT IN CS3I-SECONDARY-ADDRESS-FLAGS(I-1)
              TO TRUE
           
             SET VALUE-PRESENT IN CS3I-STREET-FLAGS
                               IN CS3I-SECONDARY-ADDRESS(I-1)
              TO TRUE
             MOVE 'Test Road'
               TO CS3I-STREET IN CS3I-SECONDARY-ADDRESS(I-1)
             SET VALUE-PRESENT IN CS3I-NUMBER-FLAGS
                               IN CS3I-SECONDARY-ADDRESS(I-1)
              TO TRUE
             MOVE I-1
               TO CS3I-NUMBER IN CS3I-SECONDARY-ADDRESS(I-1)
             SET VALUE-PRESENT IN CS3I-POSTAL-CODE-FLAGS
                               IN CS3I-SECONDARY-ADDRESS(I-1)
              TO TRUE
             MOVE 12345
               TO CS3I-POSTAL-CODE IN CS3I-SECONDARY-ADDRESS(I-1)
             SET VALUE-PRESENT IN CS3I-CITY-FLAGS
                               IN CS3I-SECONDARY-ADDRESS(I-1)
              TO TRUE
             MOVE 'Test Town'
               TO CS3I-CITY IN CS3I-SECONDARY-ADDRESS(I-1)
           END-PERFORM

           EXIT.
           
      * ---
      * Perform conversion for benchmark v3
       PERFORM-CONVERSION-V3 SECTION.
           MOVE 0 TO OPERATION-INDEX
           SET CONSUMER-TO-PROVIDER TO TRUE
           SET PARAMETER-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CUSTOMER-V3-IN
             BY REFERENCE CUSTOMER-PROVIDER-IN
             
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CUSTOMER-PROVIDER-IN
             BY REFERENCE CUSTOMER-V3-OUT           
           
           EXIT.

      * ---
      * Run invocation benchmark v6
       RUN-BENCHMARK-V6 SECTION.
           PERFORM LOAD-SCRIPTS-V6
           PERFORM INIT-INPUT-DATA-V6
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM PERFORM-CONVERSION-V6 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark v6: ' DURATION-MS 'ms' UPON CONSOLE
           
           PERFORM UNLOAD-SCRIPTS
           
           EXIT.
      
      * ---
      * Load scripts for invocation benchmark v6
       LOAD-SCRIPTS-V6 SECTION.
           MOVE 'consumer-script-v6.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v6.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
           
           EXIT.
           
      * ---
      * Initialize input data for benchmark v6
       INIT-INPUT-DATA-V6 SECTION.
           SET VALUE-PRESENT IN CS6I-CUSTOMER-FLAGS
            TO TRUE           
           
           SET VALUE-PRESENT IN CS6I-FIRST-NAME-FLAGS
            TO TRUE
           MOVE 'Test'
             TO CS6I-FIRST-NAME
           SET VALUE-PRESENT IN CS6I-LAST-NAME-FLAGS
            TO TRUE
           MOVE 'Tester'
             TO CS6I-LAST-NAME
           SET VALUE-PRESENT IN CS6I-DATE-OF-BIRTH-FLAGS
            TO TRUE
           MOVE '01.01.2000'
             TO CS6I-DATE-OF-BIRTH
           SET VALUE-PRESENT IN CS6I-GENDER-FLAGS
            TO TRUE
           MOVE 1
             TO CS6I-GENDER

      *    Primary address
           SET VALUE-PRESENT IN CS6I-PRIMARY-ADDRESS-FLAGS
            TO TRUE
           SET CS6I-STREET-ADDRESS IN CS6I-PRIMARY-ADDRESS
            TO TRUE
           
           SET VALUE-PRESENT IN CS6I-STREET-FLAGS
                             IN CS6I-PRIMARY-ADDRESS
            TO TRUE
           MOVE 'Test Street'
             TO CS6I-STREET IN CS6I-PRIMARY-ADDRESS
           SET VALUE-PRESENT IN CS6I-NUMBER-FLAGS
                             IN CS6I-PRIMARY-ADDRESS
             TO TRUE
           MOVE 17
             TO CS6I-NUMBER IN CS6I-PRIMARY-ADDRESS
           SET VALUE-PRESENT IN CS6I-POSTAL-CODE-FLAGS
                             IN CS6I-PRIMARY-ADDRESS
            TO TRUE
           MOVE 12345
             TO CS6I-POSTAL-CODE IN CS6I-PRIMARY-ADDRESS
           SET VALUE-PRESENT IN CS6I-CITY-FLAGS
                             IN CS6I-PRIMARY-ADDRESS
            TO TRUE
           MOVE 'Test City'
             TO CS6I-CITY IN CS6I-PRIMARY-ADDRESS
             
      *    Secondary addresses
           SET VALUE-PRESENT IN CS6I-SEC-ADDR-LST-FLAGS
            TO TRUE
           MOVE 2
             TO CS6I-SEC-ADDRESS-COUNT

      *    First entry (street address)
           SET VALUE-PRESENT IN CS6I-SECONDARY-ADDRESS-FLAGS(1)
            TO TRUE
           SET CS6I-STREET-ADDRESS IN CS6I-SECONDARY-ADDRESS(1)
            TO TRUE
           
           SET VALUE-PRESENT IN CS6I-STREET-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(1)
            TO TRUE
           MOVE 'Test Road'
             TO CS6I-STREET IN CS6I-SECONDARY-ADDRESS(1)
           SET VALUE-PRESENT IN CS6I-NUMBER-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(1)
            TO TRUE
           MOVE 3
             TO CS6I-NUMBER IN CS6I-SECONDARY-ADDRESS(1)
           SET VALUE-PRESENT IN CS6I-POSTAL-CODE-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(1)
            TO TRUE
           MOVE 12345
             TO CS6I-POSTAL-CODE IN CS6I-SECONDARY-ADDRESS(1)
           SET VALUE-PRESENT IN CS6I-CITY-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(1)
            TO TRUE
           MOVE 'Test Town'
             TO CS6I-CITY IN CS6I-SECONDARY-ADDRESS(1)

      *    Second entry (PO box address)
           SET VALUE-PRESENT IN CS6I-SECONDARY-ADDRESS-FLAGS(2)
            TO TRUE
           SET CS6I-PO-BOX-ADDRESS IN CS6I-SECONDARY-ADDRESS(2)
            TO TRUE
           
           SET VALUE-PRESENT IN CS6I-BOX-NO-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(2)
            TO TRUE
           MOVE 5678
             TO CS6I-BOX-NO IN CS6I-SECONDARY-ADDRESS(2)
           SET VALUE-PRESENT IN CS6I-POSTAL-CODE-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(2)
            TO TRUE
           MOVE 12346
             TO CS6I-POSTAL-CODE IN CS6I-SECONDARY-ADDRESS(2)
           SET VALUE-PRESENT IN CS6I-CITY-FLAGS
                             IN CS6I-SECONDARY-ADDRESS(2)
            TO TRUE
           MOVE 'Test Town'
             TO CS6I-CITY IN CS6I-SECONDARY-ADDRESS(2)

           EXIT.
           
      * ---
      * Perform conversion for benchmark v6
       PERFORM-CONVERSION-V6 SECTION.
           MOVE 0 TO OPERATION-INDEX
           SET CONSUMER-TO-PROVIDER TO TRUE
           SET PARAMETER-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CUSTOMER-V6-IN
             BY REFERENCE CUSTOMER-PROVIDER-IN
             
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CUSTOMER-PROVIDER-IN
             BY REFERENCE CUSTOMER-V6-OUT                       
           
           EXIT.
           
       LOAD-SIZE-BENCHMARK-SCRIPTS SECTION.
           MOVE 'consumer-script-sizes.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-sizes.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
       
           EXIT.

       RUN-BENCHMARK-SIZE-0 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-0
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-0 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 0: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-0 SECTION.
           SET VALUE-PRESENT IN P0-TEST-STRUCT-0-FLAGS
            TO TRUE
       
           EXIT.

       CONVERT-STRUCTURE-SIZE-0 SECTION.
           MOVE 7 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-0
             BY REFERENCE CONSUMER-STRUCT-0                    
       
           EXIT.

       RUN-BENCHMARK-SIZE-10 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-10
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-10 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 10: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-10 SECTION.
           SET VALUE-PRESENT IN P10-TEST-STRUCT-10-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P10-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P10-INT-FIELD-1
           SET VALUE-PRESENT IN P10-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P10-INT-FIELD-2
           SET VALUE-PRESENT IN P10-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P10-INT-FIELD-3
           SET VALUE-PRESENT IN P10-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P10-INT-FIELD-4
           SET VALUE-PRESENT IN P10-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P10-INT-FIELD-5
           SET VALUE-PRESENT IN P10-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P10-INT-FIELD-6
           SET VALUE-PRESENT IN P10-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P10-INT-FIELD-7
           SET VALUE-PRESENT IN P10-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P10-INT-FIELD-8
           SET VALUE-PRESENT IN P10-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P10-INT-FIELD-9
           SET VALUE-PRESENT IN P10-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P10-INT-FIELD-10

           SET VALUE-PRESENT IN P10-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P10-STRING-FIELD-1
           SET VALUE-PRESENT IN P10-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P10-STRING-FIELD-2
           SET VALUE-PRESENT IN P10-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P10-STRING-FIELD-3
           SET VALUE-PRESENT IN P10-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P10-STRING-FIELD-4
           SET VALUE-PRESENT IN P10-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P10-STRING-FIELD-5
           SET VALUE-PRESENT IN P10-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P10-STRING-FIELD-6
           SET VALUE-PRESENT IN P10-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P10-STRING-FIELD-7
           SET VALUE-PRESENT IN P10-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P10-STRING-FIELD-8
           SET VALUE-PRESENT IN P10-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P10-STRING-FIELD-9
           SET VALUE-PRESENT IN P10-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P10-STRING-FIELD-10
             
           EXIT.
           
       CONVERT-STRUCTURE-SIZE-10 SECTION.
           MOVE 0 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-10
             BY REFERENCE CONSUMER-STRUCT-10                    
       
           EXIT.

       RUN-BENCHMARK-SIZE-25 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-25
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-25 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 25: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-25 SECTION.
           SET VALUE-PRESENT IN P25-TEST-STRUCT-25-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P25-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P25-INT-FIELD-1
           SET VALUE-PRESENT IN P25-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P25-INT-FIELD-2
           SET VALUE-PRESENT IN P25-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P25-INT-FIELD-3
           SET VALUE-PRESENT IN P25-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P25-INT-FIELD-4
           SET VALUE-PRESENT IN P25-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P25-INT-FIELD-5
           SET VALUE-PRESENT IN P25-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P25-INT-FIELD-6
           SET VALUE-PRESENT IN P25-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P25-INT-FIELD-7
           SET VALUE-PRESENT IN P25-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P25-INT-FIELD-8
           SET VALUE-PRESENT IN P25-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P25-INT-FIELD-9
           SET VALUE-PRESENT IN P25-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P25-INT-FIELD-10
           SET VALUE-PRESENT IN P25-INT-FIELD-11-FLAGS
            TO TRUE
           MOVE 11
             TO P25-INT-FIELD-11
           SET VALUE-PRESENT IN P25-INT-FIELD-12-FLAGS
            TO TRUE
           MOVE 12
             TO P25-INT-FIELD-12
           SET VALUE-PRESENT IN P25-INT-FIELD-13-FLAGS
            TO TRUE
           MOVE 13
             TO P25-INT-FIELD-13
           SET VALUE-PRESENT IN P25-INT-FIELD-14-FLAGS
            TO TRUE
           MOVE 14
             TO P25-INT-FIELD-14
           SET VALUE-PRESENT IN P25-INT-FIELD-15-FLAGS
            TO TRUE
           MOVE 15
             TO P25-INT-FIELD-15
           SET VALUE-PRESENT IN P25-INT-FIELD-16-FLAGS
            TO TRUE
           MOVE 16
             TO P25-INT-FIELD-16
           SET VALUE-PRESENT IN P25-INT-FIELD-17-FLAGS
            TO TRUE
           MOVE 17
             TO P25-INT-FIELD-17
           SET VALUE-PRESENT IN P25-INT-FIELD-18-FLAGS
            TO TRUE
           MOVE 18
             TO P25-INT-FIELD-18
           SET VALUE-PRESENT IN P25-INT-FIELD-19-FLAGS
            TO TRUE
           MOVE 19
             TO P25-INT-FIELD-19
           SET VALUE-PRESENT IN P25-INT-FIELD-20-FLAGS
            TO TRUE
           MOVE 20
             TO P25-INT-FIELD-20
           SET VALUE-PRESENT IN P25-INT-FIELD-21-FLAGS
            TO TRUE
           MOVE 21
             TO P25-INT-FIELD-21
           SET VALUE-PRESENT IN P25-INT-FIELD-22-FLAGS
            TO TRUE
           MOVE 22
             TO P25-INT-FIELD-22
           SET VALUE-PRESENT IN P25-INT-FIELD-23-FLAGS
            TO TRUE
           MOVE 23
             TO P25-INT-FIELD-23
           SET VALUE-PRESENT IN P25-INT-FIELD-24-FLAGS
            TO TRUE
           MOVE 24
             TO P25-INT-FIELD-24
           SET VALUE-PRESENT IN P25-INT-FIELD-25-FLAGS
            TO TRUE
           MOVE 25
             TO P25-INT-FIELD-25

           SET VALUE-PRESENT IN P25-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P25-STRING-FIELD-1
           SET VALUE-PRESENT IN P25-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P25-STRING-FIELD-2
           SET VALUE-PRESENT IN P25-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P25-STRING-FIELD-3
           SET VALUE-PRESENT IN P25-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P25-STRING-FIELD-4
           SET VALUE-PRESENT IN P25-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P25-STRING-FIELD-5
           SET VALUE-PRESENT IN P25-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P25-STRING-FIELD-6
           SET VALUE-PRESENT IN P25-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P25-STRING-FIELD-7
           SET VALUE-PRESENT IN P25-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P25-STRING-FIELD-8
           SET VALUE-PRESENT IN P25-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P25-STRING-FIELD-9
           SET VALUE-PRESENT IN P25-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P25-STRING-FIELD-10
           SET VALUE-PRESENT IN P25-STRING-FIELD-11-FLAGS
            TO TRUE
           MOVE '11'
             TO P25-STRING-FIELD-11
           SET VALUE-PRESENT IN P25-STRING-FIELD-12-FLAGS
            TO TRUE
           MOVE '12'
             TO P25-STRING-FIELD-12
           SET VALUE-PRESENT IN P25-STRING-FIELD-13-FLAGS
            TO TRUE
           MOVE '13'
             TO P25-STRING-FIELD-13
           SET VALUE-PRESENT IN P25-STRING-FIELD-14-FLAGS
            TO TRUE
           MOVE '14'
             TO P25-STRING-FIELD-14
           SET VALUE-PRESENT IN P25-STRING-FIELD-15-FLAGS
            TO TRUE
           MOVE '15'
             TO P25-STRING-FIELD-15
           SET VALUE-PRESENT IN P25-STRING-FIELD-16-FLAGS
            TO TRUE
           MOVE '16'
             TO P25-STRING-FIELD-16
           SET VALUE-PRESENT IN P25-STRING-FIELD-17-FLAGS
            TO TRUE
           MOVE '17'
             TO P25-STRING-FIELD-17
           SET VALUE-PRESENT IN P25-STRING-FIELD-18-FLAGS
            TO TRUE
           MOVE '18'
             TO P25-STRING-FIELD-18
           SET VALUE-PRESENT IN P25-STRING-FIELD-19-FLAGS
            TO TRUE
           MOVE '19'
             TO P25-STRING-FIELD-19
           SET VALUE-PRESENT IN P25-STRING-FIELD-20-FLAGS
            TO TRUE
           MOVE '20'
             TO P25-STRING-FIELD-20
           SET VALUE-PRESENT IN P25-STRING-FIELD-21-FLAGS
            TO TRUE
           MOVE '21'
             TO P25-STRING-FIELD-21
           SET VALUE-PRESENT IN P25-STRING-FIELD-22-FLAGS
            TO TRUE
           MOVE '22'
             TO P25-STRING-FIELD-22
           SET VALUE-PRESENT IN P25-STRING-FIELD-23-FLAGS
            TO TRUE
           MOVE '23'
             TO P25-STRING-FIELD-23
           SET VALUE-PRESENT IN P25-STRING-FIELD-24-FLAGS
            TO TRUE
           MOVE '24'
             TO P25-STRING-FIELD-24
           SET VALUE-PRESENT IN P25-STRING-FIELD-25-FLAGS
            TO TRUE
           MOVE '25'
             TO P25-STRING-FIELD-25
             
           EXIT.

       CONVERT-STRUCTURE-SIZE-25 SECTION.
           MOVE 2 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-25
             BY REFERENCE CONSUMER-STRUCT-25                   
       
           EXIT.
           
       RUN-BENCHMARK-SIZE-50 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-50
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-50 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 50: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-50 SECTION.
           SET VALUE-PRESENT IN P50-TEST-STRUCT-50-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P50-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P50-INT-FIELD-1
           SET VALUE-PRESENT IN P50-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P50-INT-FIELD-2
           SET VALUE-PRESENT IN P50-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P50-INT-FIELD-3
           SET VALUE-PRESENT IN P50-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P50-INT-FIELD-4
           SET VALUE-PRESENT IN P50-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P50-INT-FIELD-5
           SET VALUE-PRESENT IN P50-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P50-INT-FIELD-6
           SET VALUE-PRESENT IN P50-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P50-INT-FIELD-7
           SET VALUE-PRESENT IN P50-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P50-INT-FIELD-8
           SET VALUE-PRESENT IN P50-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P50-INT-FIELD-9
           SET VALUE-PRESENT IN P50-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P50-INT-FIELD-10
           SET VALUE-PRESENT IN P50-INT-FIELD-11-FLAGS
            TO TRUE
           MOVE 11
             TO P50-INT-FIELD-11
           SET VALUE-PRESENT IN P50-INT-FIELD-12-FLAGS
            TO TRUE
           MOVE 12
             TO P50-INT-FIELD-12
           SET VALUE-PRESENT IN P50-INT-FIELD-13-FLAGS
            TO TRUE
           MOVE 13
             TO P50-INT-FIELD-13
           SET VALUE-PRESENT IN P50-INT-FIELD-14-FLAGS
            TO TRUE
           MOVE 14
             TO P50-INT-FIELD-14
           SET VALUE-PRESENT IN P50-INT-FIELD-15-FLAGS
            TO TRUE
           MOVE 15
             TO P50-INT-FIELD-15
           SET VALUE-PRESENT IN P50-INT-FIELD-16-FLAGS
            TO TRUE
           MOVE 16
             TO P50-INT-FIELD-16
           SET VALUE-PRESENT IN P50-INT-FIELD-17-FLAGS
            TO TRUE
           MOVE 17
             TO P50-INT-FIELD-17
           SET VALUE-PRESENT IN P50-INT-FIELD-18-FLAGS
            TO TRUE
           MOVE 18
             TO P50-INT-FIELD-18
           SET VALUE-PRESENT IN P50-INT-FIELD-19-FLAGS
            TO TRUE
           MOVE 19
             TO P50-INT-FIELD-19
           SET VALUE-PRESENT IN P50-INT-FIELD-20-FLAGS
            TO TRUE
           MOVE 20
             TO P50-INT-FIELD-20
           SET VALUE-PRESENT IN P50-INT-FIELD-21-FLAGS
            TO TRUE
           MOVE 21
             TO P50-INT-FIELD-21
           SET VALUE-PRESENT IN P50-INT-FIELD-22-FLAGS
            TO TRUE
           MOVE 22
             TO P50-INT-FIELD-22
           SET VALUE-PRESENT IN P50-INT-FIELD-23-FLAGS
            TO TRUE
           MOVE 23
             TO P50-INT-FIELD-23
           SET VALUE-PRESENT IN P50-INT-FIELD-24-FLAGS
            TO TRUE
           MOVE 24
             TO P50-INT-FIELD-24
           SET VALUE-PRESENT IN P50-INT-FIELD-25-FLAGS
            TO TRUE
           MOVE 25
             TO P50-INT-FIELD-25
           SET VALUE-PRESENT IN P50-INT-FIELD-26-FLAGS
            TO TRUE
           MOVE 26
             TO P50-INT-FIELD-26
           SET VALUE-PRESENT IN P50-INT-FIELD-27-FLAGS
            TO TRUE
           MOVE 27
             TO P50-INT-FIELD-27
           SET VALUE-PRESENT IN P50-INT-FIELD-28-FLAGS
            TO TRUE
           MOVE 28
             TO P50-INT-FIELD-28
           SET VALUE-PRESENT IN P50-INT-FIELD-29-FLAGS
            TO TRUE
           MOVE 29
             TO P50-INT-FIELD-29
           SET VALUE-PRESENT IN P50-INT-FIELD-30-FLAGS
            TO TRUE
           MOVE 30
             TO P50-INT-FIELD-30
           SET VALUE-PRESENT IN P50-INT-FIELD-31-FLAGS
            TO TRUE
           MOVE 31
             TO P50-INT-FIELD-31
           SET VALUE-PRESENT IN P50-INT-FIELD-32-FLAGS
            TO TRUE
           MOVE 32
             TO P50-INT-FIELD-32
           SET VALUE-PRESENT IN P50-INT-FIELD-33-FLAGS
            TO TRUE
           MOVE 33
             TO P50-INT-FIELD-33
           SET VALUE-PRESENT IN P50-INT-FIELD-34-FLAGS
            TO TRUE
           MOVE 34
             TO P50-INT-FIELD-34
           SET VALUE-PRESENT IN P50-INT-FIELD-35-FLAGS
            TO TRUE
           MOVE 35
             TO P50-INT-FIELD-35
           SET VALUE-PRESENT IN P50-INT-FIELD-36-FLAGS
            TO TRUE
           MOVE 36
             TO P50-INT-FIELD-36
           SET VALUE-PRESENT IN P50-INT-FIELD-37-FLAGS
            TO TRUE
           MOVE 37
             TO P50-INT-FIELD-37
           SET VALUE-PRESENT IN P50-INT-FIELD-38-FLAGS
            TO TRUE
           MOVE 38
             TO P50-INT-FIELD-38
           SET VALUE-PRESENT IN P50-INT-FIELD-39-FLAGS
            TO TRUE
           MOVE 39
             TO P50-INT-FIELD-39
           SET VALUE-PRESENT IN P50-INT-FIELD-40-FLAGS
            TO TRUE
           MOVE 40
             TO P50-INT-FIELD-40
           SET VALUE-PRESENT IN P50-INT-FIELD-41-FLAGS
            TO TRUE
           MOVE 41
             TO P50-INT-FIELD-41
           SET VALUE-PRESENT IN P50-INT-FIELD-42-FLAGS
            TO TRUE
           MOVE 42
             TO P50-INT-FIELD-42
           SET VALUE-PRESENT IN P50-INT-FIELD-43-FLAGS
            TO TRUE
           MOVE 43
             TO P50-INT-FIELD-43
           SET VALUE-PRESENT IN P50-INT-FIELD-44-FLAGS
            TO TRUE
           MOVE 44
             TO P50-INT-FIELD-44
           SET VALUE-PRESENT IN P50-INT-FIELD-45-FLAGS
            TO TRUE
           MOVE 45
             TO P50-INT-FIELD-45
           SET VALUE-PRESENT IN P50-INT-FIELD-46-FLAGS
            TO TRUE
           MOVE 46
             TO P50-INT-FIELD-46
           SET VALUE-PRESENT IN P50-INT-FIELD-47-FLAGS
            TO TRUE
           MOVE 47
             TO P50-INT-FIELD-47
           SET VALUE-PRESENT IN P50-INT-FIELD-48-FLAGS
            TO TRUE
           MOVE 48
             TO P50-INT-FIELD-48
           SET VALUE-PRESENT IN P50-INT-FIELD-49-FLAGS
            TO TRUE
           MOVE 49
             TO P50-INT-FIELD-49
           SET VALUE-PRESENT IN P50-INT-FIELD-50-FLAGS
            TO TRUE
           MOVE 50
             TO P50-INT-FIELD-50

           SET VALUE-PRESENT IN P50-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P50-STRING-FIELD-1
           SET VALUE-PRESENT IN P50-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P50-STRING-FIELD-2
           SET VALUE-PRESENT IN P50-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P50-STRING-FIELD-3
           SET VALUE-PRESENT IN P50-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P50-STRING-FIELD-4
           SET VALUE-PRESENT IN P50-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P50-STRING-FIELD-5
           SET VALUE-PRESENT IN P50-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P50-STRING-FIELD-6
           SET VALUE-PRESENT IN P50-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P50-STRING-FIELD-7
           SET VALUE-PRESENT IN P50-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P50-STRING-FIELD-8
           SET VALUE-PRESENT IN P50-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P50-STRING-FIELD-9
           SET VALUE-PRESENT IN P50-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P50-STRING-FIELD-10
           SET VALUE-PRESENT IN P50-STRING-FIELD-11-FLAGS
            TO TRUE
           MOVE '11'
             TO P50-STRING-FIELD-11
           SET VALUE-PRESENT IN P50-STRING-FIELD-12-FLAGS
            TO TRUE
           MOVE '12'
             TO P50-STRING-FIELD-12
           SET VALUE-PRESENT IN P50-STRING-FIELD-13-FLAGS
            TO TRUE
           MOVE '13'
             TO P50-STRING-FIELD-13
           SET VALUE-PRESENT IN P50-STRING-FIELD-14-FLAGS
            TO TRUE
           MOVE '14'
             TO P50-STRING-FIELD-14
           SET VALUE-PRESENT IN P50-STRING-FIELD-15-FLAGS
            TO TRUE
           MOVE '15'
             TO P50-STRING-FIELD-15
           SET VALUE-PRESENT IN P50-STRING-FIELD-16-FLAGS
            TO TRUE
           MOVE '16'
             TO P50-STRING-FIELD-16
           SET VALUE-PRESENT IN P50-STRING-FIELD-17-FLAGS
            TO TRUE
           MOVE '17'
             TO P50-STRING-FIELD-17
           SET VALUE-PRESENT IN P50-STRING-FIELD-18-FLAGS
            TO TRUE
           MOVE '18'
             TO P50-STRING-FIELD-18
           SET VALUE-PRESENT IN P50-STRING-FIELD-19-FLAGS
            TO TRUE
           MOVE '19'
             TO P50-STRING-FIELD-19
           SET VALUE-PRESENT IN P50-STRING-FIELD-20-FLAGS
            TO TRUE
           MOVE '20'
             TO P50-STRING-FIELD-20
           SET VALUE-PRESENT IN P50-STRING-FIELD-21-FLAGS
            TO TRUE
           MOVE '21'
             TO P50-STRING-FIELD-21
           SET VALUE-PRESENT IN P50-STRING-FIELD-22-FLAGS
            TO TRUE
           MOVE '22'
             TO P50-STRING-FIELD-22
           SET VALUE-PRESENT IN P50-STRING-FIELD-23-FLAGS
            TO TRUE
           MOVE '23'
             TO P50-STRING-FIELD-23
           SET VALUE-PRESENT IN P50-STRING-FIELD-24-FLAGS
            TO TRUE
           MOVE '24'
             TO P50-STRING-FIELD-24
           SET VALUE-PRESENT IN P50-STRING-FIELD-25-FLAGS
            TO TRUE
           MOVE '25'
             TO P50-STRING-FIELD-25
           SET VALUE-PRESENT IN P50-STRING-FIELD-26-FLAGS
            TO TRUE
           MOVE '26'
             TO P50-STRING-FIELD-26
           SET VALUE-PRESENT IN P50-STRING-FIELD-27-FLAGS
            TO TRUE
           MOVE '27'
             TO P50-STRING-FIELD-27
           SET VALUE-PRESENT IN P50-STRING-FIELD-28-FLAGS
            TO TRUE
           MOVE '28'
             TO P50-STRING-FIELD-28
           SET VALUE-PRESENT IN P50-STRING-FIELD-29-FLAGS
            TO TRUE
           MOVE '29'
             TO P50-STRING-FIELD-29
           SET VALUE-PRESENT IN P50-STRING-FIELD-30-FLAGS
            TO TRUE
           MOVE '30'
             TO P50-STRING-FIELD-30
           SET VALUE-PRESENT IN P50-STRING-FIELD-31-FLAGS
            TO TRUE
           MOVE '31'
             TO P50-STRING-FIELD-31
           SET VALUE-PRESENT IN P50-STRING-FIELD-32-FLAGS
            TO TRUE
           MOVE '32'
             TO P50-STRING-FIELD-32
           SET VALUE-PRESENT IN P50-STRING-FIELD-33-FLAGS
            TO TRUE
           MOVE '33'
             TO P50-STRING-FIELD-33
           SET VALUE-PRESENT IN P50-STRING-FIELD-34-FLAGS
            TO TRUE
           MOVE '34'
             TO P50-STRING-FIELD-34
           SET VALUE-PRESENT IN P50-STRING-FIELD-35-FLAGS
            TO TRUE
           MOVE '35'
             TO P50-STRING-FIELD-35
           SET VALUE-PRESENT IN P50-STRING-FIELD-36-FLAGS
            TO TRUE
           MOVE '36'
             TO P50-STRING-FIELD-36
           SET VALUE-PRESENT IN P50-STRING-FIELD-37-FLAGS
            TO TRUE
           MOVE '37'
             TO P50-STRING-FIELD-37
           SET VALUE-PRESENT IN P50-STRING-FIELD-38-FLAGS
            TO TRUE
           MOVE '38'
             TO P50-STRING-FIELD-38
           SET VALUE-PRESENT IN P50-STRING-FIELD-39-FLAGS
            TO TRUE
           MOVE '39'
             TO P50-STRING-FIELD-39
           SET VALUE-PRESENT IN P50-STRING-FIELD-40-FLAGS
            TO TRUE
           MOVE '40'
             TO P50-STRING-FIELD-40
           SET VALUE-PRESENT IN P50-STRING-FIELD-41-FLAGS
            TO TRUE
           MOVE '41'
             TO P50-STRING-FIELD-41
           SET VALUE-PRESENT IN P50-STRING-FIELD-42-FLAGS
            TO TRUE
           MOVE '42'
             TO P50-STRING-FIELD-42
           SET VALUE-PRESENT IN P50-STRING-FIELD-43-FLAGS
            TO TRUE
           MOVE '43'
             TO P50-STRING-FIELD-43
           SET VALUE-PRESENT IN P50-STRING-FIELD-44-FLAGS
            TO TRUE
           MOVE '44'
             TO P50-STRING-FIELD-44
           SET VALUE-PRESENT IN P50-STRING-FIELD-45-FLAGS
            TO TRUE
           MOVE '45'
             TO P50-STRING-FIELD-45
           SET VALUE-PRESENT IN P50-STRING-FIELD-46-FLAGS
            TO TRUE
           MOVE '46'
             TO P50-STRING-FIELD-46
           SET VALUE-PRESENT IN P50-STRING-FIELD-47-FLAGS
            TO TRUE
           MOVE '47'
             TO P50-STRING-FIELD-47
           SET VALUE-PRESENT IN P50-STRING-FIELD-48-FLAGS
            TO TRUE
           MOVE '48'
             TO P50-STRING-FIELD-48
           SET VALUE-PRESENT IN P50-STRING-FIELD-49-FLAGS
            TO TRUE
           MOVE '49'
             TO P50-STRING-FIELD-49
           SET VALUE-PRESENT IN P50-STRING-FIELD-50-FLAGS
            TO TRUE
           MOVE '50'
             TO P50-STRING-FIELD-50       
             
           EXIT.

       CONVERT-STRUCTURE-SIZE-50 SECTION.
           MOVE 4 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-50
             BY REFERENCE CONSUMER-STRUCT-50                    
       
           EXIT.

       RUN-BENCHMARK-SIZE-75 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-75
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-75 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 75: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-75 SECTION.
           SET VALUE-PRESENT IN P75-TEST-STRUCT-75-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P75-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P75-INT-FIELD-1
           SET VALUE-PRESENT IN P75-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P75-INT-FIELD-2
           SET VALUE-PRESENT IN P75-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P75-INT-FIELD-3
           SET VALUE-PRESENT IN P75-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P75-INT-FIELD-4
           SET VALUE-PRESENT IN P75-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P75-INT-FIELD-5
           SET VALUE-PRESENT IN P75-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P75-INT-FIELD-6
           SET VALUE-PRESENT IN P75-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P75-INT-FIELD-7
           SET VALUE-PRESENT IN P75-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P75-INT-FIELD-8
           SET VALUE-PRESENT IN P75-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P75-INT-FIELD-9
           SET VALUE-PRESENT IN P75-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P75-INT-FIELD-10
           SET VALUE-PRESENT IN P75-INT-FIELD-11-FLAGS
            TO TRUE
           MOVE 11
             TO P75-INT-FIELD-11
           SET VALUE-PRESENT IN P75-INT-FIELD-12-FLAGS
            TO TRUE
           MOVE 12
             TO P75-INT-FIELD-12
           SET VALUE-PRESENT IN P75-INT-FIELD-13-FLAGS
            TO TRUE
           MOVE 13
             TO P75-INT-FIELD-13
           SET VALUE-PRESENT IN P75-INT-FIELD-14-FLAGS
            TO TRUE
           MOVE 14
             TO P75-INT-FIELD-14
           SET VALUE-PRESENT IN P75-INT-FIELD-15-FLAGS
            TO TRUE
           MOVE 15
             TO P75-INT-FIELD-15
           SET VALUE-PRESENT IN P75-INT-FIELD-16-FLAGS
            TO TRUE
           MOVE 16
             TO P75-INT-FIELD-16
           SET VALUE-PRESENT IN P75-INT-FIELD-17-FLAGS
            TO TRUE
           MOVE 17
             TO P75-INT-FIELD-17
           SET VALUE-PRESENT IN P75-INT-FIELD-18-FLAGS
            TO TRUE
           MOVE 18
             TO P75-INT-FIELD-18
           SET VALUE-PRESENT IN P75-INT-FIELD-19-FLAGS
            TO TRUE
           MOVE 19
             TO P75-INT-FIELD-19
           SET VALUE-PRESENT IN P75-INT-FIELD-20-FLAGS
            TO TRUE
           MOVE 20
             TO P75-INT-FIELD-20
           SET VALUE-PRESENT IN P75-INT-FIELD-21-FLAGS
            TO TRUE
           MOVE 21
             TO P75-INT-FIELD-21
           SET VALUE-PRESENT IN P75-INT-FIELD-22-FLAGS
            TO TRUE
           MOVE 22
             TO P75-INT-FIELD-22
           SET VALUE-PRESENT IN P75-INT-FIELD-23-FLAGS
            TO TRUE
           MOVE 23
             TO P75-INT-FIELD-23
           SET VALUE-PRESENT IN P75-INT-FIELD-24-FLAGS
            TO TRUE
           MOVE 24
             TO P75-INT-FIELD-24
           SET VALUE-PRESENT IN P75-INT-FIELD-25-FLAGS
            TO TRUE
           MOVE 25
             TO P75-INT-FIELD-25
           SET VALUE-PRESENT IN P75-INT-FIELD-26-FLAGS
            TO TRUE
           MOVE 26
             TO P75-INT-FIELD-26
           SET VALUE-PRESENT IN P75-INT-FIELD-27-FLAGS
            TO TRUE
           MOVE 27
             TO P75-INT-FIELD-27
           SET VALUE-PRESENT IN P75-INT-FIELD-28-FLAGS
            TO TRUE
           MOVE 28
             TO P75-INT-FIELD-28
           SET VALUE-PRESENT IN P75-INT-FIELD-29-FLAGS
            TO TRUE
           MOVE 29
             TO P75-INT-FIELD-29
           SET VALUE-PRESENT IN P75-INT-FIELD-30-FLAGS
            TO TRUE
           MOVE 30
             TO P75-INT-FIELD-30
           SET VALUE-PRESENT IN P75-INT-FIELD-31-FLAGS
            TO TRUE
           MOVE 31
             TO P75-INT-FIELD-31
           SET VALUE-PRESENT IN P75-INT-FIELD-32-FLAGS
            TO TRUE
           MOVE 32
             TO P75-INT-FIELD-32
           SET VALUE-PRESENT IN P75-INT-FIELD-33-FLAGS
            TO TRUE
           MOVE 33
             TO P75-INT-FIELD-33
           SET VALUE-PRESENT IN P75-INT-FIELD-34-FLAGS
            TO TRUE
           MOVE 34
             TO P75-INT-FIELD-34
           SET VALUE-PRESENT IN P75-INT-FIELD-35-FLAGS
            TO TRUE
           MOVE 35
             TO P75-INT-FIELD-35
           SET VALUE-PRESENT IN P75-INT-FIELD-36-FLAGS
            TO TRUE
           MOVE 36
             TO P75-INT-FIELD-36
           SET VALUE-PRESENT IN P75-INT-FIELD-37-FLAGS
            TO TRUE
           MOVE 37
             TO P75-INT-FIELD-37
           SET VALUE-PRESENT IN P75-INT-FIELD-38-FLAGS
            TO TRUE
           MOVE 38
             TO P75-INT-FIELD-38
           SET VALUE-PRESENT IN P75-INT-FIELD-39-FLAGS
            TO TRUE
           MOVE 39
             TO P75-INT-FIELD-39
           SET VALUE-PRESENT IN P75-INT-FIELD-40-FLAGS
            TO TRUE
           MOVE 40
             TO P75-INT-FIELD-40
           SET VALUE-PRESENT IN P75-INT-FIELD-41-FLAGS
            TO TRUE
           MOVE 41
             TO P75-INT-FIELD-41
           SET VALUE-PRESENT IN P75-INT-FIELD-42-FLAGS
            TO TRUE
           MOVE 42
             TO P75-INT-FIELD-42
           SET VALUE-PRESENT IN P75-INT-FIELD-43-FLAGS
            TO TRUE
           MOVE 43
             TO P75-INT-FIELD-43
           SET VALUE-PRESENT IN P75-INT-FIELD-44-FLAGS
            TO TRUE
           MOVE 44
             TO P75-INT-FIELD-44
           SET VALUE-PRESENT IN P75-INT-FIELD-45-FLAGS
            TO TRUE
           MOVE 45
             TO P75-INT-FIELD-45
           SET VALUE-PRESENT IN P75-INT-FIELD-46-FLAGS
            TO TRUE
           MOVE 46
             TO P75-INT-FIELD-46
           SET VALUE-PRESENT IN P75-INT-FIELD-47-FLAGS
            TO TRUE
           MOVE 47
             TO P75-INT-FIELD-47
           SET VALUE-PRESENT IN P75-INT-FIELD-48-FLAGS
            TO TRUE
           MOVE 48
             TO P75-INT-FIELD-48
           SET VALUE-PRESENT IN P75-INT-FIELD-49-FLAGS
            TO TRUE
           MOVE 49
             TO P75-INT-FIELD-49
           SET VALUE-PRESENT IN P75-INT-FIELD-50-FLAGS
            TO TRUE
           MOVE 50
             TO P75-INT-FIELD-50
           SET VALUE-PRESENT IN P75-INT-FIELD-51-FLAGS
            TO TRUE
           MOVE 51
             TO P75-INT-FIELD-51
           SET VALUE-PRESENT IN P75-INT-FIELD-52-FLAGS
            TO TRUE
           MOVE 52
             TO P75-INT-FIELD-52
           SET VALUE-PRESENT IN P75-INT-FIELD-53-FLAGS
            TO TRUE
           MOVE 53
             TO P75-INT-FIELD-53
           SET VALUE-PRESENT IN P75-INT-FIELD-54-FLAGS
            TO TRUE
           MOVE 54
             TO P75-INT-FIELD-54
           SET VALUE-PRESENT IN P75-INT-FIELD-55-FLAGS
            TO TRUE
           MOVE 55
             TO P75-INT-FIELD-55
           SET VALUE-PRESENT IN P75-INT-FIELD-56-FLAGS
            TO TRUE
           MOVE 56
             TO P75-INT-FIELD-56
           SET VALUE-PRESENT IN P75-INT-FIELD-57-FLAGS
            TO TRUE
           MOVE 57
             TO P75-INT-FIELD-57
           SET VALUE-PRESENT IN P75-INT-FIELD-58-FLAGS
            TO TRUE
           MOVE 58
             TO P75-INT-FIELD-58
           SET VALUE-PRESENT IN P75-INT-FIELD-59-FLAGS
            TO TRUE
           MOVE 59
             TO P75-INT-FIELD-59
           SET VALUE-PRESENT IN P75-INT-FIELD-60-FLAGS
            TO TRUE
           MOVE 60
             TO P75-INT-FIELD-60
           SET VALUE-PRESENT IN P75-INT-FIELD-61-FLAGS
            TO TRUE
           MOVE 61
             TO P75-INT-FIELD-61
           SET VALUE-PRESENT IN P75-INT-FIELD-62-FLAGS
            TO TRUE
           MOVE 62
             TO P75-INT-FIELD-62
           SET VALUE-PRESENT IN P75-INT-FIELD-63-FLAGS
            TO TRUE
           MOVE 63
             TO P75-INT-FIELD-63
           SET VALUE-PRESENT IN P75-INT-FIELD-64-FLAGS
            TO TRUE
           MOVE 64
             TO P75-INT-FIELD-64
           SET VALUE-PRESENT IN P75-INT-FIELD-65-FLAGS
            TO TRUE
           MOVE 65
             TO P75-INT-FIELD-65
           SET VALUE-PRESENT IN P75-INT-FIELD-66-FLAGS
            TO TRUE
           MOVE 66
             TO P75-INT-FIELD-66
           SET VALUE-PRESENT IN P75-INT-FIELD-67-FLAGS
            TO TRUE
           MOVE 67
             TO P75-INT-FIELD-67
           SET VALUE-PRESENT IN P75-INT-FIELD-68-FLAGS
            TO TRUE
           MOVE 68
             TO P75-INT-FIELD-68
           SET VALUE-PRESENT IN P75-INT-FIELD-69-FLAGS
            TO TRUE
           MOVE 69
             TO P75-INT-FIELD-69
           SET VALUE-PRESENT IN P75-INT-FIELD-70-FLAGS
            TO TRUE
           MOVE 70
             TO P75-INT-FIELD-70
           SET VALUE-PRESENT IN P75-INT-FIELD-71-FLAGS
            TO TRUE
           MOVE 71
             TO P75-INT-FIELD-71
           SET VALUE-PRESENT IN P75-INT-FIELD-72-FLAGS
            TO TRUE
           MOVE 72
             TO P75-INT-FIELD-72
           SET VALUE-PRESENT IN P75-INT-FIELD-73-FLAGS
            TO TRUE
           MOVE 73
             TO P75-INT-FIELD-73
           SET VALUE-PRESENT IN P75-INT-FIELD-74-FLAGS
            TO TRUE
           MOVE 74
             TO P75-INT-FIELD-74
           SET VALUE-PRESENT IN P75-INT-FIELD-75-FLAGS
            TO TRUE
           MOVE 75
             TO P75-INT-FIELD-75

           SET VALUE-PRESENT IN P75-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P75-STRING-FIELD-1
           SET VALUE-PRESENT IN P75-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P75-STRING-FIELD-2
           SET VALUE-PRESENT IN P75-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P75-STRING-FIELD-3
           SET VALUE-PRESENT IN P75-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P75-STRING-FIELD-4
           SET VALUE-PRESENT IN P75-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P75-STRING-FIELD-5
           SET VALUE-PRESENT IN P75-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P75-STRING-FIELD-6
           SET VALUE-PRESENT IN P75-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P75-STRING-FIELD-7
           SET VALUE-PRESENT IN P75-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P75-STRING-FIELD-8
           SET VALUE-PRESENT IN P75-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P75-STRING-FIELD-9
           SET VALUE-PRESENT IN P75-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P75-STRING-FIELD-10
           SET VALUE-PRESENT IN P75-STRING-FIELD-11-FLAGS
            TO TRUE
           MOVE '11'
             TO P75-STRING-FIELD-11
           SET VALUE-PRESENT IN P75-STRING-FIELD-12-FLAGS
            TO TRUE
           MOVE '12'
             TO P75-STRING-FIELD-12
           SET VALUE-PRESENT IN P75-STRING-FIELD-13-FLAGS
            TO TRUE
           MOVE '13'
             TO P75-STRING-FIELD-13
           SET VALUE-PRESENT IN P75-STRING-FIELD-14-FLAGS
            TO TRUE
           MOVE '14'
             TO P75-STRING-FIELD-14
           SET VALUE-PRESENT IN P75-STRING-FIELD-15-FLAGS
            TO TRUE
           MOVE '15'
             TO P75-STRING-FIELD-15
           SET VALUE-PRESENT IN P75-STRING-FIELD-16-FLAGS
            TO TRUE
           MOVE '16'
             TO P75-STRING-FIELD-16
           SET VALUE-PRESENT IN P75-STRING-FIELD-17-FLAGS
            TO TRUE
           MOVE '17'
             TO P75-STRING-FIELD-17
           SET VALUE-PRESENT IN P75-STRING-FIELD-18-FLAGS
            TO TRUE
           MOVE '18'
             TO P75-STRING-FIELD-18
           SET VALUE-PRESENT IN P75-STRING-FIELD-19-FLAGS
            TO TRUE
           MOVE '19'
             TO P75-STRING-FIELD-19
           SET VALUE-PRESENT IN P75-STRING-FIELD-20-FLAGS
            TO TRUE
           MOVE '20'
             TO P75-STRING-FIELD-20
           SET VALUE-PRESENT IN P75-STRING-FIELD-21-FLAGS
            TO TRUE
           MOVE '21'
             TO P75-STRING-FIELD-21
           SET VALUE-PRESENT IN P75-STRING-FIELD-22-FLAGS
            TO TRUE
           MOVE '22'
             TO P75-STRING-FIELD-22
           SET VALUE-PRESENT IN P75-STRING-FIELD-23-FLAGS
            TO TRUE
           MOVE '23'
             TO P75-STRING-FIELD-23
           SET VALUE-PRESENT IN P75-STRING-FIELD-24-FLAGS
            TO TRUE
           MOVE '24'
             TO P75-STRING-FIELD-24
           SET VALUE-PRESENT IN P75-STRING-FIELD-25-FLAGS
            TO TRUE
           MOVE '25'
             TO P75-STRING-FIELD-25
           SET VALUE-PRESENT IN P75-STRING-FIELD-26-FLAGS
            TO TRUE
           MOVE '26'
             TO P75-STRING-FIELD-26
           SET VALUE-PRESENT IN P75-STRING-FIELD-27-FLAGS
            TO TRUE
           MOVE '27'
             TO P75-STRING-FIELD-27
           SET VALUE-PRESENT IN P75-STRING-FIELD-28-FLAGS
            TO TRUE
           MOVE '28'
             TO P75-STRING-FIELD-28
           SET VALUE-PRESENT IN P75-STRING-FIELD-29-FLAGS
            TO TRUE
           MOVE '29'
             TO P75-STRING-FIELD-29
           SET VALUE-PRESENT IN P75-STRING-FIELD-30-FLAGS
            TO TRUE
           MOVE '30'
             TO P75-STRING-FIELD-30
           SET VALUE-PRESENT IN P75-STRING-FIELD-31-FLAGS
            TO TRUE
           MOVE '31'
             TO P75-STRING-FIELD-31
           SET VALUE-PRESENT IN P75-STRING-FIELD-32-FLAGS
            TO TRUE
           MOVE '32'
             TO P75-STRING-FIELD-32
           SET VALUE-PRESENT IN P75-STRING-FIELD-33-FLAGS
            TO TRUE
           MOVE '33'
             TO P75-STRING-FIELD-33
           SET VALUE-PRESENT IN P75-STRING-FIELD-34-FLAGS
            TO TRUE
           MOVE '34'
             TO P75-STRING-FIELD-34
           SET VALUE-PRESENT IN P75-STRING-FIELD-35-FLAGS
            TO TRUE
           MOVE '35'
             TO P75-STRING-FIELD-35
           SET VALUE-PRESENT IN P75-STRING-FIELD-36-FLAGS
            TO TRUE
           MOVE '36'
             TO P75-STRING-FIELD-36
           SET VALUE-PRESENT IN P75-STRING-FIELD-37-FLAGS
            TO TRUE
           MOVE '37'
             TO P75-STRING-FIELD-37
           SET VALUE-PRESENT IN P75-STRING-FIELD-38-FLAGS
            TO TRUE
           MOVE '38'
             TO P75-STRING-FIELD-38
           SET VALUE-PRESENT IN P75-STRING-FIELD-39-FLAGS
            TO TRUE
           MOVE '39'
             TO P75-STRING-FIELD-39
           SET VALUE-PRESENT IN P75-STRING-FIELD-40-FLAGS
            TO TRUE
           MOVE '40'
             TO P75-STRING-FIELD-40
           SET VALUE-PRESENT IN P75-STRING-FIELD-41-FLAGS
            TO TRUE
           MOVE '41'
             TO P75-STRING-FIELD-41
           SET VALUE-PRESENT IN P75-STRING-FIELD-42-FLAGS
            TO TRUE
           MOVE '42'
             TO P75-STRING-FIELD-42
           SET VALUE-PRESENT IN P75-STRING-FIELD-43-FLAGS
            TO TRUE
           MOVE '43'
             TO P75-STRING-FIELD-43
           SET VALUE-PRESENT IN P75-STRING-FIELD-44-FLAGS
            TO TRUE
           MOVE '44'
             TO P75-STRING-FIELD-44
           SET VALUE-PRESENT IN P75-STRING-FIELD-45-FLAGS
            TO TRUE
           MOVE '45'
             TO P75-STRING-FIELD-45
           SET VALUE-PRESENT IN P75-STRING-FIELD-46-FLAGS
            TO TRUE
           MOVE '46'
             TO P75-STRING-FIELD-46
           SET VALUE-PRESENT IN P75-STRING-FIELD-47-FLAGS
            TO TRUE
           MOVE '47'
             TO P75-STRING-FIELD-47
           SET VALUE-PRESENT IN P75-STRING-FIELD-48-FLAGS
            TO TRUE
           MOVE '48'
             TO P75-STRING-FIELD-48
           SET VALUE-PRESENT IN P75-STRING-FIELD-49-FLAGS
            TO TRUE
           MOVE '49'
             TO P75-STRING-FIELD-49
           SET VALUE-PRESENT IN P75-STRING-FIELD-50-FLAGS
            TO TRUE
           MOVE '50'
             TO P75-STRING-FIELD-50
           SET VALUE-PRESENT IN P75-STRING-FIELD-51-FLAGS
            TO TRUE
           MOVE '51'
             TO P75-STRING-FIELD-51
           SET VALUE-PRESENT IN P75-STRING-FIELD-52-FLAGS
            TO TRUE
           MOVE '52'
             TO P75-STRING-FIELD-52
           SET VALUE-PRESENT IN P75-STRING-FIELD-53-FLAGS
            TO TRUE
           MOVE '53'
             TO P75-STRING-FIELD-53
           SET VALUE-PRESENT IN P75-STRING-FIELD-54-FLAGS
            TO TRUE
           MOVE '54'
             TO P75-STRING-FIELD-54
           SET VALUE-PRESENT IN P75-STRING-FIELD-55-FLAGS
            TO TRUE
           MOVE '55'
             TO P75-STRING-FIELD-55
           SET VALUE-PRESENT IN P75-STRING-FIELD-56-FLAGS
            TO TRUE
           MOVE '56'
             TO P75-STRING-FIELD-56
           SET VALUE-PRESENT IN P75-STRING-FIELD-57-FLAGS
            TO TRUE
           MOVE '57'
             TO P75-STRING-FIELD-57
           SET VALUE-PRESENT IN P75-STRING-FIELD-58-FLAGS
            TO TRUE
           MOVE '58'
             TO P75-STRING-FIELD-58
           SET VALUE-PRESENT IN P75-STRING-FIELD-59-FLAGS
            TO TRUE
           MOVE '59'
             TO P75-STRING-FIELD-59
           SET VALUE-PRESENT IN P75-STRING-FIELD-60-FLAGS
            TO TRUE
           MOVE '60'
             TO P75-STRING-FIELD-60
           SET VALUE-PRESENT IN P75-STRING-FIELD-61-FLAGS
            TO TRUE
           MOVE '61'
             TO P75-STRING-FIELD-61
           SET VALUE-PRESENT IN P75-STRING-FIELD-62-FLAGS
            TO TRUE
           MOVE '62'
             TO P75-STRING-FIELD-62
           SET VALUE-PRESENT IN P75-STRING-FIELD-63-FLAGS
            TO TRUE
           MOVE '63'
             TO P75-STRING-FIELD-63
           SET VALUE-PRESENT IN P75-STRING-FIELD-64-FLAGS
            TO TRUE
           MOVE '64'
             TO P75-STRING-FIELD-64
           SET VALUE-PRESENT IN P75-STRING-FIELD-65-FLAGS
            TO TRUE
           MOVE '65'
             TO P75-STRING-FIELD-65
           SET VALUE-PRESENT IN P75-STRING-FIELD-66-FLAGS
            TO TRUE
           MOVE '66'
             TO P75-STRING-FIELD-66
           SET VALUE-PRESENT IN P75-STRING-FIELD-67-FLAGS
            TO TRUE
           MOVE '67'
             TO P75-STRING-FIELD-67
           SET VALUE-PRESENT IN P75-STRING-FIELD-68-FLAGS
            TO TRUE
           MOVE '68'
             TO P75-STRING-FIELD-68
           SET VALUE-PRESENT IN P75-STRING-FIELD-69-FLAGS
            TO TRUE
           MOVE '69'
             TO P75-STRING-FIELD-69
           SET VALUE-PRESENT IN P75-STRING-FIELD-70-FLAGS
            TO TRUE
           MOVE '70'
             TO P75-STRING-FIELD-70
           SET VALUE-PRESENT IN P75-STRING-FIELD-71-FLAGS
            TO TRUE
           MOVE '71'
             TO P75-STRING-FIELD-71
           SET VALUE-PRESENT IN P75-STRING-FIELD-72-FLAGS
            TO TRUE
           MOVE '72'
             TO P75-STRING-FIELD-72
           SET VALUE-PRESENT IN P75-STRING-FIELD-73-FLAGS
            TO TRUE
           MOVE '73'
             TO P75-STRING-FIELD-73
           SET VALUE-PRESENT IN P75-STRING-FIELD-74-FLAGS
            TO TRUE
           MOVE '74'
             TO P75-STRING-FIELD-74
           SET VALUE-PRESENT IN P75-STRING-FIELD-75-FLAGS
            TO TRUE
           MOVE '75'
             TO P75-STRING-FIELD-75

           EXIT.

       CONVERT-STRUCTURE-SIZE-75 SECTION.
           MOVE 6 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-75
             BY REFERENCE CONSUMER-STRUCT-75                   
       
           EXIT.

       RUN-BENCHMARK-SIZE-100 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-100
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-100 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 100: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-100 SECTION.
           SET VALUE-PRESENT IN P100-TEST-STRUCT-100-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P100-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P100-INT-FIELD-1
           SET VALUE-PRESENT IN P100-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P100-INT-FIELD-2
           SET VALUE-PRESENT IN P100-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P100-INT-FIELD-3
           SET VALUE-PRESENT IN P100-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P100-INT-FIELD-4
           SET VALUE-PRESENT IN P100-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P100-INT-FIELD-5
           SET VALUE-PRESENT IN P100-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P100-INT-FIELD-6
           SET VALUE-PRESENT IN P100-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P100-INT-FIELD-7
           SET VALUE-PRESENT IN P100-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P100-INT-FIELD-8
           SET VALUE-PRESENT IN P100-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P100-INT-FIELD-9
           SET VALUE-PRESENT IN P100-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P100-INT-FIELD-10
           SET VALUE-PRESENT IN P100-INT-FIELD-11-FLAGS
            TO TRUE
           MOVE 11
             TO P100-INT-FIELD-11
           SET VALUE-PRESENT IN P100-INT-FIELD-12-FLAGS
            TO TRUE
           MOVE 12
             TO P100-INT-FIELD-12
           SET VALUE-PRESENT IN P100-INT-FIELD-13-FLAGS
            TO TRUE
           MOVE 13
             TO P100-INT-FIELD-13
           SET VALUE-PRESENT IN P100-INT-FIELD-14-FLAGS
            TO TRUE
           MOVE 14
             TO P100-INT-FIELD-14
           SET VALUE-PRESENT IN P100-INT-FIELD-15-FLAGS
            TO TRUE
           MOVE 15
             TO P100-INT-FIELD-15
           SET VALUE-PRESENT IN P100-INT-FIELD-16-FLAGS
            TO TRUE
           MOVE 16
             TO P100-INT-FIELD-16
           SET VALUE-PRESENT IN P100-INT-FIELD-17-FLAGS
            TO TRUE
           MOVE 17
             TO P100-INT-FIELD-17
           SET VALUE-PRESENT IN P100-INT-FIELD-18-FLAGS
            TO TRUE
           MOVE 18
             TO P100-INT-FIELD-18
           SET VALUE-PRESENT IN P100-INT-FIELD-19-FLAGS
            TO TRUE
           MOVE 19
             TO P100-INT-FIELD-19
           SET VALUE-PRESENT IN P100-INT-FIELD-20-FLAGS
            TO TRUE
           MOVE 20
             TO P100-INT-FIELD-20
           SET VALUE-PRESENT IN P100-INT-FIELD-21-FLAGS
            TO TRUE
           MOVE 21
             TO P100-INT-FIELD-21
           SET VALUE-PRESENT IN P100-INT-FIELD-22-FLAGS
            TO TRUE
           MOVE 22
             TO P100-INT-FIELD-22
           SET VALUE-PRESENT IN P100-INT-FIELD-23-FLAGS
            TO TRUE
           MOVE 23
             TO P100-INT-FIELD-23
           SET VALUE-PRESENT IN P100-INT-FIELD-24-FLAGS
            TO TRUE
           MOVE 24
             TO P100-INT-FIELD-24
           SET VALUE-PRESENT IN P100-INT-FIELD-25-FLAGS
            TO TRUE
           MOVE 25
             TO P100-INT-FIELD-25
           SET VALUE-PRESENT IN P100-INT-FIELD-26-FLAGS
            TO TRUE
           MOVE 26
             TO P100-INT-FIELD-26
           SET VALUE-PRESENT IN P100-INT-FIELD-27-FLAGS
            TO TRUE
           MOVE 27
             TO P100-INT-FIELD-27
           SET VALUE-PRESENT IN P100-INT-FIELD-28-FLAGS
            TO TRUE
           MOVE 28
             TO P100-INT-FIELD-28
           SET VALUE-PRESENT IN P100-INT-FIELD-29-FLAGS
            TO TRUE
           MOVE 29
             TO P100-INT-FIELD-29
           SET VALUE-PRESENT IN P100-INT-FIELD-30-FLAGS
            TO TRUE
           MOVE 30
             TO P100-INT-FIELD-30
           SET VALUE-PRESENT IN P100-INT-FIELD-31-FLAGS
            TO TRUE
           MOVE 31
             TO P100-INT-FIELD-31
           SET VALUE-PRESENT IN P100-INT-FIELD-32-FLAGS
            TO TRUE
           MOVE 32
             TO P100-INT-FIELD-32
           SET VALUE-PRESENT IN P100-INT-FIELD-33-FLAGS
            TO TRUE
           MOVE 33
             TO P100-INT-FIELD-33
           SET VALUE-PRESENT IN P100-INT-FIELD-34-FLAGS
            TO TRUE
           MOVE 34
             TO P100-INT-FIELD-34
           SET VALUE-PRESENT IN P100-INT-FIELD-35-FLAGS
            TO TRUE
           MOVE 35
             TO P100-INT-FIELD-35
           SET VALUE-PRESENT IN P100-INT-FIELD-36-FLAGS
            TO TRUE
           MOVE 36
             TO P100-INT-FIELD-36
           SET VALUE-PRESENT IN P100-INT-FIELD-37-FLAGS
            TO TRUE
           MOVE 37
             TO P100-INT-FIELD-37
           SET VALUE-PRESENT IN P100-INT-FIELD-38-FLAGS
            TO TRUE
           MOVE 38
             TO P100-INT-FIELD-38
           SET VALUE-PRESENT IN P100-INT-FIELD-39-FLAGS
            TO TRUE
           MOVE 39
             TO P100-INT-FIELD-39
           SET VALUE-PRESENT IN P100-INT-FIELD-40-FLAGS
            TO TRUE
           MOVE 40
             TO P100-INT-FIELD-40
           SET VALUE-PRESENT IN P100-INT-FIELD-41-FLAGS
            TO TRUE
           MOVE 41
             TO P100-INT-FIELD-41
           SET VALUE-PRESENT IN P100-INT-FIELD-42-FLAGS
            TO TRUE
           MOVE 42
             TO P100-INT-FIELD-42
           SET VALUE-PRESENT IN P100-INT-FIELD-43-FLAGS
            TO TRUE
           MOVE 43
             TO P100-INT-FIELD-43
           SET VALUE-PRESENT IN P100-INT-FIELD-44-FLAGS
            TO TRUE
           MOVE 44
             TO P100-INT-FIELD-44
           SET VALUE-PRESENT IN P100-INT-FIELD-45-FLAGS
            TO TRUE
           MOVE 45
             TO P100-INT-FIELD-45
           SET VALUE-PRESENT IN P100-INT-FIELD-46-FLAGS
            TO TRUE
           MOVE 46
             TO P100-INT-FIELD-46
           SET VALUE-PRESENT IN P100-INT-FIELD-47-FLAGS
            TO TRUE
           MOVE 47
             TO P100-INT-FIELD-47
           SET VALUE-PRESENT IN P100-INT-FIELD-48-FLAGS
            TO TRUE
           MOVE 48
             TO P100-INT-FIELD-48
           SET VALUE-PRESENT IN P100-INT-FIELD-49-FLAGS
            TO TRUE
           MOVE 49
             TO P100-INT-FIELD-49
           SET VALUE-PRESENT IN P100-INT-FIELD-50-FLAGS
            TO TRUE
           MOVE 50
             TO P100-INT-FIELD-50
           SET VALUE-PRESENT IN P100-INT-FIELD-51-FLAGS
            TO TRUE
           MOVE 51
             TO P100-INT-FIELD-51
           SET VALUE-PRESENT IN P100-INT-FIELD-52-FLAGS
            TO TRUE
           MOVE 52
             TO P100-INT-FIELD-52
           SET VALUE-PRESENT IN P100-INT-FIELD-53-FLAGS
            TO TRUE
           MOVE 53
             TO P100-INT-FIELD-53
           SET VALUE-PRESENT IN P100-INT-FIELD-54-FLAGS
            TO TRUE
           MOVE 54
             TO P100-INT-FIELD-54
           SET VALUE-PRESENT IN P100-INT-FIELD-55-FLAGS
            TO TRUE
           MOVE 55
             TO P100-INT-FIELD-55
           SET VALUE-PRESENT IN P100-INT-FIELD-56-FLAGS
            TO TRUE
           MOVE 56
             TO P100-INT-FIELD-56
           SET VALUE-PRESENT IN P100-INT-FIELD-57-FLAGS
            TO TRUE
           MOVE 57
             TO P100-INT-FIELD-57
           SET VALUE-PRESENT IN P100-INT-FIELD-58-FLAGS
            TO TRUE
           MOVE 58
             TO P100-INT-FIELD-58
           SET VALUE-PRESENT IN P100-INT-FIELD-59-FLAGS
            TO TRUE
           MOVE 59
             TO P100-INT-FIELD-59
           SET VALUE-PRESENT IN P100-INT-FIELD-60-FLAGS
            TO TRUE
           MOVE 60
             TO P100-INT-FIELD-60
           SET VALUE-PRESENT IN P100-INT-FIELD-61-FLAGS
            TO TRUE
           MOVE 61
             TO P100-INT-FIELD-61
           SET VALUE-PRESENT IN P100-INT-FIELD-62-FLAGS
            TO TRUE
           MOVE 62
             TO P100-INT-FIELD-62
           SET VALUE-PRESENT IN P100-INT-FIELD-63-FLAGS
            TO TRUE
           MOVE 63
             TO P100-INT-FIELD-63
           SET VALUE-PRESENT IN P100-INT-FIELD-64-FLAGS
            TO TRUE
           MOVE 64
             TO P100-INT-FIELD-64
           SET VALUE-PRESENT IN P100-INT-FIELD-65-FLAGS
            TO TRUE
           MOVE 65
             TO P100-INT-FIELD-65
           SET VALUE-PRESENT IN P100-INT-FIELD-66-FLAGS
            TO TRUE
           MOVE 66
             TO P100-INT-FIELD-66
           SET VALUE-PRESENT IN P100-INT-FIELD-67-FLAGS
            TO TRUE
           MOVE 67
             TO P100-INT-FIELD-67
           SET VALUE-PRESENT IN P100-INT-FIELD-68-FLAGS
            TO TRUE
           MOVE 68
             TO P100-INT-FIELD-68
           SET VALUE-PRESENT IN P100-INT-FIELD-69-FLAGS
            TO TRUE
           MOVE 69
             TO P100-INT-FIELD-69
           SET VALUE-PRESENT IN P100-INT-FIELD-70-FLAGS
            TO TRUE
           MOVE 70
             TO P100-INT-FIELD-70
           SET VALUE-PRESENT IN P100-INT-FIELD-71-FLAGS
            TO TRUE
           MOVE 71
             TO P100-INT-FIELD-71
           SET VALUE-PRESENT IN P100-INT-FIELD-72-FLAGS
            TO TRUE
           MOVE 72
             TO P100-INT-FIELD-72
           SET VALUE-PRESENT IN P100-INT-FIELD-73-FLAGS
            TO TRUE
           MOVE 73
             TO P100-INT-FIELD-73
           SET VALUE-PRESENT IN P100-INT-FIELD-74-FLAGS
            TO TRUE
           MOVE 74
             TO P100-INT-FIELD-74
           SET VALUE-PRESENT IN P100-INT-FIELD-75-FLAGS
            TO TRUE
           MOVE 75
             TO P100-INT-FIELD-75
           SET VALUE-PRESENT IN P100-INT-FIELD-76-FLAGS
            TO TRUE
           MOVE 76
             TO P100-INT-FIELD-76
           SET VALUE-PRESENT IN P100-INT-FIELD-77-FLAGS
            TO TRUE
           MOVE 77
             TO P100-INT-FIELD-77
           SET VALUE-PRESENT IN P100-INT-FIELD-78-FLAGS
            TO TRUE
           MOVE 78
             TO P100-INT-FIELD-78
           SET VALUE-PRESENT IN P100-INT-FIELD-79-FLAGS
            TO TRUE
           MOVE 79
             TO P100-INT-FIELD-79
           SET VALUE-PRESENT IN P100-INT-FIELD-80-FLAGS
            TO TRUE
           MOVE 80
             TO P100-INT-FIELD-80
           SET VALUE-PRESENT IN P100-INT-FIELD-81-FLAGS
            TO TRUE
           MOVE 81
             TO P100-INT-FIELD-81
           SET VALUE-PRESENT IN P100-INT-FIELD-82-FLAGS
            TO TRUE
           MOVE 82
             TO P100-INT-FIELD-82
           SET VALUE-PRESENT IN P100-INT-FIELD-83-FLAGS
            TO TRUE
           MOVE 83
             TO P100-INT-FIELD-83
           SET VALUE-PRESENT IN P100-INT-FIELD-84-FLAGS
            TO TRUE
           MOVE 84
             TO P100-INT-FIELD-84
           SET VALUE-PRESENT IN P100-INT-FIELD-85-FLAGS
            TO TRUE
           MOVE 85
             TO P100-INT-FIELD-85
           SET VALUE-PRESENT IN P100-INT-FIELD-86-FLAGS
            TO TRUE
           MOVE 86
             TO P100-INT-FIELD-86
           SET VALUE-PRESENT IN P100-INT-FIELD-87-FLAGS
            TO TRUE
           MOVE 87
             TO P100-INT-FIELD-87
           SET VALUE-PRESENT IN P100-INT-FIELD-88-FLAGS
            TO TRUE
           MOVE 88
             TO P100-INT-FIELD-88
           SET VALUE-PRESENT IN P100-INT-FIELD-89-FLAGS
            TO TRUE
           MOVE 89
             TO P100-INT-FIELD-89
           SET VALUE-PRESENT IN P100-INT-FIELD-90-FLAGS
            TO TRUE
           MOVE 90
             TO P100-INT-FIELD-90
           SET VALUE-PRESENT IN P100-INT-FIELD-91-FLAGS
            TO TRUE
           MOVE 91
             TO P100-INT-FIELD-91
           SET VALUE-PRESENT IN P100-INT-FIELD-92-FLAGS
            TO TRUE
           MOVE 92
             TO P100-INT-FIELD-92
           SET VALUE-PRESENT IN P100-INT-FIELD-93-FLAGS
            TO TRUE
           MOVE 93
             TO P100-INT-FIELD-93
           SET VALUE-PRESENT IN P100-INT-FIELD-94-FLAGS
            TO TRUE
           MOVE 94
             TO P100-INT-FIELD-94
           SET VALUE-PRESENT IN P100-INT-FIELD-95-FLAGS
            TO TRUE
           MOVE 95
             TO P100-INT-FIELD-95
           SET VALUE-PRESENT IN P100-INT-FIELD-96-FLAGS
            TO TRUE
           MOVE 96
             TO P100-INT-FIELD-96
           SET VALUE-PRESENT IN P100-INT-FIELD-97-FLAGS
            TO TRUE
           MOVE 97
             TO P100-INT-FIELD-97
           SET VALUE-PRESENT IN P100-INT-FIELD-98-FLAGS
            TO TRUE
           MOVE 98
             TO P100-INT-FIELD-98
           SET VALUE-PRESENT IN P100-INT-FIELD-99-FLAGS
            TO TRUE
           MOVE 99
             TO P100-INT-FIELD-99
           SET VALUE-PRESENT IN P100-INT-FIELD-100-FLAGS
            TO TRUE
           MOVE 100
             TO P100-INT-FIELD-100

           SET VALUE-PRESENT IN P100-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P100-STRING-FIELD-1
           SET VALUE-PRESENT IN P100-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P100-STRING-FIELD-2
           SET VALUE-PRESENT IN P100-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P100-STRING-FIELD-3
           SET VALUE-PRESENT IN P100-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P100-STRING-FIELD-4
           SET VALUE-PRESENT IN P100-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P100-STRING-FIELD-5
           SET VALUE-PRESENT IN P100-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P100-STRING-FIELD-6
           SET VALUE-PRESENT IN P100-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P100-STRING-FIELD-7
           SET VALUE-PRESENT IN P100-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P100-STRING-FIELD-8
           SET VALUE-PRESENT IN P100-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P100-STRING-FIELD-9
           SET VALUE-PRESENT IN P100-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P100-STRING-FIELD-10
           SET VALUE-PRESENT IN P100-STRING-FIELD-11-FLAGS
            TO TRUE
           MOVE '11'
             TO P100-STRING-FIELD-11
           SET VALUE-PRESENT IN P100-STRING-FIELD-12-FLAGS
            TO TRUE
           MOVE '12'
             TO P100-STRING-FIELD-12
           SET VALUE-PRESENT IN P100-STRING-FIELD-13-FLAGS
            TO TRUE
           MOVE '13'
             TO P100-STRING-FIELD-13
           SET VALUE-PRESENT IN P100-STRING-FIELD-14-FLAGS
            TO TRUE
           MOVE '14'
             TO P100-STRING-FIELD-14
           SET VALUE-PRESENT IN P100-STRING-FIELD-15-FLAGS
            TO TRUE
           MOVE '15'
             TO P100-STRING-FIELD-15
           SET VALUE-PRESENT IN P100-STRING-FIELD-16-FLAGS
            TO TRUE
           MOVE '16'
             TO P100-STRING-FIELD-16
           SET VALUE-PRESENT IN P100-STRING-FIELD-17-FLAGS
            TO TRUE
           MOVE '17'
             TO P100-STRING-FIELD-17
           SET VALUE-PRESENT IN P100-STRING-FIELD-18-FLAGS
            TO TRUE
           MOVE '18'
             TO P100-STRING-FIELD-18
           SET VALUE-PRESENT IN P100-STRING-FIELD-19-FLAGS
            TO TRUE
           MOVE '19'
             TO P100-STRING-FIELD-19
           SET VALUE-PRESENT IN P100-STRING-FIELD-20-FLAGS
            TO TRUE
           MOVE '20'
             TO P100-STRING-FIELD-20
           SET VALUE-PRESENT IN P100-STRING-FIELD-21-FLAGS
            TO TRUE
           MOVE '21'
             TO P100-STRING-FIELD-21
           SET VALUE-PRESENT IN P100-STRING-FIELD-22-FLAGS
            TO TRUE
           MOVE '22'
             TO P100-STRING-FIELD-22
           SET VALUE-PRESENT IN P100-STRING-FIELD-23-FLAGS
            TO TRUE
           MOVE '23'
             TO P100-STRING-FIELD-23
           SET VALUE-PRESENT IN P100-STRING-FIELD-24-FLAGS
            TO TRUE
           MOVE '24'
             TO P100-STRING-FIELD-24
           SET VALUE-PRESENT IN P100-STRING-FIELD-25-FLAGS
            TO TRUE
           MOVE '25'
             TO P100-STRING-FIELD-25
           SET VALUE-PRESENT IN P100-STRING-FIELD-26-FLAGS
            TO TRUE
           MOVE '26'
             TO P100-STRING-FIELD-26
           SET VALUE-PRESENT IN P100-STRING-FIELD-27-FLAGS
            TO TRUE
           MOVE '27'
             TO P100-STRING-FIELD-27
           SET VALUE-PRESENT IN P100-STRING-FIELD-28-FLAGS
            TO TRUE
           MOVE '28'
             TO P100-STRING-FIELD-28
           SET VALUE-PRESENT IN P100-STRING-FIELD-29-FLAGS
            TO TRUE
           MOVE '29'
             TO P100-STRING-FIELD-29
           SET VALUE-PRESENT IN P100-STRING-FIELD-30-FLAGS
            TO TRUE
           MOVE '30'
             TO P100-STRING-FIELD-30
           SET VALUE-PRESENT IN P100-STRING-FIELD-31-FLAGS
            TO TRUE
           MOVE '31'
             TO P100-STRING-FIELD-31
           SET VALUE-PRESENT IN P100-STRING-FIELD-32-FLAGS
            TO TRUE
           MOVE '32'
             TO P100-STRING-FIELD-32
           SET VALUE-PRESENT IN P100-STRING-FIELD-33-FLAGS
            TO TRUE
           MOVE '33'
             TO P100-STRING-FIELD-33
           SET VALUE-PRESENT IN P100-STRING-FIELD-34-FLAGS
            TO TRUE
           MOVE '34'
             TO P100-STRING-FIELD-34
           SET VALUE-PRESENT IN P100-STRING-FIELD-35-FLAGS
            TO TRUE
           MOVE '35'
             TO P100-STRING-FIELD-35
           SET VALUE-PRESENT IN P100-STRING-FIELD-36-FLAGS
            TO TRUE
           MOVE '36'
             TO P100-STRING-FIELD-36
           SET VALUE-PRESENT IN P100-STRING-FIELD-37-FLAGS
            TO TRUE
           MOVE '37'
             TO P100-STRING-FIELD-37
           SET VALUE-PRESENT IN P100-STRING-FIELD-38-FLAGS
            TO TRUE
           MOVE '38'
             TO P100-STRING-FIELD-38
           SET VALUE-PRESENT IN P100-STRING-FIELD-39-FLAGS
            TO TRUE
           MOVE '39'
             TO P100-STRING-FIELD-39
           SET VALUE-PRESENT IN P100-STRING-FIELD-40-FLAGS
            TO TRUE
           MOVE '40'
             TO P100-STRING-FIELD-40
           SET VALUE-PRESENT IN P100-STRING-FIELD-41-FLAGS
            TO TRUE
           MOVE '41'
             TO P100-STRING-FIELD-41
           SET VALUE-PRESENT IN P100-STRING-FIELD-42-FLAGS
            TO TRUE
           MOVE '42'
             TO P100-STRING-FIELD-42
           SET VALUE-PRESENT IN P100-STRING-FIELD-43-FLAGS
            TO TRUE
           MOVE '43'
             TO P100-STRING-FIELD-43
           SET VALUE-PRESENT IN P100-STRING-FIELD-44-FLAGS
            TO TRUE
           MOVE '44'
             TO P100-STRING-FIELD-44
           SET VALUE-PRESENT IN P100-STRING-FIELD-45-FLAGS
            TO TRUE
           MOVE '45'
             TO P100-STRING-FIELD-45
           SET VALUE-PRESENT IN P100-STRING-FIELD-46-FLAGS
            TO TRUE
           MOVE '46'
             TO P100-STRING-FIELD-46
           SET VALUE-PRESENT IN P100-STRING-FIELD-47-FLAGS
            TO TRUE
           MOVE '47'
             TO P100-STRING-FIELD-47
           SET VALUE-PRESENT IN P100-STRING-FIELD-48-FLAGS
            TO TRUE
           MOVE '48'
             TO P100-STRING-FIELD-48
           SET VALUE-PRESENT IN P100-STRING-FIELD-49-FLAGS
            TO TRUE
           MOVE '49'
             TO P100-STRING-FIELD-49
           SET VALUE-PRESENT IN P100-STRING-FIELD-50-FLAGS
            TO TRUE
           MOVE '50'
             TO P100-STRING-FIELD-50
           SET VALUE-PRESENT IN P100-STRING-FIELD-51-FLAGS
            TO TRUE
           MOVE '51'
             TO P100-STRING-FIELD-51
           SET VALUE-PRESENT IN P100-STRING-FIELD-52-FLAGS
            TO TRUE
           MOVE '52'
             TO P100-STRING-FIELD-52
           SET VALUE-PRESENT IN P100-STRING-FIELD-53-FLAGS
            TO TRUE
           MOVE '53'
             TO P100-STRING-FIELD-53
           SET VALUE-PRESENT IN P100-STRING-FIELD-54-FLAGS
            TO TRUE
           MOVE '54'
             TO P100-STRING-FIELD-54
           SET VALUE-PRESENT IN P100-STRING-FIELD-55-FLAGS
            TO TRUE
           MOVE '55'
             TO P100-STRING-FIELD-55
           SET VALUE-PRESENT IN P100-STRING-FIELD-56-FLAGS
            TO TRUE
           MOVE '56'
             TO P100-STRING-FIELD-56
           SET VALUE-PRESENT IN P100-STRING-FIELD-57-FLAGS
            TO TRUE
           MOVE '57'
             TO P100-STRING-FIELD-57
           SET VALUE-PRESENT IN P100-STRING-FIELD-58-FLAGS
            TO TRUE
           MOVE '58'
             TO P100-STRING-FIELD-58
           SET VALUE-PRESENT IN P100-STRING-FIELD-59-FLAGS
            TO TRUE
           MOVE '59'
             TO P100-STRING-FIELD-59
           SET VALUE-PRESENT IN P100-STRING-FIELD-60-FLAGS
            TO TRUE
           MOVE '60'
             TO P100-STRING-FIELD-60
           SET VALUE-PRESENT IN P100-STRING-FIELD-61-FLAGS
            TO TRUE
           MOVE '61'
             TO P100-STRING-FIELD-61
           SET VALUE-PRESENT IN P100-STRING-FIELD-62-FLAGS
            TO TRUE
           MOVE '62'
             TO P100-STRING-FIELD-62
           SET VALUE-PRESENT IN P100-STRING-FIELD-63-FLAGS
            TO TRUE
           MOVE '63'
             TO P100-STRING-FIELD-63
           SET VALUE-PRESENT IN P100-STRING-FIELD-64-FLAGS
            TO TRUE
           MOVE '64'
             TO P100-STRING-FIELD-64
           SET VALUE-PRESENT IN P100-STRING-FIELD-65-FLAGS
            TO TRUE
           MOVE '65'
             TO P100-STRING-FIELD-65
           SET VALUE-PRESENT IN P100-STRING-FIELD-66-FLAGS
            TO TRUE
           MOVE '66'
             TO P100-STRING-FIELD-66
           SET VALUE-PRESENT IN P100-STRING-FIELD-67-FLAGS
            TO TRUE
           MOVE '67'
             TO P100-STRING-FIELD-67
           SET VALUE-PRESENT IN P100-STRING-FIELD-68-FLAGS
            TO TRUE
           MOVE '68'
             TO P100-STRING-FIELD-68
           SET VALUE-PRESENT IN P100-STRING-FIELD-69-FLAGS
            TO TRUE
           MOVE '69'
             TO P100-STRING-FIELD-69
           SET VALUE-PRESENT IN P100-STRING-FIELD-70-FLAGS
            TO TRUE
           MOVE '70'
             TO P100-STRING-FIELD-70
           SET VALUE-PRESENT IN P100-STRING-FIELD-71-FLAGS
            TO TRUE
           MOVE '71'
             TO P100-STRING-FIELD-71
           SET VALUE-PRESENT IN P100-STRING-FIELD-72-FLAGS
            TO TRUE
           MOVE '72'
             TO P100-STRING-FIELD-72
           SET VALUE-PRESENT IN P100-STRING-FIELD-73-FLAGS
            TO TRUE
           MOVE '73'
             TO P100-STRING-FIELD-73
           SET VALUE-PRESENT IN P100-STRING-FIELD-74-FLAGS
            TO TRUE
           MOVE '74'
             TO P100-STRING-FIELD-74
           SET VALUE-PRESENT IN P100-STRING-FIELD-75-FLAGS
            TO TRUE
           MOVE '75'
             TO P100-STRING-FIELD-75
           SET VALUE-PRESENT IN P100-STRING-FIELD-76-FLAGS
            TO TRUE
           MOVE '76'
             TO P100-STRING-FIELD-76
           SET VALUE-PRESENT IN P100-STRING-FIELD-77-FLAGS
            TO TRUE
           MOVE '77'
             TO P100-STRING-FIELD-77
           SET VALUE-PRESENT IN P100-STRING-FIELD-78-FLAGS
            TO TRUE
           MOVE '78'
             TO P100-STRING-FIELD-78
           SET VALUE-PRESENT IN P100-STRING-FIELD-79-FLAGS
            TO TRUE
           MOVE '79'
             TO P100-STRING-FIELD-79
           SET VALUE-PRESENT IN P100-STRING-FIELD-80-FLAGS
            TO TRUE
           MOVE '80'
             TO P100-STRING-FIELD-80
           SET VALUE-PRESENT IN P100-STRING-FIELD-81-FLAGS
            TO TRUE
           MOVE '81'
             TO P100-STRING-FIELD-81
           SET VALUE-PRESENT IN P100-STRING-FIELD-82-FLAGS
            TO TRUE
           MOVE '82'
             TO P100-STRING-FIELD-82
           SET VALUE-PRESENT IN P100-STRING-FIELD-83-FLAGS
            TO TRUE
           MOVE '83'
             TO P100-STRING-FIELD-83
           SET VALUE-PRESENT IN P100-STRING-FIELD-84-FLAGS
            TO TRUE
           MOVE '84'
             TO P100-STRING-FIELD-84
           SET VALUE-PRESENT IN P100-STRING-FIELD-85-FLAGS
            TO TRUE
           MOVE '85'
             TO P100-STRING-FIELD-85
           SET VALUE-PRESENT IN P100-STRING-FIELD-86-FLAGS
            TO TRUE
           MOVE '86'
             TO P100-STRING-FIELD-86
           SET VALUE-PRESENT IN P100-STRING-FIELD-87-FLAGS
            TO TRUE
           MOVE '87'
             TO P100-STRING-FIELD-87
           SET VALUE-PRESENT IN P100-STRING-FIELD-88-FLAGS
            TO TRUE
           MOVE '88'
             TO P100-STRING-FIELD-88
           SET VALUE-PRESENT IN P100-STRING-FIELD-89-FLAGS
            TO TRUE
           MOVE '89'
             TO P100-STRING-FIELD-89
           SET VALUE-PRESENT IN P100-STRING-FIELD-90-FLAGS
            TO TRUE
           MOVE '90'
             TO P100-STRING-FIELD-90
           SET VALUE-PRESENT IN P100-STRING-FIELD-91-FLAGS
            TO TRUE
           MOVE '91'
             TO P100-STRING-FIELD-91
           SET VALUE-PRESENT IN P100-STRING-FIELD-92-FLAGS
            TO TRUE
           MOVE '92'
             TO P100-STRING-FIELD-92
           SET VALUE-PRESENT IN P100-STRING-FIELD-93-FLAGS
            TO TRUE
           MOVE '93'
             TO P100-STRING-FIELD-93
           SET VALUE-PRESENT IN P100-STRING-FIELD-94-FLAGS
            TO TRUE
           MOVE '94'
             TO P100-STRING-FIELD-94
           SET VALUE-PRESENT IN P100-STRING-FIELD-95-FLAGS
            TO TRUE
           MOVE '95'
             TO P100-STRING-FIELD-95
           SET VALUE-PRESENT IN P100-STRING-FIELD-96-FLAGS
            TO TRUE
           MOVE '96'
             TO P100-STRING-FIELD-96
           SET VALUE-PRESENT IN P100-STRING-FIELD-97-FLAGS
            TO TRUE
           MOVE '97'
             TO P100-STRING-FIELD-97
           SET VALUE-PRESENT IN P100-STRING-FIELD-98-FLAGS
            TO TRUE
           MOVE '98'
             TO P100-STRING-FIELD-98
           SET VALUE-PRESENT IN P100-STRING-FIELD-99-FLAGS
            TO TRUE
           MOVE '99'
             TO P100-STRING-FIELD-99
           SET VALUE-PRESENT IN P100-STRING-FIELD-100-FLAGS
            TO TRUE
           MOVE '100'
             TO P100-STRING-FIELD-100

           EXIT.

       CONVERT-STRUCTURE-SIZE-100 SECTION.
           MOVE 1 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-100
             BY REFERENCE CONSUMER-STRUCT-100                    
       
           EXIT.

       RUN-BENCHMARK-SIZE-250 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-250
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-250 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 250: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-250 SECTION.
           SET VALUE-PRESENT IN P250-TEST-STRUCT-250-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P250-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P250-INT-FIELD-1
           SET VALUE-PRESENT IN P250-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P250-INT-FIELD-2
           SET VALUE-PRESENT IN P250-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P250-INT-FIELD-3
           SET VALUE-PRESENT IN P250-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P250-INT-FIELD-4
           SET VALUE-PRESENT IN P250-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P250-INT-FIELD-5
           SET VALUE-PRESENT IN P250-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P250-INT-FIELD-6
           SET VALUE-PRESENT IN P250-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P250-INT-FIELD-7
           SET VALUE-PRESENT IN P250-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P250-INT-FIELD-8
           SET VALUE-PRESENT IN P250-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P250-INT-FIELD-9
           SET VALUE-PRESENT IN P250-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P250-INT-FIELD-10
           SET VALUE-PRESENT IN P250-INT-FIELD-11-FLAGS
            TO TRUE
           MOVE 11
             TO P250-INT-FIELD-11
           SET VALUE-PRESENT IN P250-INT-FIELD-12-FLAGS
            TO TRUE
           MOVE 12
             TO P250-INT-FIELD-12
           SET VALUE-PRESENT IN P250-INT-FIELD-13-FLAGS
            TO TRUE
           MOVE 13
             TO P250-INT-FIELD-13
           SET VALUE-PRESENT IN P250-INT-FIELD-14-FLAGS
            TO TRUE
           MOVE 14
             TO P250-INT-FIELD-14
           SET VALUE-PRESENT IN P250-INT-FIELD-15-FLAGS
            TO TRUE
           MOVE 15
             TO P250-INT-FIELD-15
           SET VALUE-PRESENT IN P250-INT-FIELD-16-FLAGS
            TO TRUE
           MOVE 16
             TO P250-INT-FIELD-16
           SET VALUE-PRESENT IN P250-INT-FIELD-17-FLAGS
            TO TRUE
           MOVE 17
             TO P250-INT-FIELD-17
           SET VALUE-PRESENT IN P250-INT-FIELD-18-FLAGS
            TO TRUE
           MOVE 18
             TO P250-INT-FIELD-18
           SET VALUE-PRESENT IN P250-INT-FIELD-19-FLAGS
            TO TRUE
           MOVE 19
             TO P250-INT-FIELD-19
           SET VALUE-PRESENT IN P250-INT-FIELD-20-FLAGS
            TO TRUE
           MOVE 20
             TO P250-INT-FIELD-20
           SET VALUE-PRESENT IN P250-INT-FIELD-21-FLAGS
            TO TRUE
           MOVE 21
             TO P250-INT-FIELD-21
           SET VALUE-PRESENT IN P250-INT-FIELD-22-FLAGS
            TO TRUE
           MOVE 22
             TO P250-INT-FIELD-22
           SET VALUE-PRESENT IN P250-INT-FIELD-23-FLAGS
            TO TRUE
           MOVE 23
             TO P250-INT-FIELD-23
           SET VALUE-PRESENT IN P250-INT-FIELD-24-FLAGS
            TO TRUE
           MOVE 24
             TO P250-INT-FIELD-24
           SET VALUE-PRESENT IN P250-INT-FIELD-25-FLAGS
            TO TRUE
           MOVE 25
             TO P250-INT-FIELD-25
           SET VALUE-PRESENT IN P250-INT-FIELD-26-FLAGS
            TO TRUE
           MOVE 26
             TO P250-INT-FIELD-26
           SET VALUE-PRESENT IN P250-INT-FIELD-27-FLAGS
            TO TRUE
           MOVE 27
             TO P250-INT-FIELD-27
           SET VALUE-PRESENT IN P250-INT-FIELD-28-FLAGS
            TO TRUE
           MOVE 28
             TO P250-INT-FIELD-28
           SET VALUE-PRESENT IN P250-INT-FIELD-29-FLAGS
            TO TRUE
           MOVE 29
             TO P250-INT-FIELD-29
           SET VALUE-PRESENT IN P250-INT-FIELD-30-FLAGS
            TO TRUE
           MOVE 30
             TO P250-INT-FIELD-30
           SET VALUE-PRESENT IN P250-INT-FIELD-31-FLAGS
            TO TRUE
           MOVE 31
             TO P250-INT-FIELD-31
           SET VALUE-PRESENT IN P250-INT-FIELD-32-FLAGS
            TO TRUE
           MOVE 32
             TO P250-INT-FIELD-32
           SET VALUE-PRESENT IN P250-INT-FIELD-33-FLAGS
            TO TRUE
           MOVE 33
             TO P250-INT-FIELD-33
           SET VALUE-PRESENT IN P250-INT-FIELD-34-FLAGS
            TO TRUE
           MOVE 34
             TO P250-INT-FIELD-34
           SET VALUE-PRESENT IN P250-INT-FIELD-35-FLAGS
            TO TRUE
           MOVE 35
             TO P250-INT-FIELD-35
           SET VALUE-PRESENT IN P250-INT-FIELD-36-FLAGS
            TO TRUE
           MOVE 36
             TO P250-INT-FIELD-36
           SET VALUE-PRESENT IN P250-INT-FIELD-37-FLAGS
            TO TRUE
           MOVE 37
             TO P250-INT-FIELD-37
           SET VALUE-PRESENT IN P250-INT-FIELD-38-FLAGS
            TO TRUE
           MOVE 38
             TO P250-INT-FIELD-38
           SET VALUE-PRESENT IN P250-INT-FIELD-39-FLAGS
            TO TRUE
           MOVE 39
             TO P250-INT-FIELD-39
           SET VALUE-PRESENT IN P250-INT-FIELD-40-FLAGS
            TO TRUE
           MOVE 40
             TO P250-INT-FIELD-40
           SET VALUE-PRESENT IN P250-INT-FIELD-41-FLAGS
            TO TRUE
           MOVE 41
             TO P250-INT-FIELD-41
           SET VALUE-PRESENT IN P250-INT-FIELD-42-FLAGS
            TO TRUE
           MOVE 42
             TO P250-INT-FIELD-42
           SET VALUE-PRESENT IN P250-INT-FIELD-43-FLAGS
            TO TRUE
           MOVE 43
             TO P250-INT-FIELD-43
           SET VALUE-PRESENT IN P250-INT-FIELD-44-FLAGS
            TO TRUE
           MOVE 44
             TO P250-INT-FIELD-44
           SET VALUE-PRESENT IN P250-INT-FIELD-45-FLAGS
            TO TRUE
           MOVE 45
             TO P250-INT-FIELD-45
           SET VALUE-PRESENT IN P250-INT-FIELD-46-FLAGS
            TO TRUE
           MOVE 46
             TO P250-INT-FIELD-46
           SET VALUE-PRESENT IN P250-INT-FIELD-47-FLAGS
            TO TRUE
           MOVE 47
             TO P250-INT-FIELD-47
           SET VALUE-PRESENT IN P250-INT-FIELD-48-FLAGS
            TO TRUE
           MOVE 48
             TO P250-INT-FIELD-48
           SET VALUE-PRESENT IN P250-INT-FIELD-49-FLAGS
            TO TRUE
           MOVE 49
             TO P250-INT-FIELD-49
           SET VALUE-PRESENT IN P250-INT-FIELD-50-FLAGS
            TO TRUE
           MOVE 50
             TO P250-INT-FIELD-50
           SET VALUE-PRESENT IN P250-INT-FIELD-51-FLAGS
            TO TRUE
           MOVE 51
             TO P250-INT-FIELD-51
           SET VALUE-PRESENT IN P250-INT-FIELD-52-FLAGS
            TO TRUE
           MOVE 52
             TO P250-INT-FIELD-52
           SET VALUE-PRESENT IN P250-INT-FIELD-53-FLAGS
            TO TRUE
           MOVE 53
             TO P250-INT-FIELD-53
           SET VALUE-PRESENT IN P250-INT-FIELD-54-FLAGS
            TO TRUE
           MOVE 54
             TO P250-INT-FIELD-54
           SET VALUE-PRESENT IN P250-INT-FIELD-55-FLAGS
            TO TRUE
           MOVE 55
             TO P250-INT-FIELD-55
           SET VALUE-PRESENT IN P250-INT-FIELD-56-FLAGS
            TO TRUE
           MOVE 56
             TO P250-INT-FIELD-56
           SET VALUE-PRESENT IN P250-INT-FIELD-57-FLAGS
            TO TRUE
           MOVE 57
             TO P250-INT-FIELD-57
           SET VALUE-PRESENT IN P250-INT-FIELD-58-FLAGS
            TO TRUE
           MOVE 58
             TO P250-INT-FIELD-58
           SET VALUE-PRESENT IN P250-INT-FIELD-59-FLAGS
            TO TRUE
           MOVE 59
             TO P250-INT-FIELD-59
           SET VALUE-PRESENT IN P250-INT-FIELD-60-FLAGS
            TO TRUE
           MOVE 60
             TO P250-INT-FIELD-60
           SET VALUE-PRESENT IN P250-INT-FIELD-61-FLAGS
            TO TRUE
           MOVE 61
             TO P250-INT-FIELD-61
           SET VALUE-PRESENT IN P250-INT-FIELD-62-FLAGS
            TO TRUE
           MOVE 62
             TO P250-INT-FIELD-62
           SET VALUE-PRESENT IN P250-INT-FIELD-63-FLAGS
            TO TRUE
           MOVE 63
             TO P250-INT-FIELD-63
           SET VALUE-PRESENT IN P250-INT-FIELD-64-FLAGS
            TO TRUE
           MOVE 64
             TO P250-INT-FIELD-64
           SET VALUE-PRESENT IN P250-INT-FIELD-65-FLAGS
            TO TRUE
           MOVE 65
             TO P250-INT-FIELD-65
           SET VALUE-PRESENT IN P250-INT-FIELD-66-FLAGS
            TO TRUE
           MOVE 66
             TO P250-INT-FIELD-66
           SET VALUE-PRESENT IN P250-INT-FIELD-67-FLAGS
            TO TRUE
           MOVE 67
             TO P250-INT-FIELD-67
           SET VALUE-PRESENT IN P250-INT-FIELD-68-FLAGS
            TO TRUE
           MOVE 68
             TO P250-INT-FIELD-68
           SET VALUE-PRESENT IN P250-INT-FIELD-69-FLAGS
            TO TRUE
           MOVE 69
             TO P250-INT-FIELD-69
           SET VALUE-PRESENT IN P250-INT-FIELD-70-FLAGS
            TO TRUE
           MOVE 70
             TO P250-INT-FIELD-70
           SET VALUE-PRESENT IN P250-INT-FIELD-71-FLAGS
            TO TRUE
           MOVE 71
             TO P250-INT-FIELD-71
           SET VALUE-PRESENT IN P250-INT-FIELD-72-FLAGS
            TO TRUE
           MOVE 72
             TO P250-INT-FIELD-72
           SET VALUE-PRESENT IN P250-INT-FIELD-73-FLAGS
            TO TRUE
           MOVE 73
             TO P250-INT-FIELD-73
           SET VALUE-PRESENT IN P250-INT-FIELD-74-FLAGS
            TO TRUE
           MOVE 74
             TO P250-INT-FIELD-74
           SET VALUE-PRESENT IN P250-INT-FIELD-75-FLAGS
            TO TRUE
           MOVE 75
             TO P250-INT-FIELD-75
           SET VALUE-PRESENT IN P250-INT-FIELD-76-FLAGS
            TO TRUE
           MOVE 76
             TO P250-INT-FIELD-76
           SET VALUE-PRESENT IN P250-INT-FIELD-77-FLAGS
            TO TRUE
           MOVE 77
             TO P250-INT-FIELD-77
           SET VALUE-PRESENT IN P250-INT-FIELD-78-FLAGS
            TO TRUE
           MOVE 78
             TO P250-INT-FIELD-78
           SET VALUE-PRESENT IN P250-INT-FIELD-79-FLAGS
            TO TRUE
           MOVE 79
             TO P250-INT-FIELD-79
           SET VALUE-PRESENT IN P250-INT-FIELD-80-FLAGS
            TO TRUE
           MOVE 80
             TO P250-INT-FIELD-80
           SET VALUE-PRESENT IN P250-INT-FIELD-81-FLAGS
            TO TRUE
           MOVE 81
             TO P250-INT-FIELD-81
           SET VALUE-PRESENT IN P250-INT-FIELD-82-FLAGS
            TO TRUE
           MOVE 82
             TO P250-INT-FIELD-82
           SET VALUE-PRESENT IN P250-INT-FIELD-83-FLAGS
            TO TRUE
           MOVE 83
             TO P250-INT-FIELD-83
           SET VALUE-PRESENT IN P250-INT-FIELD-84-FLAGS
            TO TRUE
           MOVE 84
             TO P250-INT-FIELD-84
           SET VALUE-PRESENT IN P250-INT-FIELD-85-FLAGS
            TO TRUE
           MOVE 85
             TO P250-INT-FIELD-85
           SET VALUE-PRESENT IN P250-INT-FIELD-86-FLAGS
            TO TRUE
           MOVE 86
             TO P250-INT-FIELD-86
           SET VALUE-PRESENT IN P250-INT-FIELD-87-FLAGS
            TO TRUE
           MOVE 87
             TO P250-INT-FIELD-87
           SET VALUE-PRESENT IN P250-INT-FIELD-88-FLAGS
            TO TRUE
           MOVE 88
             TO P250-INT-FIELD-88
           SET VALUE-PRESENT IN P250-INT-FIELD-89-FLAGS
            TO TRUE
           MOVE 89
             TO P250-INT-FIELD-89
           SET VALUE-PRESENT IN P250-INT-FIELD-90-FLAGS
            TO TRUE
           MOVE 90
             TO P250-INT-FIELD-90
           SET VALUE-PRESENT IN P250-INT-FIELD-91-FLAGS
            TO TRUE
           MOVE 91
             TO P250-INT-FIELD-91
           SET VALUE-PRESENT IN P250-INT-FIELD-92-FLAGS
            TO TRUE
           MOVE 92
             TO P250-INT-FIELD-92
           SET VALUE-PRESENT IN P250-INT-FIELD-93-FLAGS
            TO TRUE
           MOVE 93
             TO P250-INT-FIELD-93
           SET VALUE-PRESENT IN P250-INT-FIELD-94-FLAGS
            TO TRUE
           MOVE 94
             TO P250-INT-FIELD-94
           SET VALUE-PRESENT IN P250-INT-FIELD-95-FLAGS
            TO TRUE
           MOVE 95
             TO P250-INT-FIELD-95
           SET VALUE-PRESENT IN P250-INT-FIELD-96-FLAGS
            TO TRUE
           MOVE 96
             TO P250-INT-FIELD-96
           SET VALUE-PRESENT IN P250-INT-FIELD-97-FLAGS
            TO TRUE
           MOVE 97
             TO P250-INT-FIELD-97
           SET VALUE-PRESENT IN P250-INT-FIELD-98-FLAGS
            TO TRUE
           MOVE 98
             TO P250-INT-FIELD-98
           SET VALUE-PRESENT IN P250-INT-FIELD-99-FLAGS
            TO TRUE
           MOVE 99
             TO P250-INT-FIELD-99
           SET VALUE-PRESENT IN P250-INT-FIELD-100-FLAGS
            TO TRUE
           MOVE 100
             TO P250-INT-FIELD-100
           SET VALUE-PRESENT IN P250-INT-FIELD-101-FLAGS
            TO TRUE
           MOVE 101
             TO P250-INT-FIELD-101
           SET VALUE-PRESENT IN P250-INT-FIELD-102-FLAGS
            TO TRUE
           MOVE 102
             TO P250-INT-FIELD-102
           SET VALUE-PRESENT IN P250-INT-FIELD-103-FLAGS
            TO TRUE
           MOVE 103
             TO P250-INT-FIELD-103
           SET VALUE-PRESENT IN P250-INT-FIELD-104-FLAGS
            TO TRUE
           MOVE 104
             TO P250-INT-FIELD-104
           SET VALUE-PRESENT IN P250-INT-FIELD-105-FLAGS
            TO TRUE
           MOVE 105
             TO P250-INT-FIELD-105
           SET VALUE-PRESENT IN P250-INT-FIELD-106-FLAGS
            TO TRUE
           MOVE 106
             TO P250-INT-FIELD-106
           SET VALUE-PRESENT IN P250-INT-FIELD-107-FLAGS
            TO TRUE
           MOVE 107
             TO P250-INT-FIELD-107
           SET VALUE-PRESENT IN P250-INT-FIELD-108-FLAGS
            TO TRUE
           MOVE 108
             TO P250-INT-FIELD-108
           SET VALUE-PRESENT IN P250-INT-FIELD-109-FLAGS
            TO TRUE
           MOVE 109
             TO P250-INT-FIELD-109
           SET VALUE-PRESENT IN P250-INT-FIELD-110-FLAGS
            TO TRUE
           MOVE 110
             TO P250-INT-FIELD-110
           SET VALUE-PRESENT IN P250-INT-FIELD-111-FLAGS
            TO TRUE
           MOVE 111
             TO P250-INT-FIELD-111
           SET VALUE-PRESENT IN P250-INT-FIELD-112-FLAGS
            TO TRUE
           MOVE 112
             TO P250-INT-FIELD-112
           SET VALUE-PRESENT IN P250-INT-FIELD-113-FLAGS
            TO TRUE
           MOVE 113
             TO P250-INT-FIELD-113
           SET VALUE-PRESENT IN P250-INT-FIELD-114-FLAGS
            TO TRUE
           MOVE 114
             TO P250-INT-FIELD-114
           SET VALUE-PRESENT IN P250-INT-FIELD-115-FLAGS
            TO TRUE
           MOVE 115
             TO P250-INT-FIELD-115
           SET VALUE-PRESENT IN P250-INT-FIELD-116-FLAGS
            TO TRUE
           MOVE 116
             TO P250-INT-FIELD-116
           SET VALUE-PRESENT IN P250-INT-FIELD-117-FLAGS
            TO TRUE
           MOVE 117
             TO P250-INT-FIELD-117
           SET VALUE-PRESENT IN P250-INT-FIELD-118-FLAGS
            TO TRUE
           MOVE 118
             TO P250-INT-FIELD-118
           SET VALUE-PRESENT IN P250-INT-FIELD-119-FLAGS
            TO TRUE
           MOVE 119
             TO P250-INT-FIELD-119
           SET VALUE-PRESENT IN P250-INT-FIELD-120-FLAGS
            TO TRUE
           MOVE 120
             TO P250-INT-FIELD-120
           SET VALUE-PRESENT IN P250-INT-FIELD-121-FLAGS
            TO TRUE
           MOVE 121
             TO P250-INT-FIELD-121
           SET VALUE-PRESENT IN P250-INT-FIELD-122-FLAGS
            TO TRUE
           MOVE 122
             TO P250-INT-FIELD-122
           SET VALUE-PRESENT IN P250-INT-FIELD-123-FLAGS
            TO TRUE
           MOVE 123
             TO P250-INT-FIELD-123
           SET VALUE-PRESENT IN P250-INT-FIELD-124-FLAGS
            TO TRUE
           MOVE 124
             TO P250-INT-FIELD-124
           SET VALUE-PRESENT IN P250-INT-FIELD-125-FLAGS
            TO TRUE
           MOVE 125
             TO P250-INT-FIELD-125
           SET VALUE-PRESENT IN P250-INT-FIELD-126-FLAGS
            TO TRUE
           MOVE 126
             TO P250-INT-FIELD-126
           SET VALUE-PRESENT IN P250-INT-FIELD-127-FLAGS
            TO TRUE
           MOVE 127
             TO P250-INT-FIELD-127
           SET VALUE-PRESENT IN P250-INT-FIELD-128-FLAGS
            TO TRUE
           MOVE 128
             TO P250-INT-FIELD-128
           SET VALUE-PRESENT IN P250-INT-FIELD-129-FLAGS
            TO TRUE
           MOVE 129
             TO P250-INT-FIELD-129
           SET VALUE-PRESENT IN P250-INT-FIELD-130-FLAGS
            TO TRUE
           MOVE 130
             TO P250-INT-FIELD-130
           SET VALUE-PRESENT IN P250-INT-FIELD-131-FLAGS
            TO TRUE
           MOVE 131
             TO P250-INT-FIELD-131
           SET VALUE-PRESENT IN P250-INT-FIELD-132-FLAGS
            TO TRUE
           MOVE 132
             TO P250-INT-FIELD-132
           SET VALUE-PRESENT IN P250-INT-FIELD-133-FLAGS
            TO TRUE
           MOVE 133
             TO P250-INT-FIELD-133
           SET VALUE-PRESENT IN P250-INT-FIELD-134-FLAGS
            TO TRUE
           MOVE 134
             TO P250-INT-FIELD-134
           SET VALUE-PRESENT IN P250-INT-FIELD-135-FLAGS
            TO TRUE
           MOVE 135
             TO P250-INT-FIELD-135
           SET VALUE-PRESENT IN P250-INT-FIELD-136-FLAGS
            TO TRUE
           MOVE 136
             TO P250-INT-FIELD-136
           SET VALUE-PRESENT IN P250-INT-FIELD-137-FLAGS
            TO TRUE
           MOVE 137
             TO P250-INT-FIELD-137
           SET VALUE-PRESENT IN P250-INT-FIELD-138-FLAGS
            TO TRUE
           MOVE 138
             TO P250-INT-FIELD-138
           SET VALUE-PRESENT IN P250-INT-FIELD-139-FLAGS
            TO TRUE
           MOVE 139
             TO P250-INT-FIELD-139
           SET VALUE-PRESENT IN P250-INT-FIELD-140-FLAGS
            TO TRUE
           MOVE 140
             TO P250-INT-FIELD-140
           SET VALUE-PRESENT IN P250-INT-FIELD-141-FLAGS
            TO TRUE
           MOVE 141
             TO P250-INT-FIELD-141
           SET VALUE-PRESENT IN P250-INT-FIELD-142-FLAGS
            TO TRUE
           MOVE 142
             TO P250-INT-FIELD-142
           SET VALUE-PRESENT IN P250-INT-FIELD-143-FLAGS
            TO TRUE
           MOVE 143
             TO P250-INT-FIELD-143
           SET VALUE-PRESENT IN P250-INT-FIELD-144-FLAGS
            TO TRUE
           MOVE 144
             TO P250-INT-FIELD-144
           SET VALUE-PRESENT IN P250-INT-FIELD-145-FLAGS
            TO TRUE
           MOVE 145
             TO P250-INT-FIELD-145
           SET VALUE-PRESENT IN P250-INT-FIELD-146-FLAGS
            TO TRUE
           MOVE 146
             TO P250-INT-FIELD-146
           SET VALUE-PRESENT IN P250-INT-FIELD-147-FLAGS
            TO TRUE
           MOVE 147
             TO P250-INT-FIELD-147
           SET VALUE-PRESENT IN P250-INT-FIELD-148-FLAGS
            TO TRUE
           MOVE 148
             TO P250-INT-FIELD-148
           SET VALUE-PRESENT IN P250-INT-FIELD-149-FLAGS
            TO TRUE
           MOVE 149
             TO P250-INT-FIELD-149
           SET VALUE-PRESENT IN P250-INT-FIELD-150-FLAGS
            TO TRUE
           MOVE 150
             TO P250-INT-FIELD-150
           SET VALUE-PRESENT IN P250-INT-FIELD-151-FLAGS
            TO TRUE
           MOVE 151
             TO P250-INT-FIELD-151
           SET VALUE-PRESENT IN P250-INT-FIELD-152-FLAGS
            TO TRUE
           MOVE 152
             TO P250-INT-FIELD-152
           SET VALUE-PRESENT IN P250-INT-FIELD-153-FLAGS
            TO TRUE
           MOVE 153
             TO P250-INT-FIELD-153
           SET VALUE-PRESENT IN P250-INT-FIELD-154-FLAGS
            TO TRUE
           MOVE 154
             TO P250-INT-FIELD-154
           SET VALUE-PRESENT IN P250-INT-FIELD-155-FLAGS
            TO TRUE
           MOVE 155
             TO P250-INT-FIELD-155
           SET VALUE-PRESENT IN P250-INT-FIELD-156-FLAGS
            TO TRUE
           MOVE 156
             TO P250-INT-FIELD-156
           SET VALUE-PRESENT IN P250-INT-FIELD-157-FLAGS
            TO TRUE
           MOVE 157
             TO P250-INT-FIELD-157
           SET VALUE-PRESENT IN P250-INT-FIELD-158-FLAGS
            TO TRUE
           MOVE 158
             TO P250-INT-FIELD-158
           SET VALUE-PRESENT IN P250-INT-FIELD-159-FLAGS
            TO TRUE
           MOVE 159
             TO P250-INT-FIELD-159
           SET VALUE-PRESENT IN P250-INT-FIELD-160-FLAGS
            TO TRUE
           MOVE 160
             TO P250-INT-FIELD-160
           SET VALUE-PRESENT IN P250-INT-FIELD-161-FLAGS
            TO TRUE
           MOVE 161
             TO P250-INT-FIELD-161
           SET VALUE-PRESENT IN P250-INT-FIELD-162-FLAGS
            TO TRUE
           MOVE 162
             TO P250-INT-FIELD-162
           SET VALUE-PRESENT IN P250-INT-FIELD-163-FLAGS
            TO TRUE
           MOVE 163
             TO P250-INT-FIELD-163
           SET VALUE-PRESENT IN P250-INT-FIELD-164-FLAGS
            TO TRUE
           MOVE 164
             TO P250-INT-FIELD-164
           SET VALUE-PRESENT IN P250-INT-FIELD-165-FLAGS
            TO TRUE
           MOVE 165
             TO P250-INT-FIELD-165
           SET VALUE-PRESENT IN P250-INT-FIELD-166-FLAGS
            TO TRUE
           MOVE 166
             TO P250-INT-FIELD-166
           SET VALUE-PRESENT IN P250-INT-FIELD-167-FLAGS
            TO TRUE
           MOVE 167
             TO P250-INT-FIELD-167
           SET VALUE-PRESENT IN P250-INT-FIELD-168-FLAGS
            TO TRUE
           MOVE 168
             TO P250-INT-FIELD-168
           SET VALUE-PRESENT IN P250-INT-FIELD-169-FLAGS
            TO TRUE
           MOVE 169
             TO P250-INT-FIELD-169
           SET VALUE-PRESENT IN P250-INT-FIELD-170-FLAGS
            TO TRUE
           MOVE 170
             TO P250-INT-FIELD-170
           SET VALUE-PRESENT IN P250-INT-FIELD-171-FLAGS
            TO TRUE
           MOVE 171
             TO P250-INT-FIELD-171
           SET VALUE-PRESENT IN P250-INT-FIELD-172-FLAGS
            TO TRUE
           MOVE 172
             TO P250-INT-FIELD-172
           SET VALUE-PRESENT IN P250-INT-FIELD-173-FLAGS
            TO TRUE
           MOVE 173
             TO P250-INT-FIELD-173
           SET VALUE-PRESENT IN P250-INT-FIELD-174-FLAGS
            TO TRUE
           MOVE 174
             TO P250-INT-FIELD-174
           SET VALUE-PRESENT IN P250-INT-FIELD-175-FLAGS
            TO TRUE
           MOVE 175
             TO P250-INT-FIELD-175
           SET VALUE-PRESENT IN P250-INT-FIELD-176-FLAGS
            TO TRUE
           MOVE 176
             TO P250-INT-FIELD-176
           SET VALUE-PRESENT IN P250-INT-FIELD-177-FLAGS
            TO TRUE
           MOVE 177
             TO P250-INT-FIELD-177
           SET VALUE-PRESENT IN P250-INT-FIELD-178-FLAGS
            TO TRUE
           MOVE 178
             TO P250-INT-FIELD-178
           SET VALUE-PRESENT IN P250-INT-FIELD-179-FLAGS
            TO TRUE
           MOVE 179
             TO P250-INT-FIELD-179
           SET VALUE-PRESENT IN P250-INT-FIELD-180-FLAGS
            TO TRUE
           MOVE 180
             TO P250-INT-FIELD-180
           SET VALUE-PRESENT IN P250-INT-FIELD-181-FLAGS
            TO TRUE
           MOVE 181
             TO P250-INT-FIELD-181
           SET VALUE-PRESENT IN P250-INT-FIELD-182-FLAGS
            TO TRUE
           MOVE 182
             TO P250-INT-FIELD-182
           SET VALUE-PRESENT IN P250-INT-FIELD-183-FLAGS
            TO TRUE
           MOVE 183
             TO P250-INT-FIELD-183
           SET VALUE-PRESENT IN P250-INT-FIELD-184-FLAGS
            TO TRUE
           MOVE 184
             TO P250-INT-FIELD-184
           SET VALUE-PRESENT IN P250-INT-FIELD-185-FLAGS
            TO TRUE
           MOVE 185
             TO P250-INT-FIELD-185
           SET VALUE-PRESENT IN P250-INT-FIELD-186-FLAGS
            TO TRUE
           MOVE 186
             TO P250-INT-FIELD-186
           SET VALUE-PRESENT IN P250-INT-FIELD-187-FLAGS
            TO TRUE
           MOVE 187
             TO P250-INT-FIELD-187
           SET VALUE-PRESENT IN P250-INT-FIELD-188-FLAGS
            TO TRUE
           MOVE 188
             TO P250-INT-FIELD-188
           SET VALUE-PRESENT IN P250-INT-FIELD-189-FLAGS
            TO TRUE
           MOVE 189
             TO P250-INT-FIELD-189
           SET VALUE-PRESENT IN P250-INT-FIELD-190-FLAGS
            TO TRUE
           MOVE 190
             TO P250-INT-FIELD-190
           SET VALUE-PRESENT IN P250-INT-FIELD-191-FLAGS
            TO TRUE
           MOVE 191
             TO P250-INT-FIELD-191
           SET VALUE-PRESENT IN P250-INT-FIELD-192-FLAGS
            TO TRUE
           MOVE 192
             TO P250-INT-FIELD-192
           SET VALUE-PRESENT IN P250-INT-FIELD-193-FLAGS
            TO TRUE
           MOVE 193
             TO P250-INT-FIELD-193
           SET VALUE-PRESENT IN P250-INT-FIELD-194-FLAGS
            TO TRUE
           MOVE 194
             TO P250-INT-FIELD-194
           SET VALUE-PRESENT IN P250-INT-FIELD-195-FLAGS
            TO TRUE
           MOVE 195
             TO P250-INT-FIELD-195
           SET VALUE-PRESENT IN P250-INT-FIELD-196-FLAGS
            TO TRUE
           MOVE 196
             TO P250-INT-FIELD-196
           SET VALUE-PRESENT IN P250-INT-FIELD-197-FLAGS
            TO TRUE
           MOVE 197
             TO P250-INT-FIELD-197
           SET VALUE-PRESENT IN P250-INT-FIELD-198-FLAGS
            TO TRUE
           MOVE 198
             TO P250-INT-FIELD-198
           SET VALUE-PRESENT IN P250-INT-FIELD-199-FLAGS
            TO TRUE
           MOVE 199
             TO P250-INT-FIELD-199
           SET VALUE-PRESENT IN P250-INT-FIELD-200-FLAGS
            TO TRUE
           MOVE 200
             TO P250-INT-FIELD-200
           SET VALUE-PRESENT IN P250-INT-FIELD-201-FLAGS
            TO TRUE
           MOVE 201
             TO P250-INT-FIELD-201
           SET VALUE-PRESENT IN P250-INT-FIELD-202-FLAGS
            TO TRUE
           MOVE 202
             TO P250-INT-FIELD-202
           SET VALUE-PRESENT IN P250-INT-FIELD-203-FLAGS
            TO TRUE
           MOVE 203
             TO P250-INT-FIELD-203
           SET VALUE-PRESENT IN P250-INT-FIELD-204-FLAGS
            TO TRUE
           MOVE 204
             TO P250-INT-FIELD-204
           SET VALUE-PRESENT IN P250-INT-FIELD-205-FLAGS
            TO TRUE
           MOVE 205
             TO P250-INT-FIELD-205
           SET VALUE-PRESENT IN P250-INT-FIELD-206-FLAGS
            TO TRUE
           MOVE 206
             TO P250-INT-FIELD-206
           SET VALUE-PRESENT IN P250-INT-FIELD-207-FLAGS
            TO TRUE
           MOVE 207
             TO P250-INT-FIELD-207
           SET VALUE-PRESENT IN P250-INT-FIELD-208-FLAGS
            TO TRUE
           MOVE 208
             TO P250-INT-FIELD-208
           SET VALUE-PRESENT IN P250-INT-FIELD-209-FLAGS
            TO TRUE
           MOVE 209
             TO P250-INT-FIELD-209
           SET VALUE-PRESENT IN P250-INT-FIELD-210-FLAGS
            TO TRUE
           MOVE 210
             TO P250-INT-FIELD-210
           SET VALUE-PRESENT IN P250-INT-FIELD-211-FLAGS
            TO TRUE
           MOVE 211
             TO P250-INT-FIELD-211
           SET VALUE-PRESENT IN P250-INT-FIELD-212-FLAGS
            TO TRUE
           MOVE 212
             TO P250-INT-FIELD-212
           SET VALUE-PRESENT IN P250-INT-FIELD-213-FLAGS
            TO TRUE
           MOVE 213
             TO P250-INT-FIELD-213
           SET VALUE-PRESENT IN P250-INT-FIELD-214-FLAGS
            TO TRUE
           MOVE 214
             TO P250-INT-FIELD-214
           SET VALUE-PRESENT IN P250-INT-FIELD-215-FLAGS
            TO TRUE
           MOVE 215
             TO P250-INT-FIELD-215
           SET VALUE-PRESENT IN P250-INT-FIELD-216-FLAGS
            TO TRUE
           MOVE 216
             TO P250-INT-FIELD-216
           SET VALUE-PRESENT IN P250-INT-FIELD-217-FLAGS
            TO TRUE
           MOVE 217
             TO P250-INT-FIELD-217
           SET VALUE-PRESENT IN P250-INT-FIELD-218-FLAGS
            TO TRUE
           MOVE 218
             TO P250-INT-FIELD-218
           SET VALUE-PRESENT IN P250-INT-FIELD-219-FLAGS
            TO TRUE
           MOVE 219
             TO P250-INT-FIELD-219
           SET VALUE-PRESENT IN P250-INT-FIELD-220-FLAGS
            TO TRUE
           MOVE 220
             TO P250-INT-FIELD-220
           SET VALUE-PRESENT IN P250-INT-FIELD-221-FLAGS
            TO TRUE
           MOVE 221
             TO P250-INT-FIELD-221
           SET VALUE-PRESENT IN P250-INT-FIELD-222-FLAGS
            TO TRUE
           MOVE 222
             TO P250-INT-FIELD-222
           SET VALUE-PRESENT IN P250-INT-FIELD-223-FLAGS
            TO TRUE
           MOVE 223
             TO P250-INT-FIELD-223
           SET VALUE-PRESENT IN P250-INT-FIELD-224-FLAGS
            TO TRUE
           MOVE 224
             TO P250-INT-FIELD-224
           SET VALUE-PRESENT IN P250-INT-FIELD-225-FLAGS
            TO TRUE
           MOVE 225
             TO P250-INT-FIELD-225
           SET VALUE-PRESENT IN P250-INT-FIELD-226-FLAGS
            TO TRUE
           MOVE 226
             TO P250-INT-FIELD-226
           SET VALUE-PRESENT IN P250-INT-FIELD-227-FLAGS
            TO TRUE
           MOVE 227
             TO P250-INT-FIELD-227
           SET VALUE-PRESENT IN P250-INT-FIELD-228-FLAGS
            TO TRUE
           MOVE 228
             TO P250-INT-FIELD-228
           SET VALUE-PRESENT IN P250-INT-FIELD-229-FLAGS
            TO TRUE
           MOVE 229
             TO P250-INT-FIELD-229
           SET VALUE-PRESENT IN P250-INT-FIELD-230-FLAGS
            TO TRUE
           MOVE 230
             TO P250-INT-FIELD-230
           SET VALUE-PRESENT IN P250-INT-FIELD-231-FLAGS
            TO TRUE
           MOVE 231
             TO P250-INT-FIELD-231
           SET VALUE-PRESENT IN P250-INT-FIELD-232-FLAGS
            TO TRUE
           MOVE 232
             TO P250-INT-FIELD-232
           SET VALUE-PRESENT IN P250-INT-FIELD-233-FLAGS
            TO TRUE
           MOVE 233
             TO P250-INT-FIELD-233
           SET VALUE-PRESENT IN P250-INT-FIELD-234-FLAGS
            TO TRUE
           MOVE 234
             TO P250-INT-FIELD-234
           SET VALUE-PRESENT IN P250-INT-FIELD-235-FLAGS
            TO TRUE
           MOVE 235
             TO P250-INT-FIELD-235
           SET VALUE-PRESENT IN P250-INT-FIELD-236-FLAGS
            TO TRUE
           MOVE 236
             TO P250-INT-FIELD-236
           SET VALUE-PRESENT IN P250-INT-FIELD-237-FLAGS
            TO TRUE
           MOVE 237
             TO P250-INT-FIELD-237
           SET VALUE-PRESENT IN P250-INT-FIELD-238-FLAGS
            TO TRUE
           MOVE 238
             TO P250-INT-FIELD-238
           SET VALUE-PRESENT IN P250-INT-FIELD-239-FLAGS
            TO TRUE
           MOVE 239
             TO P250-INT-FIELD-239
           SET VALUE-PRESENT IN P250-INT-FIELD-240-FLAGS
            TO TRUE
           MOVE 240
             TO P250-INT-FIELD-240
           SET VALUE-PRESENT IN P250-INT-FIELD-241-FLAGS
            TO TRUE
           MOVE 241
             TO P250-INT-FIELD-241
           SET VALUE-PRESENT IN P250-INT-FIELD-242-FLAGS
            TO TRUE
           MOVE 242
             TO P250-INT-FIELD-242
           SET VALUE-PRESENT IN P250-INT-FIELD-243-FLAGS
            TO TRUE
           MOVE 243
             TO P250-INT-FIELD-243
           SET VALUE-PRESENT IN P250-INT-FIELD-244-FLAGS
            TO TRUE
           MOVE 244
             TO P250-INT-FIELD-244
           SET VALUE-PRESENT IN P250-INT-FIELD-245-FLAGS
            TO TRUE
           MOVE 245
             TO P250-INT-FIELD-245
           SET VALUE-PRESENT IN P250-INT-FIELD-246-FLAGS
            TO TRUE
           MOVE 246
             TO P250-INT-FIELD-246
           SET VALUE-PRESENT IN P250-INT-FIELD-247-FLAGS
            TO TRUE
           MOVE 247
             TO P250-INT-FIELD-247
           SET VALUE-PRESENT IN P250-INT-FIELD-248-FLAGS
            TO TRUE
           MOVE 248
             TO P250-INT-FIELD-248
           SET VALUE-PRESENT IN P250-INT-FIELD-249-FLAGS
            TO TRUE
           MOVE 249
             TO P250-INT-FIELD-249
           SET VALUE-PRESENT IN P250-INT-FIELD-250-FLAGS
            TO TRUE
           MOVE 250
             TO P250-INT-FIELD-250

           SET VALUE-PRESENT IN P250-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P250-STRING-FIELD-1
           SET VALUE-PRESENT IN P250-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P250-STRING-FIELD-2
           SET VALUE-PRESENT IN P250-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P250-STRING-FIELD-3
           SET VALUE-PRESENT IN P250-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P250-STRING-FIELD-4
           SET VALUE-PRESENT IN P250-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P250-STRING-FIELD-5
           SET VALUE-PRESENT IN P250-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P250-STRING-FIELD-6
           SET VALUE-PRESENT IN P250-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P250-STRING-FIELD-7
           SET VALUE-PRESENT IN P250-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P250-STRING-FIELD-8
           SET VALUE-PRESENT IN P250-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P250-STRING-FIELD-9
           SET VALUE-PRESENT IN P250-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P250-STRING-FIELD-10
           SET VALUE-PRESENT IN P250-STRING-FIELD-11-FLAGS
            TO TRUE
           MOVE '11'
             TO P250-STRING-FIELD-11
           SET VALUE-PRESENT IN P250-STRING-FIELD-12-FLAGS
            TO TRUE
           MOVE '12'
             TO P250-STRING-FIELD-12
           SET VALUE-PRESENT IN P250-STRING-FIELD-13-FLAGS
            TO TRUE
           MOVE '13'
             TO P250-STRING-FIELD-13
           SET VALUE-PRESENT IN P250-STRING-FIELD-14-FLAGS
            TO TRUE
           MOVE '14'
             TO P250-STRING-FIELD-14
           SET VALUE-PRESENT IN P250-STRING-FIELD-15-FLAGS
            TO TRUE
           MOVE '15'
             TO P250-STRING-FIELD-15
           SET VALUE-PRESENT IN P250-STRING-FIELD-16-FLAGS
            TO TRUE
           MOVE '16'
             TO P250-STRING-FIELD-16
           SET VALUE-PRESENT IN P250-STRING-FIELD-17-FLAGS
            TO TRUE
           MOVE '17'
             TO P250-STRING-FIELD-17
           SET VALUE-PRESENT IN P250-STRING-FIELD-18-FLAGS
            TO TRUE
           MOVE '18'
             TO P250-STRING-FIELD-18
           SET VALUE-PRESENT IN P250-STRING-FIELD-19-FLAGS
            TO TRUE
           MOVE '19'
             TO P250-STRING-FIELD-19
           SET VALUE-PRESENT IN P250-STRING-FIELD-20-FLAGS
            TO TRUE
           MOVE '20'
             TO P250-STRING-FIELD-20
           SET VALUE-PRESENT IN P250-STRING-FIELD-21-FLAGS
            TO TRUE
           MOVE '21'
             TO P250-STRING-FIELD-21
           SET VALUE-PRESENT IN P250-STRING-FIELD-22-FLAGS
            TO TRUE
           MOVE '22'
             TO P250-STRING-FIELD-22
           SET VALUE-PRESENT IN P250-STRING-FIELD-23-FLAGS
            TO TRUE
           MOVE '23'
             TO P250-STRING-FIELD-23
           SET VALUE-PRESENT IN P250-STRING-FIELD-24-FLAGS
            TO TRUE
           MOVE '24'
             TO P250-STRING-FIELD-24
           SET VALUE-PRESENT IN P250-STRING-FIELD-25-FLAGS
            TO TRUE
           MOVE '25'
             TO P250-STRING-FIELD-25
           SET VALUE-PRESENT IN P250-STRING-FIELD-26-FLAGS
            TO TRUE
           MOVE '26'
             TO P250-STRING-FIELD-26
           SET VALUE-PRESENT IN P250-STRING-FIELD-27-FLAGS
            TO TRUE
           MOVE '27'
             TO P250-STRING-FIELD-27
           SET VALUE-PRESENT IN P250-STRING-FIELD-28-FLAGS
            TO TRUE
           MOVE '28'
             TO P250-STRING-FIELD-28
           SET VALUE-PRESENT IN P250-STRING-FIELD-29-FLAGS
            TO TRUE
           MOVE '29'
             TO P250-STRING-FIELD-29
           SET VALUE-PRESENT IN P250-STRING-FIELD-30-FLAGS
            TO TRUE
           MOVE '30'
             TO P250-STRING-FIELD-30
           SET VALUE-PRESENT IN P250-STRING-FIELD-31-FLAGS
            TO TRUE
           MOVE '31'
             TO P250-STRING-FIELD-31
           SET VALUE-PRESENT IN P250-STRING-FIELD-32-FLAGS
            TO TRUE
           MOVE '32'
             TO P250-STRING-FIELD-32
           SET VALUE-PRESENT IN P250-STRING-FIELD-33-FLAGS
            TO TRUE
           MOVE '33'
             TO P250-STRING-FIELD-33
           SET VALUE-PRESENT IN P250-STRING-FIELD-34-FLAGS
            TO TRUE
           MOVE '34'
             TO P250-STRING-FIELD-34
           SET VALUE-PRESENT IN P250-STRING-FIELD-35-FLAGS
            TO TRUE
           MOVE '35'
             TO P250-STRING-FIELD-35
           SET VALUE-PRESENT IN P250-STRING-FIELD-36-FLAGS
            TO TRUE
           MOVE '36'
             TO P250-STRING-FIELD-36
           SET VALUE-PRESENT IN P250-STRING-FIELD-37-FLAGS
            TO TRUE
           MOVE '37'
             TO P250-STRING-FIELD-37
           SET VALUE-PRESENT IN P250-STRING-FIELD-38-FLAGS
            TO TRUE
           MOVE '38'
             TO P250-STRING-FIELD-38
           SET VALUE-PRESENT IN P250-STRING-FIELD-39-FLAGS
            TO TRUE
           MOVE '39'
             TO P250-STRING-FIELD-39
           SET VALUE-PRESENT IN P250-STRING-FIELD-40-FLAGS
            TO TRUE
           MOVE '40'
             TO P250-STRING-FIELD-40
           SET VALUE-PRESENT IN P250-STRING-FIELD-41-FLAGS
            TO TRUE
           MOVE '41'
             TO P250-STRING-FIELD-41
           SET VALUE-PRESENT IN P250-STRING-FIELD-42-FLAGS
            TO TRUE
           MOVE '42'
             TO P250-STRING-FIELD-42
           SET VALUE-PRESENT IN P250-STRING-FIELD-43-FLAGS
            TO TRUE
           MOVE '43'
             TO P250-STRING-FIELD-43
           SET VALUE-PRESENT IN P250-STRING-FIELD-44-FLAGS
            TO TRUE
           MOVE '44'
             TO P250-STRING-FIELD-44
           SET VALUE-PRESENT IN P250-STRING-FIELD-45-FLAGS
            TO TRUE
           MOVE '45'
             TO P250-STRING-FIELD-45
           SET VALUE-PRESENT IN P250-STRING-FIELD-46-FLAGS
            TO TRUE
           MOVE '46'
             TO P250-STRING-FIELD-46
           SET VALUE-PRESENT IN P250-STRING-FIELD-47-FLAGS
            TO TRUE
           MOVE '47'
             TO P250-STRING-FIELD-47
           SET VALUE-PRESENT IN P250-STRING-FIELD-48-FLAGS
            TO TRUE
           MOVE '48'
             TO P250-STRING-FIELD-48
           SET VALUE-PRESENT IN P250-STRING-FIELD-49-FLAGS
            TO TRUE
           MOVE '49'
             TO P250-STRING-FIELD-49
           SET VALUE-PRESENT IN P250-STRING-FIELD-50-FLAGS
            TO TRUE
           MOVE '50'
             TO P250-STRING-FIELD-50
           SET VALUE-PRESENT IN P250-STRING-FIELD-51-FLAGS
            TO TRUE
           MOVE '51'
             TO P250-STRING-FIELD-51
           SET VALUE-PRESENT IN P250-STRING-FIELD-52-FLAGS
            TO TRUE
           MOVE '52'
             TO P250-STRING-FIELD-52
           SET VALUE-PRESENT IN P250-STRING-FIELD-53-FLAGS
            TO TRUE
           MOVE '53'
             TO P250-STRING-FIELD-53
           SET VALUE-PRESENT IN P250-STRING-FIELD-54-FLAGS
            TO TRUE
           MOVE '54'
             TO P250-STRING-FIELD-54
           SET VALUE-PRESENT IN P250-STRING-FIELD-55-FLAGS
            TO TRUE
           MOVE '55'
             TO P250-STRING-FIELD-55
           SET VALUE-PRESENT IN P250-STRING-FIELD-56-FLAGS
            TO TRUE
           MOVE '56'
             TO P250-STRING-FIELD-56
           SET VALUE-PRESENT IN P250-STRING-FIELD-57-FLAGS
            TO TRUE
           MOVE '57'
             TO P250-STRING-FIELD-57
           SET VALUE-PRESENT IN P250-STRING-FIELD-58-FLAGS
            TO TRUE
           MOVE '58'
             TO P250-STRING-FIELD-58
           SET VALUE-PRESENT IN P250-STRING-FIELD-59-FLAGS
            TO TRUE
           MOVE '59'
             TO P250-STRING-FIELD-59
           SET VALUE-PRESENT IN P250-STRING-FIELD-60-FLAGS
            TO TRUE
           MOVE '60'
             TO P250-STRING-FIELD-60
           SET VALUE-PRESENT IN P250-STRING-FIELD-61-FLAGS
            TO TRUE
           MOVE '61'
             TO P250-STRING-FIELD-61
           SET VALUE-PRESENT IN P250-STRING-FIELD-62-FLAGS
            TO TRUE
           MOVE '62'
             TO P250-STRING-FIELD-62
           SET VALUE-PRESENT IN P250-STRING-FIELD-63-FLAGS
            TO TRUE
           MOVE '63'
             TO P250-STRING-FIELD-63
           SET VALUE-PRESENT IN P250-STRING-FIELD-64-FLAGS
            TO TRUE
           MOVE '64'
             TO P250-STRING-FIELD-64
           SET VALUE-PRESENT IN P250-STRING-FIELD-65-FLAGS
            TO TRUE
           MOVE '65'
             TO P250-STRING-FIELD-65
           SET VALUE-PRESENT IN P250-STRING-FIELD-66-FLAGS
            TO TRUE
           MOVE '66'
             TO P250-STRING-FIELD-66
           SET VALUE-PRESENT IN P250-STRING-FIELD-67-FLAGS
            TO TRUE
           MOVE '67'
             TO P250-STRING-FIELD-67
           SET VALUE-PRESENT IN P250-STRING-FIELD-68-FLAGS
            TO TRUE
           MOVE '68'
             TO P250-STRING-FIELD-68
           SET VALUE-PRESENT IN P250-STRING-FIELD-69-FLAGS
            TO TRUE
           MOVE '69'
             TO P250-STRING-FIELD-69
           SET VALUE-PRESENT IN P250-STRING-FIELD-70-FLAGS
            TO TRUE
           MOVE '70'
             TO P250-STRING-FIELD-70
           SET VALUE-PRESENT IN P250-STRING-FIELD-71-FLAGS
            TO TRUE
           MOVE '71'
             TO P250-STRING-FIELD-71
           SET VALUE-PRESENT IN P250-STRING-FIELD-72-FLAGS
            TO TRUE
           MOVE '72'
             TO P250-STRING-FIELD-72
           SET VALUE-PRESENT IN P250-STRING-FIELD-73-FLAGS
            TO TRUE
           MOVE '73'
             TO P250-STRING-FIELD-73
           SET VALUE-PRESENT IN P250-STRING-FIELD-74-FLAGS
            TO TRUE
           MOVE '74'
             TO P250-STRING-FIELD-74
           SET VALUE-PRESENT IN P250-STRING-FIELD-75-FLAGS
            TO TRUE
           MOVE '75'
             TO P250-STRING-FIELD-75
           SET VALUE-PRESENT IN P250-STRING-FIELD-76-FLAGS
            TO TRUE
           MOVE '76'
             TO P250-STRING-FIELD-76
           SET VALUE-PRESENT IN P250-STRING-FIELD-77-FLAGS
            TO TRUE
           MOVE '77'
             TO P250-STRING-FIELD-77
           SET VALUE-PRESENT IN P250-STRING-FIELD-78-FLAGS
            TO TRUE
           MOVE '78'
             TO P250-STRING-FIELD-78
           SET VALUE-PRESENT IN P250-STRING-FIELD-79-FLAGS
            TO TRUE
           MOVE '79'
             TO P250-STRING-FIELD-79
           SET VALUE-PRESENT IN P250-STRING-FIELD-80-FLAGS
            TO TRUE
           MOVE '80'
             TO P250-STRING-FIELD-80
           SET VALUE-PRESENT IN P250-STRING-FIELD-81-FLAGS
            TO TRUE
           MOVE '81'
             TO P250-STRING-FIELD-81
           SET VALUE-PRESENT IN P250-STRING-FIELD-82-FLAGS
            TO TRUE
           MOVE '82'
             TO P250-STRING-FIELD-82
           SET VALUE-PRESENT IN P250-STRING-FIELD-83-FLAGS
            TO TRUE
           MOVE '83'
             TO P250-STRING-FIELD-83
           SET VALUE-PRESENT IN P250-STRING-FIELD-84-FLAGS
            TO TRUE
           MOVE '84'
             TO P250-STRING-FIELD-84
           SET VALUE-PRESENT IN P250-STRING-FIELD-85-FLAGS
            TO TRUE
           MOVE '85'
             TO P250-STRING-FIELD-85
           SET VALUE-PRESENT IN P250-STRING-FIELD-86-FLAGS
            TO TRUE
           MOVE '86'
             TO P250-STRING-FIELD-86
           SET VALUE-PRESENT IN P250-STRING-FIELD-87-FLAGS
            TO TRUE
           MOVE '87'
             TO P250-STRING-FIELD-87
           SET VALUE-PRESENT IN P250-STRING-FIELD-88-FLAGS
            TO TRUE
           MOVE '88'
             TO P250-STRING-FIELD-88
           SET VALUE-PRESENT IN P250-STRING-FIELD-89-FLAGS
            TO TRUE
           MOVE '89'
             TO P250-STRING-FIELD-89
           SET VALUE-PRESENT IN P250-STRING-FIELD-90-FLAGS
            TO TRUE
           MOVE '90'
             TO P250-STRING-FIELD-90
           SET VALUE-PRESENT IN P250-STRING-FIELD-91-FLAGS
            TO TRUE
           MOVE '91'
             TO P250-STRING-FIELD-91
           SET VALUE-PRESENT IN P250-STRING-FIELD-92-FLAGS
            TO TRUE
           MOVE '92'
             TO P250-STRING-FIELD-92
           SET VALUE-PRESENT IN P250-STRING-FIELD-93-FLAGS
            TO TRUE
           MOVE '93'
             TO P250-STRING-FIELD-93
           SET VALUE-PRESENT IN P250-STRING-FIELD-94-FLAGS
            TO TRUE
           MOVE '94'
             TO P250-STRING-FIELD-94
           SET VALUE-PRESENT IN P250-STRING-FIELD-95-FLAGS
            TO TRUE
           MOVE '95'
             TO P250-STRING-FIELD-95
           SET VALUE-PRESENT IN P250-STRING-FIELD-96-FLAGS
            TO TRUE
           MOVE '96'
             TO P250-STRING-FIELD-96
           SET VALUE-PRESENT IN P250-STRING-FIELD-97-FLAGS
            TO TRUE
           MOVE '97'
             TO P250-STRING-FIELD-97
           SET VALUE-PRESENT IN P250-STRING-FIELD-98-FLAGS
            TO TRUE
           MOVE '98'
             TO P250-STRING-FIELD-98
           SET VALUE-PRESENT IN P250-STRING-FIELD-99-FLAGS
            TO TRUE
           MOVE '99'
             TO P250-STRING-FIELD-99
           SET VALUE-PRESENT IN P250-STRING-FIELD-100-FLAGS
            TO TRUE
           MOVE '100'
             TO P250-STRING-FIELD-100
           SET VALUE-PRESENT IN P250-STRING-FIELD-101-FLAGS
            TO TRUE
           MOVE '101'
             TO P250-STRING-FIELD-101
           SET VALUE-PRESENT IN P250-STRING-FIELD-102-FLAGS
            TO TRUE
           MOVE '102'
             TO P250-STRING-FIELD-102
           SET VALUE-PRESENT IN P250-STRING-FIELD-103-FLAGS
            TO TRUE
           MOVE '103'
             TO P250-STRING-FIELD-103
           SET VALUE-PRESENT IN P250-STRING-FIELD-104-FLAGS
            TO TRUE
           MOVE '104'
             TO P250-STRING-FIELD-104
           SET VALUE-PRESENT IN P250-STRING-FIELD-105-FLAGS
            TO TRUE
           MOVE '105'
             TO P250-STRING-FIELD-105
           SET VALUE-PRESENT IN P250-STRING-FIELD-106-FLAGS
            TO TRUE
           MOVE '106'
             TO P250-STRING-FIELD-106
           SET VALUE-PRESENT IN P250-STRING-FIELD-107-FLAGS
            TO TRUE
           MOVE '107'
             TO P250-STRING-FIELD-107
           SET VALUE-PRESENT IN P250-STRING-FIELD-108-FLAGS
            TO TRUE
           MOVE '108'
             TO P250-STRING-FIELD-108
           SET VALUE-PRESENT IN P250-STRING-FIELD-109-FLAGS
            TO TRUE
           MOVE '109'
             TO P250-STRING-FIELD-109
           SET VALUE-PRESENT IN P250-STRING-FIELD-110-FLAGS
            TO TRUE
           MOVE '110'
             TO P250-STRING-FIELD-110
           SET VALUE-PRESENT IN P250-STRING-FIELD-111-FLAGS
            TO TRUE
           MOVE '111'
             TO P250-STRING-FIELD-111
           SET VALUE-PRESENT IN P250-STRING-FIELD-112-FLAGS
            TO TRUE
           MOVE '112'
             TO P250-STRING-FIELD-112
           SET VALUE-PRESENT IN P250-STRING-FIELD-113-FLAGS
            TO TRUE
           MOVE '113'
             TO P250-STRING-FIELD-113
           SET VALUE-PRESENT IN P250-STRING-FIELD-114-FLAGS
            TO TRUE
           MOVE '114'
             TO P250-STRING-FIELD-114
           SET VALUE-PRESENT IN P250-STRING-FIELD-115-FLAGS
            TO TRUE
           MOVE '115'
             TO P250-STRING-FIELD-115
           SET VALUE-PRESENT IN P250-STRING-FIELD-116-FLAGS
            TO TRUE
           MOVE '116'
             TO P250-STRING-FIELD-116
           SET VALUE-PRESENT IN P250-STRING-FIELD-117-FLAGS
            TO TRUE
           MOVE '117'
             TO P250-STRING-FIELD-117
           SET VALUE-PRESENT IN P250-STRING-FIELD-118-FLAGS
            TO TRUE
           MOVE '118'
             TO P250-STRING-FIELD-118
           SET VALUE-PRESENT IN P250-STRING-FIELD-119-FLAGS
            TO TRUE
           MOVE '119'
             TO P250-STRING-FIELD-119
           SET VALUE-PRESENT IN P250-STRING-FIELD-120-FLAGS
            TO TRUE
           MOVE '120'
             TO P250-STRING-FIELD-120
           SET VALUE-PRESENT IN P250-STRING-FIELD-121-FLAGS
            TO TRUE
           MOVE '121'
             TO P250-STRING-FIELD-121
           SET VALUE-PRESENT IN P250-STRING-FIELD-122-FLAGS
            TO TRUE
           MOVE '122'
             TO P250-STRING-FIELD-122
           SET VALUE-PRESENT IN P250-STRING-FIELD-123-FLAGS
            TO TRUE
           MOVE '123'
             TO P250-STRING-FIELD-123
           SET VALUE-PRESENT IN P250-STRING-FIELD-124-FLAGS
            TO TRUE
           MOVE '124'
             TO P250-STRING-FIELD-124
           SET VALUE-PRESENT IN P250-STRING-FIELD-125-FLAGS
            TO TRUE
           MOVE '125'
             TO P250-STRING-FIELD-125
           SET VALUE-PRESENT IN P250-STRING-FIELD-126-FLAGS
            TO TRUE
           MOVE '126'
             TO P250-STRING-FIELD-126
           SET VALUE-PRESENT IN P250-STRING-FIELD-127-FLAGS
            TO TRUE
           MOVE '127'
             TO P250-STRING-FIELD-127
           SET VALUE-PRESENT IN P250-STRING-FIELD-128-FLAGS
            TO TRUE
           MOVE '128'
             TO P250-STRING-FIELD-128
           SET VALUE-PRESENT IN P250-STRING-FIELD-129-FLAGS
            TO TRUE
           MOVE '129'
             TO P250-STRING-FIELD-129
           SET VALUE-PRESENT IN P250-STRING-FIELD-130-FLAGS
            TO TRUE
           MOVE '130'
             TO P250-STRING-FIELD-130
           SET VALUE-PRESENT IN P250-STRING-FIELD-131-FLAGS
            TO TRUE
           MOVE '131'
             TO P250-STRING-FIELD-131
           SET VALUE-PRESENT IN P250-STRING-FIELD-132-FLAGS
            TO TRUE
           MOVE '132'
             TO P250-STRING-FIELD-132
           SET VALUE-PRESENT IN P250-STRING-FIELD-133-FLAGS
            TO TRUE
           MOVE '133'
             TO P250-STRING-FIELD-133
           SET VALUE-PRESENT IN P250-STRING-FIELD-134-FLAGS
            TO TRUE
           MOVE '134'
             TO P250-STRING-FIELD-134
           SET VALUE-PRESENT IN P250-STRING-FIELD-135-FLAGS
            TO TRUE
           MOVE '135'
             TO P250-STRING-FIELD-135
           SET VALUE-PRESENT IN P250-STRING-FIELD-136-FLAGS
            TO TRUE
           MOVE '136'
             TO P250-STRING-FIELD-136
           SET VALUE-PRESENT IN P250-STRING-FIELD-137-FLAGS
            TO TRUE
           MOVE '137'
             TO P250-STRING-FIELD-137
           SET VALUE-PRESENT IN P250-STRING-FIELD-138-FLAGS
            TO TRUE
           MOVE '138'
             TO P250-STRING-FIELD-138
           SET VALUE-PRESENT IN P250-STRING-FIELD-139-FLAGS
            TO TRUE
           MOVE '139'
             TO P250-STRING-FIELD-139
           SET VALUE-PRESENT IN P250-STRING-FIELD-140-FLAGS
            TO TRUE
           MOVE '140'
             TO P250-STRING-FIELD-140
           SET VALUE-PRESENT IN P250-STRING-FIELD-141-FLAGS
            TO TRUE
           MOVE '141'
             TO P250-STRING-FIELD-141
           SET VALUE-PRESENT IN P250-STRING-FIELD-142-FLAGS
            TO TRUE
           MOVE '142'
             TO P250-STRING-FIELD-142
           SET VALUE-PRESENT IN P250-STRING-FIELD-143-FLAGS
            TO TRUE
           MOVE '143'
             TO P250-STRING-FIELD-143
           SET VALUE-PRESENT IN P250-STRING-FIELD-144-FLAGS
            TO TRUE
           MOVE '144'
             TO P250-STRING-FIELD-144
           SET VALUE-PRESENT IN P250-STRING-FIELD-145-FLAGS
            TO TRUE
           MOVE '145'
             TO P250-STRING-FIELD-145
           SET VALUE-PRESENT IN P250-STRING-FIELD-146-FLAGS
            TO TRUE
           MOVE '146'
             TO P250-STRING-FIELD-146
           SET VALUE-PRESENT IN P250-STRING-FIELD-147-FLAGS
            TO TRUE
           MOVE '147'
             TO P250-STRING-FIELD-147
           SET VALUE-PRESENT IN P250-STRING-FIELD-148-FLAGS
            TO TRUE
           MOVE '148'
             TO P250-STRING-FIELD-148
           SET VALUE-PRESENT IN P250-STRING-FIELD-149-FLAGS
            TO TRUE
           MOVE '149'
             TO P250-STRING-FIELD-149
           SET VALUE-PRESENT IN P250-STRING-FIELD-150-FLAGS
            TO TRUE
           MOVE '150'
             TO P250-STRING-FIELD-150
           SET VALUE-PRESENT IN P250-STRING-FIELD-151-FLAGS
            TO TRUE
           MOVE '151'
             TO P250-STRING-FIELD-151
           SET VALUE-PRESENT IN P250-STRING-FIELD-152-FLAGS
            TO TRUE
           MOVE '152'
             TO P250-STRING-FIELD-152
           SET VALUE-PRESENT IN P250-STRING-FIELD-153-FLAGS
            TO TRUE
           MOVE '153'
             TO P250-STRING-FIELD-153
           SET VALUE-PRESENT IN P250-STRING-FIELD-154-FLAGS
            TO TRUE
           MOVE '154'
             TO P250-STRING-FIELD-154
           SET VALUE-PRESENT IN P250-STRING-FIELD-155-FLAGS
            TO TRUE
           MOVE '155'
             TO P250-STRING-FIELD-155
           SET VALUE-PRESENT IN P250-STRING-FIELD-156-FLAGS
            TO TRUE
           MOVE '156'
             TO P250-STRING-FIELD-156
           SET VALUE-PRESENT IN P250-STRING-FIELD-157-FLAGS
            TO TRUE
           MOVE '157'
             TO P250-STRING-FIELD-157
           SET VALUE-PRESENT IN P250-STRING-FIELD-158-FLAGS
            TO TRUE
           MOVE '158'
             TO P250-STRING-FIELD-158
           SET VALUE-PRESENT IN P250-STRING-FIELD-159-FLAGS
            TO TRUE
           MOVE '159'
             TO P250-STRING-FIELD-159
           SET VALUE-PRESENT IN P250-STRING-FIELD-160-FLAGS
            TO TRUE
           MOVE '160'
             TO P250-STRING-FIELD-160
           SET VALUE-PRESENT IN P250-STRING-FIELD-161-FLAGS
            TO TRUE
           MOVE '161'
             TO P250-STRING-FIELD-161
           SET VALUE-PRESENT IN P250-STRING-FIELD-162-FLAGS
            TO TRUE
           MOVE '162'
             TO P250-STRING-FIELD-162
           SET VALUE-PRESENT IN P250-STRING-FIELD-163-FLAGS
            TO TRUE
           MOVE '163'
             TO P250-STRING-FIELD-163
           SET VALUE-PRESENT IN P250-STRING-FIELD-164-FLAGS
            TO TRUE
           MOVE '164'
             TO P250-STRING-FIELD-164
           SET VALUE-PRESENT IN P250-STRING-FIELD-165-FLAGS
            TO TRUE
           MOVE '165'
             TO P250-STRING-FIELD-165
           SET VALUE-PRESENT IN P250-STRING-FIELD-166-FLAGS
            TO TRUE
           MOVE '166'
             TO P250-STRING-FIELD-166
           SET VALUE-PRESENT IN P250-STRING-FIELD-167-FLAGS
            TO TRUE
           MOVE '167'
             TO P250-STRING-FIELD-167
           SET VALUE-PRESENT IN P250-STRING-FIELD-168-FLAGS
            TO TRUE
           MOVE '168'
             TO P250-STRING-FIELD-168
           SET VALUE-PRESENT IN P250-STRING-FIELD-169-FLAGS
            TO TRUE
           MOVE '169'
             TO P250-STRING-FIELD-169
           SET VALUE-PRESENT IN P250-STRING-FIELD-170-FLAGS
            TO TRUE
           MOVE '170'
             TO P250-STRING-FIELD-170
           SET VALUE-PRESENT IN P250-STRING-FIELD-171-FLAGS
            TO TRUE
           MOVE '171'
             TO P250-STRING-FIELD-171
           SET VALUE-PRESENT IN P250-STRING-FIELD-172-FLAGS
            TO TRUE
           MOVE '172'
             TO P250-STRING-FIELD-172
           SET VALUE-PRESENT IN P250-STRING-FIELD-173-FLAGS
            TO TRUE
           MOVE '173'
             TO P250-STRING-FIELD-173
           SET VALUE-PRESENT IN P250-STRING-FIELD-174-FLAGS
            TO TRUE
           MOVE '174'
             TO P250-STRING-FIELD-174
           SET VALUE-PRESENT IN P250-STRING-FIELD-175-FLAGS
            TO TRUE
           MOVE '175'
             TO P250-STRING-FIELD-175
           SET VALUE-PRESENT IN P250-STRING-FIELD-176-FLAGS
            TO TRUE
           MOVE '176'
             TO P250-STRING-FIELD-176
           SET VALUE-PRESENT IN P250-STRING-FIELD-177-FLAGS
            TO TRUE
           MOVE '177'
             TO P250-STRING-FIELD-177
           SET VALUE-PRESENT IN P250-STRING-FIELD-178-FLAGS
            TO TRUE
           MOVE '178'
             TO P250-STRING-FIELD-178
           SET VALUE-PRESENT IN P250-STRING-FIELD-179-FLAGS
            TO TRUE
           MOVE '179'
             TO P250-STRING-FIELD-179
           SET VALUE-PRESENT IN P250-STRING-FIELD-180-FLAGS
            TO TRUE
           MOVE '180'
             TO P250-STRING-FIELD-180
           SET VALUE-PRESENT IN P250-STRING-FIELD-181-FLAGS
            TO TRUE
           MOVE '181'
             TO P250-STRING-FIELD-181
           SET VALUE-PRESENT IN P250-STRING-FIELD-182-FLAGS
            TO TRUE
           MOVE '182'
             TO P250-STRING-FIELD-182
           SET VALUE-PRESENT IN P250-STRING-FIELD-183-FLAGS
            TO TRUE
           MOVE '183'
             TO P250-STRING-FIELD-183
           SET VALUE-PRESENT IN P250-STRING-FIELD-184-FLAGS
            TO TRUE
           MOVE '184'
             TO P250-STRING-FIELD-184
           SET VALUE-PRESENT IN P250-STRING-FIELD-185-FLAGS
            TO TRUE
           MOVE '185'
             TO P250-STRING-FIELD-185
           SET VALUE-PRESENT IN P250-STRING-FIELD-186-FLAGS
            TO TRUE
           MOVE '186'
             TO P250-STRING-FIELD-186
           SET VALUE-PRESENT IN P250-STRING-FIELD-187-FLAGS
            TO TRUE
           MOVE '187'
             TO P250-STRING-FIELD-187
           SET VALUE-PRESENT IN P250-STRING-FIELD-188-FLAGS
            TO TRUE
           MOVE '188'
             TO P250-STRING-FIELD-188
           SET VALUE-PRESENT IN P250-STRING-FIELD-189-FLAGS
            TO TRUE
           MOVE '189'
             TO P250-STRING-FIELD-189
           SET VALUE-PRESENT IN P250-STRING-FIELD-190-FLAGS
            TO TRUE
           MOVE '190'
             TO P250-STRING-FIELD-190
           SET VALUE-PRESENT IN P250-STRING-FIELD-191-FLAGS
            TO TRUE
           MOVE '191'
             TO P250-STRING-FIELD-191
           SET VALUE-PRESENT IN P250-STRING-FIELD-192-FLAGS
            TO TRUE
           MOVE '192'
             TO P250-STRING-FIELD-192
           SET VALUE-PRESENT IN P250-STRING-FIELD-193-FLAGS
            TO TRUE
           MOVE '193'
             TO P250-STRING-FIELD-193
           SET VALUE-PRESENT IN P250-STRING-FIELD-194-FLAGS
            TO TRUE
           MOVE '194'
             TO P250-STRING-FIELD-194
           SET VALUE-PRESENT IN P250-STRING-FIELD-195-FLAGS
            TO TRUE
           MOVE '195'
             TO P250-STRING-FIELD-195
           SET VALUE-PRESENT IN P250-STRING-FIELD-196-FLAGS
            TO TRUE
           MOVE '196'
             TO P250-STRING-FIELD-196
           SET VALUE-PRESENT IN P250-STRING-FIELD-197-FLAGS
            TO TRUE
           MOVE '197'
             TO P250-STRING-FIELD-197
           SET VALUE-PRESENT IN P250-STRING-FIELD-198-FLAGS
            TO TRUE
           MOVE '198'
             TO P250-STRING-FIELD-198
           SET VALUE-PRESENT IN P250-STRING-FIELD-199-FLAGS
            TO TRUE
           MOVE '199'
             TO P250-STRING-FIELD-199
           SET VALUE-PRESENT IN P250-STRING-FIELD-200-FLAGS
            TO TRUE
           MOVE '200'
             TO P250-STRING-FIELD-200
           SET VALUE-PRESENT IN P250-STRING-FIELD-201-FLAGS
            TO TRUE
           MOVE '201'
             TO P250-STRING-FIELD-201
           SET VALUE-PRESENT IN P250-STRING-FIELD-202-FLAGS
            TO TRUE
           MOVE '202'
             TO P250-STRING-FIELD-202
           SET VALUE-PRESENT IN P250-STRING-FIELD-203-FLAGS
            TO TRUE
           MOVE '203'
             TO P250-STRING-FIELD-203
           SET VALUE-PRESENT IN P250-STRING-FIELD-204-FLAGS
            TO TRUE
           MOVE '204'
             TO P250-STRING-FIELD-204
           SET VALUE-PRESENT IN P250-STRING-FIELD-205-FLAGS
            TO TRUE
           MOVE '205'
             TO P250-STRING-FIELD-205
           SET VALUE-PRESENT IN P250-STRING-FIELD-206-FLAGS
            TO TRUE
           MOVE '206'
             TO P250-STRING-FIELD-206
           SET VALUE-PRESENT IN P250-STRING-FIELD-207-FLAGS
            TO TRUE
           MOVE '207'
             TO P250-STRING-FIELD-207
           SET VALUE-PRESENT IN P250-STRING-FIELD-208-FLAGS
            TO TRUE
           MOVE '208'
             TO P250-STRING-FIELD-208
           SET VALUE-PRESENT IN P250-STRING-FIELD-209-FLAGS
            TO TRUE
           MOVE '209'
             TO P250-STRING-FIELD-209
           SET VALUE-PRESENT IN P250-STRING-FIELD-210-FLAGS
            TO TRUE
           MOVE '210'
             TO P250-STRING-FIELD-210
           SET VALUE-PRESENT IN P250-STRING-FIELD-211-FLAGS
            TO TRUE
           MOVE '211'
             TO P250-STRING-FIELD-211
           SET VALUE-PRESENT IN P250-STRING-FIELD-212-FLAGS
            TO TRUE
           MOVE '212'
             TO P250-STRING-FIELD-212
           SET VALUE-PRESENT IN P250-STRING-FIELD-213-FLAGS
            TO TRUE
           MOVE '213'
             TO P250-STRING-FIELD-213
           SET VALUE-PRESENT IN P250-STRING-FIELD-214-FLAGS
            TO TRUE
           MOVE '214'
             TO P250-STRING-FIELD-214
           SET VALUE-PRESENT IN P250-STRING-FIELD-215-FLAGS
            TO TRUE
           MOVE '215'
             TO P250-STRING-FIELD-215
           SET VALUE-PRESENT IN P250-STRING-FIELD-216-FLAGS
            TO TRUE
           MOVE '216'
             TO P250-STRING-FIELD-216
           SET VALUE-PRESENT IN P250-STRING-FIELD-217-FLAGS
            TO TRUE
           MOVE '217'
             TO P250-STRING-FIELD-217
           SET VALUE-PRESENT IN P250-STRING-FIELD-218-FLAGS
            TO TRUE
           MOVE '218'
             TO P250-STRING-FIELD-218
           SET VALUE-PRESENT IN P250-STRING-FIELD-219-FLAGS
            TO TRUE
           MOVE '219'
             TO P250-STRING-FIELD-219
           SET VALUE-PRESENT IN P250-STRING-FIELD-220-FLAGS
            TO TRUE
           MOVE '220'
             TO P250-STRING-FIELD-220
           SET VALUE-PRESENT IN P250-STRING-FIELD-221-FLAGS
            TO TRUE
           MOVE '221'
             TO P250-STRING-FIELD-221
           SET VALUE-PRESENT IN P250-STRING-FIELD-222-FLAGS
            TO TRUE
           MOVE '222'
             TO P250-STRING-FIELD-222
           SET VALUE-PRESENT IN P250-STRING-FIELD-223-FLAGS
            TO TRUE
           MOVE '223'
             TO P250-STRING-FIELD-223
           SET VALUE-PRESENT IN P250-STRING-FIELD-224-FLAGS
            TO TRUE
           MOVE '224'
             TO P250-STRING-FIELD-224
           SET VALUE-PRESENT IN P250-STRING-FIELD-225-FLAGS
            TO TRUE
           MOVE '225'
             TO P250-STRING-FIELD-225
           SET VALUE-PRESENT IN P250-STRING-FIELD-226-FLAGS
            TO TRUE
           MOVE '226'
             TO P250-STRING-FIELD-226
           SET VALUE-PRESENT IN P250-STRING-FIELD-227-FLAGS
            TO TRUE
           MOVE '227'
             TO P250-STRING-FIELD-227
           SET VALUE-PRESENT IN P250-STRING-FIELD-228-FLAGS
            TO TRUE
           MOVE '228'
             TO P250-STRING-FIELD-228
           SET VALUE-PRESENT IN P250-STRING-FIELD-229-FLAGS
            TO TRUE
           MOVE '229'
             TO P250-STRING-FIELD-229
           SET VALUE-PRESENT IN P250-STRING-FIELD-230-FLAGS
            TO TRUE
           MOVE '230'
             TO P250-STRING-FIELD-230
           SET VALUE-PRESENT IN P250-STRING-FIELD-231-FLAGS
            TO TRUE
           MOVE '231'
             TO P250-STRING-FIELD-231
           SET VALUE-PRESENT IN P250-STRING-FIELD-232-FLAGS
            TO TRUE
           MOVE '232'
             TO P250-STRING-FIELD-232
           SET VALUE-PRESENT IN P250-STRING-FIELD-233-FLAGS
            TO TRUE
           MOVE '233'
             TO P250-STRING-FIELD-233
           SET VALUE-PRESENT IN P250-STRING-FIELD-234-FLAGS
            TO TRUE
           MOVE '234'
             TO P250-STRING-FIELD-234
           SET VALUE-PRESENT IN P250-STRING-FIELD-235-FLAGS
            TO TRUE
           MOVE '235'
             TO P250-STRING-FIELD-235
           SET VALUE-PRESENT IN P250-STRING-FIELD-236-FLAGS
            TO TRUE
           MOVE '236'
             TO P250-STRING-FIELD-236
           SET VALUE-PRESENT IN P250-STRING-FIELD-237-FLAGS
            TO TRUE
           MOVE '237'
             TO P250-STRING-FIELD-237
           SET VALUE-PRESENT IN P250-STRING-FIELD-238-FLAGS
            TO TRUE
           MOVE '238'
             TO P250-STRING-FIELD-238
           SET VALUE-PRESENT IN P250-STRING-FIELD-239-FLAGS
            TO TRUE
           MOVE '239'
             TO P250-STRING-FIELD-239
           SET VALUE-PRESENT IN P250-STRING-FIELD-240-FLAGS
            TO TRUE
           MOVE '240'
             TO P250-STRING-FIELD-240
           SET VALUE-PRESENT IN P250-STRING-FIELD-241-FLAGS
            TO TRUE
           MOVE '241'
             TO P250-STRING-FIELD-241
           SET VALUE-PRESENT IN P250-STRING-FIELD-242-FLAGS
            TO TRUE
           MOVE '242'
             TO P250-STRING-FIELD-242
           SET VALUE-PRESENT IN P250-STRING-FIELD-243-FLAGS
            TO TRUE
           MOVE '243'
             TO P250-STRING-FIELD-243
           SET VALUE-PRESENT IN P250-STRING-FIELD-244-FLAGS
            TO TRUE
           MOVE '244'
             TO P250-STRING-FIELD-244
           SET VALUE-PRESENT IN P250-STRING-FIELD-245-FLAGS
            TO TRUE
           MOVE '245'
             TO P250-STRING-FIELD-245
           SET VALUE-PRESENT IN P250-STRING-FIELD-246-FLAGS
            TO TRUE
           MOVE '246'
             TO P250-STRING-FIELD-246
           SET VALUE-PRESENT IN P250-STRING-FIELD-247-FLAGS
            TO TRUE
           MOVE '247'
             TO P250-STRING-FIELD-247
           SET VALUE-PRESENT IN P250-STRING-FIELD-248-FLAGS
            TO TRUE
           MOVE '248'
             TO P250-STRING-FIELD-248
           SET VALUE-PRESENT IN P250-STRING-FIELD-249-FLAGS
            TO TRUE
           MOVE '249'
             TO P250-STRING-FIELD-249
           SET VALUE-PRESENT IN P250-STRING-FIELD-250-FLAGS
            TO TRUE
           MOVE '250'
             TO P250-STRING-FIELD-250

           EXIT.

       CONVERT-STRUCTURE-SIZE-250 SECTION.
           MOVE 3 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-250
             BY REFERENCE CONSUMER-STRUCT-250                    
       
           EXIT.

       RUN-BENCHMARK-SIZE-500 SECTION.
           PERFORM INIT-STRUCTURE-SIZE-500
       
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE START-TIME-MS 
           
           PERFORM CONVERT-STRUCTURE-SIZE-500 NUMBER-OF-ITERATIONS TIMES
           
           CALL 'getCurrentTimeMs' USING
                BY REFERENCE END-TIME-MS
           
           COMPUTE DURATION-MS = (END-TIME-MS - START-TIME-MS)
           DISPLAY 'Benchmark size 500: ' DURATION-MS 'ms' UPON CONSOLE

           EXIT.
           
       INIT-STRUCTURE-SIZE-500 SECTION.
           SET VALUE-PRESENT IN P500-TEST-STRUCT-500-FLAGS
            TO TRUE

           SET VALUE-PRESENT IN P500-INT-FIELD-1-FLAGS
            TO TRUE
           MOVE 1
             TO P500-INT-FIELD-1
           SET VALUE-PRESENT IN P500-INT-FIELD-2-FLAGS
            TO TRUE
           MOVE 2
             TO P500-INT-FIELD-2
           SET VALUE-PRESENT IN P500-INT-FIELD-3-FLAGS
            TO TRUE
           MOVE 3
             TO P500-INT-FIELD-3
           SET VALUE-PRESENT IN P500-INT-FIELD-4-FLAGS
            TO TRUE
           MOVE 4
             TO P500-INT-FIELD-4
           SET VALUE-PRESENT IN P500-INT-FIELD-5-FLAGS
            TO TRUE
           MOVE 5
             TO P500-INT-FIELD-5
           SET VALUE-PRESENT IN P500-INT-FIELD-6-FLAGS
            TO TRUE
           MOVE 6
             TO P500-INT-FIELD-6
           SET VALUE-PRESENT IN P500-INT-FIELD-7-FLAGS
            TO TRUE
           MOVE 7
             TO P500-INT-FIELD-7
           SET VALUE-PRESENT IN P500-INT-FIELD-8-FLAGS
            TO TRUE
           MOVE 8
             TO P500-INT-FIELD-8
           SET VALUE-PRESENT IN P500-INT-FIELD-9-FLAGS
            TO TRUE
           MOVE 9
             TO P500-INT-FIELD-9
           SET VALUE-PRESENT IN P500-INT-FIELD-10-FLAGS
            TO TRUE
           MOVE 10
             TO P500-INT-FIELD-10
           SET VALUE-PRESENT IN P500-INT-FIELD-11-FLAGS
            TO TRUE
           MOVE 11
             TO P500-INT-FIELD-11
           SET VALUE-PRESENT IN P500-INT-FIELD-12-FLAGS
            TO TRUE
           MOVE 12
             TO P500-INT-FIELD-12
           SET VALUE-PRESENT IN P500-INT-FIELD-13-FLAGS
            TO TRUE
           MOVE 13
             TO P500-INT-FIELD-13
           SET VALUE-PRESENT IN P500-INT-FIELD-14-FLAGS
            TO TRUE
           MOVE 14
             TO P500-INT-FIELD-14
           SET VALUE-PRESENT IN P500-INT-FIELD-15-FLAGS
            TO TRUE
           MOVE 15
             TO P500-INT-FIELD-15
           SET VALUE-PRESENT IN P500-INT-FIELD-16-FLAGS
            TO TRUE
           MOVE 16
             TO P500-INT-FIELD-16
           SET VALUE-PRESENT IN P500-INT-FIELD-17-FLAGS
            TO TRUE
           MOVE 17
             TO P500-INT-FIELD-17
           SET VALUE-PRESENT IN P500-INT-FIELD-18-FLAGS
            TO TRUE
           MOVE 18
             TO P500-INT-FIELD-18
           SET VALUE-PRESENT IN P500-INT-FIELD-19-FLAGS
            TO TRUE
           MOVE 19
             TO P500-INT-FIELD-19
           SET VALUE-PRESENT IN P500-INT-FIELD-20-FLAGS
            TO TRUE
           MOVE 20
             TO P500-INT-FIELD-20
           SET VALUE-PRESENT IN P500-INT-FIELD-21-FLAGS
            TO TRUE
           MOVE 21
             TO P500-INT-FIELD-21
           SET VALUE-PRESENT IN P500-INT-FIELD-22-FLAGS
            TO TRUE
           MOVE 22
             TO P500-INT-FIELD-22
           SET VALUE-PRESENT IN P500-INT-FIELD-23-FLAGS
            TO TRUE
           MOVE 23
             TO P500-INT-FIELD-23
           SET VALUE-PRESENT IN P500-INT-FIELD-24-FLAGS
            TO TRUE
           MOVE 24
             TO P500-INT-FIELD-24
           SET VALUE-PRESENT IN P500-INT-FIELD-25-FLAGS
            TO TRUE
           MOVE 25
             TO P500-INT-FIELD-25
           SET VALUE-PRESENT IN P500-INT-FIELD-26-FLAGS
            TO TRUE
           MOVE 26
             TO P500-INT-FIELD-26
           SET VALUE-PRESENT IN P500-INT-FIELD-27-FLAGS
            TO TRUE
           MOVE 27
             TO P500-INT-FIELD-27
           SET VALUE-PRESENT IN P500-INT-FIELD-28-FLAGS
            TO TRUE
           MOVE 28
             TO P500-INT-FIELD-28
           SET VALUE-PRESENT IN P500-INT-FIELD-29-FLAGS
            TO TRUE
           MOVE 29
             TO P500-INT-FIELD-29
           SET VALUE-PRESENT IN P500-INT-FIELD-30-FLAGS
            TO TRUE
           MOVE 30
             TO P500-INT-FIELD-30
           SET VALUE-PRESENT IN P500-INT-FIELD-31-FLAGS
            TO TRUE
           MOVE 31
             TO P500-INT-FIELD-31
           SET VALUE-PRESENT IN P500-INT-FIELD-32-FLAGS
            TO TRUE
           MOVE 32
             TO P500-INT-FIELD-32
           SET VALUE-PRESENT IN P500-INT-FIELD-33-FLAGS
            TO TRUE
           MOVE 33
             TO P500-INT-FIELD-33
           SET VALUE-PRESENT IN P500-INT-FIELD-34-FLAGS
            TO TRUE
           MOVE 34
             TO P500-INT-FIELD-34
           SET VALUE-PRESENT IN P500-INT-FIELD-35-FLAGS
            TO TRUE
           MOVE 35
             TO P500-INT-FIELD-35
           SET VALUE-PRESENT IN P500-INT-FIELD-36-FLAGS
            TO TRUE
           MOVE 36
             TO P500-INT-FIELD-36
           SET VALUE-PRESENT IN P500-INT-FIELD-37-FLAGS
            TO TRUE
           MOVE 37
             TO P500-INT-FIELD-37
           SET VALUE-PRESENT IN P500-INT-FIELD-38-FLAGS
            TO TRUE
           MOVE 38
             TO P500-INT-FIELD-38
           SET VALUE-PRESENT IN P500-INT-FIELD-39-FLAGS
            TO TRUE
           MOVE 39
             TO P500-INT-FIELD-39
           SET VALUE-PRESENT IN P500-INT-FIELD-40-FLAGS
            TO TRUE
           MOVE 40
             TO P500-INT-FIELD-40
           SET VALUE-PRESENT IN P500-INT-FIELD-41-FLAGS
            TO TRUE
           MOVE 41
             TO P500-INT-FIELD-41
           SET VALUE-PRESENT IN P500-INT-FIELD-42-FLAGS
            TO TRUE
           MOVE 42
             TO P500-INT-FIELD-42
           SET VALUE-PRESENT IN P500-INT-FIELD-43-FLAGS
            TO TRUE
           MOVE 43
             TO P500-INT-FIELD-43
           SET VALUE-PRESENT IN P500-INT-FIELD-44-FLAGS
            TO TRUE
           MOVE 44
             TO P500-INT-FIELD-44
           SET VALUE-PRESENT IN P500-INT-FIELD-45-FLAGS
            TO TRUE
           MOVE 45
             TO P500-INT-FIELD-45
           SET VALUE-PRESENT IN P500-INT-FIELD-46-FLAGS
            TO TRUE
           MOVE 46
             TO P500-INT-FIELD-46
           SET VALUE-PRESENT IN P500-INT-FIELD-47-FLAGS
            TO TRUE
           MOVE 47
             TO P500-INT-FIELD-47
           SET VALUE-PRESENT IN P500-INT-FIELD-48-FLAGS
            TO TRUE
           MOVE 48
             TO P500-INT-FIELD-48
           SET VALUE-PRESENT IN P500-INT-FIELD-49-FLAGS
            TO TRUE
           MOVE 49
             TO P500-INT-FIELD-49
           SET VALUE-PRESENT IN P500-INT-FIELD-50-FLAGS
            TO TRUE
           MOVE 50
             TO P500-INT-FIELD-50
           SET VALUE-PRESENT IN P500-INT-FIELD-51-FLAGS
            TO TRUE
           MOVE 51
             TO P500-INT-FIELD-51
           SET VALUE-PRESENT IN P500-INT-FIELD-52-FLAGS
            TO TRUE
           MOVE 52
             TO P500-INT-FIELD-52
           SET VALUE-PRESENT IN P500-INT-FIELD-53-FLAGS
            TO TRUE
           MOVE 53
             TO P500-INT-FIELD-53
           SET VALUE-PRESENT IN P500-INT-FIELD-54-FLAGS
            TO TRUE
           MOVE 54
             TO P500-INT-FIELD-54
           SET VALUE-PRESENT IN P500-INT-FIELD-55-FLAGS
            TO TRUE
           MOVE 55
             TO P500-INT-FIELD-55
           SET VALUE-PRESENT IN P500-INT-FIELD-56-FLAGS
            TO TRUE
           MOVE 56
             TO P500-INT-FIELD-56
           SET VALUE-PRESENT IN P500-INT-FIELD-57-FLAGS
            TO TRUE
           MOVE 57
             TO P500-INT-FIELD-57
           SET VALUE-PRESENT IN P500-INT-FIELD-58-FLAGS
            TO TRUE
           MOVE 58
             TO P500-INT-FIELD-58
           SET VALUE-PRESENT IN P500-INT-FIELD-59-FLAGS
            TO TRUE
           MOVE 59
             TO P500-INT-FIELD-59
           SET VALUE-PRESENT IN P500-INT-FIELD-60-FLAGS
            TO TRUE
           MOVE 60
             TO P500-INT-FIELD-60
           SET VALUE-PRESENT IN P500-INT-FIELD-61-FLAGS
            TO TRUE
           MOVE 61
             TO P500-INT-FIELD-61
           SET VALUE-PRESENT IN P500-INT-FIELD-62-FLAGS
            TO TRUE
           MOVE 62
             TO P500-INT-FIELD-62
           SET VALUE-PRESENT IN P500-INT-FIELD-63-FLAGS
            TO TRUE
           MOVE 63
             TO P500-INT-FIELD-63
           SET VALUE-PRESENT IN P500-INT-FIELD-64-FLAGS
            TO TRUE
           MOVE 64
             TO P500-INT-FIELD-64
           SET VALUE-PRESENT IN P500-INT-FIELD-65-FLAGS
            TO TRUE
           MOVE 65
             TO P500-INT-FIELD-65
           SET VALUE-PRESENT IN P500-INT-FIELD-66-FLAGS
            TO TRUE
           MOVE 66
             TO P500-INT-FIELD-66
           SET VALUE-PRESENT IN P500-INT-FIELD-67-FLAGS
            TO TRUE
           MOVE 67
             TO P500-INT-FIELD-67
           SET VALUE-PRESENT IN P500-INT-FIELD-68-FLAGS
            TO TRUE
           MOVE 68
             TO P500-INT-FIELD-68
           SET VALUE-PRESENT IN P500-INT-FIELD-69-FLAGS
            TO TRUE
           MOVE 69
             TO P500-INT-FIELD-69
           SET VALUE-PRESENT IN P500-INT-FIELD-70-FLAGS
            TO TRUE
           MOVE 70
             TO P500-INT-FIELD-70
           SET VALUE-PRESENT IN P500-INT-FIELD-71-FLAGS
            TO TRUE
           MOVE 71
             TO P500-INT-FIELD-71
           SET VALUE-PRESENT IN P500-INT-FIELD-72-FLAGS
            TO TRUE
           MOVE 72
             TO P500-INT-FIELD-72
           SET VALUE-PRESENT IN P500-INT-FIELD-73-FLAGS
            TO TRUE
           MOVE 73
             TO P500-INT-FIELD-73
           SET VALUE-PRESENT IN P500-INT-FIELD-74-FLAGS
            TO TRUE
           MOVE 74
             TO P500-INT-FIELD-74
           SET VALUE-PRESENT IN P500-INT-FIELD-75-FLAGS
            TO TRUE
           MOVE 75
             TO P500-INT-FIELD-75
           SET VALUE-PRESENT IN P500-INT-FIELD-76-FLAGS
            TO TRUE
           MOVE 76
             TO P500-INT-FIELD-76
           SET VALUE-PRESENT IN P500-INT-FIELD-77-FLAGS
            TO TRUE
           MOVE 77
             TO P500-INT-FIELD-77
           SET VALUE-PRESENT IN P500-INT-FIELD-78-FLAGS
            TO TRUE
           MOVE 78
             TO P500-INT-FIELD-78
           SET VALUE-PRESENT IN P500-INT-FIELD-79-FLAGS
            TO TRUE
           MOVE 79
             TO P500-INT-FIELD-79
           SET VALUE-PRESENT IN P500-INT-FIELD-80-FLAGS
            TO TRUE
           MOVE 80
             TO P500-INT-FIELD-80
           SET VALUE-PRESENT IN P500-INT-FIELD-81-FLAGS
            TO TRUE
           MOVE 81
             TO P500-INT-FIELD-81
           SET VALUE-PRESENT IN P500-INT-FIELD-82-FLAGS
            TO TRUE
           MOVE 82
             TO P500-INT-FIELD-82
           SET VALUE-PRESENT IN P500-INT-FIELD-83-FLAGS
            TO TRUE
           MOVE 83
             TO P500-INT-FIELD-83
           SET VALUE-PRESENT IN P500-INT-FIELD-84-FLAGS
            TO TRUE
           MOVE 84
             TO P500-INT-FIELD-84
           SET VALUE-PRESENT IN P500-INT-FIELD-85-FLAGS
            TO TRUE
           MOVE 85
             TO P500-INT-FIELD-85
           SET VALUE-PRESENT IN P500-INT-FIELD-86-FLAGS
            TO TRUE
           MOVE 86
             TO P500-INT-FIELD-86
           SET VALUE-PRESENT IN P500-INT-FIELD-87-FLAGS
            TO TRUE
           MOVE 87
             TO P500-INT-FIELD-87
           SET VALUE-PRESENT IN P500-INT-FIELD-88-FLAGS
            TO TRUE
           MOVE 88
             TO P500-INT-FIELD-88
           SET VALUE-PRESENT IN P500-INT-FIELD-89-FLAGS
            TO TRUE
           MOVE 89
             TO P500-INT-FIELD-89
           SET VALUE-PRESENT IN P500-INT-FIELD-90-FLAGS
            TO TRUE
           MOVE 90
             TO P500-INT-FIELD-90
           SET VALUE-PRESENT IN P500-INT-FIELD-91-FLAGS
            TO TRUE
           MOVE 91
             TO P500-INT-FIELD-91
           SET VALUE-PRESENT IN P500-INT-FIELD-92-FLAGS
            TO TRUE
           MOVE 92
             TO P500-INT-FIELD-92
           SET VALUE-PRESENT IN P500-INT-FIELD-93-FLAGS
            TO TRUE
           MOVE 93
             TO P500-INT-FIELD-93
           SET VALUE-PRESENT IN P500-INT-FIELD-94-FLAGS
            TO TRUE
           MOVE 94
             TO P500-INT-FIELD-94
           SET VALUE-PRESENT IN P500-INT-FIELD-95-FLAGS
            TO TRUE
           MOVE 95
             TO P500-INT-FIELD-95
           SET VALUE-PRESENT IN P500-INT-FIELD-96-FLAGS
            TO TRUE
           MOVE 96
             TO P500-INT-FIELD-96
           SET VALUE-PRESENT IN P500-INT-FIELD-97-FLAGS
            TO TRUE
           MOVE 97
             TO P500-INT-FIELD-97
           SET VALUE-PRESENT IN P500-INT-FIELD-98-FLAGS
            TO TRUE
           MOVE 98
             TO P500-INT-FIELD-98
           SET VALUE-PRESENT IN P500-INT-FIELD-99-FLAGS
            TO TRUE
           MOVE 99
             TO P500-INT-FIELD-99
           SET VALUE-PRESENT IN P500-INT-FIELD-100-FLAGS
            TO TRUE
           MOVE 100
             TO P500-INT-FIELD-100
           SET VALUE-PRESENT IN P500-INT-FIELD-101-FLAGS
            TO TRUE
           MOVE 101
             TO P500-INT-FIELD-101
           SET VALUE-PRESENT IN P500-INT-FIELD-102-FLAGS
            TO TRUE
           MOVE 102
             TO P500-INT-FIELD-102
           SET VALUE-PRESENT IN P500-INT-FIELD-103-FLAGS
            TO TRUE
           MOVE 103
             TO P500-INT-FIELD-103
           SET VALUE-PRESENT IN P500-INT-FIELD-104-FLAGS
            TO TRUE
           MOVE 104
             TO P500-INT-FIELD-104
           SET VALUE-PRESENT IN P500-INT-FIELD-105-FLAGS
            TO TRUE
           MOVE 105
             TO P500-INT-FIELD-105
           SET VALUE-PRESENT IN P500-INT-FIELD-106-FLAGS
            TO TRUE
           MOVE 106
             TO P500-INT-FIELD-106
           SET VALUE-PRESENT IN P500-INT-FIELD-107-FLAGS
            TO TRUE
           MOVE 107
             TO P500-INT-FIELD-107
           SET VALUE-PRESENT IN P500-INT-FIELD-108-FLAGS
            TO TRUE
           MOVE 108
             TO P500-INT-FIELD-108
           SET VALUE-PRESENT IN P500-INT-FIELD-109-FLAGS
            TO TRUE
           MOVE 109
             TO P500-INT-FIELD-109
           SET VALUE-PRESENT IN P500-INT-FIELD-110-FLAGS
            TO TRUE
           MOVE 110
             TO P500-INT-FIELD-110
           SET VALUE-PRESENT IN P500-INT-FIELD-111-FLAGS
            TO TRUE
           MOVE 111
             TO P500-INT-FIELD-111
           SET VALUE-PRESENT IN P500-INT-FIELD-112-FLAGS
            TO TRUE
           MOVE 112
             TO P500-INT-FIELD-112
           SET VALUE-PRESENT IN P500-INT-FIELD-113-FLAGS
            TO TRUE
           MOVE 113
             TO P500-INT-FIELD-113
           SET VALUE-PRESENT IN P500-INT-FIELD-114-FLAGS
            TO TRUE
           MOVE 114
             TO P500-INT-FIELD-114
           SET VALUE-PRESENT IN P500-INT-FIELD-115-FLAGS
            TO TRUE
           MOVE 115
             TO P500-INT-FIELD-115
           SET VALUE-PRESENT IN P500-INT-FIELD-116-FLAGS
            TO TRUE
           MOVE 116
             TO P500-INT-FIELD-116
           SET VALUE-PRESENT IN P500-INT-FIELD-117-FLAGS
            TO TRUE
           MOVE 117
             TO P500-INT-FIELD-117
           SET VALUE-PRESENT IN P500-INT-FIELD-118-FLAGS
            TO TRUE
           MOVE 118
             TO P500-INT-FIELD-118
           SET VALUE-PRESENT IN P500-INT-FIELD-119-FLAGS
            TO TRUE
           MOVE 119
             TO P500-INT-FIELD-119
           SET VALUE-PRESENT IN P500-INT-FIELD-120-FLAGS
            TO TRUE
           MOVE 120
             TO P500-INT-FIELD-120
           SET VALUE-PRESENT IN P500-INT-FIELD-121-FLAGS
            TO TRUE
           MOVE 121
             TO P500-INT-FIELD-121
           SET VALUE-PRESENT IN P500-INT-FIELD-122-FLAGS
            TO TRUE
           MOVE 122
             TO P500-INT-FIELD-122
           SET VALUE-PRESENT IN P500-INT-FIELD-123-FLAGS
            TO TRUE
           MOVE 123
             TO P500-INT-FIELD-123
           SET VALUE-PRESENT IN P500-INT-FIELD-124-FLAGS
            TO TRUE
           MOVE 124
             TO P500-INT-FIELD-124
           SET VALUE-PRESENT IN P500-INT-FIELD-125-FLAGS
            TO TRUE
           MOVE 125
             TO P500-INT-FIELD-125
           SET VALUE-PRESENT IN P500-INT-FIELD-126-FLAGS
            TO TRUE
           MOVE 126
             TO P500-INT-FIELD-126
           SET VALUE-PRESENT IN P500-INT-FIELD-127-FLAGS
            TO TRUE
           MOVE 127
             TO P500-INT-FIELD-127
           SET VALUE-PRESENT IN P500-INT-FIELD-128-FLAGS
            TO TRUE
           MOVE 128
             TO P500-INT-FIELD-128
           SET VALUE-PRESENT IN P500-INT-FIELD-129-FLAGS
            TO TRUE
           MOVE 129
             TO P500-INT-FIELD-129
           SET VALUE-PRESENT IN P500-INT-FIELD-130-FLAGS
            TO TRUE
           MOVE 130
             TO P500-INT-FIELD-130
           SET VALUE-PRESENT IN P500-INT-FIELD-131-FLAGS
            TO TRUE
           MOVE 131
             TO P500-INT-FIELD-131
           SET VALUE-PRESENT IN P500-INT-FIELD-132-FLAGS
            TO TRUE
           MOVE 132
             TO P500-INT-FIELD-132
           SET VALUE-PRESENT IN P500-INT-FIELD-133-FLAGS
            TO TRUE
           MOVE 133
             TO P500-INT-FIELD-133
           SET VALUE-PRESENT IN P500-INT-FIELD-134-FLAGS
            TO TRUE
           MOVE 134
             TO P500-INT-FIELD-134
           SET VALUE-PRESENT IN P500-INT-FIELD-135-FLAGS
            TO TRUE
           MOVE 135
             TO P500-INT-FIELD-135
           SET VALUE-PRESENT IN P500-INT-FIELD-136-FLAGS
            TO TRUE
           MOVE 136
             TO P500-INT-FIELD-136
           SET VALUE-PRESENT IN P500-INT-FIELD-137-FLAGS
            TO TRUE
           MOVE 137
             TO P500-INT-FIELD-137
           SET VALUE-PRESENT IN P500-INT-FIELD-138-FLAGS
            TO TRUE
           MOVE 138
             TO P500-INT-FIELD-138
           SET VALUE-PRESENT IN P500-INT-FIELD-139-FLAGS
            TO TRUE
           MOVE 139
             TO P500-INT-FIELD-139
           SET VALUE-PRESENT IN P500-INT-FIELD-140-FLAGS
            TO TRUE
           MOVE 140
             TO P500-INT-FIELD-140
           SET VALUE-PRESENT IN P500-INT-FIELD-141-FLAGS
            TO TRUE
           MOVE 141
             TO P500-INT-FIELD-141
           SET VALUE-PRESENT IN P500-INT-FIELD-142-FLAGS
            TO TRUE
           MOVE 142
             TO P500-INT-FIELD-142
           SET VALUE-PRESENT IN P500-INT-FIELD-143-FLAGS
            TO TRUE
           MOVE 143
             TO P500-INT-FIELD-143
           SET VALUE-PRESENT IN P500-INT-FIELD-144-FLAGS
            TO TRUE
           MOVE 144
             TO P500-INT-FIELD-144
           SET VALUE-PRESENT IN P500-INT-FIELD-145-FLAGS
            TO TRUE
           MOVE 145
             TO P500-INT-FIELD-145
           SET VALUE-PRESENT IN P500-INT-FIELD-146-FLAGS
            TO TRUE
           MOVE 146
             TO P500-INT-FIELD-146
           SET VALUE-PRESENT IN P500-INT-FIELD-147-FLAGS
            TO TRUE
           MOVE 147
             TO P500-INT-FIELD-147
           SET VALUE-PRESENT IN P500-INT-FIELD-148-FLAGS
            TO TRUE
           MOVE 148
             TO P500-INT-FIELD-148
           SET VALUE-PRESENT IN P500-INT-FIELD-149-FLAGS
            TO TRUE
           MOVE 149
             TO P500-INT-FIELD-149
           SET VALUE-PRESENT IN P500-INT-FIELD-150-FLAGS
            TO TRUE
           MOVE 150
             TO P500-INT-FIELD-150
           SET VALUE-PRESENT IN P500-INT-FIELD-151-FLAGS
            TO TRUE
           MOVE 151
             TO P500-INT-FIELD-151
           SET VALUE-PRESENT IN P500-INT-FIELD-152-FLAGS
            TO TRUE
           MOVE 152
             TO P500-INT-FIELD-152
           SET VALUE-PRESENT IN P500-INT-FIELD-153-FLAGS
            TO TRUE
           MOVE 153
             TO P500-INT-FIELD-153
           SET VALUE-PRESENT IN P500-INT-FIELD-154-FLAGS
            TO TRUE
           MOVE 154
             TO P500-INT-FIELD-154
           SET VALUE-PRESENT IN P500-INT-FIELD-155-FLAGS
            TO TRUE
           MOVE 155
             TO P500-INT-FIELD-155
           SET VALUE-PRESENT IN P500-INT-FIELD-156-FLAGS
            TO TRUE
           MOVE 156
             TO P500-INT-FIELD-156
           SET VALUE-PRESENT IN P500-INT-FIELD-157-FLAGS
            TO TRUE
           MOVE 157
             TO P500-INT-FIELD-157
           SET VALUE-PRESENT IN P500-INT-FIELD-158-FLAGS
            TO TRUE
           MOVE 158
             TO P500-INT-FIELD-158
           SET VALUE-PRESENT IN P500-INT-FIELD-159-FLAGS
            TO TRUE
           MOVE 159
             TO P500-INT-FIELD-159
           SET VALUE-PRESENT IN P500-INT-FIELD-160-FLAGS
            TO TRUE
           MOVE 160
             TO P500-INT-FIELD-160
           SET VALUE-PRESENT IN P500-INT-FIELD-161-FLAGS
            TO TRUE
           MOVE 161
             TO P500-INT-FIELD-161
           SET VALUE-PRESENT IN P500-INT-FIELD-162-FLAGS
            TO TRUE
           MOVE 162
             TO P500-INT-FIELD-162
           SET VALUE-PRESENT IN P500-INT-FIELD-163-FLAGS
            TO TRUE
           MOVE 163
             TO P500-INT-FIELD-163
           SET VALUE-PRESENT IN P500-INT-FIELD-164-FLAGS
            TO TRUE
           MOVE 164
             TO P500-INT-FIELD-164
           SET VALUE-PRESENT IN P500-INT-FIELD-165-FLAGS
            TO TRUE
           MOVE 165
             TO P500-INT-FIELD-165
           SET VALUE-PRESENT IN P500-INT-FIELD-166-FLAGS
            TO TRUE
           MOVE 166
             TO P500-INT-FIELD-166
           SET VALUE-PRESENT IN P500-INT-FIELD-167-FLAGS
            TO TRUE
           MOVE 167
             TO P500-INT-FIELD-167
           SET VALUE-PRESENT IN P500-INT-FIELD-168-FLAGS
            TO TRUE
           MOVE 168
             TO P500-INT-FIELD-168
           SET VALUE-PRESENT IN P500-INT-FIELD-169-FLAGS
            TO TRUE
           MOVE 169
             TO P500-INT-FIELD-169
           SET VALUE-PRESENT IN P500-INT-FIELD-170-FLAGS
            TO TRUE
           MOVE 170
             TO P500-INT-FIELD-170
           SET VALUE-PRESENT IN P500-INT-FIELD-171-FLAGS
            TO TRUE
           MOVE 171
             TO P500-INT-FIELD-171
           SET VALUE-PRESENT IN P500-INT-FIELD-172-FLAGS
            TO TRUE
           MOVE 172
             TO P500-INT-FIELD-172
           SET VALUE-PRESENT IN P500-INT-FIELD-173-FLAGS
            TO TRUE
           MOVE 173
             TO P500-INT-FIELD-173
           SET VALUE-PRESENT IN P500-INT-FIELD-174-FLAGS
            TO TRUE
           MOVE 174
             TO P500-INT-FIELD-174
           SET VALUE-PRESENT IN P500-INT-FIELD-175-FLAGS
            TO TRUE
           MOVE 175
             TO P500-INT-FIELD-175
           SET VALUE-PRESENT IN P500-INT-FIELD-176-FLAGS
            TO TRUE
           MOVE 176
             TO P500-INT-FIELD-176
           SET VALUE-PRESENT IN P500-INT-FIELD-177-FLAGS
            TO TRUE
           MOVE 177
             TO P500-INT-FIELD-177
           SET VALUE-PRESENT IN P500-INT-FIELD-178-FLAGS
            TO TRUE
           MOVE 178
             TO P500-INT-FIELD-178
           SET VALUE-PRESENT IN P500-INT-FIELD-179-FLAGS
            TO TRUE
           MOVE 179
             TO P500-INT-FIELD-179
           SET VALUE-PRESENT IN P500-INT-FIELD-180-FLAGS
            TO TRUE
           MOVE 180
             TO P500-INT-FIELD-180
           SET VALUE-PRESENT IN P500-INT-FIELD-181-FLAGS
            TO TRUE
           MOVE 181
             TO P500-INT-FIELD-181
           SET VALUE-PRESENT IN P500-INT-FIELD-182-FLAGS
            TO TRUE
           MOVE 182
             TO P500-INT-FIELD-182
           SET VALUE-PRESENT IN P500-INT-FIELD-183-FLAGS
            TO TRUE
           MOVE 183
             TO P500-INT-FIELD-183
           SET VALUE-PRESENT IN P500-INT-FIELD-184-FLAGS
            TO TRUE
           MOVE 184
             TO P500-INT-FIELD-184
           SET VALUE-PRESENT IN P500-INT-FIELD-185-FLAGS
            TO TRUE
           MOVE 185
             TO P500-INT-FIELD-185
           SET VALUE-PRESENT IN P500-INT-FIELD-186-FLAGS
            TO TRUE
           MOVE 186
             TO P500-INT-FIELD-186
           SET VALUE-PRESENT IN P500-INT-FIELD-187-FLAGS
            TO TRUE
           MOVE 187
             TO P500-INT-FIELD-187
           SET VALUE-PRESENT IN P500-INT-FIELD-188-FLAGS
            TO TRUE
           MOVE 188
             TO P500-INT-FIELD-188
           SET VALUE-PRESENT IN P500-INT-FIELD-189-FLAGS
            TO TRUE
           MOVE 189
             TO P500-INT-FIELD-189
           SET VALUE-PRESENT IN P500-INT-FIELD-190-FLAGS
            TO TRUE
           MOVE 190
             TO P500-INT-FIELD-190
           SET VALUE-PRESENT IN P500-INT-FIELD-191-FLAGS
            TO TRUE
           MOVE 191
             TO P500-INT-FIELD-191
           SET VALUE-PRESENT IN P500-INT-FIELD-192-FLAGS
            TO TRUE
           MOVE 192
             TO P500-INT-FIELD-192
           SET VALUE-PRESENT IN P500-INT-FIELD-193-FLAGS
            TO TRUE
           MOVE 193
             TO P500-INT-FIELD-193
           SET VALUE-PRESENT IN P500-INT-FIELD-194-FLAGS
            TO TRUE
           MOVE 194
             TO P500-INT-FIELD-194
           SET VALUE-PRESENT IN P500-INT-FIELD-195-FLAGS
            TO TRUE
           MOVE 195
             TO P500-INT-FIELD-195
           SET VALUE-PRESENT IN P500-INT-FIELD-196-FLAGS
            TO TRUE
           MOVE 196
             TO P500-INT-FIELD-196
           SET VALUE-PRESENT IN P500-INT-FIELD-197-FLAGS
            TO TRUE
           MOVE 197
             TO P500-INT-FIELD-197
           SET VALUE-PRESENT IN P500-INT-FIELD-198-FLAGS
            TO TRUE
           MOVE 198
             TO P500-INT-FIELD-198
           SET VALUE-PRESENT IN P500-INT-FIELD-199-FLAGS
            TO TRUE
           MOVE 199
             TO P500-INT-FIELD-199
           SET VALUE-PRESENT IN P500-INT-FIELD-200-FLAGS
            TO TRUE
           MOVE 200
             TO P500-INT-FIELD-200
           SET VALUE-PRESENT IN P500-INT-FIELD-201-FLAGS
            TO TRUE
           MOVE 201
             TO P500-INT-FIELD-201
           SET VALUE-PRESENT IN P500-INT-FIELD-202-FLAGS
            TO TRUE
           MOVE 202
             TO P500-INT-FIELD-202
           SET VALUE-PRESENT IN P500-INT-FIELD-203-FLAGS
            TO TRUE
           MOVE 203
             TO P500-INT-FIELD-203
           SET VALUE-PRESENT IN P500-INT-FIELD-204-FLAGS
            TO TRUE
           MOVE 204
             TO P500-INT-FIELD-204
           SET VALUE-PRESENT IN P500-INT-FIELD-205-FLAGS
            TO TRUE
           MOVE 205
             TO P500-INT-FIELD-205
           SET VALUE-PRESENT IN P500-INT-FIELD-206-FLAGS
            TO TRUE
           MOVE 206
             TO P500-INT-FIELD-206
           SET VALUE-PRESENT IN P500-INT-FIELD-207-FLAGS
            TO TRUE
           MOVE 207
             TO P500-INT-FIELD-207
           SET VALUE-PRESENT IN P500-INT-FIELD-208-FLAGS
            TO TRUE
           MOVE 208
             TO P500-INT-FIELD-208
           SET VALUE-PRESENT IN P500-INT-FIELD-209-FLAGS
            TO TRUE
           MOVE 209
             TO P500-INT-FIELD-209
           SET VALUE-PRESENT IN P500-INT-FIELD-210-FLAGS
            TO TRUE
           MOVE 210
             TO P500-INT-FIELD-210
           SET VALUE-PRESENT IN P500-INT-FIELD-211-FLAGS
            TO TRUE
           MOVE 211
             TO P500-INT-FIELD-211
           SET VALUE-PRESENT IN P500-INT-FIELD-212-FLAGS
            TO TRUE
           MOVE 212
             TO P500-INT-FIELD-212
           SET VALUE-PRESENT IN P500-INT-FIELD-213-FLAGS
            TO TRUE
           MOVE 213
             TO P500-INT-FIELD-213
           SET VALUE-PRESENT IN P500-INT-FIELD-214-FLAGS
            TO TRUE
           MOVE 214
             TO P500-INT-FIELD-214
           SET VALUE-PRESENT IN P500-INT-FIELD-215-FLAGS
            TO TRUE
           MOVE 215
             TO P500-INT-FIELD-215
           SET VALUE-PRESENT IN P500-INT-FIELD-216-FLAGS
            TO TRUE
           MOVE 216
             TO P500-INT-FIELD-216
           SET VALUE-PRESENT IN P500-INT-FIELD-217-FLAGS
            TO TRUE
           MOVE 217
             TO P500-INT-FIELD-217
           SET VALUE-PRESENT IN P500-INT-FIELD-218-FLAGS
            TO TRUE
           MOVE 218
             TO P500-INT-FIELD-218
           SET VALUE-PRESENT IN P500-INT-FIELD-219-FLAGS
            TO TRUE
           MOVE 219
             TO P500-INT-FIELD-219
           SET VALUE-PRESENT IN P500-INT-FIELD-220-FLAGS
            TO TRUE
           MOVE 220
             TO P500-INT-FIELD-220
           SET VALUE-PRESENT IN P500-INT-FIELD-221-FLAGS
            TO TRUE
           MOVE 221
             TO P500-INT-FIELD-221
           SET VALUE-PRESENT IN P500-INT-FIELD-222-FLAGS
            TO TRUE
           MOVE 222
             TO P500-INT-FIELD-222
           SET VALUE-PRESENT IN P500-INT-FIELD-223-FLAGS
            TO TRUE
           MOVE 223
             TO P500-INT-FIELD-223
           SET VALUE-PRESENT IN P500-INT-FIELD-224-FLAGS
            TO TRUE
           MOVE 224
             TO P500-INT-FIELD-224
           SET VALUE-PRESENT IN P500-INT-FIELD-225-FLAGS
            TO TRUE
           MOVE 225
             TO P500-INT-FIELD-225
           SET VALUE-PRESENT IN P500-INT-FIELD-226-FLAGS
            TO TRUE
           MOVE 226
             TO P500-INT-FIELD-226
           SET VALUE-PRESENT IN P500-INT-FIELD-227-FLAGS
            TO TRUE
           MOVE 227
             TO P500-INT-FIELD-227
           SET VALUE-PRESENT IN P500-INT-FIELD-228-FLAGS
            TO TRUE
           MOVE 228
             TO P500-INT-FIELD-228
           SET VALUE-PRESENT IN P500-INT-FIELD-229-FLAGS
            TO TRUE
           MOVE 229
             TO P500-INT-FIELD-229
           SET VALUE-PRESENT IN P500-INT-FIELD-230-FLAGS
            TO TRUE
           MOVE 230
             TO P500-INT-FIELD-230
           SET VALUE-PRESENT IN P500-INT-FIELD-231-FLAGS
            TO TRUE
           MOVE 231
             TO P500-INT-FIELD-231
           SET VALUE-PRESENT IN P500-INT-FIELD-232-FLAGS
            TO TRUE
           MOVE 232
             TO P500-INT-FIELD-232
           SET VALUE-PRESENT IN P500-INT-FIELD-233-FLAGS
            TO TRUE
           MOVE 233
             TO P500-INT-FIELD-233
           SET VALUE-PRESENT IN P500-INT-FIELD-234-FLAGS
            TO TRUE
           MOVE 234
             TO P500-INT-FIELD-234
           SET VALUE-PRESENT IN P500-INT-FIELD-235-FLAGS
            TO TRUE
           MOVE 235
             TO P500-INT-FIELD-235
           SET VALUE-PRESENT IN P500-INT-FIELD-236-FLAGS
            TO TRUE
           MOVE 236
             TO P500-INT-FIELD-236
           SET VALUE-PRESENT IN P500-INT-FIELD-237-FLAGS
            TO TRUE
           MOVE 237
             TO P500-INT-FIELD-237
           SET VALUE-PRESENT IN P500-INT-FIELD-238-FLAGS
            TO TRUE
           MOVE 238
             TO P500-INT-FIELD-238
           SET VALUE-PRESENT IN P500-INT-FIELD-239-FLAGS
            TO TRUE
           MOVE 239
             TO P500-INT-FIELD-239
           SET VALUE-PRESENT IN P500-INT-FIELD-240-FLAGS
            TO TRUE
           MOVE 240
             TO P500-INT-FIELD-240
           SET VALUE-PRESENT IN P500-INT-FIELD-241-FLAGS
            TO TRUE
           MOVE 241
             TO P500-INT-FIELD-241
           SET VALUE-PRESENT IN P500-INT-FIELD-242-FLAGS
            TO TRUE
           MOVE 242
             TO P500-INT-FIELD-242
           SET VALUE-PRESENT IN P500-INT-FIELD-243-FLAGS
            TO TRUE
           MOVE 243
             TO P500-INT-FIELD-243
           SET VALUE-PRESENT IN P500-INT-FIELD-244-FLAGS
            TO TRUE
           MOVE 244
             TO P500-INT-FIELD-244
           SET VALUE-PRESENT IN P500-INT-FIELD-245-FLAGS
            TO TRUE
           MOVE 245
             TO P500-INT-FIELD-245
           SET VALUE-PRESENT IN P500-INT-FIELD-246-FLAGS
            TO TRUE
           MOVE 246
             TO P500-INT-FIELD-246
           SET VALUE-PRESENT IN P500-INT-FIELD-247-FLAGS
            TO TRUE
           MOVE 247
             TO P500-INT-FIELD-247
           SET VALUE-PRESENT IN P500-INT-FIELD-248-FLAGS
            TO TRUE
           MOVE 248
             TO P500-INT-FIELD-248
           SET VALUE-PRESENT IN P500-INT-FIELD-249-FLAGS
            TO TRUE
           MOVE 249
             TO P500-INT-FIELD-249
           SET VALUE-PRESENT IN P500-INT-FIELD-250-FLAGS
            TO TRUE
           MOVE 250
             TO P500-INT-FIELD-250
           SET VALUE-PRESENT IN P500-INT-FIELD-251-FLAGS
            TO TRUE
           MOVE 251
             TO P500-INT-FIELD-251
           SET VALUE-PRESENT IN P500-INT-FIELD-252-FLAGS
            TO TRUE
           MOVE 252
             TO P500-INT-FIELD-252
           SET VALUE-PRESENT IN P500-INT-FIELD-253-FLAGS
            TO TRUE
           MOVE 253
             TO P500-INT-FIELD-253
           SET VALUE-PRESENT IN P500-INT-FIELD-254-FLAGS
            TO TRUE
           MOVE 254
             TO P500-INT-FIELD-254
           SET VALUE-PRESENT IN P500-INT-FIELD-255-FLAGS
            TO TRUE
           MOVE 255
             TO P500-INT-FIELD-255
           SET VALUE-PRESENT IN P500-INT-FIELD-256-FLAGS
            TO TRUE
           MOVE 256
             TO P500-INT-FIELD-256
           SET VALUE-PRESENT IN P500-INT-FIELD-257-FLAGS
            TO TRUE
           MOVE 257
             TO P500-INT-FIELD-257
           SET VALUE-PRESENT IN P500-INT-FIELD-258-FLAGS
            TO TRUE
           MOVE 258
             TO P500-INT-FIELD-258
           SET VALUE-PRESENT IN P500-INT-FIELD-259-FLAGS
            TO TRUE
           MOVE 259
             TO P500-INT-FIELD-259
           SET VALUE-PRESENT IN P500-INT-FIELD-260-FLAGS
            TO TRUE
           MOVE 260
             TO P500-INT-FIELD-260
           SET VALUE-PRESENT IN P500-INT-FIELD-261-FLAGS
            TO TRUE
           MOVE 261
             TO P500-INT-FIELD-261
           SET VALUE-PRESENT IN P500-INT-FIELD-262-FLAGS
            TO TRUE
           MOVE 262
             TO P500-INT-FIELD-262
           SET VALUE-PRESENT IN P500-INT-FIELD-263-FLAGS
            TO TRUE
           MOVE 263
             TO P500-INT-FIELD-263
           SET VALUE-PRESENT IN P500-INT-FIELD-264-FLAGS
            TO TRUE
           MOVE 264
             TO P500-INT-FIELD-264
           SET VALUE-PRESENT IN P500-INT-FIELD-265-FLAGS
            TO TRUE
           MOVE 265
             TO P500-INT-FIELD-265
           SET VALUE-PRESENT IN P500-INT-FIELD-266-FLAGS
            TO TRUE
           MOVE 266
             TO P500-INT-FIELD-266
           SET VALUE-PRESENT IN P500-INT-FIELD-267-FLAGS
            TO TRUE
           MOVE 267
             TO P500-INT-FIELD-267
           SET VALUE-PRESENT IN P500-INT-FIELD-268-FLAGS
            TO TRUE
           MOVE 268
             TO P500-INT-FIELD-268
           SET VALUE-PRESENT IN P500-INT-FIELD-269-FLAGS
            TO TRUE
           MOVE 269
             TO P500-INT-FIELD-269
           SET VALUE-PRESENT IN P500-INT-FIELD-270-FLAGS
            TO TRUE
           MOVE 270
             TO P500-INT-FIELD-270
           SET VALUE-PRESENT IN P500-INT-FIELD-271-FLAGS
            TO TRUE
           MOVE 271
             TO P500-INT-FIELD-271
           SET VALUE-PRESENT IN P500-INT-FIELD-272-FLAGS
            TO TRUE
           MOVE 272
             TO P500-INT-FIELD-272
           SET VALUE-PRESENT IN P500-INT-FIELD-273-FLAGS
            TO TRUE
           MOVE 273
             TO P500-INT-FIELD-273
           SET VALUE-PRESENT IN P500-INT-FIELD-274-FLAGS
            TO TRUE
           MOVE 274
             TO P500-INT-FIELD-274
           SET VALUE-PRESENT IN P500-INT-FIELD-275-FLAGS
            TO TRUE
           MOVE 275
             TO P500-INT-FIELD-275
           SET VALUE-PRESENT IN P500-INT-FIELD-276-FLAGS
            TO TRUE
           MOVE 276
             TO P500-INT-FIELD-276
           SET VALUE-PRESENT IN P500-INT-FIELD-277-FLAGS
            TO TRUE
           MOVE 277
             TO P500-INT-FIELD-277
           SET VALUE-PRESENT IN P500-INT-FIELD-278-FLAGS
            TO TRUE
           MOVE 278
             TO P500-INT-FIELD-278
           SET VALUE-PRESENT IN P500-INT-FIELD-279-FLAGS
            TO TRUE
           MOVE 279
             TO P500-INT-FIELD-279
           SET VALUE-PRESENT IN P500-INT-FIELD-280-FLAGS
            TO TRUE
           MOVE 280
             TO P500-INT-FIELD-280
           SET VALUE-PRESENT IN P500-INT-FIELD-281-FLAGS
            TO TRUE
           MOVE 281
             TO P500-INT-FIELD-281
           SET VALUE-PRESENT IN P500-INT-FIELD-282-FLAGS
            TO TRUE
           MOVE 282
             TO P500-INT-FIELD-282
           SET VALUE-PRESENT IN P500-INT-FIELD-283-FLAGS
            TO TRUE
           MOVE 283
             TO P500-INT-FIELD-283
           SET VALUE-PRESENT IN P500-INT-FIELD-284-FLAGS
            TO TRUE
           MOVE 284
             TO P500-INT-FIELD-284
           SET VALUE-PRESENT IN P500-INT-FIELD-285-FLAGS
            TO TRUE
           MOVE 285
             TO P500-INT-FIELD-285
           SET VALUE-PRESENT IN P500-INT-FIELD-286-FLAGS
            TO TRUE
           MOVE 286
             TO P500-INT-FIELD-286
           SET VALUE-PRESENT IN P500-INT-FIELD-287-FLAGS
            TO TRUE
           MOVE 287
             TO P500-INT-FIELD-287
           SET VALUE-PRESENT IN P500-INT-FIELD-288-FLAGS
            TO TRUE
           MOVE 288
             TO P500-INT-FIELD-288
           SET VALUE-PRESENT IN P500-INT-FIELD-289-FLAGS
            TO TRUE
           MOVE 289
             TO P500-INT-FIELD-289
           SET VALUE-PRESENT IN P500-INT-FIELD-290-FLAGS
            TO TRUE
           MOVE 290
             TO P500-INT-FIELD-290
           SET VALUE-PRESENT IN P500-INT-FIELD-291-FLAGS
            TO TRUE
           MOVE 291
             TO P500-INT-FIELD-291
           SET VALUE-PRESENT IN P500-INT-FIELD-292-FLAGS
            TO TRUE
           MOVE 292
             TO P500-INT-FIELD-292
           SET VALUE-PRESENT IN P500-INT-FIELD-293-FLAGS
            TO TRUE
           MOVE 293
             TO P500-INT-FIELD-293
           SET VALUE-PRESENT IN P500-INT-FIELD-294-FLAGS
            TO TRUE
           MOVE 294
             TO P500-INT-FIELD-294
           SET VALUE-PRESENT IN P500-INT-FIELD-295-FLAGS
            TO TRUE
           MOVE 295
             TO P500-INT-FIELD-295
           SET VALUE-PRESENT IN P500-INT-FIELD-296-FLAGS
            TO TRUE
           MOVE 296
             TO P500-INT-FIELD-296
           SET VALUE-PRESENT IN P500-INT-FIELD-297-FLAGS
            TO TRUE
           MOVE 297
             TO P500-INT-FIELD-297
           SET VALUE-PRESENT IN P500-INT-FIELD-298-FLAGS
            TO TRUE
           MOVE 298
             TO P500-INT-FIELD-298
           SET VALUE-PRESENT IN P500-INT-FIELD-299-FLAGS
            TO TRUE
           MOVE 299
             TO P500-INT-FIELD-299
           SET VALUE-PRESENT IN P500-INT-FIELD-300-FLAGS
            TO TRUE
           MOVE 300
             TO P500-INT-FIELD-300
           SET VALUE-PRESENT IN P500-INT-FIELD-301-FLAGS
            TO TRUE
           MOVE 301
             TO P500-INT-FIELD-301
           SET VALUE-PRESENT IN P500-INT-FIELD-302-FLAGS
            TO TRUE
           MOVE 302
             TO P500-INT-FIELD-302
           SET VALUE-PRESENT IN P500-INT-FIELD-303-FLAGS
            TO TRUE
           MOVE 303
             TO P500-INT-FIELD-303
           SET VALUE-PRESENT IN P500-INT-FIELD-304-FLAGS
            TO TRUE
           MOVE 304
             TO P500-INT-FIELD-304
           SET VALUE-PRESENT IN P500-INT-FIELD-305-FLAGS
            TO TRUE
           MOVE 305
             TO P500-INT-FIELD-305
           SET VALUE-PRESENT IN P500-INT-FIELD-306-FLAGS
            TO TRUE
           MOVE 306
             TO P500-INT-FIELD-306
           SET VALUE-PRESENT IN P500-INT-FIELD-307-FLAGS
            TO TRUE
           MOVE 307
             TO P500-INT-FIELD-307
           SET VALUE-PRESENT IN P500-INT-FIELD-308-FLAGS
            TO TRUE
           MOVE 308
             TO P500-INT-FIELD-308
           SET VALUE-PRESENT IN P500-INT-FIELD-309-FLAGS
            TO TRUE
           MOVE 309
             TO P500-INT-FIELD-309
           SET VALUE-PRESENT IN P500-INT-FIELD-310-FLAGS
            TO TRUE
           MOVE 310
             TO P500-INT-FIELD-310
           SET VALUE-PRESENT IN P500-INT-FIELD-311-FLAGS
            TO TRUE
           MOVE 311
             TO P500-INT-FIELD-311
           SET VALUE-PRESENT IN P500-INT-FIELD-312-FLAGS
            TO TRUE
           MOVE 312
             TO P500-INT-FIELD-312
           SET VALUE-PRESENT IN P500-INT-FIELD-313-FLAGS
            TO TRUE
           MOVE 313
             TO P500-INT-FIELD-313
           SET VALUE-PRESENT IN P500-INT-FIELD-314-FLAGS
            TO TRUE
           MOVE 314
             TO P500-INT-FIELD-314
           SET VALUE-PRESENT IN P500-INT-FIELD-315-FLAGS
            TO TRUE
           MOVE 315
             TO P500-INT-FIELD-315
           SET VALUE-PRESENT IN P500-INT-FIELD-316-FLAGS
            TO TRUE
           MOVE 316
             TO P500-INT-FIELD-316
           SET VALUE-PRESENT IN P500-INT-FIELD-317-FLAGS
            TO TRUE
           MOVE 317
             TO P500-INT-FIELD-317
           SET VALUE-PRESENT IN P500-INT-FIELD-318-FLAGS
            TO TRUE
           MOVE 318
             TO P500-INT-FIELD-318
           SET VALUE-PRESENT IN P500-INT-FIELD-319-FLAGS
            TO TRUE
           MOVE 319
             TO P500-INT-FIELD-319
           SET VALUE-PRESENT IN P500-INT-FIELD-320-FLAGS
            TO TRUE
           MOVE 320
             TO P500-INT-FIELD-320
           SET VALUE-PRESENT IN P500-INT-FIELD-321-FLAGS
            TO TRUE
           MOVE 321
             TO P500-INT-FIELD-321
           SET VALUE-PRESENT IN P500-INT-FIELD-322-FLAGS
            TO TRUE
           MOVE 322
             TO P500-INT-FIELD-322
           SET VALUE-PRESENT IN P500-INT-FIELD-323-FLAGS
            TO TRUE
           MOVE 323
             TO P500-INT-FIELD-323
           SET VALUE-PRESENT IN P500-INT-FIELD-324-FLAGS
            TO TRUE
           MOVE 324
             TO P500-INT-FIELD-324
           SET VALUE-PRESENT IN P500-INT-FIELD-325-FLAGS
            TO TRUE
           MOVE 325
             TO P500-INT-FIELD-325
           SET VALUE-PRESENT IN P500-INT-FIELD-326-FLAGS
            TO TRUE
           MOVE 326
             TO P500-INT-FIELD-326
           SET VALUE-PRESENT IN P500-INT-FIELD-327-FLAGS
            TO TRUE
           MOVE 327
             TO P500-INT-FIELD-327
           SET VALUE-PRESENT IN P500-INT-FIELD-328-FLAGS
            TO TRUE
           MOVE 328
             TO P500-INT-FIELD-328
           SET VALUE-PRESENT IN P500-INT-FIELD-329-FLAGS
            TO TRUE
           MOVE 329
             TO P500-INT-FIELD-329
           SET VALUE-PRESENT IN P500-INT-FIELD-330-FLAGS
            TO TRUE
           MOVE 330
             TO P500-INT-FIELD-330
           SET VALUE-PRESENT IN P500-INT-FIELD-331-FLAGS
            TO TRUE
           MOVE 331
             TO P500-INT-FIELD-331
           SET VALUE-PRESENT IN P500-INT-FIELD-332-FLAGS
            TO TRUE
           MOVE 332
             TO P500-INT-FIELD-332
           SET VALUE-PRESENT IN P500-INT-FIELD-333-FLAGS
            TO TRUE
           MOVE 333
             TO P500-INT-FIELD-333
           SET VALUE-PRESENT IN P500-INT-FIELD-334-FLAGS
            TO TRUE
           MOVE 334
             TO P500-INT-FIELD-334
           SET VALUE-PRESENT IN P500-INT-FIELD-335-FLAGS
            TO TRUE
           MOVE 335
             TO P500-INT-FIELD-335
           SET VALUE-PRESENT IN P500-INT-FIELD-336-FLAGS
            TO TRUE
           MOVE 336
             TO P500-INT-FIELD-336
           SET VALUE-PRESENT IN P500-INT-FIELD-337-FLAGS
            TO TRUE
           MOVE 337
             TO P500-INT-FIELD-337
           SET VALUE-PRESENT IN P500-INT-FIELD-338-FLAGS
            TO TRUE
           MOVE 338
             TO P500-INT-FIELD-338
           SET VALUE-PRESENT IN P500-INT-FIELD-339-FLAGS
            TO TRUE
           MOVE 339
             TO P500-INT-FIELD-339
           SET VALUE-PRESENT IN P500-INT-FIELD-340-FLAGS
            TO TRUE
           MOVE 340
             TO P500-INT-FIELD-340
           SET VALUE-PRESENT IN P500-INT-FIELD-341-FLAGS
            TO TRUE
           MOVE 341
             TO P500-INT-FIELD-341
           SET VALUE-PRESENT IN P500-INT-FIELD-342-FLAGS
            TO TRUE
           MOVE 342
             TO P500-INT-FIELD-342
           SET VALUE-PRESENT IN P500-INT-FIELD-343-FLAGS
            TO TRUE
           MOVE 343
             TO P500-INT-FIELD-343
           SET VALUE-PRESENT IN P500-INT-FIELD-344-FLAGS
            TO TRUE
           MOVE 344
             TO P500-INT-FIELD-344
           SET VALUE-PRESENT IN P500-INT-FIELD-345-FLAGS
            TO TRUE
           MOVE 345
             TO P500-INT-FIELD-345
           SET VALUE-PRESENT IN P500-INT-FIELD-346-FLAGS
            TO TRUE
           MOVE 346
             TO P500-INT-FIELD-346
           SET VALUE-PRESENT IN P500-INT-FIELD-347-FLAGS
            TO TRUE
           MOVE 347
             TO P500-INT-FIELD-347
           SET VALUE-PRESENT IN P500-INT-FIELD-348-FLAGS
            TO TRUE
           MOVE 348
             TO P500-INT-FIELD-348
           SET VALUE-PRESENT IN P500-INT-FIELD-349-FLAGS
            TO TRUE
           MOVE 349
             TO P500-INT-FIELD-349
           SET VALUE-PRESENT IN P500-INT-FIELD-350-FLAGS
            TO TRUE
           MOVE 350
             TO P500-INT-FIELD-350
           SET VALUE-PRESENT IN P500-INT-FIELD-351-FLAGS
            TO TRUE
           MOVE 351
             TO P500-INT-FIELD-351
           SET VALUE-PRESENT IN P500-INT-FIELD-352-FLAGS
            TO TRUE
           MOVE 352
             TO P500-INT-FIELD-352
           SET VALUE-PRESENT IN P500-INT-FIELD-353-FLAGS
            TO TRUE
           MOVE 353
             TO P500-INT-FIELD-353
           SET VALUE-PRESENT IN P500-INT-FIELD-354-FLAGS
            TO TRUE
           MOVE 354
             TO P500-INT-FIELD-354
           SET VALUE-PRESENT IN P500-INT-FIELD-355-FLAGS
            TO TRUE
           MOVE 355
             TO P500-INT-FIELD-355
           SET VALUE-PRESENT IN P500-INT-FIELD-356-FLAGS
            TO TRUE
           MOVE 356
             TO P500-INT-FIELD-356
           SET VALUE-PRESENT IN P500-INT-FIELD-357-FLAGS
            TO TRUE
           MOVE 357
             TO P500-INT-FIELD-357
           SET VALUE-PRESENT IN P500-INT-FIELD-358-FLAGS
            TO TRUE
           MOVE 358
             TO P500-INT-FIELD-358
           SET VALUE-PRESENT IN P500-INT-FIELD-359-FLAGS
            TO TRUE
           MOVE 359
             TO P500-INT-FIELD-359
           SET VALUE-PRESENT IN P500-INT-FIELD-360-FLAGS
            TO TRUE
           MOVE 360
             TO P500-INT-FIELD-360
           SET VALUE-PRESENT IN P500-INT-FIELD-361-FLAGS
            TO TRUE
           MOVE 361
             TO P500-INT-FIELD-361
           SET VALUE-PRESENT IN P500-INT-FIELD-362-FLAGS
            TO TRUE
           MOVE 362
             TO P500-INT-FIELD-362
           SET VALUE-PRESENT IN P500-INT-FIELD-363-FLAGS
            TO TRUE
           MOVE 363
             TO P500-INT-FIELD-363
           SET VALUE-PRESENT IN P500-INT-FIELD-364-FLAGS
            TO TRUE
           MOVE 364
             TO P500-INT-FIELD-364
           SET VALUE-PRESENT IN P500-INT-FIELD-365-FLAGS
            TO TRUE
           MOVE 365
             TO P500-INT-FIELD-365
           SET VALUE-PRESENT IN P500-INT-FIELD-366-FLAGS
            TO TRUE
           MOVE 366
             TO P500-INT-FIELD-366
           SET VALUE-PRESENT IN P500-INT-FIELD-367-FLAGS
            TO TRUE
           MOVE 367
             TO P500-INT-FIELD-367
           SET VALUE-PRESENT IN P500-INT-FIELD-368-FLAGS
            TO TRUE
           MOVE 368
             TO P500-INT-FIELD-368
           SET VALUE-PRESENT IN P500-INT-FIELD-369-FLAGS
            TO TRUE
           MOVE 369
             TO P500-INT-FIELD-369
           SET VALUE-PRESENT IN P500-INT-FIELD-370-FLAGS
            TO TRUE
           MOVE 370
             TO P500-INT-FIELD-370
           SET VALUE-PRESENT IN P500-INT-FIELD-371-FLAGS
            TO TRUE
           MOVE 371
             TO P500-INT-FIELD-371
           SET VALUE-PRESENT IN P500-INT-FIELD-372-FLAGS
            TO TRUE
           MOVE 372
             TO P500-INT-FIELD-372
           SET VALUE-PRESENT IN P500-INT-FIELD-373-FLAGS
            TO TRUE
           MOVE 373
             TO P500-INT-FIELD-373
           SET VALUE-PRESENT IN P500-INT-FIELD-374-FLAGS
            TO TRUE
           MOVE 374
             TO P500-INT-FIELD-374
           SET VALUE-PRESENT IN P500-INT-FIELD-375-FLAGS
            TO TRUE
           MOVE 375
             TO P500-INT-FIELD-375
           SET VALUE-PRESENT IN P500-INT-FIELD-376-FLAGS
            TO TRUE
           MOVE 376
             TO P500-INT-FIELD-376
           SET VALUE-PRESENT IN P500-INT-FIELD-377-FLAGS
            TO TRUE
           MOVE 377
             TO P500-INT-FIELD-377
           SET VALUE-PRESENT IN P500-INT-FIELD-378-FLAGS
            TO TRUE
           MOVE 378
             TO P500-INT-FIELD-378
           SET VALUE-PRESENT IN P500-INT-FIELD-379-FLAGS
            TO TRUE
           MOVE 379
             TO P500-INT-FIELD-379
           SET VALUE-PRESENT IN P500-INT-FIELD-380-FLAGS
            TO TRUE
           MOVE 380
             TO P500-INT-FIELD-380
           SET VALUE-PRESENT IN P500-INT-FIELD-381-FLAGS
            TO TRUE
           MOVE 381
             TO P500-INT-FIELD-381
           SET VALUE-PRESENT IN P500-INT-FIELD-382-FLAGS
            TO TRUE
           MOVE 382
             TO P500-INT-FIELD-382
           SET VALUE-PRESENT IN P500-INT-FIELD-383-FLAGS
            TO TRUE
           MOVE 383
             TO P500-INT-FIELD-383
           SET VALUE-PRESENT IN P500-INT-FIELD-384-FLAGS
            TO TRUE
           MOVE 384
             TO P500-INT-FIELD-384
           SET VALUE-PRESENT IN P500-INT-FIELD-385-FLAGS
            TO TRUE
           MOVE 385
             TO P500-INT-FIELD-385
           SET VALUE-PRESENT IN P500-INT-FIELD-386-FLAGS
            TO TRUE
           MOVE 386
             TO P500-INT-FIELD-386
           SET VALUE-PRESENT IN P500-INT-FIELD-387-FLAGS
            TO TRUE
           MOVE 387
             TO P500-INT-FIELD-387
           SET VALUE-PRESENT IN P500-INT-FIELD-388-FLAGS
            TO TRUE
           MOVE 388
             TO P500-INT-FIELD-388
           SET VALUE-PRESENT IN P500-INT-FIELD-389-FLAGS
            TO TRUE
           MOVE 389
             TO P500-INT-FIELD-389
           SET VALUE-PRESENT IN P500-INT-FIELD-390-FLAGS
            TO TRUE
           MOVE 390
             TO P500-INT-FIELD-390
           SET VALUE-PRESENT IN P500-INT-FIELD-391-FLAGS
            TO TRUE
           MOVE 391
             TO P500-INT-FIELD-391
           SET VALUE-PRESENT IN P500-INT-FIELD-392-FLAGS
            TO TRUE
           MOVE 392
             TO P500-INT-FIELD-392
           SET VALUE-PRESENT IN P500-INT-FIELD-393-FLAGS
            TO TRUE
           MOVE 393
             TO P500-INT-FIELD-393
           SET VALUE-PRESENT IN P500-INT-FIELD-394-FLAGS
            TO TRUE
           MOVE 394
             TO P500-INT-FIELD-394
           SET VALUE-PRESENT IN P500-INT-FIELD-395-FLAGS
            TO TRUE
           MOVE 395
             TO P500-INT-FIELD-395
           SET VALUE-PRESENT IN P500-INT-FIELD-396-FLAGS
            TO TRUE
           MOVE 396
             TO P500-INT-FIELD-396
           SET VALUE-PRESENT IN P500-INT-FIELD-397-FLAGS
            TO TRUE
           MOVE 397
             TO P500-INT-FIELD-397
           SET VALUE-PRESENT IN P500-INT-FIELD-398-FLAGS
            TO TRUE
           MOVE 398
             TO P500-INT-FIELD-398
           SET VALUE-PRESENT IN P500-INT-FIELD-399-FLAGS
            TO TRUE
           MOVE 399
             TO P500-INT-FIELD-399
           SET VALUE-PRESENT IN P500-INT-FIELD-400-FLAGS
            TO TRUE
           MOVE 400
             TO P500-INT-FIELD-400
           SET VALUE-PRESENT IN P500-INT-FIELD-401-FLAGS
            TO TRUE
           MOVE 401
             TO P500-INT-FIELD-401
           SET VALUE-PRESENT IN P500-INT-FIELD-402-FLAGS
            TO TRUE
           MOVE 402
             TO P500-INT-FIELD-402
           SET VALUE-PRESENT IN P500-INT-FIELD-403-FLAGS
            TO TRUE
           MOVE 403
             TO P500-INT-FIELD-403
           SET VALUE-PRESENT IN P500-INT-FIELD-404-FLAGS
            TO TRUE
           MOVE 404
             TO P500-INT-FIELD-404
           SET VALUE-PRESENT IN P500-INT-FIELD-405-FLAGS
            TO TRUE
           MOVE 405
             TO P500-INT-FIELD-405
           SET VALUE-PRESENT IN P500-INT-FIELD-406-FLAGS
            TO TRUE
           MOVE 406
             TO P500-INT-FIELD-406
           SET VALUE-PRESENT IN P500-INT-FIELD-407-FLAGS
            TO TRUE
           MOVE 407
             TO P500-INT-FIELD-407
           SET VALUE-PRESENT IN P500-INT-FIELD-408-FLAGS
            TO TRUE
           MOVE 408
             TO P500-INT-FIELD-408
           SET VALUE-PRESENT IN P500-INT-FIELD-409-FLAGS
            TO TRUE
           MOVE 409
             TO P500-INT-FIELD-409
           SET VALUE-PRESENT IN P500-INT-FIELD-410-FLAGS
            TO TRUE
           MOVE 410
             TO P500-INT-FIELD-410
           SET VALUE-PRESENT IN P500-INT-FIELD-411-FLAGS
            TO TRUE
           MOVE 411
             TO P500-INT-FIELD-411
           SET VALUE-PRESENT IN P500-INT-FIELD-412-FLAGS
            TO TRUE
           MOVE 412
             TO P500-INT-FIELD-412
           SET VALUE-PRESENT IN P500-INT-FIELD-413-FLAGS
            TO TRUE
           MOVE 413
             TO P500-INT-FIELD-413
           SET VALUE-PRESENT IN P500-INT-FIELD-414-FLAGS
            TO TRUE
           MOVE 414
             TO P500-INT-FIELD-414
           SET VALUE-PRESENT IN P500-INT-FIELD-415-FLAGS
            TO TRUE
           MOVE 415
             TO P500-INT-FIELD-415
           SET VALUE-PRESENT IN P500-INT-FIELD-416-FLAGS
            TO TRUE
           MOVE 416
             TO P500-INT-FIELD-416
           SET VALUE-PRESENT IN P500-INT-FIELD-417-FLAGS
            TO TRUE
           MOVE 417
             TO P500-INT-FIELD-417
           SET VALUE-PRESENT IN P500-INT-FIELD-418-FLAGS
            TO TRUE
           MOVE 418
             TO P500-INT-FIELD-418
           SET VALUE-PRESENT IN P500-INT-FIELD-419-FLAGS
            TO TRUE
           MOVE 419
             TO P500-INT-FIELD-419
           SET VALUE-PRESENT IN P500-INT-FIELD-420-FLAGS
            TO TRUE
           MOVE 420
             TO P500-INT-FIELD-420
           SET VALUE-PRESENT IN P500-INT-FIELD-421-FLAGS
            TO TRUE
           MOVE 421
             TO P500-INT-FIELD-421
           SET VALUE-PRESENT IN P500-INT-FIELD-422-FLAGS
            TO TRUE
           MOVE 422
             TO P500-INT-FIELD-422
           SET VALUE-PRESENT IN P500-INT-FIELD-423-FLAGS
            TO TRUE
           MOVE 423
             TO P500-INT-FIELD-423
           SET VALUE-PRESENT IN P500-INT-FIELD-424-FLAGS
            TO TRUE
           MOVE 424
             TO P500-INT-FIELD-424
           SET VALUE-PRESENT IN P500-INT-FIELD-425-FLAGS
            TO TRUE
           MOVE 425
             TO P500-INT-FIELD-425
           SET VALUE-PRESENT IN P500-INT-FIELD-426-FLAGS
            TO TRUE
           MOVE 426
             TO P500-INT-FIELD-426
           SET VALUE-PRESENT IN P500-INT-FIELD-427-FLAGS
            TO TRUE
           MOVE 427
             TO P500-INT-FIELD-427
           SET VALUE-PRESENT IN P500-INT-FIELD-428-FLAGS
            TO TRUE
           MOVE 428
             TO P500-INT-FIELD-428
           SET VALUE-PRESENT IN P500-INT-FIELD-429-FLAGS
            TO TRUE
           MOVE 429
             TO P500-INT-FIELD-429
           SET VALUE-PRESENT IN P500-INT-FIELD-430-FLAGS
            TO TRUE
           MOVE 430
             TO P500-INT-FIELD-430
           SET VALUE-PRESENT IN P500-INT-FIELD-431-FLAGS
            TO TRUE
           MOVE 431
             TO P500-INT-FIELD-431
           SET VALUE-PRESENT IN P500-INT-FIELD-432-FLAGS
            TO TRUE
           MOVE 432
             TO P500-INT-FIELD-432
           SET VALUE-PRESENT IN P500-INT-FIELD-433-FLAGS
            TO TRUE
           MOVE 433
             TO P500-INT-FIELD-433
           SET VALUE-PRESENT IN P500-INT-FIELD-434-FLAGS
            TO TRUE
           MOVE 434
             TO P500-INT-FIELD-434
           SET VALUE-PRESENT IN P500-INT-FIELD-435-FLAGS
            TO TRUE
           MOVE 435
             TO P500-INT-FIELD-435
           SET VALUE-PRESENT IN P500-INT-FIELD-436-FLAGS
            TO TRUE
           MOVE 436
             TO P500-INT-FIELD-436
           SET VALUE-PRESENT IN P500-INT-FIELD-437-FLAGS
            TO TRUE
           MOVE 437
             TO P500-INT-FIELD-437
           SET VALUE-PRESENT IN P500-INT-FIELD-438-FLAGS
            TO TRUE
           MOVE 438
             TO P500-INT-FIELD-438
           SET VALUE-PRESENT IN P500-INT-FIELD-439-FLAGS
            TO TRUE
           MOVE 439
             TO P500-INT-FIELD-439
           SET VALUE-PRESENT IN P500-INT-FIELD-440-FLAGS
            TO TRUE
           MOVE 440
             TO P500-INT-FIELD-440
           SET VALUE-PRESENT IN P500-INT-FIELD-441-FLAGS
            TO TRUE
           MOVE 441
             TO P500-INT-FIELD-441
           SET VALUE-PRESENT IN P500-INT-FIELD-442-FLAGS
            TO TRUE
           MOVE 442
             TO P500-INT-FIELD-442
           SET VALUE-PRESENT IN P500-INT-FIELD-443-FLAGS
            TO TRUE
           MOVE 443
             TO P500-INT-FIELD-443
           SET VALUE-PRESENT IN P500-INT-FIELD-444-FLAGS
            TO TRUE
           MOVE 444
             TO P500-INT-FIELD-444
           SET VALUE-PRESENT IN P500-INT-FIELD-445-FLAGS
            TO TRUE
           MOVE 445
             TO P500-INT-FIELD-445
           SET VALUE-PRESENT IN P500-INT-FIELD-446-FLAGS
            TO TRUE
           MOVE 446
             TO P500-INT-FIELD-446
           SET VALUE-PRESENT IN P500-INT-FIELD-447-FLAGS
            TO TRUE
           MOVE 447
             TO P500-INT-FIELD-447
           SET VALUE-PRESENT IN P500-INT-FIELD-448-FLAGS
            TO TRUE
           MOVE 448
             TO P500-INT-FIELD-448
           SET VALUE-PRESENT IN P500-INT-FIELD-449-FLAGS
            TO TRUE
           MOVE 449
             TO P500-INT-FIELD-449
           SET VALUE-PRESENT IN P500-INT-FIELD-450-FLAGS
            TO TRUE
           MOVE 450
             TO P500-INT-FIELD-450
           SET VALUE-PRESENT IN P500-INT-FIELD-451-FLAGS
            TO TRUE
           MOVE 451
             TO P500-INT-FIELD-451
           SET VALUE-PRESENT IN P500-INT-FIELD-452-FLAGS
            TO TRUE
           MOVE 452
             TO P500-INT-FIELD-452
           SET VALUE-PRESENT IN P500-INT-FIELD-453-FLAGS
            TO TRUE
           MOVE 453
             TO P500-INT-FIELD-453
           SET VALUE-PRESENT IN P500-INT-FIELD-454-FLAGS
            TO TRUE
           MOVE 454
             TO P500-INT-FIELD-454
           SET VALUE-PRESENT IN P500-INT-FIELD-455-FLAGS
            TO TRUE
           MOVE 455
             TO P500-INT-FIELD-455
           SET VALUE-PRESENT IN P500-INT-FIELD-456-FLAGS
            TO TRUE
           MOVE 456
             TO P500-INT-FIELD-456
           SET VALUE-PRESENT IN P500-INT-FIELD-457-FLAGS
            TO TRUE
           MOVE 457
             TO P500-INT-FIELD-457
           SET VALUE-PRESENT IN P500-INT-FIELD-458-FLAGS
            TO TRUE
           MOVE 458
             TO P500-INT-FIELD-458
           SET VALUE-PRESENT IN P500-INT-FIELD-459-FLAGS
            TO TRUE
           MOVE 459
             TO P500-INT-FIELD-459
           SET VALUE-PRESENT IN P500-INT-FIELD-460-FLAGS
            TO TRUE
           MOVE 460
             TO P500-INT-FIELD-460
           SET VALUE-PRESENT IN P500-INT-FIELD-461-FLAGS
            TO TRUE
           MOVE 461
             TO P500-INT-FIELD-461
           SET VALUE-PRESENT IN P500-INT-FIELD-462-FLAGS
            TO TRUE
           MOVE 462
             TO P500-INT-FIELD-462
           SET VALUE-PRESENT IN P500-INT-FIELD-463-FLAGS
            TO TRUE
           MOVE 463
             TO P500-INT-FIELD-463
           SET VALUE-PRESENT IN P500-INT-FIELD-464-FLAGS
            TO TRUE
           MOVE 464
             TO P500-INT-FIELD-464
           SET VALUE-PRESENT IN P500-INT-FIELD-465-FLAGS
            TO TRUE
           MOVE 465
             TO P500-INT-FIELD-465
           SET VALUE-PRESENT IN P500-INT-FIELD-466-FLAGS
            TO TRUE
           MOVE 466
             TO P500-INT-FIELD-466
           SET VALUE-PRESENT IN P500-INT-FIELD-467-FLAGS
            TO TRUE
           MOVE 467
             TO P500-INT-FIELD-467
           SET VALUE-PRESENT IN P500-INT-FIELD-468-FLAGS
            TO TRUE
           MOVE 468
             TO P500-INT-FIELD-468
           SET VALUE-PRESENT IN P500-INT-FIELD-469-FLAGS
            TO TRUE
           MOVE 469
             TO P500-INT-FIELD-469
           SET VALUE-PRESENT IN P500-INT-FIELD-470-FLAGS
            TO TRUE
           MOVE 470
             TO P500-INT-FIELD-470
           SET VALUE-PRESENT IN P500-INT-FIELD-471-FLAGS
            TO TRUE
           MOVE 471
             TO P500-INT-FIELD-471
           SET VALUE-PRESENT IN P500-INT-FIELD-472-FLAGS
            TO TRUE
           MOVE 472
             TO P500-INT-FIELD-472
           SET VALUE-PRESENT IN P500-INT-FIELD-473-FLAGS
            TO TRUE
           MOVE 473
             TO P500-INT-FIELD-473
           SET VALUE-PRESENT IN P500-INT-FIELD-474-FLAGS
            TO TRUE
           MOVE 474
             TO P500-INT-FIELD-474
           SET VALUE-PRESENT IN P500-INT-FIELD-475-FLAGS
            TO TRUE
           MOVE 475
             TO P500-INT-FIELD-475
           SET VALUE-PRESENT IN P500-INT-FIELD-476-FLAGS
            TO TRUE
           MOVE 476
             TO P500-INT-FIELD-476
           SET VALUE-PRESENT IN P500-INT-FIELD-477-FLAGS
            TO TRUE
           MOVE 477
             TO P500-INT-FIELD-477
           SET VALUE-PRESENT IN P500-INT-FIELD-478-FLAGS
            TO TRUE
           MOVE 478
             TO P500-INT-FIELD-478
           SET VALUE-PRESENT IN P500-INT-FIELD-479-FLAGS
            TO TRUE
           MOVE 479
             TO P500-INT-FIELD-479
           SET VALUE-PRESENT IN P500-INT-FIELD-480-FLAGS
            TO TRUE
           MOVE 480
             TO P500-INT-FIELD-480
           SET VALUE-PRESENT IN P500-INT-FIELD-481-FLAGS
            TO TRUE
           MOVE 481
             TO P500-INT-FIELD-481
           SET VALUE-PRESENT IN P500-INT-FIELD-482-FLAGS
            TO TRUE
           MOVE 482
             TO P500-INT-FIELD-482
           SET VALUE-PRESENT IN P500-INT-FIELD-483-FLAGS
            TO TRUE
           MOVE 483
             TO P500-INT-FIELD-483
           SET VALUE-PRESENT IN P500-INT-FIELD-484-FLAGS
            TO TRUE
           MOVE 484
             TO P500-INT-FIELD-484
           SET VALUE-PRESENT IN P500-INT-FIELD-485-FLAGS
            TO TRUE
           MOVE 485
             TO P500-INT-FIELD-485
           SET VALUE-PRESENT IN P500-INT-FIELD-486-FLAGS
            TO TRUE
           MOVE 486
             TO P500-INT-FIELD-486
           SET VALUE-PRESENT IN P500-INT-FIELD-487-FLAGS
            TO TRUE
           MOVE 487
             TO P500-INT-FIELD-487
           SET VALUE-PRESENT IN P500-INT-FIELD-488-FLAGS
            TO TRUE
           MOVE 488
             TO P500-INT-FIELD-488
           SET VALUE-PRESENT IN P500-INT-FIELD-489-FLAGS
            TO TRUE
           MOVE 489
             TO P500-INT-FIELD-489
           SET VALUE-PRESENT IN P500-INT-FIELD-490-FLAGS
            TO TRUE
           MOVE 490
             TO P500-INT-FIELD-490
           SET VALUE-PRESENT IN P500-INT-FIELD-491-FLAGS
            TO TRUE
           MOVE 491
             TO P500-INT-FIELD-491
           SET VALUE-PRESENT IN P500-INT-FIELD-492-FLAGS
            TO TRUE
           MOVE 492
             TO P500-INT-FIELD-492
           SET VALUE-PRESENT IN P500-INT-FIELD-493-FLAGS
            TO TRUE
           MOVE 493
             TO P500-INT-FIELD-493
           SET VALUE-PRESENT IN P500-INT-FIELD-494-FLAGS
            TO TRUE
           MOVE 494
             TO P500-INT-FIELD-494
           SET VALUE-PRESENT IN P500-INT-FIELD-495-FLAGS
            TO TRUE
           MOVE 495
             TO P500-INT-FIELD-495
           SET VALUE-PRESENT IN P500-INT-FIELD-496-FLAGS
            TO TRUE
           MOVE 496
             TO P500-INT-FIELD-496
           SET VALUE-PRESENT IN P500-INT-FIELD-497-FLAGS
            TO TRUE
           MOVE 497
             TO P500-INT-FIELD-497
           SET VALUE-PRESENT IN P500-INT-FIELD-498-FLAGS
            TO TRUE
           MOVE 498
             TO P500-INT-FIELD-498
           SET VALUE-PRESENT IN P500-INT-FIELD-499-FLAGS
            TO TRUE
           MOVE 499
             TO P500-INT-FIELD-499
           SET VALUE-PRESENT IN P500-INT-FIELD-500-FLAGS
            TO TRUE
           MOVE 500
             TO P500-INT-FIELD-500

           SET VALUE-PRESENT IN P500-STRING-FIELD-1-FLAGS
            TO TRUE
           MOVE '1'
             TO P500-STRING-FIELD-1
           SET VALUE-PRESENT IN P500-STRING-FIELD-2-FLAGS
            TO TRUE
           MOVE '2'
             TO P500-STRING-FIELD-2
           SET VALUE-PRESENT IN P500-STRING-FIELD-3-FLAGS
            TO TRUE
           MOVE '3'
             TO P500-STRING-FIELD-3
           SET VALUE-PRESENT IN P500-STRING-FIELD-4-FLAGS
            TO TRUE
           MOVE '4'
             TO P500-STRING-FIELD-4
           SET VALUE-PRESENT IN P500-STRING-FIELD-5-FLAGS
            TO TRUE
           MOVE '5'
             TO P500-STRING-FIELD-5
           SET VALUE-PRESENT IN P500-STRING-FIELD-6-FLAGS
            TO TRUE
           MOVE '6'
             TO P500-STRING-FIELD-6
           SET VALUE-PRESENT IN P500-STRING-FIELD-7-FLAGS
            TO TRUE
           MOVE '7'
             TO P500-STRING-FIELD-7
           SET VALUE-PRESENT IN P500-STRING-FIELD-8-FLAGS
            TO TRUE
           MOVE '8'
             TO P500-STRING-FIELD-8
           SET VALUE-PRESENT IN P500-STRING-FIELD-9-FLAGS
            TO TRUE
           MOVE '9'
             TO P500-STRING-FIELD-9
           SET VALUE-PRESENT IN P500-STRING-FIELD-10-FLAGS
            TO TRUE
           MOVE '10'
             TO P500-STRING-FIELD-10
           SET VALUE-PRESENT IN P500-STRING-FIELD-11-FLAGS
            TO TRUE
           MOVE '11'
             TO P500-STRING-FIELD-11
           SET VALUE-PRESENT IN P500-STRING-FIELD-12-FLAGS
            TO TRUE
           MOVE '12'
             TO P500-STRING-FIELD-12
           SET VALUE-PRESENT IN P500-STRING-FIELD-13-FLAGS
            TO TRUE
           MOVE '13'
             TO P500-STRING-FIELD-13
           SET VALUE-PRESENT IN P500-STRING-FIELD-14-FLAGS
            TO TRUE
           MOVE '14'
             TO P500-STRING-FIELD-14
           SET VALUE-PRESENT IN P500-STRING-FIELD-15-FLAGS
            TO TRUE
           MOVE '15'
             TO P500-STRING-FIELD-15
           SET VALUE-PRESENT IN P500-STRING-FIELD-16-FLAGS
            TO TRUE
           MOVE '16'
             TO P500-STRING-FIELD-16
           SET VALUE-PRESENT IN P500-STRING-FIELD-17-FLAGS
            TO TRUE
           MOVE '17'
             TO P500-STRING-FIELD-17
           SET VALUE-PRESENT IN P500-STRING-FIELD-18-FLAGS
            TO TRUE
           MOVE '18'
             TO P500-STRING-FIELD-18
           SET VALUE-PRESENT IN P500-STRING-FIELD-19-FLAGS
            TO TRUE
           MOVE '19'
             TO P500-STRING-FIELD-19
           SET VALUE-PRESENT IN P500-STRING-FIELD-20-FLAGS
            TO TRUE
           MOVE '20'
             TO P500-STRING-FIELD-20
           SET VALUE-PRESENT IN P500-STRING-FIELD-21-FLAGS
            TO TRUE
           MOVE '21'
             TO P500-STRING-FIELD-21
           SET VALUE-PRESENT IN P500-STRING-FIELD-22-FLAGS
            TO TRUE
           MOVE '22'
             TO P500-STRING-FIELD-22
           SET VALUE-PRESENT IN P500-STRING-FIELD-23-FLAGS
            TO TRUE
           MOVE '23'
             TO P500-STRING-FIELD-23
           SET VALUE-PRESENT IN P500-STRING-FIELD-24-FLAGS
            TO TRUE
           MOVE '24'
             TO P500-STRING-FIELD-24
           SET VALUE-PRESENT IN P500-STRING-FIELD-25-FLAGS
            TO TRUE
           MOVE '25'
             TO P500-STRING-FIELD-25
           SET VALUE-PRESENT IN P500-STRING-FIELD-26-FLAGS
            TO TRUE
           MOVE '26'
             TO P500-STRING-FIELD-26
           SET VALUE-PRESENT IN P500-STRING-FIELD-27-FLAGS
            TO TRUE
           MOVE '27'
             TO P500-STRING-FIELD-27
           SET VALUE-PRESENT IN P500-STRING-FIELD-28-FLAGS
            TO TRUE
           MOVE '28'
             TO P500-STRING-FIELD-28
           SET VALUE-PRESENT IN P500-STRING-FIELD-29-FLAGS
            TO TRUE
           MOVE '29'
             TO P500-STRING-FIELD-29
           SET VALUE-PRESENT IN P500-STRING-FIELD-30-FLAGS
            TO TRUE
           MOVE '30'
             TO P500-STRING-FIELD-30
           SET VALUE-PRESENT IN P500-STRING-FIELD-31-FLAGS
            TO TRUE
           MOVE '31'
             TO P500-STRING-FIELD-31
           SET VALUE-PRESENT IN P500-STRING-FIELD-32-FLAGS
            TO TRUE
           MOVE '32'
             TO P500-STRING-FIELD-32
           SET VALUE-PRESENT IN P500-STRING-FIELD-33-FLAGS
            TO TRUE
           MOVE '33'
             TO P500-STRING-FIELD-33
           SET VALUE-PRESENT IN P500-STRING-FIELD-34-FLAGS
            TO TRUE
           MOVE '34'
             TO P500-STRING-FIELD-34
           SET VALUE-PRESENT IN P500-STRING-FIELD-35-FLAGS
            TO TRUE
           MOVE '35'
             TO P500-STRING-FIELD-35
           SET VALUE-PRESENT IN P500-STRING-FIELD-36-FLAGS
            TO TRUE
           MOVE '36'
             TO P500-STRING-FIELD-36
           SET VALUE-PRESENT IN P500-STRING-FIELD-37-FLAGS
            TO TRUE
           MOVE '37'
             TO P500-STRING-FIELD-37
           SET VALUE-PRESENT IN P500-STRING-FIELD-38-FLAGS
            TO TRUE
           MOVE '38'
             TO P500-STRING-FIELD-38
           SET VALUE-PRESENT IN P500-STRING-FIELD-39-FLAGS
            TO TRUE
           MOVE '39'
             TO P500-STRING-FIELD-39
           SET VALUE-PRESENT IN P500-STRING-FIELD-40-FLAGS
            TO TRUE
           MOVE '40'
             TO P500-STRING-FIELD-40
           SET VALUE-PRESENT IN P500-STRING-FIELD-41-FLAGS
            TO TRUE
           MOVE '41'
             TO P500-STRING-FIELD-41
           SET VALUE-PRESENT IN P500-STRING-FIELD-42-FLAGS
            TO TRUE
           MOVE '42'
             TO P500-STRING-FIELD-42
           SET VALUE-PRESENT IN P500-STRING-FIELD-43-FLAGS
            TO TRUE
           MOVE '43'
             TO P500-STRING-FIELD-43
           SET VALUE-PRESENT IN P500-STRING-FIELD-44-FLAGS
            TO TRUE
           MOVE '44'
             TO P500-STRING-FIELD-44
           SET VALUE-PRESENT IN P500-STRING-FIELD-45-FLAGS
            TO TRUE
           MOVE '45'
             TO P500-STRING-FIELD-45
           SET VALUE-PRESENT IN P500-STRING-FIELD-46-FLAGS
            TO TRUE
           MOVE '46'
             TO P500-STRING-FIELD-46
           SET VALUE-PRESENT IN P500-STRING-FIELD-47-FLAGS
            TO TRUE
           MOVE '47'
             TO P500-STRING-FIELD-47
           SET VALUE-PRESENT IN P500-STRING-FIELD-48-FLAGS
            TO TRUE
           MOVE '48'
             TO P500-STRING-FIELD-48
           SET VALUE-PRESENT IN P500-STRING-FIELD-49-FLAGS
            TO TRUE
           MOVE '49'
             TO P500-STRING-FIELD-49
           SET VALUE-PRESENT IN P500-STRING-FIELD-50-FLAGS
            TO TRUE
           MOVE '50'
             TO P500-STRING-FIELD-50
           SET VALUE-PRESENT IN P500-STRING-FIELD-51-FLAGS
            TO TRUE
           MOVE '51'
             TO P500-STRING-FIELD-51
           SET VALUE-PRESENT IN P500-STRING-FIELD-52-FLAGS
            TO TRUE
           MOVE '52'
             TO P500-STRING-FIELD-52
           SET VALUE-PRESENT IN P500-STRING-FIELD-53-FLAGS
            TO TRUE
           MOVE '53'
             TO P500-STRING-FIELD-53
           SET VALUE-PRESENT IN P500-STRING-FIELD-54-FLAGS
            TO TRUE
           MOVE '54'
             TO P500-STRING-FIELD-54
           SET VALUE-PRESENT IN P500-STRING-FIELD-55-FLAGS
            TO TRUE
           MOVE '55'
             TO P500-STRING-FIELD-55
           SET VALUE-PRESENT IN P500-STRING-FIELD-56-FLAGS
            TO TRUE
           MOVE '56'
             TO P500-STRING-FIELD-56
           SET VALUE-PRESENT IN P500-STRING-FIELD-57-FLAGS
            TO TRUE
           MOVE '57'
             TO P500-STRING-FIELD-57
           SET VALUE-PRESENT IN P500-STRING-FIELD-58-FLAGS
            TO TRUE
           MOVE '58'
             TO P500-STRING-FIELD-58
           SET VALUE-PRESENT IN P500-STRING-FIELD-59-FLAGS
            TO TRUE
           MOVE '59'
             TO P500-STRING-FIELD-59
           SET VALUE-PRESENT IN P500-STRING-FIELD-60-FLAGS
            TO TRUE
           MOVE '60'
             TO P500-STRING-FIELD-60
           SET VALUE-PRESENT IN P500-STRING-FIELD-61-FLAGS
            TO TRUE
           MOVE '61'
             TO P500-STRING-FIELD-61
           SET VALUE-PRESENT IN P500-STRING-FIELD-62-FLAGS
            TO TRUE
           MOVE '62'
             TO P500-STRING-FIELD-62
           SET VALUE-PRESENT IN P500-STRING-FIELD-63-FLAGS
            TO TRUE
           MOVE '63'
             TO P500-STRING-FIELD-63
           SET VALUE-PRESENT IN P500-STRING-FIELD-64-FLAGS
            TO TRUE
           MOVE '64'
             TO P500-STRING-FIELD-64
           SET VALUE-PRESENT IN P500-STRING-FIELD-65-FLAGS
            TO TRUE
           MOVE '65'
             TO P500-STRING-FIELD-65
           SET VALUE-PRESENT IN P500-STRING-FIELD-66-FLAGS
            TO TRUE
           MOVE '66'
             TO P500-STRING-FIELD-66
           SET VALUE-PRESENT IN P500-STRING-FIELD-67-FLAGS
            TO TRUE
           MOVE '67'
             TO P500-STRING-FIELD-67
           SET VALUE-PRESENT IN P500-STRING-FIELD-68-FLAGS
            TO TRUE
           MOVE '68'
             TO P500-STRING-FIELD-68
           SET VALUE-PRESENT IN P500-STRING-FIELD-69-FLAGS
            TO TRUE
           MOVE '69'
             TO P500-STRING-FIELD-69
           SET VALUE-PRESENT IN P500-STRING-FIELD-70-FLAGS
            TO TRUE
           MOVE '70'
             TO P500-STRING-FIELD-70
           SET VALUE-PRESENT IN P500-STRING-FIELD-71-FLAGS
            TO TRUE
           MOVE '71'
             TO P500-STRING-FIELD-71
           SET VALUE-PRESENT IN P500-STRING-FIELD-72-FLAGS
            TO TRUE
           MOVE '72'
             TO P500-STRING-FIELD-72
           SET VALUE-PRESENT IN P500-STRING-FIELD-73-FLAGS
            TO TRUE
           MOVE '73'
             TO P500-STRING-FIELD-73
           SET VALUE-PRESENT IN P500-STRING-FIELD-74-FLAGS
            TO TRUE
           MOVE '74'
             TO P500-STRING-FIELD-74
           SET VALUE-PRESENT IN P500-STRING-FIELD-75-FLAGS
            TO TRUE
           MOVE '75'
             TO P500-STRING-FIELD-75
           SET VALUE-PRESENT IN P500-STRING-FIELD-76-FLAGS
            TO TRUE
           MOVE '76'
             TO P500-STRING-FIELD-76
           SET VALUE-PRESENT IN P500-STRING-FIELD-77-FLAGS
            TO TRUE
           MOVE '77'
             TO P500-STRING-FIELD-77
           SET VALUE-PRESENT IN P500-STRING-FIELD-78-FLAGS
            TO TRUE
           MOVE '78'
             TO P500-STRING-FIELD-78
           SET VALUE-PRESENT IN P500-STRING-FIELD-79-FLAGS
            TO TRUE
           MOVE '79'
             TO P500-STRING-FIELD-79
           SET VALUE-PRESENT IN P500-STRING-FIELD-80-FLAGS
            TO TRUE
           MOVE '80'
             TO P500-STRING-FIELD-80
           SET VALUE-PRESENT IN P500-STRING-FIELD-81-FLAGS
            TO TRUE
           MOVE '81'
             TO P500-STRING-FIELD-81
           SET VALUE-PRESENT IN P500-STRING-FIELD-82-FLAGS
            TO TRUE
           MOVE '82'
             TO P500-STRING-FIELD-82
           SET VALUE-PRESENT IN P500-STRING-FIELD-83-FLAGS
            TO TRUE
           MOVE '83'
             TO P500-STRING-FIELD-83
           SET VALUE-PRESENT IN P500-STRING-FIELD-84-FLAGS
            TO TRUE
           MOVE '84'
             TO P500-STRING-FIELD-84
           SET VALUE-PRESENT IN P500-STRING-FIELD-85-FLAGS
            TO TRUE
           MOVE '85'
             TO P500-STRING-FIELD-85
           SET VALUE-PRESENT IN P500-STRING-FIELD-86-FLAGS
            TO TRUE
           MOVE '86'
             TO P500-STRING-FIELD-86
           SET VALUE-PRESENT IN P500-STRING-FIELD-87-FLAGS
            TO TRUE
           MOVE '87'
             TO P500-STRING-FIELD-87
           SET VALUE-PRESENT IN P500-STRING-FIELD-88-FLAGS
            TO TRUE
           MOVE '88'
             TO P500-STRING-FIELD-88
           SET VALUE-PRESENT IN P500-STRING-FIELD-89-FLAGS
            TO TRUE
           MOVE '89'
             TO P500-STRING-FIELD-89
           SET VALUE-PRESENT IN P500-STRING-FIELD-90-FLAGS
            TO TRUE
           MOVE '90'
             TO P500-STRING-FIELD-90
           SET VALUE-PRESENT IN P500-STRING-FIELD-91-FLAGS
            TO TRUE
           MOVE '91'
             TO P500-STRING-FIELD-91
           SET VALUE-PRESENT IN P500-STRING-FIELD-92-FLAGS
            TO TRUE
           MOVE '92'
             TO P500-STRING-FIELD-92
           SET VALUE-PRESENT IN P500-STRING-FIELD-93-FLAGS
            TO TRUE
           MOVE '93'
             TO P500-STRING-FIELD-93
           SET VALUE-PRESENT IN P500-STRING-FIELD-94-FLAGS
            TO TRUE
           MOVE '94'
             TO P500-STRING-FIELD-94
           SET VALUE-PRESENT IN P500-STRING-FIELD-95-FLAGS
            TO TRUE
           MOVE '95'
             TO P500-STRING-FIELD-95
           SET VALUE-PRESENT IN P500-STRING-FIELD-96-FLAGS
            TO TRUE
           MOVE '96'
             TO P500-STRING-FIELD-96
           SET VALUE-PRESENT IN P500-STRING-FIELD-97-FLAGS
            TO TRUE
           MOVE '97'
             TO P500-STRING-FIELD-97
           SET VALUE-PRESENT IN P500-STRING-FIELD-98-FLAGS
            TO TRUE
           MOVE '98'
             TO P500-STRING-FIELD-98
           SET VALUE-PRESENT IN P500-STRING-FIELD-99-FLAGS
            TO TRUE
           MOVE '99'
             TO P500-STRING-FIELD-99
           SET VALUE-PRESENT IN P500-STRING-FIELD-100-FLAGS
            TO TRUE
           MOVE '100'
             TO P500-STRING-FIELD-100
           SET VALUE-PRESENT IN P500-STRING-FIELD-101-FLAGS
            TO TRUE
           MOVE '101'
             TO P500-STRING-FIELD-101
           SET VALUE-PRESENT IN P500-STRING-FIELD-102-FLAGS
            TO TRUE
           MOVE '102'
             TO P500-STRING-FIELD-102
           SET VALUE-PRESENT IN P500-STRING-FIELD-103-FLAGS
            TO TRUE
           MOVE '103'
             TO P500-STRING-FIELD-103
           SET VALUE-PRESENT IN P500-STRING-FIELD-104-FLAGS
            TO TRUE
           MOVE '104'
             TO P500-STRING-FIELD-104
           SET VALUE-PRESENT IN P500-STRING-FIELD-105-FLAGS
            TO TRUE
           MOVE '105'
             TO P500-STRING-FIELD-105
           SET VALUE-PRESENT IN P500-STRING-FIELD-106-FLAGS
            TO TRUE
           MOVE '106'
             TO P500-STRING-FIELD-106
           SET VALUE-PRESENT IN P500-STRING-FIELD-107-FLAGS
            TO TRUE
           MOVE '107'
             TO P500-STRING-FIELD-107
           SET VALUE-PRESENT IN P500-STRING-FIELD-108-FLAGS
            TO TRUE
           MOVE '108'
             TO P500-STRING-FIELD-108
           SET VALUE-PRESENT IN P500-STRING-FIELD-109-FLAGS
            TO TRUE
           MOVE '109'
             TO P500-STRING-FIELD-109
           SET VALUE-PRESENT IN P500-STRING-FIELD-110-FLAGS
            TO TRUE
           MOVE '110'
             TO P500-STRING-FIELD-110
           SET VALUE-PRESENT IN P500-STRING-FIELD-111-FLAGS
            TO TRUE
           MOVE '111'
             TO P500-STRING-FIELD-111
           SET VALUE-PRESENT IN P500-STRING-FIELD-112-FLAGS
            TO TRUE
           MOVE '112'
             TO P500-STRING-FIELD-112
           SET VALUE-PRESENT IN P500-STRING-FIELD-113-FLAGS
            TO TRUE
           MOVE '113'
             TO P500-STRING-FIELD-113
           SET VALUE-PRESENT IN P500-STRING-FIELD-114-FLAGS
            TO TRUE
           MOVE '114'
             TO P500-STRING-FIELD-114
           SET VALUE-PRESENT IN P500-STRING-FIELD-115-FLAGS
            TO TRUE
           MOVE '115'
             TO P500-STRING-FIELD-115
           SET VALUE-PRESENT IN P500-STRING-FIELD-116-FLAGS
            TO TRUE
           MOVE '116'
             TO P500-STRING-FIELD-116
           SET VALUE-PRESENT IN P500-STRING-FIELD-117-FLAGS
            TO TRUE
           MOVE '117'
             TO P500-STRING-FIELD-117
           SET VALUE-PRESENT IN P500-STRING-FIELD-118-FLAGS
            TO TRUE
           MOVE '118'
             TO P500-STRING-FIELD-118
           SET VALUE-PRESENT IN P500-STRING-FIELD-119-FLAGS
            TO TRUE
           MOVE '119'
             TO P500-STRING-FIELD-119
           SET VALUE-PRESENT IN P500-STRING-FIELD-120-FLAGS
            TO TRUE
           MOVE '120'
             TO P500-STRING-FIELD-120
           SET VALUE-PRESENT IN P500-STRING-FIELD-121-FLAGS
            TO TRUE
           MOVE '121'
             TO P500-STRING-FIELD-121
           SET VALUE-PRESENT IN P500-STRING-FIELD-122-FLAGS
            TO TRUE
           MOVE '122'
             TO P500-STRING-FIELD-122
           SET VALUE-PRESENT IN P500-STRING-FIELD-123-FLAGS
            TO TRUE
           MOVE '123'
             TO P500-STRING-FIELD-123
           SET VALUE-PRESENT IN P500-STRING-FIELD-124-FLAGS
            TO TRUE
           MOVE '124'
             TO P500-STRING-FIELD-124
           SET VALUE-PRESENT IN P500-STRING-FIELD-125-FLAGS
            TO TRUE
           MOVE '125'
             TO P500-STRING-FIELD-125
           SET VALUE-PRESENT IN P500-STRING-FIELD-126-FLAGS
            TO TRUE
           MOVE '126'
             TO P500-STRING-FIELD-126
           SET VALUE-PRESENT IN P500-STRING-FIELD-127-FLAGS
            TO TRUE
           MOVE '127'
             TO P500-STRING-FIELD-127
           SET VALUE-PRESENT IN P500-STRING-FIELD-128-FLAGS
            TO TRUE
           MOVE '128'
             TO P500-STRING-FIELD-128
           SET VALUE-PRESENT IN P500-STRING-FIELD-129-FLAGS
            TO TRUE
           MOVE '129'
             TO P500-STRING-FIELD-129
           SET VALUE-PRESENT IN P500-STRING-FIELD-130-FLAGS
            TO TRUE
           MOVE '130'
             TO P500-STRING-FIELD-130
           SET VALUE-PRESENT IN P500-STRING-FIELD-131-FLAGS
            TO TRUE
           MOVE '131'
             TO P500-STRING-FIELD-131
           SET VALUE-PRESENT IN P500-STRING-FIELD-132-FLAGS
            TO TRUE
           MOVE '132'
             TO P500-STRING-FIELD-132
           SET VALUE-PRESENT IN P500-STRING-FIELD-133-FLAGS
            TO TRUE
           MOVE '133'
             TO P500-STRING-FIELD-133
           SET VALUE-PRESENT IN P500-STRING-FIELD-134-FLAGS
            TO TRUE
           MOVE '134'
             TO P500-STRING-FIELD-134
           SET VALUE-PRESENT IN P500-STRING-FIELD-135-FLAGS
            TO TRUE
           MOVE '135'
             TO P500-STRING-FIELD-135
           SET VALUE-PRESENT IN P500-STRING-FIELD-136-FLAGS
            TO TRUE
           MOVE '136'
             TO P500-STRING-FIELD-136
           SET VALUE-PRESENT IN P500-STRING-FIELD-137-FLAGS
            TO TRUE
           MOVE '137'
             TO P500-STRING-FIELD-137
           SET VALUE-PRESENT IN P500-STRING-FIELD-138-FLAGS
            TO TRUE
           MOVE '138'
             TO P500-STRING-FIELD-138
           SET VALUE-PRESENT IN P500-STRING-FIELD-139-FLAGS
            TO TRUE
           MOVE '139'
             TO P500-STRING-FIELD-139
           SET VALUE-PRESENT IN P500-STRING-FIELD-140-FLAGS
            TO TRUE
           MOVE '140'
             TO P500-STRING-FIELD-140
           SET VALUE-PRESENT IN P500-STRING-FIELD-141-FLAGS
            TO TRUE
           MOVE '141'
             TO P500-STRING-FIELD-141
           SET VALUE-PRESENT IN P500-STRING-FIELD-142-FLAGS
            TO TRUE
           MOVE '142'
             TO P500-STRING-FIELD-142
           SET VALUE-PRESENT IN P500-STRING-FIELD-143-FLAGS
            TO TRUE
           MOVE '143'
             TO P500-STRING-FIELD-143
           SET VALUE-PRESENT IN P500-STRING-FIELD-144-FLAGS
            TO TRUE
           MOVE '144'
             TO P500-STRING-FIELD-144
           SET VALUE-PRESENT IN P500-STRING-FIELD-145-FLAGS
            TO TRUE
           MOVE '145'
             TO P500-STRING-FIELD-145
           SET VALUE-PRESENT IN P500-STRING-FIELD-146-FLAGS
            TO TRUE
           MOVE '146'
             TO P500-STRING-FIELD-146
           SET VALUE-PRESENT IN P500-STRING-FIELD-147-FLAGS
            TO TRUE
           MOVE '147'
             TO P500-STRING-FIELD-147
           SET VALUE-PRESENT IN P500-STRING-FIELD-148-FLAGS
            TO TRUE
           MOVE '148'
             TO P500-STRING-FIELD-148
           SET VALUE-PRESENT IN P500-STRING-FIELD-149-FLAGS
            TO TRUE
           MOVE '149'
             TO P500-STRING-FIELD-149
           SET VALUE-PRESENT IN P500-STRING-FIELD-150-FLAGS
            TO TRUE
           MOVE '150'
             TO P500-STRING-FIELD-150
           SET VALUE-PRESENT IN P500-STRING-FIELD-151-FLAGS
            TO TRUE
           MOVE '151'
             TO P500-STRING-FIELD-151
           SET VALUE-PRESENT IN P500-STRING-FIELD-152-FLAGS
            TO TRUE
           MOVE '152'
             TO P500-STRING-FIELD-152
           SET VALUE-PRESENT IN P500-STRING-FIELD-153-FLAGS
            TO TRUE
           MOVE '153'
             TO P500-STRING-FIELD-153
           SET VALUE-PRESENT IN P500-STRING-FIELD-154-FLAGS
            TO TRUE
           MOVE '154'
             TO P500-STRING-FIELD-154
           SET VALUE-PRESENT IN P500-STRING-FIELD-155-FLAGS
            TO TRUE
           MOVE '155'
             TO P500-STRING-FIELD-155
           SET VALUE-PRESENT IN P500-STRING-FIELD-156-FLAGS
            TO TRUE
           MOVE '156'
             TO P500-STRING-FIELD-156
           SET VALUE-PRESENT IN P500-STRING-FIELD-157-FLAGS
            TO TRUE
           MOVE '157'
             TO P500-STRING-FIELD-157
           SET VALUE-PRESENT IN P500-STRING-FIELD-158-FLAGS
            TO TRUE
           MOVE '158'
             TO P500-STRING-FIELD-158
           SET VALUE-PRESENT IN P500-STRING-FIELD-159-FLAGS
            TO TRUE
           MOVE '159'
             TO P500-STRING-FIELD-159
           SET VALUE-PRESENT IN P500-STRING-FIELD-160-FLAGS
            TO TRUE
           MOVE '160'
             TO P500-STRING-FIELD-160
           SET VALUE-PRESENT IN P500-STRING-FIELD-161-FLAGS
            TO TRUE
           MOVE '161'
             TO P500-STRING-FIELD-161
           SET VALUE-PRESENT IN P500-STRING-FIELD-162-FLAGS
            TO TRUE
           MOVE '162'
             TO P500-STRING-FIELD-162
           SET VALUE-PRESENT IN P500-STRING-FIELD-163-FLAGS
            TO TRUE
           MOVE '163'
             TO P500-STRING-FIELD-163
           SET VALUE-PRESENT IN P500-STRING-FIELD-164-FLAGS
            TO TRUE
           MOVE '164'
             TO P500-STRING-FIELD-164
           SET VALUE-PRESENT IN P500-STRING-FIELD-165-FLAGS
            TO TRUE
           MOVE '165'
             TO P500-STRING-FIELD-165
           SET VALUE-PRESENT IN P500-STRING-FIELD-166-FLAGS
            TO TRUE
           MOVE '166'
             TO P500-STRING-FIELD-166
           SET VALUE-PRESENT IN P500-STRING-FIELD-167-FLAGS
            TO TRUE
           MOVE '167'
             TO P500-STRING-FIELD-167
           SET VALUE-PRESENT IN P500-STRING-FIELD-168-FLAGS
            TO TRUE
           MOVE '168'
             TO P500-STRING-FIELD-168
           SET VALUE-PRESENT IN P500-STRING-FIELD-169-FLAGS
            TO TRUE
           MOVE '169'
             TO P500-STRING-FIELD-169
           SET VALUE-PRESENT IN P500-STRING-FIELD-170-FLAGS
            TO TRUE
           MOVE '170'
             TO P500-STRING-FIELD-170
           SET VALUE-PRESENT IN P500-STRING-FIELD-171-FLAGS
            TO TRUE
           MOVE '171'
             TO P500-STRING-FIELD-171
           SET VALUE-PRESENT IN P500-STRING-FIELD-172-FLAGS
            TO TRUE
           MOVE '172'
             TO P500-STRING-FIELD-172
           SET VALUE-PRESENT IN P500-STRING-FIELD-173-FLAGS
            TO TRUE
           MOVE '173'
             TO P500-STRING-FIELD-173
           SET VALUE-PRESENT IN P500-STRING-FIELD-174-FLAGS
            TO TRUE
           MOVE '174'
             TO P500-STRING-FIELD-174
           SET VALUE-PRESENT IN P500-STRING-FIELD-175-FLAGS
            TO TRUE
           MOVE '175'
             TO P500-STRING-FIELD-175
           SET VALUE-PRESENT IN P500-STRING-FIELD-176-FLAGS
            TO TRUE
           MOVE '176'
             TO P500-STRING-FIELD-176
           SET VALUE-PRESENT IN P500-STRING-FIELD-177-FLAGS
            TO TRUE
           MOVE '177'
             TO P500-STRING-FIELD-177
           SET VALUE-PRESENT IN P500-STRING-FIELD-178-FLAGS
            TO TRUE
           MOVE '178'
             TO P500-STRING-FIELD-178
           SET VALUE-PRESENT IN P500-STRING-FIELD-179-FLAGS
            TO TRUE
           MOVE '179'
             TO P500-STRING-FIELD-179
           SET VALUE-PRESENT IN P500-STRING-FIELD-180-FLAGS
            TO TRUE
           MOVE '180'
             TO P500-STRING-FIELD-180
           SET VALUE-PRESENT IN P500-STRING-FIELD-181-FLAGS
            TO TRUE
           MOVE '181'
             TO P500-STRING-FIELD-181
           SET VALUE-PRESENT IN P500-STRING-FIELD-182-FLAGS
            TO TRUE
           MOVE '182'
             TO P500-STRING-FIELD-182
           SET VALUE-PRESENT IN P500-STRING-FIELD-183-FLAGS
            TO TRUE
           MOVE '183'
             TO P500-STRING-FIELD-183
           SET VALUE-PRESENT IN P500-STRING-FIELD-184-FLAGS
            TO TRUE
           MOVE '184'
             TO P500-STRING-FIELD-184
           SET VALUE-PRESENT IN P500-STRING-FIELD-185-FLAGS
            TO TRUE
           MOVE '185'
             TO P500-STRING-FIELD-185
           SET VALUE-PRESENT IN P500-STRING-FIELD-186-FLAGS
            TO TRUE
           MOVE '186'
             TO P500-STRING-FIELD-186
           SET VALUE-PRESENT IN P500-STRING-FIELD-187-FLAGS
            TO TRUE
           MOVE '187'
             TO P500-STRING-FIELD-187
           SET VALUE-PRESENT IN P500-STRING-FIELD-188-FLAGS
            TO TRUE
           MOVE '188'
             TO P500-STRING-FIELD-188
           SET VALUE-PRESENT IN P500-STRING-FIELD-189-FLAGS
            TO TRUE
           MOVE '189'
             TO P500-STRING-FIELD-189
           SET VALUE-PRESENT IN P500-STRING-FIELD-190-FLAGS
            TO TRUE
           MOVE '190'
             TO P500-STRING-FIELD-190
           SET VALUE-PRESENT IN P500-STRING-FIELD-191-FLAGS
            TO TRUE
           MOVE '191'
             TO P500-STRING-FIELD-191
           SET VALUE-PRESENT IN P500-STRING-FIELD-192-FLAGS
            TO TRUE
           MOVE '192'
             TO P500-STRING-FIELD-192
           SET VALUE-PRESENT IN P500-STRING-FIELD-193-FLAGS
            TO TRUE
           MOVE '193'
             TO P500-STRING-FIELD-193
           SET VALUE-PRESENT IN P500-STRING-FIELD-194-FLAGS
            TO TRUE
           MOVE '194'
             TO P500-STRING-FIELD-194
           SET VALUE-PRESENT IN P500-STRING-FIELD-195-FLAGS
            TO TRUE
           MOVE '195'
             TO P500-STRING-FIELD-195
           SET VALUE-PRESENT IN P500-STRING-FIELD-196-FLAGS
            TO TRUE
           MOVE '196'
             TO P500-STRING-FIELD-196
           SET VALUE-PRESENT IN P500-STRING-FIELD-197-FLAGS
            TO TRUE
           MOVE '197'
             TO P500-STRING-FIELD-197
           SET VALUE-PRESENT IN P500-STRING-FIELD-198-FLAGS
            TO TRUE
           MOVE '198'
             TO P500-STRING-FIELD-198
           SET VALUE-PRESENT IN P500-STRING-FIELD-199-FLAGS
            TO TRUE
           MOVE '199'
             TO P500-STRING-FIELD-199
           SET VALUE-PRESENT IN P500-STRING-FIELD-200-FLAGS
            TO TRUE
           MOVE '200'
             TO P500-STRING-FIELD-200
           SET VALUE-PRESENT IN P500-STRING-FIELD-201-FLAGS
            TO TRUE
           MOVE '201'
             TO P500-STRING-FIELD-201
           SET VALUE-PRESENT IN P500-STRING-FIELD-202-FLAGS
            TO TRUE
           MOVE '202'
             TO P500-STRING-FIELD-202
           SET VALUE-PRESENT IN P500-STRING-FIELD-203-FLAGS
            TO TRUE
           MOVE '203'
             TO P500-STRING-FIELD-203
           SET VALUE-PRESENT IN P500-STRING-FIELD-204-FLAGS
            TO TRUE
           MOVE '204'
             TO P500-STRING-FIELD-204
           SET VALUE-PRESENT IN P500-STRING-FIELD-205-FLAGS
            TO TRUE
           MOVE '205'
             TO P500-STRING-FIELD-205
           SET VALUE-PRESENT IN P500-STRING-FIELD-206-FLAGS
            TO TRUE
           MOVE '206'
             TO P500-STRING-FIELD-206
           SET VALUE-PRESENT IN P500-STRING-FIELD-207-FLAGS
            TO TRUE
           MOVE '207'
             TO P500-STRING-FIELD-207
           SET VALUE-PRESENT IN P500-STRING-FIELD-208-FLAGS
            TO TRUE
           MOVE '208'
             TO P500-STRING-FIELD-208
           SET VALUE-PRESENT IN P500-STRING-FIELD-209-FLAGS
            TO TRUE
           MOVE '209'
             TO P500-STRING-FIELD-209
           SET VALUE-PRESENT IN P500-STRING-FIELD-210-FLAGS
            TO TRUE
           MOVE '210'
             TO P500-STRING-FIELD-210
           SET VALUE-PRESENT IN P500-STRING-FIELD-211-FLAGS
            TO TRUE
           MOVE '211'
             TO P500-STRING-FIELD-211
           SET VALUE-PRESENT IN P500-STRING-FIELD-212-FLAGS
            TO TRUE
           MOVE '212'
             TO P500-STRING-FIELD-212
           SET VALUE-PRESENT IN P500-STRING-FIELD-213-FLAGS
            TO TRUE
           MOVE '213'
             TO P500-STRING-FIELD-213
           SET VALUE-PRESENT IN P500-STRING-FIELD-214-FLAGS
            TO TRUE
           MOVE '214'
             TO P500-STRING-FIELD-214
           SET VALUE-PRESENT IN P500-STRING-FIELD-215-FLAGS
            TO TRUE
           MOVE '215'
             TO P500-STRING-FIELD-215
           SET VALUE-PRESENT IN P500-STRING-FIELD-216-FLAGS
            TO TRUE
           MOVE '216'
             TO P500-STRING-FIELD-216
           SET VALUE-PRESENT IN P500-STRING-FIELD-217-FLAGS
            TO TRUE
           MOVE '217'
             TO P500-STRING-FIELD-217
           SET VALUE-PRESENT IN P500-STRING-FIELD-218-FLAGS
            TO TRUE
           MOVE '218'
             TO P500-STRING-FIELD-218
           SET VALUE-PRESENT IN P500-STRING-FIELD-219-FLAGS
            TO TRUE
           MOVE '219'
             TO P500-STRING-FIELD-219
           SET VALUE-PRESENT IN P500-STRING-FIELD-220-FLAGS
            TO TRUE
           MOVE '220'
             TO P500-STRING-FIELD-220
           SET VALUE-PRESENT IN P500-STRING-FIELD-221-FLAGS
            TO TRUE
           MOVE '221'
             TO P500-STRING-FIELD-221
           SET VALUE-PRESENT IN P500-STRING-FIELD-222-FLAGS
            TO TRUE
           MOVE '222'
             TO P500-STRING-FIELD-222
           SET VALUE-PRESENT IN P500-STRING-FIELD-223-FLAGS
            TO TRUE
           MOVE '223'
             TO P500-STRING-FIELD-223
           SET VALUE-PRESENT IN P500-STRING-FIELD-224-FLAGS
            TO TRUE
           MOVE '224'
             TO P500-STRING-FIELD-224
           SET VALUE-PRESENT IN P500-STRING-FIELD-225-FLAGS
            TO TRUE
           MOVE '225'
             TO P500-STRING-FIELD-225
           SET VALUE-PRESENT IN P500-STRING-FIELD-226-FLAGS
            TO TRUE
           MOVE '226'
             TO P500-STRING-FIELD-226
           SET VALUE-PRESENT IN P500-STRING-FIELD-227-FLAGS
            TO TRUE
           MOVE '227'
             TO P500-STRING-FIELD-227
           SET VALUE-PRESENT IN P500-STRING-FIELD-228-FLAGS
            TO TRUE
           MOVE '228'
             TO P500-STRING-FIELD-228
           SET VALUE-PRESENT IN P500-STRING-FIELD-229-FLAGS
            TO TRUE
           MOVE '229'
             TO P500-STRING-FIELD-229
           SET VALUE-PRESENT IN P500-STRING-FIELD-230-FLAGS
            TO TRUE
           MOVE '230'
             TO P500-STRING-FIELD-230
           SET VALUE-PRESENT IN P500-STRING-FIELD-231-FLAGS
            TO TRUE
           MOVE '231'
             TO P500-STRING-FIELD-231
           SET VALUE-PRESENT IN P500-STRING-FIELD-232-FLAGS
            TO TRUE
           MOVE '232'
             TO P500-STRING-FIELD-232
           SET VALUE-PRESENT IN P500-STRING-FIELD-233-FLAGS
            TO TRUE
           MOVE '233'
             TO P500-STRING-FIELD-233
           SET VALUE-PRESENT IN P500-STRING-FIELD-234-FLAGS
            TO TRUE
           MOVE '234'
             TO P500-STRING-FIELD-234
           SET VALUE-PRESENT IN P500-STRING-FIELD-235-FLAGS
            TO TRUE
           MOVE '235'
             TO P500-STRING-FIELD-235
           SET VALUE-PRESENT IN P500-STRING-FIELD-236-FLAGS
            TO TRUE
           MOVE '236'
             TO P500-STRING-FIELD-236
           SET VALUE-PRESENT IN P500-STRING-FIELD-237-FLAGS
            TO TRUE
           MOVE '237'
             TO P500-STRING-FIELD-237
           SET VALUE-PRESENT IN P500-STRING-FIELD-238-FLAGS
            TO TRUE
           MOVE '238'
             TO P500-STRING-FIELD-238
           SET VALUE-PRESENT IN P500-STRING-FIELD-239-FLAGS
            TO TRUE
           MOVE '239'
             TO P500-STRING-FIELD-239
           SET VALUE-PRESENT IN P500-STRING-FIELD-240-FLAGS
            TO TRUE
           MOVE '240'
             TO P500-STRING-FIELD-240
           SET VALUE-PRESENT IN P500-STRING-FIELD-241-FLAGS
            TO TRUE
           MOVE '241'
             TO P500-STRING-FIELD-241
           SET VALUE-PRESENT IN P500-STRING-FIELD-242-FLAGS
            TO TRUE
           MOVE '242'
             TO P500-STRING-FIELD-242
           SET VALUE-PRESENT IN P500-STRING-FIELD-243-FLAGS
            TO TRUE
           MOVE '243'
             TO P500-STRING-FIELD-243
           SET VALUE-PRESENT IN P500-STRING-FIELD-244-FLAGS
            TO TRUE
           MOVE '244'
             TO P500-STRING-FIELD-244
           SET VALUE-PRESENT IN P500-STRING-FIELD-245-FLAGS
            TO TRUE
           MOVE '245'
             TO P500-STRING-FIELD-245
           SET VALUE-PRESENT IN P500-STRING-FIELD-246-FLAGS
            TO TRUE
           MOVE '246'
             TO P500-STRING-FIELD-246
           SET VALUE-PRESENT IN P500-STRING-FIELD-247-FLAGS
            TO TRUE
           MOVE '247'
             TO P500-STRING-FIELD-247
           SET VALUE-PRESENT IN P500-STRING-FIELD-248-FLAGS
            TO TRUE
           MOVE '248'
             TO P500-STRING-FIELD-248
           SET VALUE-PRESENT IN P500-STRING-FIELD-249-FLAGS
            TO TRUE
           MOVE '249'
             TO P500-STRING-FIELD-249
           SET VALUE-PRESENT IN P500-STRING-FIELD-250-FLAGS
            TO TRUE
           MOVE '250'
             TO P500-STRING-FIELD-250
           SET VALUE-PRESENT IN P500-STRING-FIELD-251-FLAGS
            TO TRUE
           MOVE '251'
             TO P500-STRING-FIELD-251
           SET VALUE-PRESENT IN P500-STRING-FIELD-252-FLAGS
            TO TRUE
           MOVE '252'
             TO P500-STRING-FIELD-252
           SET VALUE-PRESENT IN P500-STRING-FIELD-253-FLAGS
            TO TRUE
           MOVE '253'
             TO P500-STRING-FIELD-253
           SET VALUE-PRESENT IN P500-STRING-FIELD-254-FLAGS
            TO TRUE
           MOVE '254'
             TO P500-STRING-FIELD-254
           SET VALUE-PRESENT IN P500-STRING-FIELD-255-FLAGS
            TO TRUE
           MOVE '255'
             TO P500-STRING-FIELD-255
           SET VALUE-PRESENT IN P500-STRING-FIELD-256-FLAGS
            TO TRUE
           MOVE '256'
             TO P500-STRING-FIELD-256
           SET VALUE-PRESENT IN P500-STRING-FIELD-257-FLAGS
            TO TRUE
           MOVE '257'
             TO P500-STRING-FIELD-257
           SET VALUE-PRESENT IN P500-STRING-FIELD-258-FLAGS
            TO TRUE
           MOVE '258'
             TO P500-STRING-FIELD-258
           SET VALUE-PRESENT IN P500-STRING-FIELD-259-FLAGS
            TO TRUE
           MOVE '259'
             TO P500-STRING-FIELD-259
           SET VALUE-PRESENT IN P500-STRING-FIELD-260-FLAGS
            TO TRUE
           MOVE '260'
             TO P500-STRING-FIELD-260
           SET VALUE-PRESENT IN P500-STRING-FIELD-261-FLAGS
            TO TRUE
           MOVE '261'
             TO P500-STRING-FIELD-261
           SET VALUE-PRESENT IN P500-STRING-FIELD-262-FLAGS
            TO TRUE
           MOVE '262'
             TO P500-STRING-FIELD-262
           SET VALUE-PRESENT IN P500-STRING-FIELD-263-FLAGS
            TO TRUE
           MOVE '263'
             TO P500-STRING-FIELD-263
           SET VALUE-PRESENT IN P500-STRING-FIELD-264-FLAGS
            TO TRUE
           MOVE '264'
             TO P500-STRING-FIELD-264
           SET VALUE-PRESENT IN P500-STRING-FIELD-265-FLAGS
            TO TRUE
           MOVE '265'
             TO P500-STRING-FIELD-265
           SET VALUE-PRESENT IN P500-STRING-FIELD-266-FLAGS
            TO TRUE
           MOVE '266'
             TO P500-STRING-FIELD-266
           SET VALUE-PRESENT IN P500-STRING-FIELD-267-FLAGS
            TO TRUE
           MOVE '267'
             TO P500-STRING-FIELD-267
           SET VALUE-PRESENT IN P500-STRING-FIELD-268-FLAGS
            TO TRUE
           MOVE '268'
             TO P500-STRING-FIELD-268
           SET VALUE-PRESENT IN P500-STRING-FIELD-269-FLAGS
            TO TRUE
           MOVE '269'
             TO P500-STRING-FIELD-269
           SET VALUE-PRESENT IN P500-STRING-FIELD-270-FLAGS
            TO TRUE
           MOVE '270'
             TO P500-STRING-FIELD-270
           SET VALUE-PRESENT IN P500-STRING-FIELD-271-FLAGS
            TO TRUE
           MOVE '271'
             TO P500-STRING-FIELD-271
           SET VALUE-PRESENT IN P500-STRING-FIELD-272-FLAGS
            TO TRUE
           MOVE '272'
             TO P500-STRING-FIELD-272
           SET VALUE-PRESENT IN P500-STRING-FIELD-273-FLAGS
            TO TRUE
           MOVE '273'
             TO P500-STRING-FIELD-273
           SET VALUE-PRESENT IN P500-STRING-FIELD-274-FLAGS
            TO TRUE
           MOVE '274'
             TO P500-STRING-FIELD-274
           SET VALUE-PRESENT IN P500-STRING-FIELD-275-FLAGS
            TO TRUE
           MOVE '275'
             TO P500-STRING-FIELD-275
           SET VALUE-PRESENT IN P500-STRING-FIELD-276-FLAGS
            TO TRUE
           MOVE '276'
             TO P500-STRING-FIELD-276
           SET VALUE-PRESENT IN P500-STRING-FIELD-277-FLAGS
            TO TRUE
           MOVE '277'
             TO P500-STRING-FIELD-277
           SET VALUE-PRESENT IN P500-STRING-FIELD-278-FLAGS
            TO TRUE
           MOVE '278'
             TO P500-STRING-FIELD-278
           SET VALUE-PRESENT IN P500-STRING-FIELD-279-FLAGS
            TO TRUE
           MOVE '279'
             TO P500-STRING-FIELD-279
           SET VALUE-PRESENT IN P500-STRING-FIELD-280-FLAGS
            TO TRUE
           MOVE '280'
             TO P500-STRING-FIELD-280
           SET VALUE-PRESENT IN P500-STRING-FIELD-281-FLAGS
            TO TRUE
           MOVE '281'
             TO P500-STRING-FIELD-281
           SET VALUE-PRESENT IN P500-STRING-FIELD-282-FLAGS
            TO TRUE
           MOVE '282'
             TO P500-STRING-FIELD-282
           SET VALUE-PRESENT IN P500-STRING-FIELD-283-FLAGS
            TO TRUE
           MOVE '283'
             TO P500-STRING-FIELD-283
           SET VALUE-PRESENT IN P500-STRING-FIELD-284-FLAGS
            TO TRUE
           MOVE '284'
             TO P500-STRING-FIELD-284
           SET VALUE-PRESENT IN P500-STRING-FIELD-285-FLAGS
            TO TRUE
           MOVE '285'
             TO P500-STRING-FIELD-285
           SET VALUE-PRESENT IN P500-STRING-FIELD-286-FLAGS
            TO TRUE
           MOVE '286'
             TO P500-STRING-FIELD-286
           SET VALUE-PRESENT IN P500-STRING-FIELD-287-FLAGS
            TO TRUE
           MOVE '287'
             TO P500-STRING-FIELD-287
           SET VALUE-PRESENT IN P500-STRING-FIELD-288-FLAGS
            TO TRUE
           MOVE '288'
             TO P500-STRING-FIELD-288
           SET VALUE-PRESENT IN P500-STRING-FIELD-289-FLAGS
            TO TRUE
           MOVE '289'
             TO P500-STRING-FIELD-289
           SET VALUE-PRESENT IN P500-STRING-FIELD-290-FLAGS
            TO TRUE
           MOVE '290'
             TO P500-STRING-FIELD-290
           SET VALUE-PRESENT IN P500-STRING-FIELD-291-FLAGS
            TO TRUE
           MOVE '291'
             TO P500-STRING-FIELD-291
           SET VALUE-PRESENT IN P500-STRING-FIELD-292-FLAGS
            TO TRUE
           MOVE '292'
             TO P500-STRING-FIELD-292
           SET VALUE-PRESENT IN P500-STRING-FIELD-293-FLAGS
            TO TRUE
           MOVE '293'
             TO P500-STRING-FIELD-293
           SET VALUE-PRESENT IN P500-STRING-FIELD-294-FLAGS
            TO TRUE
           MOVE '294'
             TO P500-STRING-FIELD-294
           SET VALUE-PRESENT IN P500-STRING-FIELD-295-FLAGS
            TO TRUE
           MOVE '295'
             TO P500-STRING-FIELD-295
           SET VALUE-PRESENT IN P500-STRING-FIELD-296-FLAGS
            TO TRUE
           MOVE '296'
             TO P500-STRING-FIELD-296
           SET VALUE-PRESENT IN P500-STRING-FIELD-297-FLAGS
            TO TRUE
           MOVE '297'
             TO P500-STRING-FIELD-297
           SET VALUE-PRESENT IN P500-STRING-FIELD-298-FLAGS
            TO TRUE
           MOVE '298'
             TO P500-STRING-FIELD-298
           SET VALUE-PRESENT IN P500-STRING-FIELD-299-FLAGS
            TO TRUE
           MOVE '299'
             TO P500-STRING-FIELD-299
           SET VALUE-PRESENT IN P500-STRING-FIELD-300-FLAGS
            TO TRUE
           MOVE '300'
             TO P500-STRING-FIELD-300
           SET VALUE-PRESENT IN P500-STRING-FIELD-301-FLAGS
            TO TRUE
           MOVE '301'
             TO P500-STRING-FIELD-301
           SET VALUE-PRESENT IN P500-STRING-FIELD-302-FLAGS
            TO TRUE
           MOVE '302'
             TO P500-STRING-FIELD-302
           SET VALUE-PRESENT IN P500-STRING-FIELD-303-FLAGS
            TO TRUE
           MOVE '303'
             TO P500-STRING-FIELD-303
           SET VALUE-PRESENT IN P500-STRING-FIELD-304-FLAGS
            TO TRUE
           MOVE '304'
             TO P500-STRING-FIELD-304
           SET VALUE-PRESENT IN P500-STRING-FIELD-305-FLAGS
            TO TRUE
           MOVE '305'
             TO P500-STRING-FIELD-305
           SET VALUE-PRESENT IN P500-STRING-FIELD-306-FLAGS
            TO TRUE
           MOVE '306'
             TO P500-STRING-FIELD-306
           SET VALUE-PRESENT IN P500-STRING-FIELD-307-FLAGS
            TO TRUE
           MOVE '307'
             TO P500-STRING-FIELD-307
           SET VALUE-PRESENT IN P500-STRING-FIELD-308-FLAGS
            TO TRUE
           MOVE '308'
             TO P500-STRING-FIELD-308
           SET VALUE-PRESENT IN P500-STRING-FIELD-309-FLAGS
            TO TRUE
           MOVE '309'
             TO P500-STRING-FIELD-309
           SET VALUE-PRESENT IN P500-STRING-FIELD-310-FLAGS
            TO TRUE
           MOVE '310'
             TO P500-STRING-FIELD-310
           SET VALUE-PRESENT IN P500-STRING-FIELD-311-FLAGS
            TO TRUE
           MOVE '311'
             TO P500-STRING-FIELD-311
           SET VALUE-PRESENT IN P500-STRING-FIELD-312-FLAGS
            TO TRUE
           MOVE '312'
             TO P500-STRING-FIELD-312
           SET VALUE-PRESENT IN P500-STRING-FIELD-313-FLAGS
            TO TRUE
           MOVE '313'
             TO P500-STRING-FIELD-313
           SET VALUE-PRESENT IN P500-STRING-FIELD-314-FLAGS
            TO TRUE
           MOVE '314'
             TO P500-STRING-FIELD-314
           SET VALUE-PRESENT IN P500-STRING-FIELD-315-FLAGS
            TO TRUE
           MOVE '315'
             TO P500-STRING-FIELD-315
           SET VALUE-PRESENT IN P500-STRING-FIELD-316-FLAGS
            TO TRUE
           MOVE '316'
             TO P500-STRING-FIELD-316
           SET VALUE-PRESENT IN P500-STRING-FIELD-317-FLAGS
            TO TRUE
           MOVE '317'
             TO P500-STRING-FIELD-317
           SET VALUE-PRESENT IN P500-STRING-FIELD-318-FLAGS
            TO TRUE
           MOVE '318'
             TO P500-STRING-FIELD-318
           SET VALUE-PRESENT IN P500-STRING-FIELD-319-FLAGS
            TO TRUE
           MOVE '319'
             TO P500-STRING-FIELD-319
           SET VALUE-PRESENT IN P500-STRING-FIELD-320-FLAGS
            TO TRUE
           MOVE '320'
             TO P500-STRING-FIELD-320
           SET VALUE-PRESENT IN P500-STRING-FIELD-321-FLAGS
            TO TRUE
           MOVE '321'
             TO P500-STRING-FIELD-321
           SET VALUE-PRESENT IN P500-STRING-FIELD-322-FLAGS
            TO TRUE
           MOVE '322'
             TO P500-STRING-FIELD-322
           SET VALUE-PRESENT IN P500-STRING-FIELD-323-FLAGS
            TO TRUE
           MOVE '323'
             TO P500-STRING-FIELD-323
           SET VALUE-PRESENT IN P500-STRING-FIELD-324-FLAGS
            TO TRUE
           MOVE '324'
             TO P500-STRING-FIELD-324
           SET VALUE-PRESENT IN P500-STRING-FIELD-325-FLAGS
            TO TRUE
           MOVE '325'
             TO P500-STRING-FIELD-325
           SET VALUE-PRESENT IN P500-STRING-FIELD-326-FLAGS
            TO TRUE
           MOVE '326'
             TO P500-STRING-FIELD-326
           SET VALUE-PRESENT IN P500-STRING-FIELD-327-FLAGS
            TO TRUE
           MOVE '327'
             TO P500-STRING-FIELD-327
           SET VALUE-PRESENT IN P500-STRING-FIELD-328-FLAGS
            TO TRUE
           MOVE '328'
             TO P500-STRING-FIELD-328
           SET VALUE-PRESENT IN P500-STRING-FIELD-329-FLAGS
            TO TRUE
           MOVE '329'
             TO P500-STRING-FIELD-329
           SET VALUE-PRESENT IN P500-STRING-FIELD-330-FLAGS
            TO TRUE
           MOVE '330'
             TO P500-STRING-FIELD-330
           SET VALUE-PRESENT IN P500-STRING-FIELD-331-FLAGS
            TO TRUE
           MOVE '331'
             TO P500-STRING-FIELD-331
           SET VALUE-PRESENT IN P500-STRING-FIELD-332-FLAGS
            TO TRUE
           MOVE '332'
             TO P500-STRING-FIELD-332
           SET VALUE-PRESENT IN P500-STRING-FIELD-333-FLAGS
            TO TRUE
           MOVE '333'
             TO P500-STRING-FIELD-333
           SET VALUE-PRESENT IN P500-STRING-FIELD-334-FLAGS
            TO TRUE
           MOVE '334'
             TO P500-STRING-FIELD-334
           SET VALUE-PRESENT IN P500-STRING-FIELD-335-FLAGS
            TO TRUE
           MOVE '335'
             TO P500-STRING-FIELD-335
           SET VALUE-PRESENT IN P500-STRING-FIELD-336-FLAGS
            TO TRUE
           MOVE '336'
             TO P500-STRING-FIELD-336
           SET VALUE-PRESENT IN P500-STRING-FIELD-337-FLAGS
            TO TRUE
           MOVE '337'
             TO P500-STRING-FIELD-337
           SET VALUE-PRESENT IN P500-STRING-FIELD-338-FLAGS
            TO TRUE
           MOVE '338'
             TO P500-STRING-FIELD-338
           SET VALUE-PRESENT IN P500-STRING-FIELD-339-FLAGS
            TO TRUE
           MOVE '339'
             TO P500-STRING-FIELD-339
           SET VALUE-PRESENT IN P500-STRING-FIELD-340-FLAGS
            TO TRUE
           MOVE '340'
             TO P500-STRING-FIELD-340
           SET VALUE-PRESENT IN P500-STRING-FIELD-341-FLAGS
            TO TRUE
           MOVE '341'
             TO P500-STRING-FIELD-341
           SET VALUE-PRESENT IN P500-STRING-FIELD-342-FLAGS
            TO TRUE
           MOVE '342'
             TO P500-STRING-FIELD-342
           SET VALUE-PRESENT IN P500-STRING-FIELD-343-FLAGS
            TO TRUE
           MOVE '343'
             TO P500-STRING-FIELD-343
           SET VALUE-PRESENT IN P500-STRING-FIELD-344-FLAGS
            TO TRUE
           MOVE '344'
             TO P500-STRING-FIELD-344
           SET VALUE-PRESENT IN P500-STRING-FIELD-345-FLAGS
            TO TRUE
           MOVE '345'
             TO P500-STRING-FIELD-345
           SET VALUE-PRESENT IN P500-STRING-FIELD-346-FLAGS
            TO TRUE
           MOVE '346'
             TO P500-STRING-FIELD-346
           SET VALUE-PRESENT IN P500-STRING-FIELD-347-FLAGS
            TO TRUE
           MOVE '347'
             TO P500-STRING-FIELD-347
           SET VALUE-PRESENT IN P500-STRING-FIELD-348-FLAGS
            TO TRUE
           MOVE '348'
             TO P500-STRING-FIELD-348
           SET VALUE-PRESENT IN P500-STRING-FIELD-349-FLAGS
            TO TRUE
           MOVE '349'
             TO P500-STRING-FIELD-349
           SET VALUE-PRESENT IN P500-STRING-FIELD-350-FLAGS
            TO TRUE
           MOVE '350'
             TO P500-STRING-FIELD-350
           SET VALUE-PRESENT IN P500-STRING-FIELD-351-FLAGS
            TO TRUE
           MOVE '351'
             TO P500-STRING-FIELD-351
           SET VALUE-PRESENT IN P500-STRING-FIELD-352-FLAGS
            TO TRUE
           MOVE '352'
             TO P500-STRING-FIELD-352
           SET VALUE-PRESENT IN P500-STRING-FIELD-353-FLAGS
            TO TRUE
           MOVE '353'
             TO P500-STRING-FIELD-353
           SET VALUE-PRESENT IN P500-STRING-FIELD-354-FLAGS
            TO TRUE
           MOVE '354'
             TO P500-STRING-FIELD-354
           SET VALUE-PRESENT IN P500-STRING-FIELD-355-FLAGS
            TO TRUE
           MOVE '355'
             TO P500-STRING-FIELD-355
           SET VALUE-PRESENT IN P500-STRING-FIELD-356-FLAGS
            TO TRUE
           MOVE '356'
             TO P500-STRING-FIELD-356
           SET VALUE-PRESENT IN P500-STRING-FIELD-357-FLAGS
            TO TRUE
           MOVE '357'
             TO P500-STRING-FIELD-357
           SET VALUE-PRESENT IN P500-STRING-FIELD-358-FLAGS
            TO TRUE
           MOVE '358'
             TO P500-STRING-FIELD-358
           SET VALUE-PRESENT IN P500-STRING-FIELD-359-FLAGS
            TO TRUE
           MOVE '359'
             TO P500-STRING-FIELD-359
           SET VALUE-PRESENT IN P500-STRING-FIELD-360-FLAGS
            TO TRUE
           MOVE '360'
             TO P500-STRING-FIELD-360
           SET VALUE-PRESENT IN P500-STRING-FIELD-361-FLAGS
            TO TRUE
           MOVE '361'
             TO P500-STRING-FIELD-361
           SET VALUE-PRESENT IN P500-STRING-FIELD-362-FLAGS
            TO TRUE
           MOVE '362'
             TO P500-STRING-FIELD-362
           SET VALUE-PRESENT IN P500-STRING-FIELD-363-FLAGS
            TO TRUE
           MOVE '363'
             TO P500-STRING-FIELD-363
           SET VALUE-PRESENT IN P500-STRING-FIELD-364-FLAGS
            TO TRUE
           MOVE '364'
             TO P500-STRING-FIELD-364
           SET VALUE-PRESENT IN P500-STRING-FIELD-365-FLAGS
            TO TRUE
           MOVE '365'
             TO P500-STRING-FIELD-365
           SET VALUE-PRESENT IN P500-STRING-FIELD-366-FLAGS
            TO TRUE
           MOVE '366'
             TO P500-STRING-FIELD-366
           SET VALUE-PRESENT IN P500-STRING-FIELD-367-FLAGS
            TO TRUE
           MOVE '367'
             TO P500-STRING-FIELD-367
           SET VALUE-PRESENT IN P500-STRING-FIELD-368-FLAGS
            TO TRUE
           MOVE '368'
             TO P500-STRING-FIELD-368
           SET VALUE-PRESENT IN P500-STRING-FIELD-369-FLAGS
            TO TRUE
           MOVE '369'
             TO P500-STRING-FIELD-369
           SET VALUE-PRESENT IN P500-STRING-FIELD-370-FLAGS
            TO TRUE
           MOVE '370'
             TO P500-STRING-FIELD-370
           SET VALUE-PRESENT IN P500-STRING-FIELD-371-FLAGS
            TO TRUE
           MOVE '371'
             TO P500-STRING-FIELD-371
           SET VALUE-PRESENT IN P500-STRING-FIELD-372-FLAGS
            TO TRUE
           MOVE '372'
             TO P500-STRING-FIELD-372
           SET VALUE-PRESENT IN P500-STRING-FIELD-373-FLAGS
            TO TRUE
           MOVE '373'
             TO P500-STRING-FIELD-373
           SET VALUE-PRESENT IN P500-STRING-FIELD-374-FLAGS
            TO TRUE
           MOVE '374'
             TO P500-STRING-FIELD-374
           SET VALUE-PRESENT IN P500-STRING-FIELD-375-FLAGS
            TO TRUE
           MOVE '375'
             TO P500-STRING-FIELD-375
           SET VALUE-PRESENT IN P500-STRING-FIELD-376-FLAGS
            TO TRUE
           MOVE '376'
             TO P500-STRING-FIELD-376
           SET VALUE-PRESENT IN P500-STRING-FIELD-377-FLAGS
            TO TRUE
           MOVE '377'
             TO P500-STRING-FIELD-377
           SET VALUE-PRESENT IN P500-STRING-FIELD-378-FLAGS
            TO TRUE
           MOVE '378'
             TO P500-STRING-FIELD-378
           SET VALUE-PRESENT IN P500-STRING-FIELD-379-FLAGS
            TO TRUE
           MOVE '379'
             TO P500-STRING-FIELD-379
           SET VALUE-PRESENT IN P500-STRING-FIELD-380-FLAGS
            TO TRUE
           MOVE '380'
             TO P500-STRING-FIELD-380
           SET VALUE-PRESENT IN P500-STRING-FIELD-381-FLAGS
            TO TRUE
           MOVE '381'
             TO P500-STRING-FIELD-381
           SET VALUE-PRESENT IN P500-STRING-FIELD-382-FLAGS
            TO TRUE
           MOVE '382'
             TO P500-STRING-FIELD-382
           SET VALUE-PRESENT IN P500-STRING-FIELD-383-FLAGS
            TO TRUE
           MOVE '383'
             TO P500-STRING-FIELD-383
           SET VALUE-PRESENT IN P500-STRING-FIELD-384-FLAGS
            TO TRUE
           MOVE '384'
             TO P500-STRING-FIELD-384
           SET VALUE-PRESENT IN P500-STRING-FIELD-385-FLAGS
            TO TRUE
           MOVE '385'
             TO P500-STRING-FIELD-385
           SET VALUE-PRESENT IN P500-STRING-FIELD-386-FLAGS
            TO TRUE
           MOVE '386'
             TO P500-STRING-FIELD-386
           SET VALUE-PRESENT IN P500-STRING-FIELD-387-FLAGS
            TO TRUE
           MOVE '387'
             TO P500-STRING-FIELD-387
           SET VALUE-PRESENT IN P500-STRING-FIELD-388-FLAGS
            TO TRUE
           MOVE '388'
             TO P500-STRING-FIELD-388
           SET VALUE-PRESENT IN P500-STRING-FIELD-389-FLAGS
            TO TRUE
           MOVE '389'
             TO P500-STRING-FIELD-389
           SET VALUE-PRESENT IN P500-STRING-FIELD-390-FLAGS
            TO TRUE
           MOVE '390'
             TO P500-STRING-FIELD-390
           SET VALUE-PRESENT IN P500-STRING-FIELD-391-FLAGS
            TO TRUE
           MOVE '391'
             TO P500-STRING-FIELD-391
           SET VALUE-PRESENT IN P500-STRING-FIELD-392-FLAGS
            TO TRUE
           MOVE '392'
             TO P500-STRING-FIELD-392
           SET VALUE-PRESENT IN P500-STRING-FIELD-393-FLAGS
            TO TRUE
           MOVE '393'
             TO P500-STRING-FIELD-393
           SET VALUE-PRESENT IN P500-STRING-FIELD-394-FLAGS
            TO TRUE
           MOVE '394'
             TO P500-STRING-FIELD-394
           SET VALUE-PRESENT IN P500-STRING-FIELD-395-FLAGS
            TO TRUE
           MOVE '395'
             TO P500-STRING-FIELD-395
           SET VALUE-PRESENT IN P500-STRING-FIELD-396-FLAGS
            TO TRUE
           MOVE '396'
             TO P500-STRING-FIELD-396
           SET VALUE-PRESENT IN P500-STRING-FIELD-397-FLAGS
            TO TRUE
           MOVE '397'
             TO P500-STRING-FIELD-397
           SET VALUE-PRESENT IN P500-STRING-FIELD-398-FLAGS
            TO TRUE
           MOVE '398'
             TO P500-STRING-FIELD-398
           SET VALUE-PRESENT IN P500-STRING-FIELD-399-FLAGS
            TO TRUE
           MOVE '399'
             TO P500-STRING-FIELD-399
           SET VALUE-PRESENT IN P500-STRING-FIELD-400-FLAGS
            TO TRUE
           MOVE '400'
             TO P500-STRING-FIELD-400
           SET VALUE-PRESENT IN P500-STRING-FIELD-401-FLAGS
            TO TRUE
           MOVE '401'
             TO P500-STRING-FIELD-401
           SET VALUE-PRESENT IN P500-STRING-FIELD-402-FLAGS
            TO TRUE
           MOVE '402'
             TO P500-STRING-FIELD-402
           SET VALUE-PRESENT IN P500-STRING-FIELD-403-FLAGS
            TO TRUE
           MOVE '403'
             TO P500-STRING-FIELD-403
           SET VALUE-PRESENT IN P500-STRING-FIELD-404-FLAGS
            TO TRUE
           MOVE '404'
             TO P500-STRING-FIELD-404
           SET VALUE-PRESENT IN P500-STRING-FIELD-405-FLAGS
            TO TRUE
           MOVE '405'
             TO P500-STRING-FIELD-405
           SET VALUE-PRESENT IN P500-STRING-FIELD-406-FLAGS
            TO TRUE
           MOVE '406'
             TO P500-STRING-FIELD-406
           SET VALUE-PRESENT IN P500-STRING-FIELD-407-FLAGS
            TO TRUE
           MOVE '407'
             TO P500-STRING-FIELD-407
           SET VALUE-PRESENT IN P500-STRING-FIELD-408-FLAGS
            TO TRUE
           MOVE '408'
             TO P500-STRING-FIELD-408
           SET VALUE-PRESENT IN P500-STRING-FIELD-409-FLAGS
            TO TRUE
           MOVE '409'
             TO P500-STRING-FIELD-409
           SET VALUE-PRESENT IN P500-STRING-FIELD-410-FLAGS
            TO TRUE
           MOVE '410'
             TO P500-STRING-FIELD-410
           SET VALUE-PRESENT IN P500-STRING-FIELD-411-FLAGS
            TO TRUE
           MOVE '411'
             TO P500-STRING-FIELD-411
           SET VALUE-PRESENT IN P500-STRING-FIELD-412-FLAGS
            TO TRUE
           MOVE '412'
             TO P500-STRING-FIELD-412
           SET VALUE-PRESENT IN P500-STRING-FIELD-413-FLAGS
            TO TRUE
           MOVE '413'
             TO P500-STRING-FIELD-413
           SET VALUE-PRESENT IN P500-STRING-FIELD-414-FLAGS
            TO TRUE
           MOVE '414'
             TO P500-STRING-FIELD-414
           SET VALUE-PRESENT IN P500-STRING-FIELD-415-FLAGS
            TO TRUE
           MOVE '415'
             TO P500-STRING-FIELD-415
           SET VALUE-PRESENT IN P500-STRING-FIELD-416-FLAGS
            TO TRUE
           MOVE '416'
             TO P500-STRING-FIELD-416
           SET VALUE-PRESENT IN P500-STRING-FIELD-417-FLAGS
            TO TRUE
           MOVE '417'
             TO P500-STRING-FIELD-417
           SET VALUE-PRESENT IN P500-STRING-FIELD-418-FLAGS
            TO TRUE
           MOVE '418'
             TO P500-STRING-FIELD-418
           SET VALUE-PRESENT IN P500-STRING-FIELD-419-FLAGS
            TO TRUE
           MOVE '419'
             TO P500-STRING-FIELD-419
           SET VALUE-PRESENT IN P500-STRING-FIELD-420-FLAGS
            TO TRUE
           MOVE '420'
             TO P500-STRING-FIELD-420
           SET VALUE-PRESENT IN P500-STRING-FIELD-421-FLAGS
            TO TRUE
           MOVE '421'
             TO P500-STRING-FIELD-421
           SET VALUE-PRESENT IN P500-STRING-FIELD-422-FLAGS
            TO TRUE
           MOVE '422'
             TO P500-STRING-FIELD-422
           SET VALUE-PRESENT IN P500-STRING-FIELD-423-FLAGS
            TO TRUE
           MOVE '423'
             TO P500-STRING-FIELD-423
           SET VALUE-PRESENT IN P500-STRING-FIELD-424-FLAGS
            TO TRUE
           MOVE '424'
             TO P500-STRING-FIELD-424
           SET VALUE-PRESENT IN P500-STRING-FIELD-425-FLAGS
            TO TRUE
           MOVE '425'
             TO P500-STRING-FIELD-425
           SET VALUE-PRESENT IN P500-STRING-FIELD-426-FLAGS
            TO TRUE
           MOVE '426'
             TO P500-STRING-FIELD-426
           SET VALUE-PRESENT IN P500-STRING-FIELD-427-FLAGS
            TO TRUE
           MOVE '427'
             TO P500-STRING-FIELD-427
           SET VALUE-PRESENT IN P500-STRING-FIELD-428-FLAGS
            TO TRUE
           MOVE '428'
             TO P500-STRING-FIELD-428
           SET VALUE-PRESENT IN P500-STRING-FIELD-429-FLAGS
            TO TRUE
           MOVE '429'
             TO P500-STRING-FIELD-429
           SET VALUE-PRESENT IN P500-STRING-FIELD-430-FLAGS
            TO TRUE
           MOVE '430'
             TO P500-STRING-FIELD-430
           SET VALUE-PRESENT IN P500-STRING-FIELD-431-FLAGS
            TO TRUE
           MOVE '431'
             TO P500-STRING-FIELD-431
           SET VALUE-PRESENT IN P500-STRING-FIELD-432-FLAGS
            TO TRUE
           MOVE '432'
             TO P500-STRING-FIELD-432
           SET VALUE-PRESENT IN P500-STRING-FIELD-433-FLAGS
            TO TRUE
           MOVE '433'
             TO P500-STRING-FIELD-433
           SET VALUE-PRESENT IN P500-STRING-FIELD-434-FLAGS
            TO TRUE
           MOVE '434'
             TO P500-STRING-FIELD-434
           SET VALUE-PRESENT IN P500-STRING-FIELD-435-FLAGS
            TO TRUE
           MOVE '435'
             TO P500-STRING-FIELD-435
           SET VALUE-PRESENT IN P500-STRING-FIELD-436-FLAGS
            TO TRUE
           MOVE '436'
             TO P500-STRING-FIELD-436
           SET VALUE-PRESENT IN P500-STRING-FIELD-437-FLAGS
            TO TRUE
           MOVE '437'
             TO P500-STRING-FIELD-437
           SET VALUE-PRESENT IN P500-STRING-FIELD-438-FLAGS
            TO TRUE
           MOVE '438'
             TO P500-STRING-FIELD-438
           SET VALUE-PRESENT IN P500-STRING-FIELD-439-FLAGS
            TO TRUE
           MOVE '439'
             TO P500-STRING-FIELD-439
           SET VALUE-PRESENT IN P500-STRING-FIELD-440-FLAGS
            TO TRUE
           MOVE '440'
             TO P500-STRING-FIELD-440
           SET VALUE-PRESENT IN P500-STRING-FIELD-441-FLAGS
            TO TRUE
           MOVE '441'
             TO P500-STRING-FIELD-441
           SET VALUE-PRESENT IN P500-STRING-FIELD-442-FLAGS
            TO TRUE
           MOVE '442'
             TO P500-STRING-FIELD-442
           SET VALUE-PRESENT IN P500-STRING-FIELD-443-FLAGS
            TO TRUE
           MOVE '443'
             TO P500-STRING-FIELD-443
           SET VALUE-PRESENT IN P500-STRING-FIELD-444-FLAGS
            TO TRUE
           MOVE '444'
             TO P500-STRING-FIELD-444
           SET VALUE-PRESENT IN P500-STRING-FIELD-445-FLAGS
            TO TRUE
           MOVE '445'
             TO P500-STRING-FIELD-445
           SET VALUE-PRESENT IN P500-STRING-FIELD-446-FLAGS
            TO TRUE
           MOVE '446'
             TO P500-STRING-FIELD-446
           SET VALUE-PRESENT IN P500-STRING-FIELD-447-FLAGS
            TO TRUE
           MOVE '447'
             TO P500-STRING-FIELD-447
           SET VALUE-PRESENT IN P500-STRING-FIELD-448-FLAGS
            TO TRUE
           MOVE '448'
             TO P500-STRING-FIELD-448
           SET VALUE-PRESENT IN P500-STRING-FIELD-449-FLAGS
            TO TRUE
           MOVE '449'
             TO P500-STRING-FIELD-449
           SET VALUE-PRESENT IN P500-STRING-FIELD-450-FLAGS
            TO TRUE
           MOVE '450'
             TO P500-STRING-FIELD-450
           SET VALUE-PRESENT IN P500-STRING-FIELD-451-FLAGS
            TO TRUE
           MOVE '451'
             TO P500-STRING-FIELD-451
           SET VALUE-PRESENT IN P500-STRING-FIELD-452-FLAGS
            TO TRUE
           MOVE '452'
             TO P500-STRING-FIELD-452
           SET VALUE-PRESENT IN P500-STRING-FIELD-453-FLAGS
            TO TRUE
           MOVE '453'
             TO P500-STRING-FIELD-453
           SET VALUE-PRESENT IN P500-STRING-FIELD-454-FLAGS
            TO TRUE
           MOVE '454'
             TO P500-STRING-FIELD-454
           SET VALUE-PRESENT IN P500-STRING-FIELD-455-FLAGS
            TO TRUE
           MOVE '455'
             TO P500-STRING-FIELD-455
           SET VALUE-PRESENT IN P500-STRING-FIELD-456-FLAGS
            TO TRUE
           MOVE '456'
             TO P500-STRING-FIELD-456
           SET VALUE-PRESENT IN P500-STRING-FIELD-457-FLAGS
            TO TRUE
           MOVE '457'
             TO P500-STRING-FIELD-457
           SET VALUE-PRESENT IN P500-STRING-FIELD-458-FLAGS
            TO TRUE
           MOVE '458'
             TO P500-STRING-FIELD-458
           SET VALUE-PRESENT IN P500-STRING-FIELD-459-FLAGS
            TO TRUE
           MOVE '459'
             TO P500-STRING-FIELD-459
           SET VALUE-PRESENT IN P500-STRING-FIELD-460-FLAGS
            TO TRUE
           MOVE '460'
             TO P500-STRING-FIELD-460
           SET VALUE-PRESENT IN P500-STRING-FIELD-461-FLAGS
            TO TRUE
           MOVE '461'
             TO P500-STRING-FIELD-461
           SET VALUE-PRESENT IN P500-STRING-FIELD-462-FLAGS
            TO TRUE
           MOVE '462'
             TO P500-STRING-FIELD-462
           SET VALUE-PRESENT IN P500-STRING-FIELD-463-FLAGS
            TO TRUE
           MOVE '463'
             TO P500-STRING-FIELD-463
           SET VALUE-PRESENT IN P500-STRING-FIELD-464-FLAGS
            TO TRUE
           MOVE '464'
             TO P500-STRING-FIELD-464
           SET VALUE-PRESENT IN P500-STRING-FIELD-465-FLAGS
            TO TRUE
           MOVE '465'
             TO P500-STRING-FIELD-465
           SET VALUE-PRESENT IN P500-STRING-FIELD-466-FLAGS
            TO TRUE
           MOVE '466'
             TO P500-STRING-FIELD-466
           SET VALUE-PRESENT IN P500-STRING-FIELD-467-FLAGS
            TO TRUE
           MOVE '467'
             TO P500-STRING-FIELD-467
           SET VALUE-PRESENT IN P500-STRING-FIELD-468-FLAGS
            TO TRUE
           MOVE '468'
             TO P500-STRING-FIELD-468
           SET VALUE-PRESENT IN P500-STRING-FIELD-469-FLAGS
            TO TRUE
           MOVE '469'
             TO P500-STRING-FIELD-469
           SET VALUE-PRESENT IN P500-STRING-FIELD-470-FLAGS
            TO TRUE
           MOVE '470'
             TO P500-STRING-FIELD-470
           SET VALUE-PRESENT IN P500-STRING-FIELD-471-FLAGS
            TO TRUE
           MOVE '471'
             TO P500-STRING-FIELD-471
           SET VALUE-PRESENT IN P500-STRING-FIELD-472-FLAGS
            TO TRUE
           MOVE '472'
             TO P500-STRING-FIELD-472
           SET VALUE-PRESENT IN P500-STRING-FIELD-473-FLAGS
            TO TRUE
           MOVE '473'
             TO P500-STRING-FIELD-473
           SET VALUE-PRESENT IN P500-STRING-FIELD-474-FLAGS
            TO TRUE
           MOVE '474'
             TO P500-STRING-FIELD-474
           SET VALUE-PRESENT IN P500-STRING-FIELD-475-FLAGS
            TO TRUE
           MOVE '475'
             TO P500-STRING-FIELD-475
           SET VALUE-PRESENT IN P500-STRING-FIELD-476-FLAGS
            TO TRUE
           MOVE '476'
             TO P500-STRING-FIELD-476
           SET VALUE-PRESENT IN P500-STRING-FIELD-477-FLAGS
            TO TRUE
           MOVE '477'
             TO P500-STRING-FIELD-477
           SET VALUE-PRESENT IN P500-STRING-FIELD-478-FLAGS
            TO TRUE
           MOVE '478'
             TO P500-STRING-FIELD-478
           SET VALUE-PRESENT IN P500-STRING-FIELD-479-FLAGS
            TO TRUE
           MOVE '479'
             TO P500-STRING-FIELD-479
           SET VALUE-PRESENT IN P500-STRING-FIELD-480-FLAGS
            TO TRUE
           MOVE '480'
             TO P500-STRING-FIELD-480
           SET VALUE-PRESENT IN P500-STRING-FIELD-481-FLAGS
            TO TRUE
           MOVE '481'
             TO P500-STRING-FIELD-481
           SET VALUE-PRESENT IN P500-STRING-FIELD-482-FLAGS
            TO TRUE
           MOVE '482'
             TO P500-STRING-FIELD-482
           SET VALUE-PRESENT IN P500-STRING-FIELD-483-FLAGS
            TO TRUE
           MOVE '483'
             TO P500-STRING-FIELD-483
           SET VALUE-PRESENT IN P500-STRING-FIELD-484-FLAGS
            TO TRUE
           MOVE '484'
             TO P500-STRING-FIELD-484
           SET VALUE-PRESENT IN P500-STRING-FIELD-485-FLAGS
            TO TRUE
           MOVE '485'
             TO P500-STRING-FIELD-485
           SET VALUE-PRESENT IN P500-STRING-FIELD-486-FLAGS
            TO TRUE
           MOVE '486'
             TO P500-STRING-FIELD-486
           SET VALUE-PRESENT IN P500-STRING-FIELD-487-FLAGS
            TO TRUE
           MOVE '487'
             TO P500-STRING-FIELD-487
           SET VALUE-PRESENT IN P500-STRING-FIELD-488-FLAGS
            TO TRUE
           MOVE '488'
             TO P500-STRING-FIELD-488
           SET VALUE-PRESENT IN P500-STRING-FIELD-489-FLAGS
            TO TRUE
           MOVE '489'
             TO P500-STRING-FIELD-489
           SET VALUE-PRESENT IN P500-STRING-FIELD-490-FLAGS
            TO TRUE
           MOVE '490'
             TO P500-STRING-FIELD-490
           SET VALUE-PRESENT IN P500-STRING-FIELD-491-FLAGS
            TO TRUE
           MOVE '491'
             TO P500-STRING-FIELD-491
           SET VALUE-PRESENT IN P500-STRING-FIELD-492-FLAGS
            TO TRUE
           MOVE '492'
             TO P500-STRING-FIELD-492
           SET VALUE-PRESENT IN P500-STRING-FIELD-493-FLAGS
            TO TRUE
           MOVE '493'
             TO P500-STRING-FIELD-493
           SET VALUE-PRESENT IN P500-STRING-FIELD-494-FLAGS
            TO TRUE
           MOVE '494'
             TO P500-STRING-FIELD-494
           SET VALUE-PRESENT IN P500-STRING-FIELD-495-FLAGS
            TO TRUE
           MOVE '495'
             TO P500-STRING-FIELD-495
           SET VALUE-PRESENT IN P500-STRING-FIELD-496-FLAGS
            TO TRUE
           MOVE '496'
             TO P500-STRING-FIELD-496
           SET VALUE-PRESENT IN P500-STRING-FIELD-497-FLAGS
            TO TRUE
           MOVE '497'
             TO P500-STRING-FIELD-497
           SET VALUE-PRESENT IN P500-STRING-FIELD-498-FLAGS
            TO TRUE
           MOVE '498'
             TO P500-STRING-FIELD-498
           SET VALUE-PRESENT IN P500-STRING-FIELD-499-FLAGS
            TO TRUE
           MOVE '499'
             TO P500-STRING-FIELD-499
           SET VALUE-PRESENT IN P500-STRING-FIELD-500-FLAGS
            TO TRUE
           MOVE '500'
             TO P500-STRING-FIELD-500

           EXIT.

       CONVERT-STRUCTURE-SIZE-500 SECTION.
           MOVE 5 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE
           
           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-STRUCT-500
             BY REFERENCE CONSUMER-STRUCT-500                    
       
           EXIT.
           
      * ---
      * Print provider data structure for test purposes
       PRINT-PROVIDER-DATA SECTION.
           DISPLAY "Mapped first name '"
                   CSPI-FIRST-NAME
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped last name '"
                   CSPI-LAST-NAME
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped date of birth '"
                   CSPI-DATE-OF-BIRTH
                   "'" UPON CONSOLE
                                      
           DISPLAY "Mapped gender "
                   CSPI-GENDER
                   UPON CONSOLE
                   
           DISPLAY "Mapped street '"
                   CSPI-STREET IN CSPI-PRIMARY-ADDRESS
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped number "
                   CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS
                   UPON CONSOLE
                   
           DISPLAY "Mapped postal code "
                   CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS
                   UPON CONSOLE
                   
           DISPLAY "Mapped city '"
                   CSPI-CITY IN CSPI-PRIMARY-ADDRESS
                   "'" UPON CONSOLE

           EVALUATE TRUE
           WHEN CSPI-STREET-ADDRESS IN CSPI-PRIMARY-ADDRESS-NEW
               DISPLAY "Mapped street (new) '"
                       CSPI-STREET IN CSPI-PRIMARY-ADDRESS-NEW
                       "'" UPON CONSOLE
                   
               DISPLAY "Mapped number (new) "
                       CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS-NEW
                       UPON CONSOLE
                   
               DISPLAY "Mapped postal code (new) "
                       CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS-NEW
                       UPON CONSOLE
                   
               DISPLAY "Mapped city (new) '"
                       CSPI-CITY IN CSPI-PRIMARY-ADDRESS-NEW
                       "'" UPON CONSOLE
                    
           WHEN CSPI-PO-BOX-ADDRESS IN CSPI-PRIMARY-ADDRESS-NEW
           
               DISPLAY "Mapped PO box number (new) "
                       CSPI-BOX-NO IN CSPI-PRIMARY-ADDRESS-NEW
                       UPON CONSOLE
                   
               DISPLAY "Mapped postal code (new) "
                       CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS-NEW
                       UPON CONSOLE
                   
               DISPLAY "Mapped city (new) '"
                       CSPI-CITY IN CSPI-PRIMARY-ADDRESS-NEW
                       "'" UPON CONSOLE           
                       
           WHEN OTHER
               CONTINUE
               
           END-EVALUATE
           
           DISPLAY "Secondary address count "
                   CSPI-SEC-ADDRESS-COUNT 
                   IN CSPI-SECONDARY-ADDRESSES
                   UPON CONSOLE
                   
           PERFORM VARYING I-1 FROM 1 BY 1 UNTIL 
                   I-1 > CSPI-SEC-ADDRESS-COUNT
                   IN CSPI-SECONDARY-ADDRESSES
                   
               DISPLAY "Secondary address " I-1
                       UPON CONSOLE

               DISPLAY "Mapped street '"
                       CSPI-STREET IN CSPI-SECONDARY-ADDRESS(I-1)
                       "'" UPON CONSOLE
                   
               DISPLAY "Mapped number "
                       CSPI-NUMBER IN CSPI-SECONDARY-ADDRESS(I-1)
                       UPON CONSOLE
                   
               DISPLAY "Mapped postal code "
                       CSPI-POSTAL-CODE IN CSPI-SECONDARY-ADDRESS(I-1)
                       UPON CONSOLE
                   
                DISPLAY "Mapped city '"
                       CSPI-CITY IN CSPI-SECONDARY-ADDRESS(I-1)
                       "'" UPON CONSOLE                   
           END-PERFORM
           
           DISPLAY "Secondary address count (new) "
                   CSPI-SEC-ADDR-NEW-COUNT 
                   IN CSPI-SECONDARY-ADDRESSES-NEW
                   UPON CONSOLE

           PERFORM VARYING I-1 FROM 1 BY 1 UNTIL 
                   I-1 > CSPI-SEC-ADDR-NEW-COUNT
                   IN CSPI-SECONDARY-ADDRESSES-NEW

               EVALUATE TRUE
               WHEN CSPI-STREET-ADDRESS 
                    IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                    
                   DISPLAY "Mapped street (new) '"
                           CSPI-STREET 
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           "'" UPON CONSOLE
                   
                   DISPLAY "Mapped number (new) "
                           CSPI-NUMBER 
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           UPON CONSOLE
                   
                   DISPLAY "Mapped postal code (new) "
                           CSPI-POSTAL-CODE
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           UPON CONSOLE
                   
                   DISPLAY "Mapped city (new) '"
                           CSPI-CITY
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           "'" UPON CONSOLE
                    
               WHEN CSPI-PO-BOX-ADDRESS 
                    IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
           
                   DISPLAY "Mapped PO box number (new) "
                           CSPI-BOX-NO
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           UPON CONSOLE
                   
                   DISPLAY "Mapped postal code (new) "
                           CSPI-POSTAL-CODE
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           UPON CONSOLE
                   
                   DISPLAY "Mapped city (new) '"
                           CSPI-CITY
                           IN CSPI-SECONDARY-ADDRESS-NEW(I-1)
                           "'" UPON CONSOLE           
                       
               WHEN OTHER
                   CONTINUE
               
               END-EVALUATE
           END-PERFORM

           EXIT.

      * ---
      * Unload loaded conversion scripts (any benchmark)
       UNLOAD-SCRIPTS SECTION.
           CALL 'unloadScripts'
           
           EXIT.
           
