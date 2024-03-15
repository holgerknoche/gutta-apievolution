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
       
       PROCEDURE DIVISION.
           
           
      *    Initial call to load the modules and make the other functions
      *    available
           CALL 'timer'
           CALL 'apimapper'      
           
           PERFORM RUN-BENCHMARK-V1
           PERFORM RUN-BENCHMARK-V3
           PERFORM RUN-BENCHMARK-V6
                                             
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
           
