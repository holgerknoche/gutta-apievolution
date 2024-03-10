123456*Conversion invocation from COBOL

       IDENTIFICATION DIVISION.
       PROGRAM-ID. APICONV.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TIMINGS.
           05 START-TIME PIC 9(8).
           05 END-TIME PIC 9(8).
           05 DURATION PIC 9(8).

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
           
      * Structures for Customer, provider view
       01 CUSTOMER-PROVIDER-IN.
           COPY CUSTOMRP REPLACING '*-' BY CSPI-.
           
       01 CUSTOMER-PROVIDER-OUT.
           COPY CUSTOMRP REPLACING '*-' BY CSPO-.
       
       PROCEDURE DIVISION.
           
           
      *    Initial call to load the module and make the other functions
      *    available
           CALL 'apimapper'      
           
           PERFORM RUN-BENCHMARK-V3
                                  
           DISPLAY "Mapped first name '"
                   CSPI-FIRST-NAME
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped last name '"
                   CSPI-LAST-NAME
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

           GOBACK.
      
      * ---
      * Run invocation benchmark v1
       RUN-BENCHMARK-V1 SECTION.
           PERFORM LOAD-SCRIPTS-V1
           PERFORM INIT-INPUT-DATA-V1
           
           ACCEPT START-TIME FROM TIME
           
           PERFORM PERFORM-CONVERSION-V1 1 TIMES
           
           ACCEPT END-TIME FROM TIME
           COMPUTE DURATION = (END-TIME - START-TIME)
           DISPLAY 'Benchmark v1: ' DURATION 'cs' UPON CONSOLE
           
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
           
           EXIT.
           
      * ---
      * Run invocation benchmark v3
       RUN-BENCHMARK-V3 SECTION.
           PERFORM LOAD-SCRIPTS-V3
           PERFORM INIT-INPUT-DATA-V3
           
           ACCEPT START-TIME FROM TIME
           
           PERFORM PERFORM-CONVERSION-V3 1 TIMES
           
           ACCEPT END-TIME FROM TIME
           COMPUTE DURATION = (END-TIME - START-TIME)
           DISPLAY 'Benchmark v3: ' DURATION 'cs' UPON CONSOLE
           
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
           
           EXIT.
           
      * ---
      * Unload loaded conversion scripts (any benchmark)
       UNLOAD-SCRIPTS SECTION.
           CALL 'unloadScripts'         
           
           EXIT.
           
