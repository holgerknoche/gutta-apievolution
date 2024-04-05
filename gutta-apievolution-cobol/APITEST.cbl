123456*Conversion tests in COBOL

       IDENTIFICATION DIVISION.
       PROGRAM-ID. APITEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 INDEXES.
           05 I-1 PIC S9(9) BINARY.
       
       01 FLAGS.
           05 TEST-SUCCESS-FLAG PIC X.
               88 TEST-SUCCESSFUL VALUE 'Y'.
               88 TEST-FAILED VALUE 'N'.
       
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
       
       01 CONSUMER-PARAMETER.
           COPY CPARAM REPLACING '*-' BY CSP-.
           
       01 CONSUMER-RESULT.
           COPY CRESULT REPLACING '*-' BY CSR-.
           
       01 PROVIDER-PARAMETER.
           COPY PPARAM REPLACING '*-' BY PRP-.
           
       01 PROVIDER-RESULT.
           COPY PRESULT REPLACING '*-' BY PRR-.
           
       01 CONS-MONO-TO-POLY.
           05 CMP-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.
             
           05 CMP-FIELD-1-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.
           05 CMP-FIELD-1 PIC S9(9) BINARY.
           
       01 PROV-MONO-TO-POLY.
           05 PVP-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.

           05 PVP-TYPE-DISC PIC S9(9) BINARY.
             88 SUPER-TYPE VALUE 4.
             88 SUB-TYPE-A VALUE 5.
             88 SUB-TYPE-B VALUE 6.
             
           05 PVP-DATA PIC X(16).
           
           05 PVP-SUPER-TYPE REDEFINES PVP-DATA.
             10 PVP-FIELD-1-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 PVP-FIELD-1 PIC S9(9) BINARY.       

      * Structures for Customer, version 1
       01 CUSTOMER-V1-IN.
           COPY CUSTOMR1 REPLACING '*-' BY CS1I-.

       01 CUSTOMER-V3-IN.
           COPY CUSTOMR3 REPLACING '*-' BY CS3I-.

       01 CUSTOMER-V6-IN.
           COPY CUSTOMR6 REPLACING '*-' BY CS6I-.
           
       01 CUSTOMER-PROVIDER-IN.
           COPY CUSTOMRP REPLACING '*-' BY CSPI-.

       PROCEDURE DIVISION.
       
      *    Initial call to load the modules and make the other functions
      *    available
           CALL 'timer'
           CALL 'apimapper'
        
           PERFORM LOAD-TEST-SCRIPT     
           PERFORM PERFORM-TESTS
           PERFORM UNLOAD-TEST-SCRIPT
           
           PERFORM CUSTOMER-TEST-V1
           PERFORM CUSTOMER-TEST-V3
           PERFORM CUSTOMER-TEST-V6
           
           GOBACK.
           
       LOAD-TEST-SCRIPT SECTION.
           MOVE 'test-consumer-script.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'test-provider-script.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
       
           EXIT
           .

       UNLOAD-TEST-SCRIPT SECTION.
           CALL 'unloadScripts'
       
           EXIT
           .
 
       PERFORM-TESTS SECTION.
           PERFORM PERFORM-BASIC-TEST
           PERFORM PERFORM-MONO-TO-POLY-TEST
           PERFORM PERFORM-POLY-TO-MONO-TEST
       
           EXIT
           .
 
      * ---
      * Basic parameter and result conversion
       PERFORM-BASIC-TEST SECTION.
           SET TEST-SUCCESSFUL TO TRUE

           DISPLAY 'Running basic test (C2P)...'
             UPON CONSOLE             
           PERFORM PREP-CONS-PARM-BASIC
           PERFORM CONV-PARM-BASIC
           PERFORM CHECK-PARM-BASIC
           
           PERFORM PRINT-TEST-STATUS
           
           SET TEST-SUCCESSFUL TO TRUE

           DISPLAY 'Running basic test (P2C)...'
             UPON CONSOLE
           
           PERFORM PREP-PROV-RESULT-BASIC
           PERFORM CONV-RESULT-BASIC
           PERFORM CHECK-RESULT-BASIC
           
           PERFORM PRINT-TEST-STATUS           
           EXIT
           .
           
       PREP-CONS-PARM-BASIC SECTION.
           SET VALUE-PRESENT IN CSP-FLAGS
            TO TRUE
            
           SET VALUE-PRESENT IN CSP-FIELD-A-FLAGS
            TO TRUE
           MOVE 'Test value'
            TO CSP-FIELD-A
            
           SET VALUE-PRESENT IN CSP-TEST-ENUM-FLAGS
            TO TRUE
           SET VALUE-B IN CSP-TEST-ENUM
            TO TRUE
            
           SET VALUE-PRESENT IN CSP-TEST-LIST-FLAGS
            TO TRUE
           MOVE 2
             TO CSP-TEST-LIST-COUNT
             
           PERFORM VARYING I-1 FROM 1 BY 1
                   UNTIL I-1 = 2
                   
               SET VALUE-PRESENT IN CSP-TEST-LIST-ENTRY-FLAGS(I-1)
                TO TRUE
               SET VALUE-A IN CSP-TEST-LIST-ENUM(I-1)
                TO TRUE                   
           END-PERFORM
            
           EXIT
           .
           
       CONV-PARM-BASIC SECTION.
           MOVE 6 TO OPERATION-INDEX
           SET CONSUMER-TO-PROVIDER TO TRUE
           SET PARAMETER-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CONSUMER-PARAMETER
             BY REFERENCE PROVIDER-PARAMETER
             
           EXIT
           .
           
       CHECK-PARM-BASIC SECTION.
           IF NOT VALUE-PRESENT IN PRP-FLAGS
               DISPLAY 'Provider parameter value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN PRP-TEST-FIELD-FLAGS
               DISPLAY 'Missing value for test-field'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF PRP-TEST-FIELD NOT = 'Test value'
               DISPLAY 'Unexpected value "'
                       PRP-TEST-FIELD
                       '" for test-field'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN PRP-TEST-ENUM-FLAGS
               DISPLAY 'Missing value for test-enum'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF NOT VALUE-2 IN PRP-TEST-ENUM
               DISPLAY 'Unexpected value '
                       PRP-TEST-ENUM
                       ' for test-enum'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN PRP-TEST-LIST-FLAGS
               DISPLAY 'Missing value for test-list'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF PRP-TEST-LIST-COUNT NOT = 2
               DISPLAY 'Unexpected value '
                       PRP-TEST-LIST-COUNT
                       ' for size of test-list'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           PERFORM VARYING I-1 FROM 1 BY 1
                   UNTIL I-1 = 2
                 
                 IF NOT VALUE-PRESENT 
                        IN PRP-TEST-LIST-ENTRY-FLAGS(I-1)
                     DISPLAY 'Missing value for element'
                             I-1
                             ' of test-list'
                     SET TEST-FAILED
                      TO TRUE
                 END-IF
                 IF NOT VALUE-1 IN PRP-TEST-LIST-ENUM(I-1)
                     DISPLAY 'Unexpected value'
                             PRP-TEST-LIST-ENUM(I-1)
                             ' for element '
                             I-1
                             ' of test-list'
                     SET TEST-FAILED
                      TO TRUE
                 END-IF                 
           END-PERFORM
           
           EXIT
           .

       PREP-PROV-RESULT-BASIC SECTION.
           SET VALUE-PRESENT IN PRR-FLAGS
            TO TRUE
            
           SET VALUE-PRESENT IN PRR-RET-FIELD-FLAGS
            TO TRUE
           MOVE 'Test value'
            TO PRR-RET-FIELD
            
           SET VALUE-PRESENT IN PRR-RESULT-ENUM-FLAGS
            TO TRUE
           SET VALUE-2 IN PRR-RESULT-ENUM
            TO TRUE
            
           SET VALUE-PRESENT IN PRR-RESULT-LIST-FLAGS
            TO TRUE
           MOVE 3
             TO PRR-RESULT-LIST-COUNT
             
           PERFORM VARYING I-1 FROM 1 BY 1
                   UNTIL I-1 = 3
                   
               SET VALUE-PRESENT IN PRR-RESULT-LIST-ENTRY-FLAGS(I-1)
                TO TRUE
               SET VALUE-1 IN PRR-RESULT-LIST-ENUM(I-1)
                TO TRUE                   
           END-PERFORM

           EXIT
           .
           
       CONV-RESULT-BASIC SECTION.
           MOVE 6 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROVIDER-RESULT
             BY REFERENCE CONSUMER-RESULT

           EXIT
           .
           
       CHECK-RESULT-BASIC SECTION.
           IF NOT VALUE-PRESENT IN CSR-FLAGS
               DISPLAY 'Consumer result value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
          
          IF NOT VALUE-PRESENT IN CSR-RESULT-ENUM-FLAGS
               DISPLAY 'Missing value for result-enum'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF NOT VALUE-B IN CSR-RESULT-ENUM
               DISPLAY 'Unexpected value '
                       CSR-RESULT-ENUM
                       ' for result-enum'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSR-RESULT-FIELD-FLAGS
               DISPLAY 'Missing value for result-field'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSR-RESULT-FIELD NOT = 'Test value'
               DISPLAY 'Unexpected value "'
                       CSR-RESULT-FIELD
                       '" for result-field'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
                      
           IF NOT VALUE-PRESENT IN CSR-RESULT-LIST-FLAGS
               DISPLAY 'Missing value for result-list'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSR-RESULT-LIST-COUNT NOT = 3
               DISPLAY 'Unexpected value '
                       CSR-RESULT-LIST-COUNT
                       ' for size of result-list'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           PERFORM VARYING I-1 FROM 1 BY 1
                   UNTIL I-1 = 3
                 
                 IF NOT VALUE-PRESENT 
                        IN CSR-RESULT-LIST-ENTRY-FLAGS(I-1)
                     DISPLAY 'Missing value for element'
                             I-1
                             ' of result-list'
                     SET TEST-FAILED
                      TO TRUE
                 END-IF
                 IF NOT VALUE-A IN CSR-RESULT-LIST-ENUM(I-1)
                     DISPLAY 'Unexpected value'
                             CSR-RESULT-LIST-ENUM(I-1)
                             ' for element '
                             I-1
                             ' of result-list'
                     SET TEST-FAILED
                      TO TRUE
                 END-IF                 
           END-PERFORM

           EXIT
           .

      * ---
      * Mono-to-poly conversion test
       PERFORM-MONO-TO-POLY-TEST SECTION.
           SET TEST-SUCCESSFUL TO TRUE

           DISPLAY 'Running mono-to-poly mapping test...'
             UPON CONSOLE             
           PERFORM PREP-MONO-TO-POLY-PARM
           PERFORM CONV-MONO-TO-POLY
           PERFORM CHECK-MONO-TO-POLY
           
           PERFORM PRINT-TEST-STATUS
           
           EXIT
           .

       PREP-MONO-TO-POLY-PARM SECTION.
           SET VALUE-PRESENT IN CMP-FLAGS
            TO TRUE
            
           SET VALUE-PRESENT IN CMP-FIELD-1-FLAGS
            TO TRUE
           MOVE 17
             TO CMP-FIELD-1
           .
           
       CONV-MONO-TO-POLY SECTION.
           MOVE 1 TO OPERATION-INDEX
           SET CONSUMER-TO-PROVIDER TO TRUE
           SET PARAMETER-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE CONS-MONO-TO-POLY
             BY REFERENCE PROV-MONO-TO-POLY

           EXIT
           .
           
       CHECK-MONO-TO-POLY SECTION.
           IF NOT VALUE-PRESENT IN PVP-FLAGS
               DISPLAY 'Provider value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT SUPER-TYPE IN PVP-TYPE-DISC
               DISPLAY 'Unexpected type discriminator '
                       PVP-TYPE-DISC
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN PVP-FIELD-1-FLAGS
               DISPLAY 'Missing value for field-1'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF PVP-FIELD-1 NOT = 17
               DISPLAY 'Unexpected value "'
                       PVP-FIELD-1
                       '" for field-1'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           EXIT
           .
          
      * ---
      * Poly-to-mono conversion test
       PERFORM-POLY-TO-MONO-TEST SECTION.
           SET TEST-SUCCESSFUL TO TRUE

           DISPLAY 'Running poly-to-mono mapping test...'
             UPON CONSOLE             
           PERFORM PREP-POLY-TO-MONO-PARM
           PERFORM CONV-POLY-TO-MONO
           PERFORM CHECK-POLY-TO-MONO
           
           PERFORM PRINT-TEST-STATUS
           
           EXIT
           .

       PREP-POLY-TO-MONO-PARM SECTION.
           SET VALUE-PRESENT IN PVP-FLAGS
            TO TRUE
           SET SUPER-TYPE IN PVP-TYPE-DISC
            TO TRUE
           
           SET VALUE-PRESENT IN PVP-FIELD-1-FLAGS
            TO TRUE
           MOVE 25
             TO PVP-FIELD-1
           .
           
       CONV-POLY-TO-MONO SECTION.
           MOVE 1 TO OPERATION-INDEX
           SET PROVIDER-TO-CONSUMER TO TRUE
           SET RESULT-MAPPING TO TRUE

           CALL 'convertData' USING
             BY VALUE OPERATION-INDEX
             BY VALUE MAPPING-DIRECTION
             BY VALUE MAPPING-TYPE
             BY REFERENCE PROV-MONO-TO-POLY
             BY REFERENCE CONS-MONO-TO-POLY

           EXIT
           .
           
       CHECK-POLY-TO-MONO SECTION.
           IF NOT VALUE-PRESENT IN CMP-FLAGS
               DISPLAY 'Consumer value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CMP-FIELD-1-FLAGS
               DISPLAY 'Missing value for field-1'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CMP-FIELD-1 NOT = 25
               DISPLAY 'Unexpected value "'
                       PVP-FIELD-1
                       '" for field-1'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           EXIT
           .

      *---
      * Customer test v1
       CUSTOMER-TEST-V1 SECTION.
           DISPLAY 'Running Customer test v1...'
              UPON CONSOLE
           SET TEST-SUCCESSFUL
            TO TRUE
       
           PERFORM LOAD-SCRIPTS-V1
           
           PERFORM INIT-INPUT-DATA-V1
           PERFORM PERFORM-CONVERSION-V1
           PERFORM CHECK-CONVERSION-V1
           PERFORM PRINT-TEST-STATUS
           
           PERFORM UNLOAD-TEST-SCRIPT
           EXIT
           .

       LOAD-SCRIPTS-V1 SECTION.
           MOVE 'consumer-script-v1.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v1.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
           
           EXIT
           .

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
             
           EXIT
           .
           
       CHECK-CONVERSION-V1 SECTION.
           IF NOT VALUE-PRESENT IN CSPI-CUSTOMER-FLAGS
               DISPLAY 'Provider value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-FIRST-NAME-FLAGS
               DISPLAY 'Missing value for first-name'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-FIRST-NAME NOT = 'Test'
               DISPLAY 'Unexpected value "'
                       CSPI-FIRST-NAME
                       '" for first-name'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-LAST-NAME-FLAGS
               DISPLAY 'Missing value for last-name'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-LAST-NAME NOT = 'Tester'
               DISPLAY 'Unexpected value "'
                       CSPI-LAST-NAME
                       '" for last-name'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-ABSENT IN CSPI-DATE-OF-BIRTH-FLAGS
               DISPLAY 'Unexpected value for date-of-birth'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-ABSENT IN CSPI-GENDER-NEW-FLAGS
               DISPLAY 'Unexpected value for gender-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-ABSENT IN CSPI-PRIMARY-ADDRESS-NEW-FLAGS
               DISPLAY 'Unexpected value for primary-address-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
                      
           IF NOT VALUE-ABSENT IN CSPI-SEC-ADDR-NEW-LST-FLAGS
               DISPLAY 'Unexpected value for secondary-addresses-new'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE           
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-PRIMARY-ADDRESS-FLAGS
               DISPLAY 'Missing value for primary-address'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE           
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-POSTAL-CODE-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for postal-code '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS
              NOT = 12345
              
               DISPLAY 'Unexpected value '
                       CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS
                       ' for postal-code in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-CITY-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for city '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-CITY IN CSPI-PRIMARY-ADDRESS
              NOT = 'Test City'
              
               DISPLAY 'Unexpected value "'
                       CSPI-CITY IN CSPI-PRIMARY-ADDRESS
                       '" for city in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-STREET-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for street '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-STREET IN CSPI-PRIMARY-ADDRESS
              NOT = 'Test Street'
              
               DISPLAY 'Unexpected value "'
                       CSPI-STREET IN CSPI-PRIMARY-ADDRESS
                       '" for street in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-NUMBER-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for number '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS
              NOT = 17
              
               DISPLAY 'Unexpected value '
                       CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS
                       ' for number in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-ABSENT IN CSPI-SEC-ADDR-LST-FLAGS
               DISPLAY 'Unexpected value for secondary-addresses'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-GENDER-FLAGS
               DISPLAY 'Missing value for gender'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-GENDER NOT = 1
               DISPLAY 'Unexpected value '
                       CSPI-GENDER
                       ' for gender'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           EXIT
           .

      *---
      * Customer test v3
       CUSTOMER-TEST-V3 SECTION.
           DISPLAY 'Running Customer test v3...'
              UPON CONSOLE
           SET TEST-SUCCESSFUL
            TO TRUE
       
           PERFORM LOAD-SCRIPTS-V3
           
           PERFORM INIT-INPUT-DATA-V3
           PERFORM PERFORM-CONVERSION-V3
           PERFORM CHECK-CONVERSION-V3
           PERFORM PRINT-TEST-STATUS
           
           PERFORM UNLOAD-TEST-SCRIPT
           EXIT
           .

       LOAD-SCRIPTS-V3 SECTION.
           MOVE 'consumer-script-v3.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v3.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
           
           EXIT
           .

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
           .
           
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
       
           EXIT
           .
           
       CHECK-CONVERSION-V3 SECTION.
           IF NOT VALUE-PRESENT IN CSPI-CUSTOMER-FLAGS
               DISPLAY 'Provider value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-FIRST-NAME-FLAGS
               DISPLAY 'Missing value for first-name'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-FIRST-NAME NOT = 'Test'
               DISPLAY 'Unexpected value "'
                       CSPI-FIRST-NAME
                       '" for first-name'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-LAST-NAME-FLAGS
               DISPLAY 'Missing value for last-name'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-LAST-NAME NOT = 'Tester'
               DISPLAY 'Unexpected value "'
                       CSPI-LAST-NAME
                       '" for last-name'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-DATE-OF-BIRTH-FLAGS
               DISPLAY 'Missing value for date-of-birth'
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-DATE-OF-BIRTH NOT = '01.01.2000'
               DISPLAY 'Unexpected value "'
                       CSPI-DATE-OF-BIRTH
                       '" for date-of-birth'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-ABSENT IN CSPI-GENDER-NEW-FLAGS
               DISPLAY 'Unexpected value for gender-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-ABSENT IN CSPI-PRIMARY-ADDRESS-NEW-FLAGS
               DISPLAY 'Unexpected value for primary-address-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
                      
           IF NOT VALUE-ABSENT IN CSPI-SEC-ADDR-NEW-LST-FLAGS
               DISPLAY 'Unexpected value for secondary-addresses-new'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE           
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-PRIMARY-ADDRESS-FLAGS
               DISPLAY 'Missing value for primary-address'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE           
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-POSTAL-CODE-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for postal-code '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS
              NOT = 12345
              
               DISPLAY 'Unexpected value '
                       CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS
                       ' for postal-code in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-CITY-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for city '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-CITY IN CSPI-PRIMARY-ADDRESS
              NOT = 'Test City'
              
               DISPLAY 'Unexpected value "'
                       CSPI-CITY IN CSPI-PRIMARY-ADDRESS
                       '" for city in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-STREET-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for street '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-STREET IN CSPI-PRIMARY-ADDRESS
              NOT = 'Test Street'
              
               DISPLAY 'Unexpected value "'
                       CSPI-STREET IN CSPI-PRIMARY-ADDRESS
                       '" for street in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-NUMBER-FLAGS
                  IN CSPI-PRIMARY-ADDRESS
               DISPLAY 'Missing value for number '
                       'in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS
              NOT = 17
              
               DISPLAY 'Unexpected value '
                       CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS
                       ' for number in primary-address'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-SEC-ADDR-LST-FLAGS
               DISPLAY 'Missing value for secondary-addresses'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF CSPI-SEC-ADDRESS-COUNT NOT = 2
               DISPLAY 'Unexpected element count '
                       CSPI-SEC-ADDRESS-COUNT
                       ' for secondary-addresses'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           PERFORM VARYING I-1 FROM 1 BY 1
                   UNTIL I-1 > CSPI-SEC-ADDRESS-COUNT
           
               IF NOT VALUE-PRESENT 
                      IN CSPI-SECONDARY-ADDRESS-FLAGS(I-1)
         
                   DISPLAY 'Missing value for secondary-address '
                           I-1
                   SET TEST-FAILED
                    TO TRUE
               END-IF

               IF NOT VALUE-PRESENT IN CSPI-POSTAL-CODE-FLAGS
                      IN CSPI-SECONDARY-ADDRESS(I-1)
                   DISPLAY 'Missing value for postal-code '
                          'in secondary-address '
                          I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
                END-IF
               IF CSPI-POSTAL-CODE 
                  IN CSPI-SECONDARY-ADDRESS(I-1)
                  NOT = 12345
              
                   DISPLAY 'Unexpected value '
                           CSPI-POSTAL-CODE 
                           IN CSPI-SECONDARY-ADDRESS(I-1)
                           ' for postal-code in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF

               IF NOT VALUE-PRESENT IN CSPI-CITY-FLAGS
                  IN CSPI-SECONDARY-ADDRESS(I-1)
                   DISPLAY 'Missing value for city '
                           'in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF
               IF CSPI-CITY IN CSPI-SECONDARY-ADDRESS(I-1)
                  NOT = 'Test Town'
              
                   DISPLAY 'Unexpected value "'
                           CSPI-CITY IN 
                           CSPI-SECONDARY-ADDRESS(I-1)
                           '" for city in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF

               IF NOT VALUE-PRESENT IN CSPI-STREET-FLAGS
                      IN CSPI-SECONDARY-ADDRESS(I-1)
                   DISPLAY 'Missing value for street '
                           'in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF
               IF CSPI-STREET IN CSPI-SECONDARY-ADDRESS(I-1)
                  NOT = 'Test Road'
              
                   DISPLAY 'Unexpected value "'
                           CSPI-STREET IN 
                           CSPI-SECONDARY-ADDRESS(I-1)
                           '" for street in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF

               IF NOT VALUE-PRESENT IN CSPI-NUMBER-FLAGS
                      IN CSPI-SECONDARY-ADDRESS(I-1)
                   DISPLAY 'Missing value for number '
                           'in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF
               IF CSPI-NUMBER IN CSPI-SECONDARY-ADDRESS(I-1)
                  NOT = I-1
              
                   DISPLAY 'Unexpected value '
                           CSPI-NUMBER IN 
                           CSPI-SECONDARY-ADDRESS(I-1)
                           ' for number in secondary-address '
                           I-1
                      UPON CONSOLE
                   SET TEST-FAILED
                    TO TRUE
               END-IF               
           END-PERFORM
           
           IF NOT VALUE-PRESENT IN CSPI-GENDER-FLAGS
               DISPLAY 'Missing value for gender'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-GENDER NOT = 1
               DISPLAY 'Unexpected value '
                       CSPI-GENDER
                       ' for gender'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
       
           EXIT
           .

      *---
      * Customer test v6
       CUSTOMER-TEST-V6 SECTION.
           DISPLAY 'Running Customer test v6...'
              UPON CONSOLE
           SET TEST-SUCCESSFUL
            TO TRUE
       
           PERFORM LOAD-SCRIPTS-V6
           
           PERFORM INIT-INPUT-DATA-V6
           PERFORM PERFORM-CONVERSION-V6
           PERFORM CHECK-CONVERSION-V6
           PERFORM PRINT-TEST-STATUS
           
           PERFORM UNLOAD-TEST-SCRIPT
           EXIT
           .

       LOAD-SCRIPTS-V6 SECTION.
           MOVE 'consumer-script-v6.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v6.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
           
           EXIT
           .

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
       
           EXIT
           .
           
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

           EXIT
           .
           
       CHECK-CONVERSION-V6 SECTION.
           IF NOT VALUE-PRESENT IN CSPI-CUSTOMER-FLAGS
               DISPLAY 'Provider value not present'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-FIRST-NAME-FLAGS
               DISPLAY 'Missing value for first-name'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-FIRST-NAME NOT = 'Test'
               DISPLAY 'Unexpected value "'
                       CSPI-FIRST-NAME
                       '" for first-name'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-LAST-NAME-FLAGS
               DISPLAY 'Missing value for last-name'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-LAST-NAME NOT = 'Tester'
               DISPLAY 'Unexpected value "'
                       CSPI-LAST-NAME
                       '" for last-name'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-DATE-OF-BIRTH-FLAGS
               DISPLAY 'Missing value for date-of-birth'
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-DATE-OF-BIRTH NOT = '01.01.2000'
               DISPLAY 'Unexpected value "'
                       CSPI-DATE-OF-BIRTH
                       '" for date-of-birth'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-GENDER-NEW-FLAGS
               DISPLAY 'Missing value for gender-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF NOT FEMALE IN CSPI-GENDER-NEW
               DISPLAY 'Unexpected value '
                       CSPI-GENDER-NEW
                       ' for gender-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-PRIMARY-ADDRESS-NEW-FLAGS
               DISPLAY 'Missing value for primary-address-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT CSPI-STREET-ADDRESS IN CSPI-PRIMARY-ADDRESS-NEW
               DISPLAY 'Unexpected type id '
                       CSPI-TYPE-ID IN CSPI-PRIMARY-ADDRESS-NEW
                       ' in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           
           IF NOT VALUE-PRESENT IN CSPI-POSTAL-CODE-FLAGS
                  IN CSPI-PRIMARY-ADDRESS-NEW
               DISPLAY 'Missing value for postal-code '
                       'in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS-NEW
              NOT = 12345
              
               DISPLAY 'Unexpected value '
                       CSPI-POSTAL-CODE IN CSPI-PRIMARY-ADDRESS-NEW
                       ' for postal-code in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-CITY-FLAGS
                  IN CSPI-PRIMARY-ADDRESS-NEW
               DISPLAY 'Missing value for city '
                       'in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-CITY IN CSPI-PRIMARY-ADDRESS-NEW
              NOT = 'Test City'
              
               DISPLAY 'Unexpected value "'
                       CSPI-CITY IN CSPI-PRIMARY-ADDRESS-NEW
                       '" for city in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-STREET-FLAGS
                  IN CSPI-PRIMARY-ADDRESS-NEW
               DISPLAY 'Missing value for street '
                       'in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-STREET IN CSPI-PRIMARY-ADDRESS-NEW
              NOT = 'Test Street'
              
               DISPLAY 'Unexpected value "'
                       CSPI-STREET IN CSPI-PRIMARY-ADDRESS-NEW
                       '" for street in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-NUMBER-FLAGS
                  IN CSPI-PRIMARY-ADDRESS-NEW
               DISPLAY 'Missing value for number '
                       'in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS-NEW
              NOT = 17
              
               DISPLAY 'Unexpected value '
                       CSPI-NUMBER IN CSPI-PRIMARY-ADDRESS-NEW
                       ' for number in primary-address-new'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
                      
           IF NOT VALUE-PRESENT IN CSPI-SEC-ADDR-NEW-LST-FLAGS
               DISPLAY 'Missing value for secondary-addresses-new'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE           
           END-IF

           IF CSPI-SEC-ADDR-NEW-COUNT NOT = 2
               DISPLAY 'Unexpected element count '
                       CSPI-SEC-ADDR-NEW-COUNT
                       ' for secondary-addresses-new'
               SET TEST-FAILED
                TO TRUE
           END-IF
                   
           IF NOT VALUE-PRESENT 
                  IN CSPI-SEC-ADDRESS-NEW-FLAGS(1)
         
               DISPLAY 'Missing value for secondary-address-new(1)'
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT CSPI-STREET-ADDRESS IN CSPI-TYPE-ID
                  IN CSPI-SECONDARY-ADDRESS-NEW(1)
           
               DISPLAY 'Unexpected type id '
                       CSPI-TYPE-ID IN CSPI-SECONDARY-ADDRESS-NEW(1)
                       ' in secondary-address-new(1)'
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-POSTAL-CODE-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(1)
               DISPLAY 'Missing value for postal-code '
                       'in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-POSTAL-CODE IN CSPI-SECONDARY-ADDRESS-NEW(1)
              NOT = 12345
              
               DISPLAY 'Unexpected value '
                       CSPI-POSTAL-CODE 
                       IN CSPI-SECONDARY-ADDRESS-NEW(1)
                       ' for postal-code in secondary-address-new(1) '
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-CITY-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(1)
               DISPLAY 'Missing value for city '
                       'in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-CITY IN CSPI-SECONDARY-ADDRESS-NEW(1)
              NOT = 'Test Town'
              
               DISPLAY 'Unexpected value "'
                       CSPI-CITY IN 
                       CSPI-SECONDARY-ADDRESS-NEW(1)
                       '" for city in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-STREET-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(1)
               DISPLAY 'Missing value for street '
                       'in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-STREET IN CSPI-SECONDARY-ADDRESS-NEW(1)
              NOT = 'Test Road'
              
               DISPLAY 'Unexpected value "'
                       CSPI-STREET IN 
                       CSPI-SECONDARY-ADDRESS-NEW(1)
                       '" for street in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-NUMBER-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(1)
               DISPLAY 'Missing value for number '
                       'in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-NUMBER IN CSPI-SECONDARY-ADDRESS-NEW(1)
              NOT = 3
              
               DISPLAY 'Unexpected value '
                       CSPI-NUMBER IN 
                       CSPI-SECONDARY-ADDRESS-NEW(1)
                           ' for number in secondary-address-new(1)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF           

           IF NOT VALUE-PRESENT 
                  IN CSPI-SEC-ADDRESS-NEW-FLAGS(2)
         
               DISPLAY 'Missing value for secondary-address-new(2)'
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT CSPI-PO-BOX-ADDRESS IN CSPI-TYPE-ID
                  IN CSPI-SECONDARY-ADDRESS-NEW(2)
           
               DISPLAY 'Unexpected type id '
                       CSPI-TYPE-ID IN CSPI-SECONDARY-ADDRESS-NEW(2)
                       ' in secondary-address-new(2)'
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-POSTAL-CODE-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(2)
               DISPLAY 'Missing value for postal-code '
                       'in secondary-address-new(2)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-POSTAL-CODE IN CSPI-SECONDARY-ADDRESS-NEW(2)
              NOT = 12346
              
               DISPLAY 'Unexpected value '
                       CSPI-POSTAL-CODE 
                       IN CSPI-SECONDARY-ADDRESS-NEW(2)
                       ' for postal-code in secondary-address-new(2)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-CITY-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(2)
               DISPLAY 'Missing value for city '
                       'in secondary-address-new(2)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-CITY IN CSPI-SECONDARY-ADDRESS-NEW(2)
              NOT = 'Test Town'
              
               DISPLAY 'Unexpected value "'
                       CSPI-CITY IN 
                       CSPI-SECONDARY-ADDRESS-NEW(2)
                       '" for city in secondary-address-new(2)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-PRESENT IN CSPI-BOX-NO-FLAGS
                  IN CSPI-SECONDARY-ADDRESS-NEW(2)
               DISPLAY 'Missing value for box-no '
                       'in secondary-address-new(2)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
           IF CSPI-BOX-NO IN CSPI-SECONDARY-ADDRESS-NEW(2)
              NOT = 5678
              
               DISPLAY 'Unexpected value "'
                       CSPI-BOX-NO IN 
                       CSPI-SECONDARY-ADDRESS-NEW(2)
                       '" for box-no in secondary-address-new(2)'
                  UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF

           IF NOT VALUE-ABSENT IN CSPI-PRIMARY-ADDRESS-FLAGS
               DISPLAY 'Unexpected value for primary-address'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE           
           END-IF
                      
           IF NOT VALUE-ABSENT IN CSPI-SEC-ADDR-LST-FLAGS
               DISPLAY 'Unexpected value for secondary-addresses'
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
                      
           IF NOT VALUE-ABSENT IN CSPI-GENDER-FLAGS
               DISPLAY 'Unexpected value for gender'
                 UPON CONSOLE
               DISPLAY CSPI-GENDER-FLAGS
                 UPON CONSOLE
               SET TEST-FAILED
                TO TRUE
           END-IF
       
           EXIT
           .

       PRINT-TEST-STATUS SECTION.
           IF TEST-SUCCESSFUL
               DISPLAY 'SUCCESS' UPON CONSOLE
           ELSE
               DISPLAY 'FAILED' UPON CONSOLE
           END-IF
           
           EXIT
           .
           
