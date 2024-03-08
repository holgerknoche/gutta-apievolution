123456*Conversion invocation from COBOL

       IDENTIFICATION DIVISION.
       PROGRAM-ID. APICONV.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TIMINGS.
           05 START-TIME PIC 9(8).
           05 END-TIME PIC 9(8).
           05 DURATION PIC 9(8).
           
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
           
      * Structures for Customer, provider view
       01 CUSTOMER-PROVIDER-IN.
           COPY CUSTOMRP REPLACING '*-' BY CSPI-.
           
       01 CUSTOMER-PROVIDER-OUT.
           COPY CUSTOMRP REPLACING '*-' BY CSPO-.
       
       PROCEDURE DIVISION.
           ACCEPT START-TIME FROM TIME
           
      *    Call to load the module
           CALL 'apimapper'
           
           MOVE 'consumer-script-v1.dat'
             TO CONSUMER-SCRIPT-NAME
           MOVE 'provider-script-v1.dat'
             TO PROVIDER-SCRIPT-NAME
           
           CALL 'loadScripts' USING
                BY REFERENCE CONSUMER-SCRIPT-NAME
                BY REFERENCE PROVIDER-SCRIPT-NAME
                
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
                
           MOVE 0 TO OPERATION-INDEX
           SET CONSUMER-TO-PROVIDER TO TRUE
           SET PARAMETER-MAPPING TO TRUE

           PERFORM 10000000 TIMES
              CALL 'convertData' USING
                BY VALUE OPERATION-INDEX
                BY VALUE MAPPING-DIRECTION
                BY VALUE MAPPING-TYPE
                BY REFERENCE CUSTOMER-V1-IN
                BY REFERENCE CUSTOMER-PROVIDER-IN
           END-PERFORM

           CALL 'unloadScripts'
           
           ACCEPT END-TIME FROM TIME
           COMPUTE DURATION = (END-TIME - START-TIME)
       
           DISPLAY "Mapped first name '"
                   CSPI-FIRST-NAME
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped last name '"
                   CSPI-LAST-NAME
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped street name '"
                   CSPI-STREET IN CSPI-PRIMARY-ADDRESS
                   "'" UPON CONSOLE
                   
           DISPLAY "Mapped gender '"
                   CSPI-GENDER
                   "'" UPON CONSOLE
       
           DISPLAY 'Took ' DURATION 'cs' UPON CONSOLE
       
           GOBACK.
           
