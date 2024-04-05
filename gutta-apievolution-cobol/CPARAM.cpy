      * Consumer parameter for tests
           05 '*-'FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.
               
           05 '*-'FIELD-A-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.             
           05 '*-'FIELD-A PIC X(30).
           
           05 '*-'TEST-ENUM-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.            
           05 '*-'TEST-ENUM PIC S9(9) BINARY.
             88 VALUE-A VALUE 0.
             88 VALUE-B VALUE 1.
           
           05 '*-'TEST-LIST.
             10 '*-'TEST-LIST-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             
             10 '*-'TEST-LIST-COUNT PIC S9(9) BINARY.
             
             10 '*-'TEST-LIST-ENTRY OCCURS 10.
               15 '*-'TEST-LIST-ENTRY-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               15 '*-'TEST-LIST-ENUM PIC S9(9) BINARY.
                 88 VALUE-A VALUE 0.
                 88 VALUE-B VALUE 1.

