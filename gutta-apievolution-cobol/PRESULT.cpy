      * Provider result for tests
           05 '*-'FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.

           05 '*-'RET-FIELD-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.             
           05 '*-'RET-FIELD PIC X(30).

           05 '*-'RESULT-ENUM-FLAGS PIC 9 BINARY.
             88 VALUE-ABSENT VALUE 0.
             88 VALUE-PRESENT VALUE 1.
             88 VALUE-UNREPRESENTABLE VALUE 2.            
           05 '*-'RESULT-ENUM PIC S9(9) BINARY.
             88 VALUE-1 VALUE 0.
             88 VALUE-2 VALUE 1.

           05 '*-'RESULT-LIST.
             10 '*-'RESULT-LIST-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             
             10 '*-'RESULT-LIST-COUNT PIC S9(9) BINARY.
             
             10 '*-'RESULT-LIST-ENTRY OCCURS 10.
               15 '*-'RESULT-LIST-ENTRY-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               15 '*-'RESULT-LIST-ENUM PIC S9(9) BINARY.
                 88 VALUE-1 VALUE 0.
                 88 VALUE-2 VALUE 1.

