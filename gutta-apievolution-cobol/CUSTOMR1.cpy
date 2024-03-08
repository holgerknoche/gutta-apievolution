      * Customer structure, revision 1
           05 '*-'CUSTOMER.
             10 '*-'CUSTOMER-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
                   
             10 '*-'FIRST-NAME-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 '*-'FIRST-NAME PIC X(20).
               
             10 '*-'LAST-NAME-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 '*-'LAST-NAME PIC X(20).
               
             10 '*-'GENDER-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 '*-'GENDER PIC S9(9) BINARY.
               
             10 '*-'ADDRESS.
               15 '*-'ADDRESS-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
                       
               15 '*-'STREET-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               15 '*-'STREET PIC X(20).
                   
               15 '*-'NUMBER-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               15 '*-'NUMBER PIC S9(9) BINARY.

               15 '*-'POSTAL-CODE-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               15 '*-'POSTAL-CODE PIC S9(9) BINARY.

               15 '*-'CITY-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               15 '*-'CITY PIC X(20).

