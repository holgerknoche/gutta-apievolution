      * Customer structure, provider revisions 1 to 6
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

             10 '*-'DATE-OF-BIRTH-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 '*-'DATE-OF-BIRTH PIC X(10).

             10 '*-'GENDER-NEW-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 '*-'GENDER-NEW PIC S9(9) BINARY.
               88 MALE VALUE 0.
               88 FEMALE VALUE 1.
               88 THIRD VALUE 2.

             10 '*-'PRIMARY-ADDRESS-NEW.
               15 '*-'PRIMARY-ADDRESS-NEW-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
                       
               15 '*-'TYPE-ID PIC S9(9) BINARY.
                 88 '*-'STREET-ADDRESS VALUE 3.
                 88 '*-'PO-BOX-ADDRESS VALUE 4.
                   
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
                   
               15 '*-'DATA PIC X(26).
                   
               15 '*-'STREET-ADDRESS-DATA REDEFINES '*-'DATA.
                 20 '*-'STREET-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'STREET PIC X(20).
                       
                 20 '*-'NUMBER-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'NUMBER PIC S9(9) BINARY.
                 
               15 '*-'PO-BOX-ADDRESS-DATA REDEFINES '*-'DATA.
                 20 '*-'BOX-NO-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'BOX-NO PIC S9(9) BINARY.
                   
             10 '*-'SECONDARY-ADDRESSES-NEW.
               15 '*-'SEC-ADDR-NEW-LST-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.    
             
               15 '*-'SEC-ADDR-NEW-COUNT PIC S9(9) BINARY.
             
               15 '*-'SECONDARY-ADDRESS-NEW OCCURS 10.
                 20 '*-'SEC-ADDRESS-NEW-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.

                 20 '*-'TYPE-ID PIC S9(9) BINARY.
                   88 '*-'STREET-ADDRESS VALUE 3.
                   88 '*-'PO-BOX-ADDRESS VALUE 4.
                
                 20 '*-'POSTAL-CODE-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'POSTAL-CODE PIC S9(9) BINARY.

                 20 '*-'CITY-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'CITY PIC X(20).
                   
                 20 '*-'DATA PIC X(26).
                   
                 20 '*-'STREET-ADDRESS-DATA REDEFINES '*-'DATA.
                   25 '*-'STREET-FLAGS PIC 9 BINARY.
                     88 VALUE-ABSENT VALUE 0.
                     88 VALUE-PRESENT VALUE 1.
                     88 VALUE-UNREPRESENTABLE VALUE 2.
                   25 '*-'STREET PIC X(20).
                       
                   25 '*-'NUMBER-FLAGS PIC 9 BINARY.
                     88 VALUE-ABSENT VALUE 0.
                     88 VALUE-PRESENT VALUE 1.
                     88 VALUE-UNREPRESENTABLE VALUE 2.
                   25 '*-'NUMBER PIC S9(9) BINARY.                       

                 20 '*-'PO-BOX-ADDRESS-DATA REDEFINES '*-'DATA.
                   25 '*-'BOX-NO-FLAGS PIC 9 BINARY.
                     88 VALUE-ABSENT VALUE 0.
                     88 VALUE-PRESENT VALUE 1.
                     88 VALUE-UNREPRESENTABLE VALUE 2.
                   25 '*-'BOX-NO PIC S9(9) BINARY.
                   
             10 '*-'PRIMARY-ADDRESS.
               15 '*-'PRIMARY-ADDRESS-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.

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
              
             10 '*-'SECONDARY-ADDRESSES.
               15 '*-'SEC-ADDR-LST-FLAGS PIC 9 BINARY.
                 88 VALUE-ABSENT VALUE 0.
                 88 VALUE-PRESENT VALUE 1.
                 88 VALUE-UNREPRESENTABLE VALUE 2.
               
               15 '*-'SEC-ADDRESS-COUNT PIC S9(9) BINARY.
             
               15 '*-'SECONDARY-ADDRESS OCCURS 10.
                 20 '*-'SECONDARY-ADDRESS-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.

                 20 '*-'POSTAL-CODE-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'POSTAL-CODE PIC S9(9) BINARY.

                 20 '*-'CITY-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'CITY PIC X(20).

                 20 '*-'STREET-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'STREET PIC X(20).
                   
                 20 '*-'NUMBER-FLAGS PIC 9 BINARY.
                   88 VALUE-ABSENT VALUE 0.
                   88 VALUE-PRESENT VALUE 1.
                   88 VALUE-UNREPRESENTABLE VALUE 2.
                 20 '*-'NUMBER PIC S9(9) BINARY.

             10 '*-'GENDER-FLAGS PIC 9 BINARY.
               88 VALUE-ABSENT VALUE 0.
               88 VALUE-PRESENT VALUE 1.
               88 VALUE-UNREPRESENTABLE VALUE 2.
             10 '*-'GENDER PIC S9(9) BINARY.

