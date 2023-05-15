       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CONTACTS.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT CONTACT ASSIGN TO "CONTACTS.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FILESTATUS.

       DATA DIVISION.
           FILE SECTION.
           FD  CONTACT.

           01  FS-PERSON.
               02 FS-NAME.
                   03 FS-FIRSTNAME          PIC A(10).
                   03 FS-LASTNAME           PIC A(10).
               02 FS-PHONENUMBER            PIC 9(10).

       WORKING-STORAGE SECTION.
           01  WS-PERSON.
               02 WS-NAME.
                   03 WS-FIRSTNAME          PIC A(10).
                   03 WS-LASTNAME           PIC A(10).
               02 WS-PHONENUMBER            PIC 9(10).
           01  WS-FILESTATUS     PIC 99.
           01  CHOICE PIC 9.



       PROCEDURE DIVISION.

       DISPLAY "MY CONTACTS".
       PERFORM UNTIL CHOICE = 4
           DISPLAY "(1) List    (2) Add    (3) Delete    (4) Exit"
           ACCEPT CHOICE
           
           IF CHOICE = 1
               PERFORM LIST-CONTACTS
           ELSE IF CHOICE = 2
               PERFORM ADD-NEW-FRIEND
           ELSE IF CHOICE = 3
               PERFORM DELETE-CONTACT
           END-IF

       END-PERFORM.
       STOP RUN.




      ******************************************************************
      *    Add contact                                                 *
      ******************************************************************
       ADD-NEW-FRIEND.
       DISPLAY "Fist name: ".
       ACCEPT WS-FIRSTNAME.
       DISPLAY "Last name: "
       ACCEPT WS-LASTNAME.
       DISPLAY "Phone number: ".
       ACCEPT WS-PHONENUMBER.
      
       OPEN EXTEND  CONTACT.
       IF WS-FILESTATUS IS NOT EQUAL 0
           OPEN OUTPUT CONTACT
       END-IF.

       MOVE WS-PERSON TO FS-PERSON.
       WRITE FS-PERSON
       END-WRITE.

       CLOSE CONTACT.
       DISPLAY "Saved " WS-NAME " :D".


      ******************************************************************
      *  List all contacts                                             *
      ******************************************************************
       LIST-CONTACTS.
       OPEN INPUT CONTACT.
       PERFORM UNTIL WS-FILESTATUS = 10
           READ CONTACT INTO WS-PERSON
           AT END MOVE 10 TO WS-FILESTATUS
           NOT AT END DISPLAY WS-FIRSTNAME "  " WS-PHONENUMBER
           END-READ
       END-PERFORM.
       CLOSE CONTACT.

       
       DELETE-CONTACT.

