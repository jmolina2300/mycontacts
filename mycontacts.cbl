       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CONTACTS.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT CONTACT ASSIGN TO "CONTACTS.txt"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FILESTATUS.

           SELECT SEL-CONTACT ASSIGN TO "TEMP"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FILESTATUS.

       DATA DIVISION.
           FILE SECTION.
           FD  CONTACT.

           01  FS-PERSON.
               02 FS-NAME.
                   05 FS-FIRSTNAME          PIC A(10).
                   05 FS-LASTNAME           PIC A(10).
               02 FS-PHONENUMBER            PIC 9(10).

           FD  SEL-CONTACT.
           01 FS-SEL-PERSON.
               02 FS-SEL-NAME.
                   05 FS-SEL-FIRSTNAME          PIC A(10).
                   05 FS-SEL-LASTNAME           PIC A(10).
               02 FS-SEL-PHONENUMBER            PIC 9(10).


       WORKING-STORAGE SECTION.
           01  WS-PERSON.
               02 WS-NAME.
                   05 WS-FIRSTNAME          PIC A(10).
                   05 WS-LASTNAME           PIC A(10).
               02 WS-PHONENUMBER            PIC 9(10).
           01  WS-FILESTATUS     PIC 99.
           01  CHOICE PIC 9.

           01 WS-SEL-CONTACT                PIC A(10).



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

       OPEN EXTEND CONTACT.
       IF WS-FILESTATUS IS NOT EQUAL 0
           OPEN OUTPUT CONTACT
       END-IF.
       IF WS-FILESTATUS IS NOT EQUAL 0
           DISPLAY "File error: " WS-FILESTATUS
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
       IF  WS-FILESTATUS IS NOT EQUAL 0
           DISPLAY "No contacts to list"
           EXIT PARAGRAPH
       END-IF.
       PERFORM UNTIL WS-FILESTATUS = 10
           READ CONTACT INTO WS-PERSON
           AT END MOVE 10 TO WS-FILESTATUS 
           NOT AT END DISPLAY " " WS-FIRSTNAME "  " WS-PHONENUMBER
           END-READ
       END-PERFORM.
       CLOSE CONTACT.


       TRANSFER-CONTACT.
           MOVE FS-PERSON TO FS-SEL-PERSON.
           WRITE FS-SEL-PERSON
           END-WRITE.

       DELETE-CONTACT.
       DISPLAY "Contact to delete: ".
       ACCEPT WS-SEL-CONTACT.
      * OPEN THE CONTACT FILE FOR READING
       OPEN INPUT CONTACT.
       IF  WS-FILESTATUS IS NOT EQUAL 0
           DISPLAY "No contacts"
           EXIT PARAGRAPH
       END-IF.

      * OPEN THE TEMPORARY FILE FOR WRITING
       OPEN OUTPUT SEL-CONTACT
       IF  WS-FILESTATUS IS NOT EQUAL 0
           DISPLAY "ERROR CREATING TEMPORARY FILE"
           CLOSE CONTACT
           EXIT PARAGRAPH
       END-IF.

       PERFORM UNTIL WS-FILESTATUS = 10
           READ CONTACT INTO WS-PERSON
           AT END 
               MOVE 10 TO WS-FILESTATUS 
           NOT AT END

      * Copy the contact to the temporary file if we want to keep it
           IF WS-FIRSTNAME NOT EQUAL WS-SEL-CONTACT
           PERFORM TRANSFER-CONTACT
           END-IF

           END-READ
       END-PERFORM.
       CLOSE CONTACT.
       CLOSE SEL-CONTACT.

      * DELETE the CONTACTS FILE AND COPY over the stuff we want
      * OPEN THE CONTACT FILE FOR WRITING
       OPEN OUTPUT CONTACT.
       IF  WS-FILESTATUS IS NOT EQUAL 0
           DISPLAY "No contacts file to remove"
           EXIT PARAGRAPH
       END-IF.

      * OPEN THE TEMPORARY FILE FOR READING
       OPEN INPUT SEL-CONTACT
       IF  WS-FILESTATUS IS NOT EQUAL 0
           DISPLAY "ERROR FINDING TEMPORARY FILE!"
           CLOSE CONTACT
           EXIT PARAGRAPH
       END-IF.
       
       DISPLAY " ".
       DISPLAY "UPDATED CONTACT LIST: ".
       PERFORM UNTIL WS-FILESTATUS = 10
           READ SEL-CONTACT INTO WS-PERSON
           AT END 
               MOVE 10 TO WS-FILESTATUS 
           NOT AT END

           DISPLAY " " WS-FIRSTNAME "  " WS-PHONENUMBER
      
      * Transfer the current CONTACT TO the main contact buffer
           MOVE FS-SEL-PERSON TO FS-PERSON
           WRITE FS-PERSON
           END-WRITE

           END-READ
       END-PERFORM.
       CLOSE CONTACT.
       CLOSE SEL-CONTACT.



       


