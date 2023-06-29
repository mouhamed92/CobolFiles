       IDENTIFICATION DIVISION.

       PROGRAM-ID. Emp-Pay.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EMPLOYEFILE ASSIGN TO
            "C:/work space/Cobol path/labs/Files/EMPFILE.DAT"
            FILE STATUS IS FILE-CHECK-KEY
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT PAYROLL ASSIGN TO
            "C:/work space/Cobol path/labs/Files/PAYROLLIN.DAT"
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEFILE.
       01 EMPDETAILS.
           88 ENDOFFILE VALUE HIGH-VALUES.
           02 EMPDATA    PIC   X(44).
           02 EMPINFO   REDEFINES EMPDATA.
             04 EMPID    PIC   9(7).
             04 EMPNAME.
                 05 LASTNAME    PIC  X(10).
                 05 FIRSTNAME   PIC  X(10).
             04 STARTDATE.
                 05 START-YEAR   PIC  9(4).
                 05 START-MONTH  PIC  9(2).
                 05 START-DAY    PIC  9(2).
             04 HOURSWORKED      PIC  9(3).
             04 HOURLYRATE       PIC  9(3)V99.
           02 DEPARTMENT         PIC  X(30).
           02 GENDER             PIC  X.

       FD PAYROLL.
       01 EMPAYROLL.
            04 PAY-EMPID       PIC  9(7).
            04 PAY-EMPFNAME    PIC  X(10).
            04 PAY-EMPLNAME    PIC  X(10).
            04 PAY-AMOUNT      PIC  9(5)V99.
            04 PAY-DEPARTMENT  PIC  X(30).

       WORKING-STORAGE SECTION.

       01 WS-WORK-AREA.
           05 FILE-CHECK-KEY     PIC X(2).
           05 WS-EMPLOYE-COUNT   PIC 9(5).

       PROCEDURE DIVISION.

           0100-READ-EMPLOYEES.
             OPEN INPUT EMPLOYEFILE.
             OPEN OUTPUT PAYROLL.
             INITIALIZE WS-EMPLOYE-COUNT.
             IF FILE-CHECK-KEY NOT = "00"
                 DISPLAY "NON ZERO FILE STATUS" FILE-CHECK-KEY
                 GO TO 0900-END-PROGRAM
             END-IF.

             READ EMPLOYEFILE
              AT END SET ENDOFFILE TO TRUE
             END-READ.

             PERFORM 0200-PROCESS-EMP UNTIL ENDOFFILE.
             PERFORM 0900-END-PROGRAM.
           0100-END.

           0200-PROCESS-EMP.
              MOVE EMPID TO PAY-EMPID.
              MOVE FIRSTNAME TO PAY-EMPFNAME.
              MOVE LASTNAME TO PAY-EMPLNAME.
              COMPUTE PAY-AMOUNT = HOURSWORKED * HOURLYRATE .
              MOVE DEPARTMENT TO PAY-DEPARTMENT.
              WRITE EMPAYROLL.
              ADD 1 TO WS-EMPLOYE-COUNT.
              READ EMPLOYEFILE
              AT END SET ENDOFFILE TO TRUE
              END-READ.
           0200-END.

           0900-END-PROGRAM.
            CLOSE EMPLOYEFILE , PAYROLL.
            DISPLAY "NUMBER OF EMPLOYEE PROCESSED : ", WS-EMPLOYE-COUNT
            STOP RUN.

       END PROGRAM Emp-Pay.
