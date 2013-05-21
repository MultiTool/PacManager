{$R+}
Unit Print;
{**************************************************************}
{* Tries out BIOS parallel printer functions.                 *}
{**************************************************************}


INTERFACE

USES Abutil2;


TYPE

        Statustype = Record
            StatusString : string;
            PrinterError : boolean; {True if printer error, false otherwise}
        END;

        ActionType = (NONE,INIT,LF,FF);

VAR
        PrinterStatus : statustype;

PROCEDURE PrintString(PrintNum : byte; VAR PrinterStatus : statustype; StringToPrint : string; Action : actiontype);
(*  Initialize, LineFeed, Formfeed : char); *)


(*   PROCEDURE PrintChar(PrinterNum:BYTE;InChar:CHAR);
  PROCEDURE PrintLine(PrinterNum:BYTE;InStr:STRING);
{  PROCEDURE InitPrinter(PrinterNum:BYTE); }
  PROCEDURE GetPrinterStatus(PrinterNum:BYTE; VAR PrinterStatus : statustype); *)


IMPLEMENTATION

USES CRT,DOS;

(* CONST
  LineFeed : String = #10;
  LineSpacing : String = #27 + #50;
  PrintStyle : String = #27 + #107 + #48; *)


     PROCEDURE ShowPrinterStatus(Statusnum:BYTE; VAR PrinterStatus : statustype);
      {******************************************************************}
      {* Translate the port status byte (Reg.ah) after InitPrinter.     *}
      {* Example call: ShowPrinterStatus(Reg.ah);                       *}
      {******************************************************************}
    CONST
       PrintStatusStr : ARRAY[0..7] OF STRING[40] =
         ('PRINTER TIMED-OUT',
          'unused',
          'unused',
          'I/O ERROR',
          'PRINTER SELECTED',
          'OUT OF PAPER',
          'PRINTER ACKNOWLEDGE',
          'PRINTER NOT BUSY');
    VAR
      temp,BitIndex : BYTE;
    BEGIN
      FOR BitIndex:=0 TO 7 DO BEGIN
        temp:=(Statusnum SHR BitIndex) AND $01;
        CASE Temp of
          0 : BEGIN
              END;
          1 : BEGIN
                PrinterStatus.StatusString := PrintStatusStr[BitIndex];
                CASE Bitindex Of
                  0 : BEGIN
                        PrinterStatus.PrinterError := TRUE;
                        {'printer timed-out'}
                      END;
                  1 : BEGIN
                        PrinterStatus.PrinterError := FALSE;
                        {'unused'}
                      END;
                  2 : BEGIN
                        PrinterStatus.PrinterError := FALSE;
                        {'unused'}
                      END;
                  3 : BEGIN
                        PrinterStatus.PrinterError := TRUE;
                        {'I/O error'}
                      END;
                  4 : BEGIN
                        PrinterStatus.PrinterError := FALSE;
                        {'printer selected'}
                      END;
                  5 : BEGIN
                        PrinterStatus.PrinterError := TRUE;
                        {'out of paper'}
                      END;
                  6 : BEGIN
                        PrinterStatus.PrinterError := FALSE;
                        {'printer acknowledge'}
                      END;
                  7 : BEGIN
                        PrinterStatus.PrinterError := FALSE;
                        {'printer not busy'}
                      END;
                END;

            END;
      END;
    END;
  END;




  PROCEDURE PrintChar(PrinterNum:BYTE;InChar:CHAR; VAR PrinterStatus : statustype);
  {******************************************************************}
  {* Send one ascii char to printer.                                *}
  {******************************************************************}
  VAR
    //dummy reg : REGISTERS;
    status : BYTE;
  BEGIN
    (* dummy
    WITH reg DO BEGIN
      ah:=$00;
      al:=ORD(InChar);
      dx:=PrinterNum;  {0=LPT1, 1=LPT2, 2=LPT3.}
      INTR($17,reg);
      status:=ah;
      ShowPrinterStatus(ah, PrinterStatus);
    END;
    *)
  END;


(*  PROCEDURE PrintStr(PrinterNum:BYTE;InStr:STRING; VAR PrinterStatus : statustype);
  {******************************************************************}
  {* Send one text string to printer. //No Line feed                *}
  {******************************************************************}
  VAR cnt : BYTE;
  BEGIN
    IF Trim(instr) <> '' THEN BEGIN
      cnt := 1;
      REPEAT
        CASE Instr[cnt] of
          #174 :  BEGIN
                    PrintChar(PrinterNum, #27, PrinterStatus);
                    PrintChar(PrinterNum, #68, PrinterStatus);
                  END;
          #175 :  BEGIN
                    PrintChar(PrinterNum, #27, PrinterStatus);
                    PrintChar(PrinterNum, #70, PrinterStatus);
                  END
        ELSE
           PrintChar(PrinterNum,InStr[cnt], PrinterStatus);
        END;
        Cnt := Cnt + 1;
      UNTIL Cnt = Length(Instr);
    END;
  END;  *)

  PROCEDURE PrintStr(PrinterNum:BYTE;InStr:STRING; VAR PrinterStatus : statustype);
  {******************************************************************}
  {* Send one text string to printer. //No Line feed                *}
  {******************************************************************}
  VAR cnt : BYTE;
  BEGIN
    FOR cnt:=1 TO LENGTH(InStr) DO BEGIN
      PrintChar(PrinterNum,InStr[cnt], PrinterStatus);
    END;
  END;


  PROCEDURE InitPrinter(PrinterNum:BYTE; VAR PrinterStatus : statustype);
  {******************************************************************}
  {* Initialize printer.                                            *}
  {******************************************************************}
  VAR
    //dummy reg : REGISTERS;
    status : BYTE;
  BEGIN
    (* dummy
    WITH reg DO BEGIN
      ah:=$01;
      dx:=PrinterNum;  {0=LPT1, 1=LPT2, 2=LPT3.}
      INTR($17,reg);
      status:=ah;
      ShowPrinterStatus(ah, PrinterStatus);
    END;
    *)
  END;

  PROCEDURE GetPrinterStatus(PrinterNum:BYTE; VAR PrinterStatus : statustype);
  {******************************************************************}
  {* Just get and write out the status byte from the printer.       *}
  {******************************************************************}
    VAR
          (* dummy reg : REGISTERS; *)
      status : BYTE;
    BEGIN
     (* dummy
      WITH reg DO BEGIN
        ah:=$02;
        dx:=PrinterNum;  {0=LPT1, 1=LPT2, 2=LPT3.}
        INTR($17,reg);
        status:=ah;
        ShowPrinterStatus(ah, PrinterStatus);
      END;
      *)
    END;

  PROCEDURE PrintString(PrintNum : byte; VAR PrinterStatus : statustype; StringToPrint : string; Action : actiontype);
    (* Initialize, LineFeed, Formfeed : char); *)
    BEGIN
      With PrinterStatus DO BEGIN
        GetPrinterStatus(PrintNum, PrinterStatus);
        IF Not(PrinterError) THEN BEGIN
          IF Action = Init (* Initialize = 'Y'*) THEN
            InitPrinter(PrintNum, PrinterStatus);
          IF NOT(PrinterError) THEN
            PrintStr(PrintNum, StringToPrint, PrinterStatus);
          IF Action = LF {LineFeed = 'Y'}  THEN BEGIN
            PrintChar(PrintNum, #10, PrinterStatus);
            PrintChar(PrintNum, #13, PrinterStatus);
          END;
          IF Action = FF {FormFeed = 'Y'} THEN
            PrintChar(PrintNum, #12, PrinterStatus);
        END ELSE BEGIN
          (* Writeln('Printer error.');
          Writeln('Printer reports ', PrinterStatus.StatusString);
          Readln; *)
        END;


      END;
    END;

BEGIN
(*  InitPrinter(0, PrinterStatus);
  PrintChar(0, 'A', PrinterStatus);
  PrintChar(0, #10, PrinterStatus);
  PrintChar(0, 'Z', PrinterStatus);
  PrintString(0, 'This is a test', PrinterStatus);
  PrintChar(0, #12, PrinterStatus); *)
END.

