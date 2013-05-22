{$R+}
Unit ABUtil2;

Interface

USES Dos, Crt, Graph, NeoDbfio, Mouser;

CONST
{  MaxPages = 15; }
{  Height = 40;}
(*   MaxPages = 10;   {for last resort} *)
   MaxPages = 18;   {for last resort/Pac-Manager F7 function}
(*   MaxPages = 10;   {for nupacman}*)
(*   MaxPages = 7;    {for adr} *)

   Seed = 2328;
   Upper = 126; (*range of ascii characters that can be written*)
   Lower = 32;  (*to a .dbf file. . .32 is space, 126 is ~*)
   Range = 95;


TYPE
    AlphabetType = Array[1..26] OF Char;

    AreaSaverType = RECORD
      Wdt,Hgt : BYTE;
      AreaPtr : POINTER;
    END;

    PrinterConfigType = RECORD
                        Bold_On,
                        Bold_Off,
                        Compressed,
                        Underline_On,
                        Underline_Off,
                        FontSelect,
                        Reset : string;
                      END;


{    menutype = ARRAY[1..20] OF STRING[50];}
{    menutype = ARRAY[1..30] OF STRING[42];}
{   menutype = ARRAY[1..20] OF STRING[50]; }
(*   menutype = ARRAY[1..20] OF STRING[42]; *)
    menutype = ARRAY[1..20] OF STRING[65]; (*for nupacman*)

(*   MessageType = String[80]; *)
   MessageType = String[200];

   indextype = ARRAY[1..60] OF STRING;

   MenuEntryType = Record
                    Menuitem,
                    Description : string;
                  END;

(*   MenuEntryType = Record
                     Menuitem,
                     Description,
                     Description2,
                     Description3 : string;
                   END; *)

   PlusMenuType = Array[1..20] OF MenuEntryType;

   PlusMenuPtrType = ^PlusMenuType;

   PageType = Record
                Menusize : integer;
                Submenu : PlusMenuType;
              END;
   PageHandleType = ^PageType;

   BigBetterMenuType = RECORD
                   NumOfPages : byte;
                   Pages      : Array[1..MaxPages] OF PageHandleType;
                                                    (*  Record
                                                        Menusize : word;
                                                        submenu  : Plusmenutype;
                                                      END;  *)
                 END;

   BigMenuPtrType = ^BigBetterMenuType;

   BigMenuType = RECORD
                   NumOfPages : byte;
                   Pages      : Array[1..MaxPages] OF Record
                                                        Menusize : integer;
                                                        submenu  : menutype;
                                                      END;
                 END;


{***********************************************************************}
PROCEDURE SAY(Xloc, Yloc : integer; Message : MessageType; MessageColor : byte);

FUNCTION Pad(Instring : string; TargetLength : byte) : String;

PROCEDURE Enterdate(Xloc, Yloc : integer; VAR Date : string);

PROCEDURE SaveArea(Xorg,Yorg,NewWdt,NewHgt:BYTE;VAR Background:AreaSaverType);

PROCEDURE RestoreArea(Xorg,Yorg:BYTE;VAR Background:AreaSaverType);

FUNCTION trim(instring:STRING):STRING;

PROCEDURE DrawBox(Xloc, Yloc, Width, Height, Forecolor, BackColor, HeadFootColor, MessageColor : integer;
       Header, Footer, Message : string; UL, Top, UR, Side, LL, LR, Fill, Shadow : Char);

{ PROCEDURE readstring(xloc,yloc:INTEGER; sense:STRING; VAR inputstr:STRING;
    strlength: INTEGER; VAR exitkey : STRING; MaxLength : Integer; WrapAllowed : boolean; HotZone : byte);}



PROCEDURE Choosefrommenu(VAR menu:menutype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte);

PROCEDURE Choosefromindex(VAR menu:indextype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte);

PROCEDURE PulldownMenu (FileName : string; VAR Choice : string; Width : integer;
  VAR Exitkey : string; Forecolor, Backcolor : byte);

PROCEDURE MultipleChoiceMenu (FileName : string; VAR Choice : string; Width : integer;
  VAR Exitkey : string; MenuNum : integer);

PROCEDURE Screen(Message, Message2  : MessageType; Forecolor, Backcolor, MessageColor : integer);

PROCEDURE Frame(Color, Xloc,Yloc, Width, Height : Integer);

PROCEDURE Getchunky(Xloc,Yloc:INTEGER; VAR inputstr:STRING; Colr, Forecolor:INTEGER;
    Formatstr:STRING;VAR exitkey:STRING);

PROCEDURE newreadstring(xloc,yloc:INTEGER; sense:STRING; VAR inputstr:STRING;
    strlength: INTEGER; VAR exitkey : STRING; MaxLength : Integer; WrapAllowed : boolean;
    HotZone : byte; VAR Cursorplace : byte; VAR Insertmode : boolean (*; BlankMessage : string*) );

PROCEDURE readstring(xloc,yloc:INTEGER; sense:STRING; VAR inputstr:STRING;
    strlength: INTEGER; VAR exitkey : STRING;
    MessageColor,Backcolor: byte; EchoInput : boolean {; MaxLength : Integer; WrapAllowed : boolean; HotZone : byte});




PROCEDURE  WriteExitCode(KeyString, Filename : string);

FUNCTION ConvertCase(Instring : string) : String;

(* PROCEDURE DeleteARecord(VAR Filespecs : dbfheadertype; FileName : string;
  VAR FileRecord : Recordarraytype; Index : integer); *)

PROCEDURE EnhancedMenu(VAR bigmenu : bigmenutype; VAR PageNum : word;
  Xloc, Yloc, Width, Height : integer; VAR SubMenuNum : integer; VAR Exitkey : string;
  Forecolor, Backcolor : word; ResetWindow : char);

PROCEDURE Choosefromptrmenu(VAR menu: plusmenutype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte; DescX, DescY : byte);

PROCEDURE CFPTRmenu(VAR menu: plusmenutype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte; DescX, DescY : byte);



PROCEDURE EnhancedPtrMenu(VAR bigmenu : bigmenuptrtype; VAR PageNum : word;
  Xloc, Yloc, Width, Height : integer; VAR SubMenuNum : integer; VAR Exitkey : string;
  Forecolor, Backcolor : word; ResetWindow : char; DescX, DescY : byte; ExitKeys : string);

PROCEDURE EnhPtrMenu(VAR bigmenu : bigmenuptrtype; VAR PageNum : word;
  Xloc, Yloc, Width, Height : integer; VAR SubMenuNum : integer; VAR Exitkey : string;
  Forecolor, Backcolor : word; ResetWindow : char; DescX, DescY : byte);



PROCEDURE BuildBigMenu(Filename : string; VAR Menu : BigMenuType; VAR Height : integer;
  VAR FieldNum, FN2, FN3, FN4, StartRec, EndRec : integer; VAR Empty : boolean; BlanksAllow : Char);

PROCEDURE BuildBigPtrMenu(Filename : string; VAR Menu : BigMenuPtrType; VAR Height : integer;
  FieldNum, FN2, FN3, FN4, DescField, StartRec, EndRec : integer; VAR Empty : boolean; BlanksAllow : Char);

PROCEDURE DisposeMenu(VAR Menu : BigMenuPtrType);

PROCEDURE InitPtrMenu(VAR Menu : BigMenuPtrtype);

PROCEDURE BBPtrMenu(Filename : string; VAR Menu : BigMenuPtrType; VAR Height : integer;
  FieldNum, FN2, FN3, FN4, DescField, StartRec, EndRec : integer; VAR Empty : boolean);



PROCEDURE BuildBigMenu2(Filename : string; VAR Menu : BigMenuType; VAR Height : integer;
{  VAR FieldNum , StartRec, EndRec : integer; } VAR Empty : boolean; BlanksAllow : Char);



FUNCTION st(instring:STRING;newlength:BYTE):STRING;

PROCEDURE Center(Message : MessageType; Ypos : byte; MessageColor : byte);

PROCEDURE DialogueBox(VAR Reply : string; Prompt : string; VAR Colorscheme : byte);

PROCEDURE CopyFile(filename1,filename2:STRING);

PROCEDURE DeleteFile(Filename : string);

PROCEDURE GetPrinterConfig(Filename : string; VAR Config : PrinterConfigType);

PROCEDURE ChoosefromAlphabet(VAR menu : Alphabettype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte);

FUNCTION RTrim(Instring : string) : string;

FUNCTION LTrim(Instring : string) : string;

PROCEDURE RestoreScreen;

FUNCTION EncryptPassword(Instring : string) : string;

FUNCTION DecryptPassword(Instring : string) : string;

FUNCTION IsBlank(Instring : string) : boolean;

Implementation
{**********************************************************************}

PROCEDURE SAY(Xloc, Yloc : integer; Message : Messagetype; MessageColor : byte);
  VAR
    OldColor : word;
    Tempstring : string;
    WindWidth : byte;
    Wrap : boolean;
    Done : boolean;
    i,
    CutPoint : byte;
  BEGIN
    OldColor := TextAttr;
    Wrap := FALSE;
    WindWidth := Lo(WindMax) - Lo(WindMin);
    IF Length(Message) > (WindWidth - 3) THEN BEGIN  {allow for the shadow of the box}
      Wrap := TRUE;
      Done := FALSE;
      i := WindWidth - 3;
      While (i > 1) AND Not(Done) DO BEGIN
        If Message[i] = ' ' THEN BEGIN
          Done := TRUE;
          CutPoint := i;
        END ELSE
          Dec(i, 1);
      END;
      Tempstring := Copy(Message, CutPoint + 1, (Length(Message) - Cutpoint + 1));
      Delete(Message, CutPoint + 1, (Length(Message) - Cutpoint + 1));
      Message := Trim(Message);
    END;

    TextColor(MessageColor);
    GoToXY(Xloc, Yloc);
    Write(Message);
    IF Wrap THEN BEGIN
      GoToXY(Xloc, Yloc + 1);
      Write(Trim(Tempstring));
    END;
    TextAttr := OldColor;
  END;


FUNCTION Pad(Instring : string; TargetLength : byte) : String;
 VAR
   i : integer;
   startpos : byte;
 BEGIN

   If Length(instring) < TargetLength THEN BEGIN
     Startpos := Length(instring) + 1;
     For i := Startpos  to TargetLength DO
       Instring := Instring + ' ';
   END;
   Pad := Instring;

 END;


PROCEDURE Enterdate(Xloc, Yloc : integer; VAR Date : string);

VAR
  Ch : char;
  index : integer;


BEGIN

  Date := '';
  Index := 0;
  CH := ' ';
  GotoXY(Xloc + 1 , Yloc);
  TextBackground(Black);
  TextColor(LightGray);
  Write('  ');
  TextColor(White);
  TextBackground(Blue);
  Write('/');
  TextBackground(Black);
  TextColor(LightGray);
  Write('  ');
  TextColor(White);
  TextBackground(Blue);
  Write('/');
  TextBackground(Black);
  TextColor(LightGray);
  Write('  ');
  GotoXY(Xloc+1, Yloc);


  Repeat
    Ch := Readkey;
    CASE Ch OF
      '0'..'9' : BEGIN
                   Date := Date + Ch;
                   CASE index Of
                     1 :  GoToXY(WhereX, Yloc);
                     2 :  GoToXY(WhereX+1, Yloc);
                     3 :  GoToXY(WhereX, Yloc);
                     4 :  GoToXY(WhereX+1, Yloc);
                     5 :  GoToXY(WhereX, Yloc);
                     6 :  GoToXY(WhereX, Yloc);
                   END;
                   Write(Ch);
                   Inc(index);

                 END;

        #8 : BEGIN
               CASE index of
                 1, 2, 4, 6 : BEGIN
                                GoToXY(WhereX - 1, Yloc);
                                Dec(index);
                                Delete(Date, Length(Date), 1);
                              END;

                 3, 5       : BEGIN
                                GoToXY(WhereX - 2, Yloc);
                                Dec(index);
                                Delete(Date, Length(Date), 1);
                              END;
               END;
             END;
    END;
  Until Ch = #13;
  CASE Length(Date) OF
    5 : BEGIN
        END;

    6 : BEGIN
          Date := Date[1] + Date[2] + '/' + Date[3] + Date[4] + '/' + Date[5] + Date[6];
        END;
  END;

END;



  PROCEDURE SaveArea(Xorg,Yorg,NewWdt,NewHgt:BYTE;VAR Background:AreaSaverType);
  {************************************************************************}
  {************************************************************************}
  CONST
    scrWdt = 80; scrHgt = 25;
  TYPE
    ColorChar     = RECORD
      C : CHAR; {Ascii char.}
      A : BYTE; {Color foreground/background attribute.}
    END;
    TextScreenType = ARRAY [1..scrHgt,1..scrWdt] OF ColorChar;
  VAR
    (* TextScreen: TextScreenType ABSOLUTE $B800:$0000;{Mapped onto video mem.} *)
    col,row,right,bottom : BYTE;
    offset,Sizeofwdt : WORD;
  BEGIN
(*    Right:=Xorg+NewWdt-1; bottom:=Yorg+NewHgt-1;
    WITH Background DO BEGIN
      Wdt:=NewWdt; Hgt:=NewHgt; SizeofWdt:=Wdt*2;
      GETMEM(AreaPtr,Hgt*SizeofWdt);
      offset:=0;
      FOR row:=Yorg TO bottom DO BEGIN
        MOVE(TextScreen[row,Xorg],PTR(SEG(AreaPtr^),OFS(AreaPtr^)+offset)^,SizeofWdt);
        INC(offset,SizeofWdt);
      END;
    END;
    *)
  END;

  PROCEDURE RestoreArea(Xorg,Yorg:BYTE;VAR Background:AreaSaverType);
  {************************************************************************}
  {************************************************************************}
  CONST
    scrWdt = 80; scrHgt = 25;
  TYPE
    ColorChar     = RECORD
      C : CHAR; {Ascii char.}
      // A : BYTE; {Color foreground/background attribute.}
    END;
    TextScreenType = ARRAY [1..scrHgt,1..scrWdt] OF ColorChar;
  VAR
    (* TextScreen: TextScreenType ABSOLUTE $B800:$0000;{Mapped onto video mem.} *)
    col,row,right,bottom : BYTE;
    offset,Sizeofwdt : WORD;
  BEGIN
    (*
    WITH Background DO BEGIN
      Right:=Xorg+Wdt-1; bottom:=Yorg+Hgt-1;
      SizeofWdt:=Wdt*2;
      offset:=0;
      FOR row:=Yorg TO bottom DO BEGIN
        MOVE(PTR(SEG(AreaPtr^),OFS(AreaPtr^)+offset)^,TextScreen[row,Xorg],SizeofWdt);
        INC(offset,SizeofWdt);
      END;
      FREEMEM(AreaPtr,Hgt*SizeofWdt);
      AreaPtr:=NIL;
    END;
    *)
  END;


FUNCTION trim(instring:STRING):STRING;
  {***************************************************}
  {*  Returns a copy of instring with leading and    *}
  {*  trailing blanks removed.                       *}
  {***************************************************}
  VAR first,last : INTEGER;
  BEGIN
    first:=1; last:=LENGTH(instring);
    WHILE (first<=last) AND (instring[first]=' ') DO INC(first,1);
    IF first<=last THEN BEGIN
      WHILE (instring[last]=' ') DO DEC(last,1);
      trim:=COPY(instring,first,last+1-first);
    END ELSE trim:='';
  END;


PROCEDURE DrawBox(Xloc, Yloc, Width, Height, Forecolor, BackColor, HeadFootColor, MessageColor : integer;
       Header, Footer, Message : string; UL, Top, UR, Side, LL, LR, Fill, Shadow : Char);

{***********************************************************************}
{PROCEDURE for drawing a text box with a border and shadow.  Border style
passed as the char parameters.  Other parameters include upper and lower
labels for the box and a central text string "message."}
{ASCII Characters for different styles are:
  double line border:      #201 ,#205, #187, #186, #200, #188
  single line border:      #218, #196, #191, #179, #192, #217
  double side, single top: #214, #196, #183, #186, #211, #189
  single side, double top: #213, #205, #184, #179, #212, #190   }

{***********************************************************************}

VAR
  i, j : integer;
  OldColor : byte;
  ch : char;

BEGIN

  OldColor := TextAttr;
  Window(Xloc,Yloc, Xloc + (Width + 2), Yloc + (Height+1));
  TextBackground(BackColor);
  TextColor(Forecolor);
  Write(UL);
  For i := 1 to (width - 2) DO
    Write(Top);
  Write(UR);

  TextAttr := OldColor;
  Writeln(Fill,Fill);

  TextColor(ForeColor);
  TextBackground(BackColor);
  For i := 1 to (height - 2) DO BEGIN
    Textcolor(Forecolor);
    Write(Side);

    Write(' ':(width - 2));
    Write(Side);
    TextAttr := OldColor;
    Textcolor(Black);
    Write(Shadow);
    Writeln(Shadow);
    TextColor(ForeColor);
    TextBackground(BackColor);
  END;
  TextColor(ForeColor);
  TextBackground(BackColor);
  Write(LL);
  For i := 1 to (width - 2) DO
    Write(Top);
  Write(LR);
  TextAttr := OldColor;
  TextColor(Black);
  Write(Shadow);
  Writeln(Shadow);

  TextAttr := OldColor;
  Write(Fill,Fill);
  TextColor(Black);
  For i := 1 to (Width - 1) DO
    Write(Shadow);
    Writeln(Shadow);




  TextColor(HeadFootColor);
  TextBackground(BackColor);
  GoToXY(((Width Div 2) - (Length(Header)) DIV 2),1);
  Write(Header);
  TextColor(MessageColor);
  GoToXY(((Width Div 2) - (Length(Footer)) DIV 2),Height);
  Write(Footer);
  GoToXY(((Width Div 2) - (Length(Message)) DIV 2), Height DIV 2);
  Write(Message);
  GoToXY(Lo(WindMax),Hi(WindMax));
 { TextColor(BackColor);
  Write(' ');}
  TextAttr := OldColor;



END;



PROCEDURE readstring(xloc,yloc:INTEGER; sense:STRING; VAR inputstr:STRING;
    strlength: INTEGER; VAR exitkey : STRING; MessageColor,BackColor : byte; EchoInput : boolean);
  {***************************************************}
  {*  User input for string fields of set length.    *}
  {***************************************************}
  VAR
    cursorplace,err           : INTEGER;
    inchar                    : CHAR;
    oldcolors                 : BYTE;
    insertmode                : BOOLEAN;
    NoEchoCnt                 : BYTE;
    AsteriskCnt               : BYTE;
  BEGIN
    oldcolors:=TEXTATTR;
    TEXTCOLOR(MessageColor);
    TEXTBACKGROUND(BackColor);
    exitkey:=''; cursorplace:=1; insertmode:=FALSE;
    inputstr:=COPY(inputstr,1,strlength);
    GOTOXY(xloc,yloc);
    WRITE(inputstr);
    GOTOXY(xloc+cursorplace-1,yloc);
    REPEAT
      IF KEYPRESSED THEN BEGIN
        inchar:=READKEY;
        IF POS('C',sense)>0 THEN inchar:=UPCASE(inchar);
        CASE inchar OF
          #1  :  exitkey := 'CTRLA';
          #2  :  exitkey := 'CTRLB';
          #3  :  exitkey := 'CTRLC';
          #4  :  exitkey := 'CTRLDEL';
          #5  :  exitkey := 'CTRLE';
          #6  :  exitkey := 'CTRLF';
          #7  :  exitkey := 'CTRLG';
          #8  : BEGIN  { User pressed backspace.}
                  IF cursorplace>1 THEN BEGIN
                    DEC(cursorplace,1); DELETE(inputstr,cursorplace,1);
                    inputstr:=inputstr+' ';
                  END;
                END;
          #9  :  {ctrl + i} exitkey := 'CTRLINS';
          #10 :  exitkey := 'CTRLJ';
          #11 : {ctrl + k}  exitkey := 'CUT';
          #12 :  exitkey := 'CTRLL';
          #13 :  exitkey:='RETURN';
          #14 :  exitkey := 'CTRLN';
          #15 :  exitkey := 'CTRLO';
          #16 :  {ctrl+P} exitkey :='PASTE';
          #17 :   exitkey := 'CTRLQ';
          #18  :  Exitkey := 'CTRLR';
          #19 :   Exitkey := 'CTRLS';
          #20 :   Exitkey := 'CTRLT';
          #21 :   Exitkey := 'CTRLU';
          #22 :   Exitkey := 'CTRLV';
          #23 :   Exitkey := 'CTRLW';
     (*     #24 :   Exitkey := 'CTRLX';
          #25 :   Exitkey := 'CTRLY';  *)
         {CTRLX, Y in PM, Up arrow, down arrow in ADR}
         {24 = Up arrow/#25 = down arrow, want these to work as they do}
          #26 :   Exitkey := 'CTRLZ';
          #27 :   exitkey:='ESC';
(*        #32 : BEGIN {space bar}
                      insert(' ',inputstr, cursorplace);
                      delete(inputstr,strlength,1);
                      inc(cursorplace);
                 END; *)

          #0  : BEGIN  { User pressed extended key.}
                  inchar:=READKEY;
                  CASE inchar OF
                    #4  :  Exitkey := 'CTRLDEL';
                    #6  :  Exitkey := 'CTRLINS';
                    #59 :  Exitkey := 'F1';
                    #60 :  ExitKey := 'F2';
                    #61..#67 : BEGIN
                               END;  {Function keys 3-10.}
                    #68 : Exitkey := 'F10';
                    #71 : cursorplace:=1; {home}
                    #79 : cursorplace:=strlength; {END}
                    #77 : {right arrow}
                      IF cursorplace<strlength THEN INC(cursorplace,1);
                    #75 : {left arrow}
                      IF cursorplace>1 THEN DEC(cursorplace,1);
                    #72 : exitkey:='UP';  {up arrow}
                    #80 : exitkey:='DOWN'; {down arrow}
                    #73 : exitkey:='PAGEUP';  {pageup}
                    #81 : exitkey:='PAGEDOWN'; {pagedown}
                    #82 : insertmode:=NOT(insertmode); { Toggle insert }
                    #83 : BEGIN  { Delete }
                            DELETE(inputstr,cursorplace,1);
                            inputstr:=inputstr+' ';
                          END;
                    #117 : BEGIN   {ctrl + end}
                             cursorplace := Length(RTrim(inputstr)) + 1;
                           END;
                    #119 : exitkey := 'CTRLHOME';
                    #120 : exitkey := 'ALT1';
                    #121 : exitkey := 'ALT2';
                    #122 : exitkey := 'ALT3';
                    #123 : exitkey := 'ALT4';
                    #124 : exitkey := 'ALT5';
                    #125 : exitkey := 'ALT6';
                    #126 : exitkey := 'ALT7';
                    #127 : exitkey := 'ALT8';
                    #128 : exitkey := 'ALT9';
                    #129 : exitkey := 'ALT0';
                  END; {END, CASE}
                END; {END, BEGIN}
          ELSE; BEGIN
                  If insertmode THEN BEGIN
                    DELETE(inputstr,strlength, 1);
                    INSERT(inchar, inputstr, cursorplace);
                  END ELSE
                     inputstr[cursorplace] := inchar;
                  IF cursorplace<strlength THEN INC(cursorplace,1);
                END;
        END;  { End of case statement.}
        GOTOXY(xloc,yloc);
        If EchoInput THEN
          WRITE(inputstr)
        ELSE  BEGIN
          AsteriskCnt := Length(Trim(Inputstr));
          For NoEchoCnt := 1 to AsteriskCnt DO
          Write('*');
        END;
        GOTOXY(xloc+cursorplace-1,yloc);
      END;
     UNTIL exitkey<>'';
    inputstr:=COPY(inputstr,1,strlength);
    TextAttr := oldcolors;
    IF EchoInput THEN BEGIN
      TextColor(MessageColor);
      GOTOXY(xloc,yloc);
      WRITELN(inputstr);
    END;
    TEXTATTR:=oldcolors;
  END;  { End of readstring. }


PROCEDURE newreadstring(xloc,yloc:INTEGER; sense:STRING; VAR inputstr:STRING;
    strlength: INTEGER; VAR exitkey : STRING; MAXLength : integer; WrapAllowed : boolean;
    HotZone : byte; VAR Cursorplace : byte; VAR InsertMode : boolean (*; BlankMessage : string*));
  {***************************************************}
  {*  User input for string fields of set length.    *}
  {***************************************************}
  VAR
    oldlength,
    err           : INTEGER;
    inchar                    : CHAR;
    oldcolors                 : BYTE;
{    insertmode                : BOOLEAN; }
    i                         : BYTE;
    ScreenCount : longint;
    BlankDelay : longint;
    PauseDelay : word;

(*   PROCEDURE GetDelay(VAR HowLongBeforeWaiting : longint; VAR HowLongtoWait : word; VAR BlankMessage : string);
     VAR
       DelayStr : string;
       Infile : text;
       DirInfo: SearchRec;
       DelayNum : longint;
       Errcode : integer;
     BEGIN
       FindFirst('delay.txt', Archive, DirInfo);
       IF DosError = 0 THEN BEGIN
         Assign(infile, 'delay.txt');
         Reset(infile);
         IF NOT EOF(infile) THEN BEGIN
           Readln(infile, Delaystr);
           Val(DelayStr, DelayNum, Errcode);
         IF Errcode = 0 THEN
           HowLongBeforeWaiting := DelayNum
         ELSE
           HowLongBeforeWaiting := 100000;
         END;
         IF NOT EOF(infile) THEN BEGIN
           Readln(infile, Delaystr);
           Val(DelayStr, DelayNum, Errcode);
         IF Errcode = 0 THEN
           HowLongToWait := DelayNum
         ELSE
           HowLongToWait := 1000;
         END;
         IF NOT EOF(infile) THEN BEGIN
           Readln(infile, BlankMessage);
           BlankMessage := Trim(BlankMessage);
         END ELSE
           BlankMessage := 'Press any key to return to program';
         Close(infile);
       END ELSE BEGIN
         HowLongBeforeWaiting := 100000;
         HowLongToWait := 1000;
         BlankMessage := 'Press any key to return to program'
       END;
     END; *)

(*   PROCEDURE SaveScreen(VAR Screencount : longint; ScreenMessage : string);
     VAR
       ScreenArea : areasavertype;
       CurrentMin, CurrentMax : word;
       Restart : char;
       XX, YY : byte;
       MsgColor : byte;
       Blanker : string;
       Counter : byte;
       OldCursorplace : byte;
       Catchchar : char;


     BEGIN
       CurrentMin := WindMin; CurrentMax := WindMax;
       Window(1, 1, 80, 25);
       SaveArea(1, 1, 80, 25, ScreenArea);
       TextBackground(Black);
       OldCursorPlace := Cursorplace;
       Clrscr;
       Randomize;
       Blanker := '';
       ScreenMessage := Trim(ScreenMessage);
       For counter := 1 to Length(ScreenMessage) DO
         Blanker := Blanker + ' ';
       While NOT Keypressed DO BEGIN
       {Repeat}
         XX := Random(80 - Length(ScreenMessage)) + 1;
         YY := Random(20)+1;
         MsgColor := Random(15)+1;
         Say(XX, YY, ScreenMessage, MsgColor);
         Delay(PauseDelay);
         Say(XX, YY, Blanker, Black);
       {Until KeyPressed;}
       END;
       CatchChar := readkey;
       IF CatchChar = #0 THEN BEGIN
         Catchchar := Readkey;
         WRITE('FANCY KEY!');
         Readln;
       END;
       ScreenCount := 0;
       RestoreArea(1, 1, ScreenArea);
       WindMin := CurrentMin; WindMax := CurrentMax;
       Cursorplace := OldCursorplace;
     END; *)


  BEGIN
    Screencount := 0;
    (* GetDelay(BlankDelay, PauseDelay, BlankMessage); *)
    oldcolors:=TEXTATTR; TEXTCOLOR(LightGray); TEXTBACKGROUND(Black);
    exitkey:=''; {cursorplace:=1;  insertmode:=FALSE;}
    inputstr:=COPY(inputstr,1,strlength);
    GOTOXY(xloc,yloc); WRITE(inputstr); GOTOXY(xloc+cursorplace-1,yloc);
    REPEAT
      IF NOT KEYPRESSED THEN BEGIN
        Inc(Screencount);
 (*     GotoXY(2,2);
        Write('      ');
        GoToXY(2,2);
        Write(Screencount);
       Delay(10); *)
(*        IF Screencount > BlankDelay THEN
          SaveScreen(Screencount, BlankMessage); *)

      END ELSE
{      IF KEYPRESSED THEN } BEGIN
        inchar:=READKEY;
        Screencount := 0;
        IF POS('C',sense)>0 THEN inchar:=UPCASE(inchar);
        CASE inchar OF
          #1  : BEGIN
                  exitkey := 'CTRLA';
                END;
          #2  : BEGIN  { User pressed ctrl + B}
                  exitkey := 'CTRLB';
                END;
          #3  : BEGIN  { User pressed ctrl + C}
                  exitkey := 'CTRLC';
                END;

          #4  : BEGIN  { User pressed ctrl + D}
                  exitkey := 'CTRLDEL';
                END;
          #5  : BEGIN
                  exitkey := 'CTRLE';
                END;
          #6  : BEGIN {User pressed ctrl + F}
                  exitkey := 'CTRLF';
                END;
          #7  : BEGIN
                  exitkey := 'CTRLG';
                END;
(*          #8  : BEGIN
                  exitkey := 'CTRLH';
                END; *)

          #9  : BEGIN  {User pressed ctrl + i}
                  exitkey := 'CTRLINS';
                END;
          #10 : BEGIN
                  exitkey := 'CTRLJ';
                END;
          #11 : BEGIN  {User pressed ctrl + k}
                  exitkey := 'CUT';
                END;
          #12 : BEGIN
                  exitkey := 'CTRLL';
                END;

          #13 : BEGIN  { User pressed return.}
                  exitkey:='RETURN';
                  cursorplace := 1;
                END;
          #14 : BEGIN
                  exitkey := 'CTRLN';
                END;
          #15 : BEGIN
                  exitkey := 'CTRLO';
                END;
          #16 : BEGIN
                  exitkey :='PASTE';
                END;
          #17 : BEGIN
                  exitkey := 'CTRLQ';
                END;
         #18  : BEGIN  {User pressed ctrl + R}
                  Exitkey := 'CTRLR';
                END;
         #19 : BEGIN
                 Exitkey := 'CTRLS';
               END;
         #20 : BEGIN
                 Exitkey := 'CTRLT';
               END;
         #21 : BEGIN
                 Exitkey := 'CTRLU';
               END;
         #22 : BEGIN
                 Exitkey := 'CTRLV';
               END;
         #23 : BEGIN
                 Exitkey := 'CTRLW';
               END;
         #24 :   Exitkey := 'CTRLX';
         #25 :   Exitkey := 'CTRLY';
         {24 = Up arrow/#25 = down arrow, want these to work as they do}
         #26 : BEGIN
                 Exitkey := 'CTRLZ';
               END;
          #27 : BEGIN  { User pressed escape.}
                  exitkey:='ESC';
                END;
       (*   #32 : BEGIN {space bar}
                   If cursorplace < maxlength THEN BEGIN
                      insert(' ',inputstr, cursorplace);
                      delete(inputstr,strlength,1);
                      inc(cursorplace);
                   END ELSE BEGIN
                     Exitkey := 'WRAP';
                     cursorplace := 1;
                   END;

                END; *)

          #8  : BEGIN  { User pressed backspace.}
                  IF cursorplace>1 THEN BEGIN
                    DEC(cursorplace,1); DELETE(inputstr,cursorplace,1);
                    inputstr:=inputstr+' ';
                  END;
                END;
          #0  : BEGIN  { User pressed extended key.}
                  inchar:=READKEY;
                  CASE inchar OF
                    #4  : Exitkey := 'CTRLDEL'; {Ctrl}{Del}
                    #6  : Exitkey := 'CTRLINS'; {Ctrl}{Ins}
                    #59 : Exitkey := 'F1';
                    #60 : ExitKey := 'F2';
                    #61 : Exitkey := 'F3';
                    #62 : Exitkey := 'F4';
                    #63 : Exitkey := 'F5';
                    #64 : Exitkey := 'F6';
                    #65 : Exitkey := 'F7';
                    #66 : Exitkey := 'F8';
                    #67 : Exitkey := 'F9';
                    #68 : Exitkey := 'F10';
                    #71 : cursorplace:=1; {home}
                    #72 : BEGIN
                            exitkey:='UP';  {up arrow}
                            cursorplace := 1;
                          END;
                    #73 : exitkey:='PAGEUP';  {pageup}
                    #75 : BEGIN {left arrow}
                            IF cursorplace>1 THEN DEC(cursorplace,1);
                            IF cursorplace = 1 THEN BEGIN
                              Exitkey := 'UP';
                              cursorplace := strlength;
                            END;
                          END;
                    #77 : BEGIN {right arrow}
                            IF cursorplace<strlength THEN INC(cursorplace,1);
                            IF cursorplace=strlength THEN BEGIN
                              EXITKEY := 'WRAP';
                              cursorplace := 1;
                            END;
                          END;
                    #79 : cursorplace:=strlength; {END}
                    #80 : BEGIN
                            exitkey:='DOWN'; {down arrow}
                            cursorplace := 1;
                          END;
                    #81 : exitkey:='PAGEDOWN'; {pagedown}
                    #82 : BEGIN
                            insertmode:=NOT(insertmode); { Toggle insert }
                            exitkey := 'INS';
                          END;
                    #83 : BEGIN  { Delete }
                            DELETE(inputstr,cursorplace,1);
                            inputstr:=inputstr+' ';
                          END;
                    #117: BEGIN   {ctrl + end}
                            IF Length(Trim(inputstr)) < MaxLength - 2 THEN
                               cursorplace := Length(RTrim(inputstr)) + 2
                            ELSE
                               cursorplace := MaxLength;
                          END;

                    #119 : exitkey := 'CTRLHOME';
                    #120 : exitkey := 'ALT1';
                    #121 : exitkey := 'ALT2';
                    #122 : exitkey := 'ALT3';
                    #123 : exitkey := 'ALT4';
                    #124 : exitkey := 'ALT5';
                    #125 : exitkey := 'ALT6';
                    #126 : exitkey := 'ALT7';
                    #127 : exitkey := 'ALT8';
                    #128 : exitkey := 'ALT9';
                    #129 : exitkey := 'ALT0';
                  END {CASE};
                END; {OF #0 BEGIN..END}
             ELSE BEGIN   {CASE..ELSE}
               IF insertmode THEN BEGIN
                 DELETE(inputstr,strlength,1);
                 INSERT(inchar,inputstr,cursorplace);
               END ELSE
                 inputstr[cursorplace]:=inchar;
               END;
               IF cursorplace<strlength THEN
                  INC(cursorplace,1)
               ELSE IF cursorplace=strlength THEN BEGIN
                  EXITKEY := 'WRAP';
                  Cursorplace := 1;
             END;
           END;  { End of case statement.}

             GOTOXY(xloc,yloc); WRITE(inputstr); GOTOXY(xloc+cursorplace-1,yloc);

{        IF length(trim(inputstr)) = MaxLength THEN
          Exitkey := 'RETURN';
        IF WrapAllowed THEN BEGIN
          IF cursorplace >= HotZone AND Pos(substr, S) > 0
        END; }

        END; (* ELSE BEGIN
        Inc(Screencount);
        GotoXY(2,2);
        Write(Screencount);
      END; *)

{      IF Screencount > BlankDelay THEN
        SaveScreen(Screencount, BlankMessage); }
(*      GOTOXY(xloc,yloc); WRITE(inputstr); GOTOXY(xloc+cursorplace-1,yloc); *)

    UNTIL exitkey<>'';
    inputstr:=COPY(inputstr,1,strlength);
    TEXTATTR:=oldcolors; GOTOXY(xloc,yloc); WRITELN(inputstr);
  END;  { End of readstring. }


PROCEDURE Choosefrommenu(VAR menu: menutype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte);
  {************************************************************}
  {* Presents the user with a list of choices and gets the    *}
  {* user's choice through cursor keys or mouse.              *}
  {************************************************************}
  CONST barcolor = YELLOW; charwdt=8; charhgt=8;
  VAR
    row,column  : INTEGER;
    instr       : CHAR;
    lbutton,rbutton,oldlbutton  : BOOLEAN;
    mousex,mousey : INTEGER;

    FUNCTION inview(VAR xloc,yloc:INTEGER; VAR thisview: VIEWPORTTYPE):BOOLEAN;
    {**************************************************************}
    BEGIN
      WITH thisview DO BEGIN
        IF (xloc<=x2) AND (yloc<=y2) AND (xloc>=x1) AND (yloc>=y1) THEN
          inview:=TRUE
        ELSE inview:=FALSE;
      END;
    END;

    PROCEDURE highlight(xloc,yloc:INTEGER;instr:STRING);
    {************************************************************}
    VAR blanker : STRING; oldcolor : BYTE;
    BEGIN
      hidemouse; oldcolor:=TEXTATTR;
      TEXTBACKGROUND(Forecolor); TEXTCOLOR(Black);
      GOTOXY(xloc,yloc); WRITE(instr);
      TEXTATTR:=oldcolor; showmouse;
    END;

    PROCEDURE indexsearch(xpick,ypick:INTEGER);
    VAR xtest,ytest,strlength:INTEGER; found:BOOLEAN;
      choiceview : VIEWPORTTYPE; indexnum:INTEGER;
    BEGIN
      { Adjust mouse to screen coords.}
      xpick:=(xpick DIV charwdt)-LO(WINDMIN);
      ypick:=(ypick DIV charhgt)-HI(WINDMIN);
      found:=FALSE;  indexnum:=0;
      REPEAT
        INC(indexnum,1);
        xtest:=Xloc+(((indexnum-1) DIV Hgt)*Wdt);
        ytest:=Yloc+(indexnum-1) MOD Hgt;
        strlength:=LENGTH(menu[indexnum]);
        WITH choiceview DO BEGIN
          x1:=xtest; y1:=ytest-1; x2:=xtest+strlength; y2:=y1+1;
        END;
        IF inview(xpick,ypick,choiceview) THEN BEGIN
          found:=TRUE;
          hidemouse;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          Choice:=indexnum;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
          exitkey:='RETURN';
          showmouse;
        END;
      UNTIL (indexnum>=menusize) OR found;
    END;


  BEGIN
    exitkey:='';
    lbutton:=FALSE; rbutton:=FALSE; mousewait; { Clear mouse variables. }
    TEXTCOLOR(Forecolor); TEXTBACKGROUND(Backcolor);
    FOR Row:=1 TO menusize DO BEGIN   { PRINT OUT MENU. }
      GOTOXY(Xloc+(((Row-1) DIV Hgt)*Wdt)+1,Yloc+((Row-1) MOD Hgt)+1);
      WRITE(menu[Row]);
    END;

    If Choice > Menusize THEN Choice := Menusize;

    highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
    showmouse;
    REPEAT
      oldlbutton:=lbutton;
      getmouse(mousex,mousey,lbutton,rbutton);
      IF lbutton AND NOT(oldlbutton) THEN BEGIN  { Lbutton just pressed. }
        indexsearch(mousex,mousey);
      END;
      IF KEYPRESSED THEN BEGIN
        Instr:=READKEY;
        IF Instr=#0 THEN BEGIN
          Instr:=READKEY;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          CASE instr OF
            #59 : BEGIN         {f1}
                    Exitkey := 'F1';
                  END;
            #60 : BEGIN
                    Exitkey := 'F2';
                  END;

            #71 : BEGIN
                    ExitKey := 'HOME';
                  END;

            #72 : BEGIN   { Up arrow. }
                    Choice:=Choice-1;
                  END;

            #73 : BEGIN
                    ExitKey := 'PAGEUP';
                  END;

            #75 : BEGIN   {  Left arrow. }
                    Choice:=Choice-Hgt;
                  END;


            #77 : BEGIN   {  Right arrow. }
                    Choice:=Choice+Hgt;
                  END;

            #79 : BEGIN
                    ExitKey := 'END';
                  END;

            #80 : BEGIN   { Down arrow. }
                    Choice:=Choice+1;
                  END;

            #81 : BEGIN
                    Exitkey := 'PAGEDOWN';
                  END;

            #83 : BEGIN
                    Exitkey := 'DEL';
                  END;



          END;
          IF Choice>menusize THEN Choice:=1;             { WRAP. }
          IF Choice<1 THEN Choice:=menusize;

          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
        END ELSE BEGIN
          CASE Instr OF
            #13 : exitkey:='RETURN';
            #27 : exitkey:='ESC';
          END;
        END;
      END;
    UNTIL exitkey <> '';
    hidemouse;
  END;  { End of Choosefrommenu. }





PROCEDURE Choosefromindex(VAR menu:indextype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte);
  {************************************************************}
  {* Presents the user with a list of choices and gets the    *}
  {* user's choice through cursor keys or mouse.              *}
  {************************************************************}
  CONST barcolor = YELLOW; charwdt=8; charhgt=8;
  VAR
    row,column  : INTEGER;
    instr       : CHAR;
    lbutton,rbutton,oldlbutton  : BOOLEAN;
    mousex,mousey : INTEGER;

    FUNCTION inview(VAR xloc,yloc:INTEGER; VAR thisview:VIEWPORTTYPE):BOOLEAN;
    {**************************************************************}
    BEGIN
      WITH thisview DO BEGIN
        IF (xloc<=x2) AND (yloc<=y2) AND (xloc>=x1) AND (yloc>=y1) THEN
          inview:=TRUE
        ELSE inview:=FALSE;
      END;
    END;

    PROCEDURE highlight(xloc,yloc:INTEGER;instr:STRING);
    {************************************************************}
    VAR blanker : STRING; oldcolor : BYTE;
    BEGIN
      hidemouse; oldcolor:=TEXTATTR;
      TEXTBACKGROUND(Forecolor); TEXTCOLOR(Black);
      GOTOXY(xloc,yloc); WRITE(instr);
      TEXTATTR:=oldcolor; showmouse;
    END;

    PROCEDURE indexsearch(xpick,ypick:INTEGER);
    VAR xtest,ytest,strlength:INTEGER; found:BOOLEAN;
      choiceview : VIEWPORTTYPE; indexnum:INTEGER;
    BEGIN
      { Adjust mouse to screen coords.}
      xpick:=(xpick DIV charwdt)-LO(WINDMIN);
      ypick:=(ypick DIV charhgt)-HI(WINDMIN);
      found:=FALSE;  indexnum:=0;
      REPEAT
        INC(indexnum,1);
        xtest:=Xloc+(((indexnum-1) DIV Hgt)*Wdt);
        ytest:=Yloc+(indexnum-1) MOD Hgt;
        strlength:=LENGTH(menu[indexnum]);
        WITH choiceview DO BEGIN
          x1:=xtest; y1:=ytest-1; x2:=xtest+strlength; y2:=y1+1;
        END;
        IF inview(xpick,ypick,choiceview) THEN BEGIN
          found:=TRUE;
          hidemouse;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          Choice:=indexnum;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
          exitkey:='RETURN';
          showmouse;
        END;
      UNTIL (indexnum>=menusize) OR found;
    END;


  BEGIN
    exitkey:='';
    lbutton:=FALSE; rbutton:=FALSE; mousewait; { Clear mouse variables. }
    TEXTCOLOR(Forecolor); TEXTBACKGROUND(Backcolor);
    FOR Row:=1 TO menusize DO BEGIN   { PRINT OUT MENU. }
      GOTOXY(Xloc+(((Row-1) DIV Hgt)*Wdt)+1,Yloc+((Row-1) MOD Hgt)+1);
      WRITE(menu[Row]);
    END;

    highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
    showmouse;
    REPEAT
      oldlbutton:=lbutton;
      getmouse(mousex,mousey,lbutton,rbutton);
      IF lbutton AND NOT(oldlbutton) THEN BEGIN  { Lbutton just pressed. }
        indexsearch(mousex,mousey);
      END;
      IF KEYPRESSED THEN BEGIN
        Instr:=READKEY;
        IF Instr=#0 THEN BEGIN
          Instr:=READKEY;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          CASE instr OF
            #59 : BEGIN
                    Exitkey := 'F1'
                  END;
            #60 : BEGIN
                    Exitkey := 'F2';
                  END;
            #80 : BEGIN   { Down arrow. }
                    Choice:=Choice+1;
                  END;
            #83 : BEGIN
                    Exitkey := 'DEL';
                  END;
            #72 : BEGIN   { Up arrow. }
                    Choice:=Choice-1;
                  END;
            #77 : BEGIN   {  Right arrow. }
                    Choice:=Choice+Hgt;
                  END;
            #75 : BEGIN   {  Left arrow. }
                    Choice:=Choice-Hgt;
                  END;
          END;
          IF Choice>menusize THEN Choice:=1;             { WRAP. }
          IF Choice<1 THEN Choice:=menusize;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
        END ELSE BEGIN
          CASE Instr OF
            #13 : exitkey:='RETURN';
            #27 : exitkey:='ESC';

          END;
        END;
      END;
    UNTIL exitkey<>'';
    hidemouse;
  END;  { End of Choosefrommenu. }



PROCEDURE PulldownMenu(FileName : string; VAR Choice : string; Width : integer;
  VAR Exitkey : string; Forecolor, Backcolor : byte);

  VAR
   MenuSpecs  : DBFheadertype;
   MenuRecord : recordarraytype;
   Index : longint;
   MenuNum, i : integer;
   Menu : indextype;
 {  ExitKey, }
   Results : string;

  BEGIN

    index := 1;
    opendbasefile(menuspecs, FileName, results);
    IF Results = 'GOOD' THEN BEGIN
      While index <= menuspecs.numofrecs DO BEGIN
        ReadDBFrecord(menuspecs, index, menurecord, results);
        Menu[index] := Trim(MenuRecord[1]);
        inc(index);
      END;
      MenuNum := 1;
      Choosefromindex(Menu, index - 1,1,1,Width,18,MenuNum, Exitkey, Forecolor, Backcolor);
      IF ExitKey = 'ESC' THEN
        Choice := ''
      ELSE
        Choice := Menu[MenuNum];
      closedbasefile(Menuspecs);
    END;


  END;


PROCEDURE MultipleChoiceMenu (FileName : string; VAR Choice : string; Width : integer;
   VAR Exitkey : string; MenuNum : integer);

  VAR
   MenuSpecs  : DBFheadertype;
   MenuRecord : recordarraytype;
   Index : longint;
   i : integer;
   Menu : indextype;
   Results : string;

  BEGIN

    MenuNum := 1;
    index := 1;
    choice := '';
    exitkey := '';
    opendbasefile(menuspecs, FileName, results);
    IF Results = 'GOOD' THEN BEGIN

      While index <= menuspecs.numofrecs DO BEGIN
        ReadDBFrecord(menuspecs, index, menurecord, results);
        Menu[index] := Trim(MenuRecord[1]);
        inc(index);
      END;
      Choosefromindex(Menu,index - 1,1,1,Width,18,MenuNum, Exitkey,15,1);
      Choice := Menu[MenuNum];
      closedbasefile(Menuspecs);
    END;


  END;



PROCEDURE Screen(Message, Message2 : MessageType; Forecolor, Backcolor, MessageColor : integer);
VAR
  i, j, xloc, yloc, width, height : integer;


BEGIN
  Window(1,1, 80, 25);
  TextBackground(Backcolor);
  Clrscr;
  TextColor(MessageColor);
  GoToXY(((80 Div 2) - (Length(Message)) DIV 2),1);
  Write(Message);

  Window(1,2, 80, 25);
  TextBackground(Backcolor);
{  clrscr; }
  TextColor(Forecolor);
  For i := 1 to 80 DO BEGIN
    For J := 1 to 24 DO BEGIN
      Write(#176);
    END;
  END;
  GoToXY(1,23);
  For i := 1 to 79 DO
    Write(#176);
  GoToXY(2,2);
  TextColor(MessageColor);
  Center(Message2, 24, MessageColor);

END;

PROCEDURE Frame(Color, Xloc,Yloc, Width, Height : Integer);

 VAR
   i : integer;

 BEGIN
    TextColor(Color);
    GoToXY(Xloc,Yloc);
    Write(#213);
    For i := 1 To Height DO BEGIN
      GoToXY(Xloc,Yloc+i);
      Write(#179);
    END;
    GoToXY(Xloc,Yloc + Height);
    Write(#212);
    GoToXY(Xloc + Width,Yloc);
    Write(#184);
    For i := 1 To Height DO BEGIN
      GoToXY(Xloc + Width,Yloc+i);
      Write(#179);
    END;
    GoToXY(Xloc+Width,Yloc+Height);
    Write(#190);
  END;

  PROCEDURE Getchunky(Xloc,Yloc:INTEGER; VAR inputstr:STRING; Colr, ForeColor :INTEGER;
    Formatstr:STRING;VAR exitkey:STRING);
  {********************************************************************}
  {* THIS PROCEDURE GETS HH:MM:SS OR ANY OTHER CHUNKY NUMBER FORMAT.  *}
  {* DATE FORMAT='12/31/99',   TIME FORMAT='23:59:59',                *}
  {* LATLON FORMAT='179:59:59.999',  ETC.                             *}
  {********************************************************************}
  CONST maxfields = 80;

  VAR
    maxlength,numoffields,thisfield  : INTEGER;
    cnt,err,cursorplace,numval       : INTEGER;
    Numlist                          : ARRAY[1..Maxfields,1..2] OF INTEGER;
    Limit                            : ARRAY[1..Maxfields] OF LONGINT;
    Tformatstr,Workstr               : STRING;
    innumber                         : BOOLEAN;
    inchar                           : CHAR;
    oldcolors                        : BYTE;

    PROCEDURE checkcursor;
    BEGIN
      IF Cursorplace<Numlist[Thisfield,1] THEN BEGIN
                                                  {  Skip over punctuation.}
        DEC(Thisfield,1);
      END ELSE BEGIN
        IF Cursorplace>Numlist[Thisfield,2] THEN INC(Thisfield,1);
      END;
      IF thisfield<1 THEN thisfield:=1; { KEEP THISFIELD IN RANGE. }
      IF thisfield>numoffields THEN thisfield:=numoffields;
      {KEEP CURSOR IN RANGE.}
      IF cursorplace<numlist[thisfield,1] THEN
         cursorplace:=numlist[thisfield,1];
      IF cursorplace>numlist[thisfield,2] THEN
         cursorplace:=numlist[thisfield,2];
    END;

  BEGIN
    oldcolors:=TEXTATTR;
    TEXTCOLOR(Forecolor); TEXTBACKGROUND(Colr);
    Maxlength:=LENGTH(Formatstr);
    exitkey:=''; { Initialize variables. }
    Innumber:=FALSE; Numoffields:=0; Thisfield:=1;

    Workstr:=inputstr;
    { IF inputstr is declared too short, make Formatstr fit.}
    Tformatstr:=Formatstr+' ';
    FOR Cnt:=1 TO LENGTH(Tformatstr) DO BEGIN
                                    { Parse out number fields in formatstr.}

      IF (Tformatstr[Cnt]>='0') AND (Tformatstr[Cnt]<='9') THEN BEGIN
                                      {  IF character in format is numeric.}
        IF NOT Innumber THEN BEGIN
          Numoffields:=Numoffields+1;
          Numlist[Numoffields,1]:=Cnt;    { BEGINNING OF FIELD.}
          Innumber:=TRUE;
        END;
        IF (Workstr[Cnt]<'0') OR (Workstr[Cnt]>'9') THEN Workstr[Cnt]:='0';
      END ELSE BEGIN               {  IF character in format is non-numeric.}
        IF Innumber THEN BEGIN
          Numlist[Numoffields,2]:=Cnt-1;     { END OF FIELD.}
          { FIND MAXIMUM FOR EACH FIELD.}
          VAL(
            COPY(Tformatstr,Numlist[Numoffields,1],
            Numlist[Numoffields,2]-Numlist[Numoffields,1]+1),
            Limit[Numoffields],err);
          Innumber:=FALSE;
        END;
        Workstr[Cnt]:=Tformatstr[Cnt];
      END;
    END;  { End of for loop. }
    setlength(workstr,length(formatstr)); { Set length of working string. }
    inputstr:=COPY(Workstr,1,LENGTH(Formatstr));
    Cursorplace:=Numlist[1,1];
    GOTOXY(Xloc,Yloc); WRITELN(inputstr); GOTOXY(xloc+cursorplace-1,yloc);
    REPEAT
      IF KEYPRESSED THEN BEGIN
        inchar:=READKEY;
        CASE inchar OF
          #13 : exitkey:='RETURN'; { User pressed return.}
          #27 : exitkey:='ESC'; { User pressed escape.}
          #8  : BEGIN  { User pressed backspace.}
                  DEC(cursorplace,1); checkcursor;
                  inputstr[cursorplace]:='0';
                END;
          #0  : BEGIN  { User pressed extended key.}
                  inchar:=READKEY;
                  CASE inchar OF
                    #59..#68 : BEGIN exitkey:='F'+CHR(ORD(inchar)-10); END;
                                                        {Function keys 1-10.}
                    #71 : cursorplace:=numlist[1,1]; {home}
                    #79 : cursorplace:=numlist[numoffields,2]; {END}
                    #77 : BEGIN INC(cursorplace,1); checkcursor; END;
                                                                {right arrow}
                    #75 : BEGIN DEC(cursorplace,1); checkcursor; END;
                                                                 {left arrow}
                    #72 : exitkey:='UP';  {up arrow}
                    #80 : exitkey:='DOWN'; {down arrow}
                    #73 : exitkey:='PAGEUP';  {pageup}
                    #81 : exitkey:='PAGEDOWN'; {pagedown}
                    #83 : inputstr[cursorplace]:='0';  { Delete }
                  END;
                END;
          ' ' : inputstr[cursorplace]:='0';
          '0'..'9' : BEGIN
                  inputstr[cursorplace]:=inchar;
                  INC(cursorplace,1); checkcursor;
                END;
        END;  { End of case statement.}
        GOTOXY(xloc,yloc); WRITE(inputstr); GOTOXY(xloc+cursorplace-1,yloc);
      END;  { IF KEYPRESSED.}
    UNTIL exitkey<>'';
    TEXTATTR:=oldcolors;
    GOTOXY(Xloc,Yloc); WRITE(inputstr);

    IF exitkey<>'ESC' THEN BEGIN  { Range check. }
      FOR Cnt:=1 TO Thisfield DO BEGIN
        VAL(COPY(inputstr,Numlist[Cnt,1],Numlist[Cnt,2]-numlist[cnt,1]+1 )
              ,numval,err);
        IF numval>Limit[Cnt] THEN exitkey:='OUT OF RANGE';
      END;
    END;
  END;   { END OF Getchunky. }


PROCEDURE  WriteExitCode(KeyString, Filename : string);
  VAR
    Outfile : text;
    s : string;

  BEGIN
     Assign(outfile, Filename);
     Rewrite(outfile);
     Writeln(outfile, KeyString);
     Close(outfile);

  END;


(* PROCEDURE DeleteARecord(Index : integer);

  VAR
    TempRec : recordarraytype;
    Results : string;
    count : integer;

  BEGIN
     Write('Deleting record. . .');
     Opendbasefile(PatientSpecs, 'prefills.dbf', Results);
     IF Results = 'GOOD' THEN BEGIN
       count := index;
       While count <= patientspecs.numofrecs DO BEGIN
         Readdbfrecord(patientSpecs, count + 1, patientRecord, Results);
         TempRec := patientRecord;
         Writedbfrecord(patientSpecs, count, TempRec, Results);
         Inc(count);
       END;
       patientspecs.numofrecs := patientspecs.numofrecs - 1;
       Closedbasefile(patientspecs);
     END;
  END;  *)

  FUNCTION ConvertCase(Instring : string) : String;
    VAR
      i : integer;
    BEGIN
      For i := 1 to Length(instring) DO BEGIN
       Instring[i] := Upcase(instring[i]);
      END;
      ConvertCase := Instring;
    END;

PROCEDURE Choosefromptrmenu(VAR menu: plusmenutype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte; DescX, DescY : byte);
  {************************************************************}
  {* Presents the user with a list of choices and gets the    *}
  {* user's choice through cursor keys or mouse.              *}
  {************************************************************}
  CONST barcolor = YELLOW; charwdt=8; charhgt=8;
  VAR
    row,column  : INTEGER;
    instr       : CHAR;
    lbutton,rbutton,oldlbutton  : BOOLEAN;
    mousex,mousey : INTEGER;

    FUNCTION inview(VAR xloc,yloc:INTEGER; VAR thisview: VIEWPORTTYPE):BOOLEAN;
    {**************************************************************}
    BEGIN
      WITH thisview DO BEGIN
        IF (xloc<=x2) AND (yloc<=y2) AND (xloc>=x1) AND (yloc>=y1) THEN
          inview:=TRUE
        ELSE inview:=FALSE;
      END;
    END;

    PROCEDURE highlight(xloc,yloc:INTEGER;instr:STRING);
    {************************************************************}
    VAR blanker : STRING; oldcolor : BYTE;
    BEGIN
      hidemouse; oldcolor:=TEXTATTR;
      TEXTBACKGROUND(Forecolor); TEXTCOLOR(Black);
      If Length(Trim(menu[Choice].description)) > 0 THEN BEGIN
        Say(DescX-1, DescY, #16, Red+Blink);
        Say(DescX, DescY, menu[Choice].description, Red);
        Say(WhereX, WhereY, #17, Red+Blink);
      END ELSE BEGIN
        TextAttr := Oldcolor;
        Say(DescX-1, DescY, ' ', Blue);
        Say(DescX, DescY, menu[Choice].description, Blue);
        Say(WhereX, WhereY, ' ', Blue);
      END;
  (*    Say(DescX, DescY+1, menu[Choice].description2, Black);
      Say(DescX, DescY+2, menu[Choice].description3, Black); *)
        TEXTBACKGROUND(Forecolor); TEXTCOLOR(Black);
        GOTOXY(xloc,yloc); WRITE(instr);
        TEXTATTR:=oldcolor; showmouse;
    END;

    PROCEDURE indexsearch(xpick,ypick:INTEGER);
    VAR xtest,ytest,strlength:INTEGER; found:BOOLEAN;
      choiceview : VIEWPORTTYPE; indexnum:INTEGER;
    BEGIN
      { Adjust mouse to screen coords.}
      xpick:=(xpick DIV charwdt)-LO(WINDMIN);
      ypick:=(ypick DIV charhgt)-HI(WINDMIN);
      found:=FALSE;  indexnum:=0;
      REPEAT
        INC(indexnum,1);
        xtest:=Xloc+(((indexnum-1) DIV Hgt)*Wdt);
        ytest:=Yloc+(indexnum-1) MOD Hgt;
        strlength:=LENGTH(menu[indexnum].menuitem);
        WITH choiceview DO BEGIN
          x1:=xtest; y1:=ytest-1; x2:=xtest+strlength; y2:=y1+1;
        END;
        IF inview(xpick,ypick,choiceview) THEN BEGIN
          found:=TRUE;
          hidemouse;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice].menuitem);
(*        GoToXY(1, Hi(windmax));
          WRITE(menu[Choice].description^);
          Readln; *)
          Choice:=indexnum;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice].Menuitem);
          exitkey:='RETURN';
          showmouse;
        END;
      UNTIL (indexnum>=menusize) OR found;
    END;


  BEGIN
    exitkey:='';
    lbutton:=FALSE; rbutton:=FALSE; mousewait; { Clear mouse variables. }
    TEXTCOLOR(Forecolor); TEXTBACKGROUND(Backcolor);
    FOR Row:=1 TO menusize DO BEGIN   { PRINT OUT MENU. }
      GOTOXY(Xloc+(((Row-1) DIV Hgt)*Wdt)+1,Yloc+((Row-1) MOD Hgt)+1);
      WRITE(menu[Row].menuitem);
    END;

    If Choice > Menusize THEN Choice := Menusize;

    highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice].menuitem);

    showmouse;
    REPEAT
      oldlbutton:=lbutton;
      getmouse(mousex,mousey,lbutton,rbutton);
      IF lbutton AND NOT(oldlbutton) THEN BEGIN  { Lbutton just pressed. }
        indexsearch(mousex,mousey);
      END;
      IF KEYPRESSED THEN BEGIN
        Instr:=READKEY;
        IF Instr=#0 THEN BEGIN
          Instr:=READKEY;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice].menuitem);
          CASE instr OF
            #59 : BEGIN         {f1}
                    Exitkey := 'F1';

                  END;
            #60 : BEGIN
                    Exitkey := 'F2';
                  END;

            #71 : BEGIN
                    ExitKey := 'HOME';
                  END;

            #72 : BEGIN   { Up arrow. }
                    Choice:=Choice-1;
                  END;

            #73 : BEGIN
                    ExitKey := 'PAGEUP';
                  END;

            #75 : BEGIN   {  Left arrow. }
                    Choice:=Choice-Hgt;
                  END;


            #77 : BEGIN   {  Right arrow. }
                    Choice:=Choice+Hgt;
                  END;

            #79 : BEGIN
                    ExitKey := 'END';
                  END;

            #80 : BEGIN   { Down arrow. }
                    Choice:=Choice+1;
                  END;

            #81 : BEGIN
                    Exitkey := 'PAGEDOWN';
                  END;

            #83 : BEGIN
                    Exitkey := 'DEL';
                  END;



          END;
          IF Choice>menusize THEN Choice:=1;             { WRAP. }
          IF Choice<1 THEN Choice:=menusize;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice].menuitem);
        END ELSE BEGIN
          CASE Instr OF
            #13 : exitkey:='RETURN';
            #27 : exitkey:='ESC';
          END;
        END;
      END;
    UNTIL exitkey <> '';
    hidemouse;
  END;  { End of Choosefrommenu. }


PROCEDURE CFPTRmenu(VAR menu: plusmenutype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte; DescX, DescY : byte);
  {************************************************************}
  {* Presents the user with a list of choices and gets the    *}
  {* user's choice through cursor keys or mouse.              *}
  {************************************************************}
  CONST barcolor = YELLOW; charwdt=8; charhgt=8;
  VAR
    row,column  : INTEGER;
    instr       : CHAR;
    lbutton,rbutton,oldlbutton  : BOOLEAN;
    mousex,mousey : INTEGER;

    FUNCTION inview(VAR xloc,yloc:INTEGER; VAR thisview: VIEWPORTTYPE):BOOLEAN;
    {**************************************************************}
    BEGIN
      WITH thisview DO BEGIN
        IF (xloc<=x2) AND (yloc<=y2) AND (xloc>=x1) AND (yloc>=y1) THEN
          inview:=TRUE
        ELSE inview:=FALSE;
      END;
    END;

    PROCEDURE ShowDescription(Dsx, Dsy : byte);
      VAR
        DescriptArea : areasavertype;
        DesMin, DesMax : word;
      BEGIN
        SaveArea(Dsx, Dsy, 55, 10, DescriptArea); {72, 20}
        DesMin := WindMin; DesMax := WindMax;
        DrawBox(Dsx,Dsy,53,6,LightGray,Blue,15,15,'','','',
          #218, #196, #191, #179, #192, #217, #0, #0);
        Say(DsX+5, DsY+2, menu[Choice].description, Black);
(*        Say(DsX+5, DsY+3, menu[Choice].description2, Black);
        Say(DsX+5, DsY+4, menu[Choice].description3, Black); *)
        RestoreArea(Dsx, Dsy, DescriptArea);
        WindMin := DesMin; WindMax := DesMax;
      END;


    PROCEDURE highlight(xloc,yloc:INTEGER;instr:STRING);
    {************************************************************}
    VAR blanker : STRING; oldcolor : BYTE;
    BEGIN
      hidemouse; oldcolor:=TEXTATTR;
      TEXTBACKGROUND(Forecolor); TEXTCOLOR(Black);
      GOTOXY(xloc,yloc); WRITE(instr);
      GOTOXY(DescX, DescY);
      Write(menu[Choice].description);
(*    GOTOXY(DescX, DescY+1);
      Write(menu[Choice].description2);
      Say(DescX, DescY, menu[Choice].description, Black);
      Say(DescX, WhereY+1, menu[Choice].description2, Black); *)
      TEXTATTR:=oldcolor; showmouse;

    END;

    PROCEDURE indexsearch(xpick,ypick:INTEGER);
    VAR xtest,ytest,strlength:INTEGER; found:BOOLEAN;
      choiceview : VIEWPORTTYPE; indexnum:INTEGER;
    BEGIN
      { Adjust mouse to screen coords.}
      xpick:=(xpick DIV charwdt)-LO(WINDMIN);
      ypick:=(ypick DIV charhgt)-HI(WINDMIN);
      found:=FALSE;  indexnum:=0;
      REPEAT
        INC(indexnum,1);
        xtest:=Xloc+(((indexnum-1) DIV Hgt)*Wdt);
        ytest:=Yloc+(indexnum-1) MOD Hgt;
        strlength:=LENGTH(menu[indexnum].menuitem);
        WITH choiceview DO BEGIN
          x1:=xtest; y1:=ytest-1; x2:=xtest+strlength; y2:=y1+1;
        END;
        IF inview(xpick,ypick,choiceview) THEN BEGIN
          found:=TRUE;
          hidemouse;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice].menuitem);

(*        GoToXY(1, Hi(windmax));
          WRITE(menu[Choice].description^);
          Readln; *)
          Choice:=indexnum;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice].Menuitem);

          exitkey:='RETURN';
          showmouse;
        END;
      UNTIL (indexnum>=menusize) OR found;
    END;


  BEGIN
    exitkey:='';
    lbutton:=FALSE; rbutton:=FALSE; mousewait; { Clear mouse variables. }
    TEXTCOLOR(Forecolor); TEXTBACKGROUND(Backcolor);
    FOR Row:=1 TO menusize DO BEGIN   { PRINT OUT MENU. }
      GOTOXY(Xloc+(((Row-1) DIV Hgt)*Wdt)+1,Yloc+((Row-1) MOD Hgt)+1);
      WRITE(menu[Row].menuitem);
    END;

    If Choice > Menusize THEN Choice := Menusize;

    highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice].menuitem);
    ShowDescription(DescX, DescY);
    showmouse;
    REPEAT
      oldlbutton:=lbutton;
      getmouse(mousex,mousey,lbutton,rbutton);
      IF lbutton AND NOT(oldlbutton) THEN BEGIN  { Lbutton just pressed. }
        indexsearch(mousex,mousey);
      END;
      IF KEYPRESSED THEN BEGIN
        Instr:=READKEY;
        IF Instr=#0 THEN BEGIN
          Instr:=READKEY;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice].menuitem);
          CASE instr OF
            #59 : BEGIN         {f1}
                    Exitkey := 'F1';

                  END;
            #60 : BEGIN
                    Exitkey := 'F2';
                  END;

            #71 : BEGIN
                    ExitKey := 'HOME';
                  END;

            #72 : BEGIN   { Up arrow. }
                    Choice:=Choice-1;
                  END;

            #73 : BEGIN
                    ExitKey := 'PAGEUP';
                  END;

            #75 : BEGIN   {  Left arrow. }
                    Choice:=Choice-Hgt;
                  END;


            #77 : BEGIN   {  Right arrow. }
                    Choice:=Choice+Hgt;
                  END;

            #79 : BEGIN
                    ExitKey := 'END';
                  END;

            #80 : BEGIN   { Down arrow. }
                    Choice:=Choice+1;
                  END;

            #81 : BEGIN
                    Exitkey := 'PAGEDOWN';
                  END;

            #83 : BEGIN
                    Exitkey := 'DEL';
                  END;



          END;
          IF Choice>menusize THEN Choice:=1;             { WRAP. }
          IF Choice<1 THEN Choice:=menusize;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice].menuitem);
        END ELSE BEGIN
          CASE Instr OF
            #13 : exitkey:='RETURN';
            #27 : exitkey:='ESC';
          END;
        END;
      END;
    UNTIL exitkey <> '';
    hidemouse;
  END;  { End of Choosefrommenu. }



PROCEDURE EnhancedMenu(VAR bigmenu : bigmenutype; VAR PageNum : word;
  Xloc, Yloc, Width, Height : integer; VAR SubMenuNum : integer; VAR Exitkey : string;
  Forecolor, Backcolor : word; ResetWindow : char);

  VAR
    HoldMin, HoldMax : word;


  PROCEDURE Indicate(Page : word);
    BEGIN
      TextColor(Yellow);
      If Page = 1 THEN
         BEGIN
           (* Center('PG DOWN '+#25,2,Yellow); *)
           Say(2, 2, 'PG DOWN '+#25, Yellow);
         END;

      IF (Page > 1) AND (Page < (BigMenu.NumOfPages)) THEN
         BEGIN
           Say(2, 2, #24+' PG UP '+#4+' PG DOWN '+#25, Yellow);

         END;

      IF Page = BigMenu.NumOfPages THEN
        BEGIN
          Say(2, 2, 'PG UP '+#24, Yellow);
        END;

    END;


  BEGIN
    IF ResetWindow = 'Y' THEN BEGIN
      HoldMin := WindMin; HoldMax := WindMax;
      Window(Xloc, Yloc, Width + 1, Height + 2);
    END;

  REPEAT
    With bigmenu.Pages[pagenum] DO BEGIN
      Textbackground(backcolor);
      Clrscr;

      IF bigmenu.numofpages > 1 THEN Indicate(PageNum);
      ChoosefromMenu(submenu, menusize, 1, 1, width, height, SubMenuNum, Exitkey, Forecolor, Backcolor);

      IF Exitkey = 'PAGEUP' THEN BEGIN
        Clrscr;
        DEC(pagenum, 1);
        IF PageNum < 1 THEN PAGENUM := 1;
        EXITKEY := '';
      END;
      IF Exitkey = 'PAGEDOWN' THEN BEGIN
        Clrscr;
        INC(pagenum, 1);
        IF Pagenum > BigMenu.NumOfPages THEN pagenum := BigMenu.Numofpages;
        exitkey := '';
      END;
      IF Exitkey = 'HOME' THEN BEGIN
        Clrscr;
        Pagenum := 1;
        SubmenuNum := 1;
        exitkey := '';
      END;
      IF ExitKey = 'END' THEN BEGIN
        clrscr;
        Pagenum := BigMenu.NumOfPages;
        SubMenuNum := BigMenu.Pages[PageNum].menusize;
      END;

    END;

  UNTIL (exitkey = 'ESC') OR (exitkey = 'RETURN') OR (exitkey = 'F1') OR (exitkey = 'F2') OR (exitkey = 'DEL');
  IF ResetWindow = 'Y' THEN
    WindMin := HoldMin; WindMax := HoldMax;

END;



PROCEDURE EnhancedPtrMenu(VAR bigmenu : bigmenuptrtype; VAR PageNum : word;
  Xloc, Yloc, Width, Height : integer; VAR SubMenuNum : integer; VAR Exitkey : string;
  Forecolor, Backcolor : word; ResetWindow : char; DescX, DescY : byte; ExitKeys : string);

  VAR
    HoldMin, HoldMax : word;
    i : byte;

  PROCEDURE Indicate(Page : word);
    BEGIN
      TextColor(Yellow);
      If Page = 1 THEN
         BEGIN
(*           Say(1, 1, 'PG DOWN '+#25, Yellow); *)
           Center('PG DOWN '+#25,1, Yellow);
         END;

      IF (Page > 1) AND (Page < (BigMenu^.NumOfPages)) THEN
         BEGIN
           Center(#24+' PG UP '+#4+' PG DOWN '+#25, 1, Yellow);
(*           Say(1, 1, #24+' PG UP '+#4+' PG DOWN '+#25, Yellow); *)

         END;

      IF Page = BigMenu^.NumOfPages THEN
        BEGIN
          Center('PG UP'+#24, 1, Yellow);
(*          Say(1, 1, 'PG UP'+#24, Yellow); *)
        END;

    END;


  BEGIN
    IF ResetWindow = 'Y' THEN BEGIN
      HoldMin := WindMin; HoldMax := WindMax;
      Window(Xloc, Yloc, Width + 1, Height+2);
    END;

  REPEAT
    With bigmenu^.Pages[pagenum]^ DO BEGIN
      Textbackground(backcolor);
      Clrscr;

      IF bigmenu^.numofpages > 1 THEN Indicate(PageNum);
      ChoosefromPtrMenu(submenu, menusize, 1, 1, width, height, SubMenuNum, Exitkey, Forecolor, Backcolor,  DescX, DescY);

      IF Exitkey = 'PAGEUP' THEN BEGIN
        Clrscr;
        DEC(pagenum, 1);
        IF PageNum < 1 THEN PAGENUM := 1;
        EXITKEY := '';
      END;
      IF Exitkey = 'PAGEDOWN' THEN BEGIN
        Clrscr;
        INC(pagenum, 1);
        IF Pagenum > BigMenu^.NumOfPages THEN pagenum := BigMenu^.Numofpages;
        exitkey := '';
      END;
      IF Exitkey = 'HOME' THEN BEGIN
        Clrscr;
        Pagenum := 1;
        SubmenuNum := 1;
        exitkey := '';
      END;
      IF ExitKey = 'END' THEN BEGIN
        clrscr;
        Pagenum := BigMenu^.NumOfPages;
        SubMenuNum := BigMenu^.Pages[PageNum]^.menusize;
      END;

    END;

(*  UNTIL (exitkey = 'ESC') OR (exitkey = 'RETURN') OR (exitkey = 'F1') OR (exitkey = 'F2') OR (exitkey = 'DEL'); *)
    UNTIL Pos(exitkey, exitkeys) > 0;
(*  For i:= 1 to MaxPages DO
    Dispose(BigMenu^.Pages[i]);
  Dispose(BigMenu);  *)
  IF ResetWindow = 'Y' THEN
    WindMin := HoldMin; WindMax := HoldMax;

END;


PROCEDURE EnhPtrMenu(VAR bigmenu : bigmenuptrtype; VAR PageNum : word;
  Xloc, Yloc, Width, Height : integer; VAR SubMenuNum : integer; VAR Exitkey : string;
  Forecolor, Backcolor : word; ResetWindow : char; DescX, DescY : byte);

  VAR
    HoldMin, HoldMax : word;
    i : byte;

  PROCEDURE Indicate(Page : word);
    BEGIN
      TextColor(Yellow);
      If Page = 1 THEN
         BEGIN
           Say(1, 1, 'PG DOWN '+#25, Yellow);
         END;

      IF (Page > 1) AND (Page < (BigMenu^.NumOfPages)) THEN
         BEGIN
           Say(1, 1, #24+' PG UP '+#4+' PG DOWN '+#25, Yellow);

         END;

      IF Page = BigMenu^.NumOfPages THEN
        BEGIN
          Say(1, 1, 'PG UP'+#24, Yellow);
        END;

    END;


  BEGIN
    IF ResetWindow = 'Y' THEN BEGIN
      HoldMin := WindMin; HoldMax := WindMax;
      Window(Xloc, Yloc, Width + 1, Height+2);
    END;

  REPEAT
    With bigmenu^.Pages[pagenum]^ DO BEGIN
      Textbackground(backcolor);
      Clrscr;

      IF bigmenu^.numofpages > 1 THEN Indicate(PageNum);
      CFPtrMenu(submenu, menusize, 1, 1, width, height, SubMenuNum, Exitkey, Forecolor, Backcolor,  DescX, DescY);

      IF Exitkey = 'PAGEUP' THEN BEGIN
        Clrscr;
        DEC(pagenum, 1);
        IF PageNum < 1 THEN PAGENUM := 1;
        EXITKEY := '';
      END;
      IF Exitkey = 'PAGEDOWN' THEN BEGIN
        Clrscr;
        INC(pagenum, 1);
        IF Pagenum > BigMenu^.NumOfPages THEN pagenum := BigMenu^.Numofpages;
        exitkey := '';
      END;
      IF Exitkey = 'HOME' THEN BEGIN
        Clrscr;
        Pagenum := 1;
        SubmenuNum := 1;
        exitkey := '';
      END;
      IF ExitKey = 'END' THEN BEGIN
        clrscr;
        Pagenum := BigMenu^.NumOfPages;
        SubMenuNum := BigMenu^.Pages[PageNum]^.menusize;
      END;

    END;

  UNTIL (exitkey = 'ESC') OR (exitkey = 'RETURN') OR (exitkey = 'F1') OR (exitkey = 'F2') OR (exitkey = 'DEL');
  For i:= 1 to MaxPages DO
    Dispose(BigMenu^.Pages[i]);
  Dispose(BigMenu);
  IF ResetWindow = 'Y' THEN
    WindMin := HoldMin; WindMax := HoldMax;

END;




PROCEDURE BuildBigMenu(Filename : string; VAR Menu : BigMenuType; VAR Height : integer;
  VAR FieldNum, FN2, FN3, Fn4, StartRec, EndRec : integer; VAR Empty : boolean; BlanksAllow : Char);
{StartRec = 0, should targ - 1; }
  PROCEDURE InitMenu(VAR Menu : BigMenutype);
    VAR
      i : word;

    BEGIN
      For i := 1 to MaxPages DO BEGIN
        Menu.Pages[i].menusize := 0;
      END;
    END;






  VAR
    Dbfspecs   : dbfheadertype;
    Dbfrecord  : recordarraytype;
    Results    : string;
    i,
    Count      : integer;
    recnum : integer;
    FString, Fstring2, Fstring3, FString4,
    Tempstring : string;

    PageNum : word;



  BEGIN
    Empty := TRUE;
    Opendbasefile(Dbfspecs, FileName, Results);
    If Results = 'GOOD' THEN BEGIN
      InitMenu(Menu);
      If Dbfspecs.numofrecs > 0 THEN BEGIN       {database isn't empty}
        Empty := FALSE;
        PageNum := 1;
        Recnum := startrec;
        {Recnum := 0;} { FieldNum := 1;} { Count := 1;} Menu.Pages[pagenum].Menusize := 0;
        While recnum < Endrec {usually should be numofrecs} DO BEGIN
          {Fieldnum := 1;} Inc(Recnum); Inc(Menu.Pages[Pagenum].Menusize);
          Readonefield(dbfspecs, Recnum, FieldNum, Fstring);
          IF Fn2 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN2, FString2)
          ELSE
            Fstring2 := '';

          IF Fn3 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN3, Fstring3)
          ELSE
            Fstring3 := '';

          IF Fn4 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN4, Fstring4)
          ELSE
            Fstring4 := '';

          Tempstring := Trim(FString)+' '+Trim(Fstring2)+' '+Trim(Fstring3)+' '+Trim(Fstring4);

         IF BlanksAllow = 'N' THEN BEGIN
            While (trim(fstring) = '') AND (FieldNum < dbfspecs.numoffields) DO BEGIN
              inc(fieldnum);
              Readonefield(dbfspecs, Recnum, fieldnum, fstring);
            END;
         END;


        {  IF BlanksAllow = 'N' THEN BEGIN
            While (trim(tempstring) = '') AND (FieldNum < dbfspecs.numoffields) DO BEGIN
              inc(fieldnum);
              Readdbfrecord(dbfspecs, Recnum, Temprec, Results);
              Tempst
            END;
          END; }
          Menu.Pages[PageNum].Submenu[Menu.Pages[pagenum].Menusize] := Trim(Tempstring);

          IF (Menu.Pages[pagenum].Menusize = Height) AND (recnum < endrec) THEN BEGIN
            Inc(PageNum);
            Menu.Pages[pagenum].Menusize := 0;
          END;
        END;
        Menu.NumOfPages := PageNum;
      END;
    END;
    Closedbasefile(Dbfspecs);
  END;

PROCEDURE DisposeMenu(VAR Menu : BigMenuPtrType);
  VAR
    i : word;

  BEGIN
      For i := 1 to MaxPages DO
        Dispose(Menu^.Pages[i]);
      Dispose(Menu);

  END;

  PROCEDURE InitPtrMenu(VAR Menu : BigMenuPtrtype);
    VAR
      i : word;

    BEGIN
        Menu := NIL;
        New(Menu);
        For i := 1 to MaxPages DO BEGIN
          New(Menu^.Pages[i]);
          Menu^.Pages[i]^.menusize := 0;
        END;
    END;



PROCEDURE BuildBigPtrMenu(Filename : string; VAR Menu : BigMenuPtrType; VAR Height : integer;
  FieldNum, FN2, FN3, Fn4, DescField, StartRec, EndRec : integer; VAR Empty : boolean; BlanksAllow : Char);
  {StartRec = 0, should targ - 1; }







  VAR
    Dbfspecs   : dbfheadertype;
    Dbfrecord  : recordarraytype;
    Results    : string;
    i,
    Count      : integer;
    recnum : integer;
    FString, Fstring2, Fstring3, FString4,
    Tempstring : string;

    PageNum : word;

  BEGIN
    Empty := TRUE;
    Opendbasefile(Dbfspecs, FileName, Results);
    If Results = 'GOOD' THEN BEGIN
      (* InitPtrMenu(Menu); *)
      If Dbfspecs.numofrecs > 0 THEN BEGIN       {database isn't empty}
        Empty := FALSE;
        PageNum := 1;
        Recnum := startrec;
        IF FieldNum = 0 THEN FieldNum := 1;  {Fieldnum := 0 when blanks allow = 'N', ie,
            put the first non-blank field in the menu}
        IF EndRec = 0 THEN Endrec := dbfspecs.numofrecs;  {If you want them all, put zero as end rec}
        {Recnum := 0;} {FieldNum := 1;} { Count := 1;} Menu^.Pages[pagenum]^.Menusize := 0;
        While recnum < Endrec {usually should be numofrecs} DO BEGIN
          {Fieldnum := 1;} Inc(Recnum); Inc(Menu^.Pages[Pagenum]^.Menusize);
          Readonefield(dbfspecs, Recnum, FieldNum, Fstring);
          IF Fn2 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN2, FString2)
          ELSE
            Fstring2 := '';

          IF Fn3 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN3, Fstring3)
          ELSE
            Fstring3 := '';

          IF Fn4 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN4, Fstring4)
          ELSE
            Fstring4 := '';


            IF BlanksAllow = 'N' THEN
              Tempstring := FString+' '+Trim(Fstring2)+' '+Trim(Fstring3)+' '+Trim(Fstring4)
            ELSE BEGIN
              Tempstring := Trim(FString)+' '+Trim(Fstring2)+' '+Trim(Fstring3)+' '+Trim(Fstring4);
              Tempstring := Trim(Tempstring);
            END;


         IF BlanksAllow = 'N' THEN BEGIN
            While (trim(fstring) = '') AND (FieldNum < dbfspecs.numoffields) DO BEGIN
              inc(fieldnum);
              Readonefield(dbfspecs, Recnum, fieldnum, fstring);
            END;
            IF (trim(fstring) <> '') AND (fieldnum < dbfspecs.numoffields) THEN BEGIN
              fstring2 := '';
              While (trim(fstring2) = '') AND (FieldNum < dbfspecs.numoffields) DO BEGIN
                Inc(fieldnum);
                Readonefield(dbfspecs, Recnum, fieldnum, fstring2);
              END;
            END;
         END;

          IF BlanksAllow = 'N' THEN BEGIN
            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].MenuItem := FString;
            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description := Fstring2;
            Fieldnum := 1;
          END ELSE BEGIN
            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].MenuItem := Trim(Tempstring);
            Readonefield(dbfspecs, Recnum, DescField, Tempstring);
            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description := Tempstring;
          END;

          IF (Menu^.Pages[pagenum]^.Menusize = Height) AND (recnum < endrec) THEN BEGIN
            Inc(PageNum);
            Menu^.Pages[pagenum]^.Menusize := 0;
          END;
        END;
        Menu^.NumOfPages := PageNum;
      END;
    END;
    Closedbasefile(Dbfspecs);
  END;


PROCEDURE BBPtrMenu(Filename : string; VAR Menu : BigMenuPtrType; VAR Height : integer;
  FieldNum, FN2, FN3, FN4, DescField, StartRec, EndRec : integer; VAR Empty : boolean);

  PROCEDURE InitMenu(VAR Menu : BigMenuPtrtype);
    VAR
      i : word;

    BEGIN
      Menu := NIL;
      New(Menu);
      For i := 1 to MaxPages DO BEGIN
        New(Menu^.Pages[i]);
        Menu^.Pages[i]^.menusize := 0;
      END;
    END;






  VAR
    Dbfspecs   : dbfheadertype;
    Dbfrecord  : recordarraytype;
    Results    : string;
    i,
    Count      : integer;
    recnum : integer;
    FString, Fstring2, Fstring3, FString4,
    Tempstring : string;

    PageNum : word;



  BEGIN
    Empty := TRUE;
    Opendbasefile(Dbfspecs, FileName, Results);
    If Results = 'GOOD' THEN BEGIN
      InitMenu(Menu);
      If Dbfspecs.numofrecs > 0 THEN BEGIN       {database isn't empty}
        Empty := FALSE;
        PageNum := 1;
        Recnum := startrec;
        IF FieldNum = 0 THEN FieldNum := 1;  {Fieldnum := 0 when blanks allow = 'N', ie,
            put the first non-blank field in the menu}
        IF EndRec = 0 THEN Endrec := dbfspecs.numofrecs;  {If you want them all, put zero as end rec}
        {Recnum := 0;} {FieldNum := 1;} { Count := 1;} Menu^.Pages[pagenum]^.Menusize := 0;
        While recnum < Endrec {usually should be numofrecs} DO BEGIN
          {Fieldnum := 1;} Inc(Recnum); Inc(Menu^.Pages[Pagenum]^.Menusize);
          Readonefield(dbfspecs, Recnum, FieldNum, Fstring);
          IF Fn2 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN2, FString2)
          ELSE
            Fstring2 := '';

          IF Fn3 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN3, Fstring3)
          ELSE
            Fstring3 := '';

          IF Fn4 <> 0 THEN
            Readonefield(dbfspecs, Recnum, FN4, Fstring4)
          ELSE
            Fstring4 := '';

            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].MenuItem := FString;
            Readonefield(dbfspecs, Recnum, DescField, Tempstring);
            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description := Tempstring;
(*          Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description2 := Fstring2;
            Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description3 := Fstring3;
            Writeln(Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description);
            Writeln(Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description2);
            Writeln(Menu^.Pages[PageNum]^.Submenu[Menu^.Pages[pagenum]^.Menusize].Description3);
            Readln; *)


          IF (Menu^.Pages[pagenum]^.Menusize = Height) AND (recnum < endrec) THEN BEGIN
            Inc(PageNum);
            Menu^.Pages[pagenum]^.Menusize := 0;
          END;
        END;
        Menu^.NumOfPages := PageNum;
      END;
    END;
    Closedbasefile(Dbfspecs);
  END;



PROCEDURE BuildBigMenu2(Filename : string; VAR Menu : BigMenuType; VAR Height : integer;
{  VAR FieldNum , StartRec, EndRec : integer; } VAR Empty : boolean; BlanksAllow : Char);
{StartRec = 0, should targ - 1; }
  PROCEDURE InitMenu(VAR Menu : BigMenutype);
    VAR
      i : word;

    BEGIN
      For i := 1 to MaxPages DO BEGIN
        Menu.Pages[i].menusize := 0;
      END;
    END;






  VAR
    Fieldnum   : integer;
    Dbfspecs   : dbfheadertype;
    Dbfrecord  : recordarraytype;
    Results    : string;
    i,
    Count      : integer;
    recnum : integer;
    Tempstring : string;
    PageNum : word;


  BEGIN
    Empty := TRUE;
    Opendbasefile(Dbfspecs, FileName, Results);
    If Results = 'GOOD' THEN BEGIN
      InitMenu(Menu);
      If Dbfspecs.numofrecs > 0 THEN BEGIN       {database isn't empty}
        Empty := FALSE;
        PageNum := 1;
{        Recnum := startrec; }
        Recnum := 0; { FieldNum := 1;} { Count := 1;} Menu.Pages[pagenum].Menusize := 0;
        While recnum < dbfspecs.numofrecs {Endrec} {usually should be numofrecs} DO BEGIN
          Fieldnum := 1; Inc(Recnum); Inc(Menu.Pages[Pagenum].Menusize);
          Readonefield(dbfspecs, Recnum, fieldnum, Tempstring);
          IF BlanksAllow = 'N' THEN BEGIN
            While (trim(tempstring) = '') AND (FieldNum < dbfspecs.numoffields) DO BEGIN
              inc(fieldnum);
              Readonefield(dbfspecs, Recnum, fieldnum, Tempstring);
            END;
          END;
          Menu.Pages[PageNum].Submenu[Menu.Pages[pagenum].Menusize] := Trim(Tempstring);

          IF (Menu.Pages[pagenum].Menusize = Height) AND (recnum < dbfspecs.numofrecs {endrec}) THEN BEGIN
            Inc(PageNum);
            Menu.Pages[pagenum].Menusize := 0;
          END;
        END;
        Menu.NumOfPages := PageNum;
      END;
    END;
    Closedbasefile(Dbfspecs);
  END;


FUNCTION st(instring:STRING;newlength:BYTE):STRING;
  {************************************************************}
  {* Returns a Left-justified version of instring.            *}
  {* If instring is longer than newlength, then the return    *}
  {* value is truncated.                                      *}
  {************************************************************}
  VAR tempstr:STRING;
  BEGIN
    setlength(tempstr,newlength);
    FILLCHAR(tempstr[1],newlength,' ');
    MOVE(instring[1],tempstr[1],LENGTH(instring));
    st:=tempstr;
  END;

PROCEDURE Center(Message : MessageType; Ypos : byte; MessageColor : byte);
  VAR
    CenterPos,
    Width,
    CalcPos,
    Offset : byte;
    OldColors : word;
  BEGIN
    OldColors := TextAttr;
    TextColor(MessageColor);
    Width := Lo(WindMax) - Lo(WindMin);
    Offset := Length(Message) DIV 2;
    CenterPos := Width DIV 2;
    CalcPos := CenterPos - Offset;
    GoToXY(CalcPos, Ypos);
    Write(Message);
    TextAttr := OldColors;
  END;

PROCEDURE DialogueBox(VAR Reply : string; Prompt : string; VAR Colorscheme : byte);
  CONST
    Menu : Menutype=('Y','N','','','','','','','','','','','','','','',
      '','','',''{,'','','','','','','','','',''});
  VAR
    MenuNum : integer;
    Returnkey : string;


  BEGIN
    Reply := ' ';
    MenuNum := 1;
    TextAttr := Colorscheme;
    DrawBox(38,20,40,4,15,4,15,15,'','',Prompt, #218, #196, #191, #179, #192, #217, #176, #177);
    Choosefrommenu(Menu,2,5,2,2,1,MenuNum, Returnkey,15,4);
    Reply := Trim(Menu[MenuNum]);
  END;

PROCEDURE CopyFile(filename1,filename2:STRING);
  {**************************************************************}
  {**************************************************************}
  CONST BufSize=10000;
  TYPE BufferType=ARRAY[1..BufSize] OF BYTE;
  VAR
    BufferArray : BufferType;
    RoundSize,NumOfLastBytes  : LONGINT;
    fileloc   : LONGINT;
    fileinfo  : SEARCHREC;
    infile,outfile : FILE;

  BEGIN
    FINDFIRST(filename1,$00,fileinfo);
    IF DOSERROR=0 THEN BEGIN
      ASSIGN(infile,filename1);
      RESET(infile,1);

      ASSIGN(outfile,filename2);
      REWRITE(outfile,1);

      NumOfLastBytes:=FileInfo.size MOD BufSize;
      RoundSize:=FileInfo.size-NumOfLastBytes;

    { Copy the bulk of the file.}
      fileloc:=0;
      WHILE fileloc<roundsize DO BEGIN
        SEEK(infile,fileloc);
        BLOCKREAD(infile,BufferArray,BufSize);

        SEEK(outfile,fileloc);
        BLOCKWRITE(outfile,BufferArray,BufSize);
        INC(FileLoc,bufsize);
      END;

     { Copy what's left over; the tail end.}
      SEEK(infile,fileloc);
      BLOCKREAD(infile,BufferArray,NumOfLastBytes);

      SEEK(outfile,fileloc);
      BLOCKWRITE(outfile,BufferArray,NumOfLastBytes);

      CLOSE(infile);
      CLOSE(outfile);
    END ELSE BEGIN
      WRITE('File not found.');
    END;
  END;  {End of CopyFile.}


PROCEDURE DeleteFile(Filename : string);
 VAR
  Tempfile : File;
  DirInfo: SearchRec;
BEGIN
  FindFirst(FileName, Archive, DirInfo); { Same as DIR *.PAS }
  IF DosError = 0 THEN BEGIN
    Assign(Tempfile, Filename);
    Erase(Tempfile);
  END;
END;

PROCEDURE GetPrinterConfig(Filename : string; VAR Config : PrinterConfigType);
  VAR
    DirInfo: SearchRec;
    ConfigSpec : dbfheadertype;
    configrec : recordarraytype;
    results : string;
  BEGIN
    With Config DO BEGIN
      Bold_On := '';
      Bold_Off := '';
      Compressed := '';
      Underline_On := '';
      Underline_Off := '';
      FontSelect := '';
      Reset := '';
      FindFirst(FileName, Archive, DirInfo);
      IF DosError = 0 Then Begin
        Opendbasefile(ConfigSpec, Filename, Results);
        Readdbfrecord(ConfigSpec, 1, ConfigRec, Results);
        Bold_On       := Trim(ConfigRec[1]);
        Bold_Off      := Trim(ConfigRec[2]);
        Compressed    := Trim(ConfigRec[3]);
        Underline_On  := Trim(ConfigRec[4]);
        Underline_Off := Trim(ConfigRec[5]);
        FontSelect    := Trim(ConfigRec[6]);
        Reset         := Trim(ConfigRec[7]);
        Closedbasefile(ConfigSpec);
      END ELSE BEGIN
        Writeln(Filename, ' not found');
        Readln;
      END;
    END;
  END;


PROCEDURE ChoosefromAlphabet(VAR menu:Alphabettype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER;
    VAR exitkey:STRING; Forecolor, BackColor : byte);
  {************************************************************}
  {* Presents the user with a list of choices and gets the    *}
  {* user's choice through cursor keys or mouse.              *}
  {************************************************************}
  CONST barcolor = YELLOW; charwdt=8; charhgt=8;
  VAR
    row,column  : INTEGER;
    instr       : CHAR;
    lbutton,rbutton,oldlbutton  : BOOLEAN;
    mousex,mousey : INTEGER;

    FUNCTION inview(VAR xloc,yloc:INTEGER; VAR thisview:VIEWPORTTYPE):BOOLEAN;
    {**************************************************************}
    BEGIN
      WITH thisview DO BEGIN
        IF (xloc<=x2) AND (yloc<=y2) AND (xloc>=x1) AND (yloc>=y1) THEN
          inview:=TRUE
        ELSE inview:=FALSE;
      END;
    END;

    PROCEDURE highlight(xloc,yloc:INTEGER;instr:STRING);
    {************************************************************}
    VAR blanker : STRING; oldcolor : BYTE;
    BEGIN
      hidemouse; oldcolor:=TEXTATTR;
      TEXTBACKGROUND(Forecolor); TEXTCOLOR(Black);
      GOTOXY(xloc,yloc); WRITE(instr);
      TEXTATTR:=oldcolor; showmouse;
    END;

    PROCEDURE indexsearch(xpick,ypick:INTEGER);
    VAR xtest,ytest,strlength:INTEGER; found:BOOLEAN;
      choiceview : VIEWPORTTYPE; indexnum:INTEGER;
    BEGIN
      { Adjust mouse to screen coords.}
      xpick:=(xpick DIV charwdt)-LO(WINDMIN);
      ypick:=(ypick DIV charhgt)-HI(WINDMIN);
      found:=FALSE;  indexnum:=0;
      REPEAT
        INC(indexnum,1);
        xtest:=Xloc+(((indexnum-1) DIV Hgt)*Wdt);
        ytest:=Yloc+(indexnum-1) MOD Hgt;
        strlength:=LENGTH(menu[indexnum]);
        WITH choiceview DO BEGIN
          x1:=xtest; y1:=ytest-1; x2:=xtest+strlength; y2:=y1+1;
        END;
        IF inview(xpick,ypick,choiceview) THEN BEGIN
          found:=TRUE;
          hidemouse;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          Choice:=indexnum;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
          exitkey:='RETURN';
          showmouse;
        END;
      UNTIL (indexnum>=menusize) OR found;
    END;


  BEGIN
    exitkey:='';
    lbutton:=FALSE; rbutton:=FALSE; mousewait; { Clear mouse variables. }
    TEXTCOLOR(Forecolor); TEXTBACKGROUND(Backcolor);
    FOR Row:=1 TO menusize DO BEGIN   { PRINT OUT MENU. }
      GOTOXY(Xloc+(((Row-1) DIV Hgt)*Wdt)+1,Yloc+((Row-1) MOD Hgt)+1);
      WRITE(menu[Row]);
    END;

    highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
    showmouse;
    REPEAT
      oldlbutton:=lbutton;
      getmouse(mousex,mousey,lbutton,rbutton);
      IF lbutton AND NOT(oldlbutton) THEN BEGIN  { Lbutton just pressed. }
        indexsearch(mousex,mousey);
      END;
      IF KEYPRESSED THEN BEGIN
        Instr:=READKEY;
        IF Instr <> #0 THEN BEGIN
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          CASE Instr OF
            #65..#90 :  BEGIN
                          Choice := Ord(instr) - 64;
                          Exitkey := 'RETURN';
                        END;
            #97..#122 : BEGIN
                          Instr := Upcase(instr);
                          Choice := Ord(instr) - 64;
                          Exitkey := 'RETURN';
                         END;
          END;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
        END;
        IF Instr=#0 THEN BEGIN
          Instr:=READKEY;
          GOTOXY(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1);
          TEXTCOLOR(Forecolor); WRITE(menu[Choice]);
          CASE instr OF
            #60 : BEGIN
                    Exitkey := 'F2';
                  END;
            #80 : BEGIN   { Down arrow. }
                    Choice:=Choice+1;
                  END;
            #72 : BEGIN   { Up arrow. }
                    Choice:=Choice-1;
                  END;
            #77 : BEGIN   {  Right arrow. }
                    Choice:=Choice+Hgt;
                  END;
            #75 : BEGIN   {  Left arrow. }
                    Choice:=Choice-Hgt;
                  END;
          END;
          IF Choice>menusize THEN Choice:=1;             { WRAP. }
          IF Choice<1 THEN Choice:=menusize;
          highlight(Xloc+(((Choice-1) DIV Hgt)*Wdt)+1,Yloc+((Choice-1) MOD Hgt)+1,menu[Choice]);
        END ELSE BEGIN
          CASE Instr OF
            #13 : exitkey:='RETURN';
            #27 : exitkey:='ESC';
          END;
        END;
      END;
    UNTIL exitkey<>'';
    hidemouse;
  END;  { End of Choosefrommenu. }


FUNCTION RTrim(Instring : string) : string;
{****************************************}
{Function like dBase's rtrim(), removes
trailing blanks}
{****************************************}
VAR
  i : byte;

BEGIN


  i := Length(instring);
  While instring[i] = ' ' DO BEGIN
    Delete(instring, i, 1);
    Dec(i);
  END;
  RTrim := Instring;

END;

FUNCTION LTrim(Instring : string) : string;
{****************************************}
{Function like dBase's rtrim(), removes
trailing blanks}
{****************************************}
VAR
  i : byte;

BEGIN


  i := 1;
  While instring[i] = ' ' DO BEGIN
    Delete(instring, i, 1);
    Inc(i);
  END;
  LTrim := Instring;

END;

(* PROCEDURE UNDERLINE (VAR Message : string;  VAR Destination : file; LF : char);
  {IF LF = 'Y' THEN Do a writeln at the end}

  VAR
    i,
    MessageLength : byte;

  BEGIN
    Message := Trim(Message);
    Write(destination, Message);
    MessageLength := Length(Message);
    For i := 1 to MessageLength DO
      write(Destination, #8);
    For i := 1 to MessageLength DO
      write(Destination, #95);
    IF LF = 'Y' THEN
      Writeln(Destination);
  END; *)

  PROCEDURE RestoreScreen;
    BEGIN
      Window(1, 1, 80, 25);
      TextColor(LightGray);
      TextBackground(Black);
      Clrscr;
    END;



  FUNCTION EncryptPassword (Instring : string) : String;
    VAR
      Outstring : string;
      Cnt : byte;
      Charval : integer;
    BEGIN
       Randseed := Seed;
       Outstring := '';
       Cnt := Length(Instring);


       For cnt := 1 to Length(Instring) DO BEGIN
         CharVal := Ord(Instring[cnt]) + Random(Range);
         If Charval > Upper THEN BEGIN
           Charval := Charval - Range;
           IF Charval < Lower Then
             Charval := Charval + Range;
         END;
         Outstring := Outstring + Chr(Charval);

       END;

       EncryptPassword := Outstring;
    END;

    FUNCTION DecryptPassword (Instring : string) : String;
      VAR
        Outstring : string;
        Cnt : byte;
        Charval : integer;
      BEGIN
        Randseed := Seed;
        Outstring := '';
        Cnt := Length(Instring);

        For cnt := 1 to Length(Instring) DO BEGIN
         CharVal := Ord(Instring[cnt]) - Random(Range);
         If Charval < Lower THEN BEGIN
           Charval := Charval + Range;
           IF Charval > Upper Then
             Charval := Charval - Range;
         END;
         Outstring := Outstring + Chr(Charval);
        END;
        DecryptPassword := Outstring;
      END;

FUNCTION IsBlank (Instring : string) : boolean;
  VAR
    Tempstring : string;
  BEGIN
    IF Length(Trim(Instring)) = 0 THEN
      IsBlank := TRUE
    ELSE
      IsBlank := FALSE;
  END;



END.  {Implementation}
