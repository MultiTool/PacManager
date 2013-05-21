{$R+}
Unit PacManager;
(* 1/14/2004 - adding 'M10' department, adding another field to pm_dbf.cfg
have to push the other fields back one. Knock wood *)


(* 6/21/2000. . .adding 'M5' department, had to increase number of
departments, also restructure pm_dbf.cfg to add one more counter. . .
had to push default dept field back one *)

(* need to restructure the config file. . .

it should be something like

pm_cfg.dbf


date, time, default dept, current dept
default dept, current dept are pointers to a record number in

counters.dbf

one record for each department

fields in counters.dbf are

Department number (same as record number), counter value, prefix,
department name, how about filename of freeform labels?

1          1            P       Main Packaging         lbllist.dbf
2          1            C       Compounding            cmpd
3          1            M4      ICU Pharmacy
4          1            M5      OR Pharmacy
5          1            M6      MC Pharmacy
6          1            IV      Civas
. . .etc. . .  *)



(*Notes have been moved to the end of the program *)
Interface

USES DOS,CRT,NEODBFIO,mouser,GRAPH, ABUtil2, Print;

CONST

   julianbase:ARRAY[1..12] OF INTEGER=(0,31,59,90,120,151,181,212,243,273,
       304,334);
   leapbase:ARRAY[1..12] OF INTEGER=(0,31,60,91,121,152,182,213,244,274,
        305,335);


   Depts : Menutype = ('P','C','BP','PS','M4','M6','Q','M5','MX','',
         '','','','','','','','','','');


   lotletters:ARRAY[1..78] OF STRING=('A','B','C'
     ,'D','E','F','G','H','I','J','K','L','M','N','O','P'
     ,'Q','R','S','T','U','V','W','X','Y','Z','AA','AB','AC','AD','AE','AF'
     ,'AG','AH','AI','AJ','AK','AL','AM','AN','AO','AP'
     ,'AQ','AR','AS','AT','AU','AV','AW','AX','AY','AZ','BA','BB','BC','BD'
     ,'BE','BF','BG','BH','BI','BJ','BK','BL','BM','BN'
     ,'BO ','BP','BQ','BR','BS','BT','BU','BV','BW','BX','BY','BZ');

   AnswerSet : Set of Char = ['Y','N',#27];
   NumOfLines = 5;
   CharsPerLine = 40;
   Title = 'Pac-Manager';
{   Version = '';}

{   Version = 'VCU/MCVHA PHARMACY SERVICES';}
{   Version = 'Revised 3/14/2001'; }
{   Version = 'Revised 1/14/2004'; }
    Version = 'Revised 5/20/2011';



   NumOfDepts = 9;
   EnterEscape = ' ENTER - select '+#4+' ESC - cancel ';
   F2Escape = ' F2 - Done '+#4+' ESC - abort ';
   MaxLabelListRecs = 200; {Number of labels that can be saved.  This is
                            arbitrary.  The actual limit is height of menu
                            times X max pages, 15 * 18 = 270}
   EchoInput = TRUE;
   DateFormat = '12/31/9999';

TYPE

   PrefixType = String[5];
   Labeltype =  Array[1..5] OF String;
   PromptType = Array[1..5] OF String[6];
(*   StyleType = Array[1..Numoflines] of Byte; *)

   DrugType = Record
                Name,
                Form,
                Conc,
                Units,
                Exp,
                Aux1,
                Aux2,
                Glass,
                Freeze,
                PFL,
                TimeReqd,
                Dose,
                ExpDate,
                Company,
                Lot,
                Mfg_Exp,
                Min_Dose,
                Stand_Dose,
                Alt_Conc,
                UnitDose,
                Max_Dose,
                Max_Conc,
                ChkDiscardDate,
                DiscardDate,
                Storage,
                Warn1,
                Warn2,
                DrugInfo,
                OurLot  : string; (* MCV Lot *)
              END;

   DrugPtrType = ^DrugType;


   ManuType = Record
                CompanyName,
                LotNumber,
                MExpDate,
                Packager : string;
                ExpInt : longint;
              END;

   ConfigType = Record
                  Date : string; {date on the last label printed}
                  LotNumbers : Array[1..NumOfDepts] OF Byte;
                  DefaultDept : Integer;
                  LabelsPerRow : byte;
                  CurrentDept : integer;
                  Lotletter : string;
                  ShortDate : string;
                  JulianDate : word;
                  Prefix : string;
                  MCVName : string;
                END;

(*   ReconType = Record
                 CheckDiscardDate : boolean;
                 DiscardDate : string;
               END; *)

VAR
{   Style : styletype; }
(*   ReconInfo : Recontype;
   NumOfLines, CharsPerLine : byte;
   Medidose : boolean;  *)
   h, min, s, hund : Word;
   y, mon, day, dow : Word;
   PrinterStatus : statustype;
   DateChanged, Quit : boolean;
   LoopCounter : byte;
   NonStand, ChkConc, ChkUD,
   DrugEntered : boolean;  {info entered by F3 (Pac-Manager) method}
   PrinterConfig : PrinterConfigType;
(*   ShortDate : string;  {date in mmddyy form to be used in lot number}
   CurrentDept : integer;
   LotLetter : string;   *)
   Config : configtype;
   Expdate : string;
(*   Prefix : Prefixtype; *)
   DrugHandle : drugptrtype;
   CommandOption : string;
   Manu : ManuType;
   LabelFileName, LabelInfoFile : string;  {LabelFileName is the .dbf file that}
                                           {contains the descriptions/text file names}
                                           {LabelInfoFile is the text file that contains}
                                           {the five lines of label text}
   PrintBackground,
   HelpBackground : AreaSaverType;
   LabelInfo : Labeltype;
   DosesString : string[3];
{   DataSource : char; }
   LotUsed, InfoEntered, InfoDeleted : boolean;
   LabelCounter: integer;
   NumberOfLabels : string;
   NumberOfPrints : integer;
   Dbfinfo, Loop, Del : char;
   i : integer;
   MoreLabels : char;
   LengthMenu, ListMenuNum : integer;
(*   Returnkey : string; *)
   ColorScheme : byte;
   HoldWinMin,HoldWinMax:WORD;
   Subdir : string;
   Xloc, Yloc : integer;
   Delfile : text;
   BigMenu : bigmenuptrtype;
   Height : integer;
   PageNum : word;
   RecordNum : integer;
   Item : string;
   Empty : boolean;
   Edit : char;
   Alphabet : alphabettype;
   DrugBkgrnd : AreaSaverType;
   DrugFile, DrugIndex : string;
(*   JulianDate : word; *)
   Retrieved,                              {was label retrieved from a freeform saved label database?}
   ManuInfoEntered : boolean;
   RecNum : longint; {record number in drug database}
   Interactive : char;
   DirInfo: SearchRec;
   PidFile: text; (* little text file, pm.pid - if it is running, this file
   will exist, should cut down on multiple instances of PM running *)

PROCEDURE InitDrugHandle (VAR Drug : drugtype);
PROCEDURE InitManu(VAR ManuInfo : Manutype);
FUNCTION BlankLine : string;
PROCEDURE GetSetDate(VAR Config : ConfigType; Interactive : char);
PROCEDURE FunctionKey(Xplace, Yplace : byte);
PROCEDURE EnterInfo(VAR Info : Labeltype; VAR Entered : boolean ; LoopVar : char;
  VAR DrugEntered, ManuInfoEntered, Retrieved, LotUsed : boolean; VAR LabelFileName : string;
  VAR Recnum : longint; VAR Quit : boolean;  VAR DrugFile : string);
PROCEDURE printlabels(VAR Line: labeltype; destination:STRING);
PROCEDURE UpdateConfig(VAR Config : configtype; VAR DateChanged : boolean);
PROCEDURE WriteOutput(VAR Line : labeltype; Numoflabels : integer; Filename : string; MEntered : boolean;
  VAR DrugInfo : DrugPtrType);
PROCEDURE WriteManuInfo(VAR WMManu : manutype; WMFilename : string; WMDrugRec : integer; VAR DrugInfo : DrugPtrType);
PROCEDURE SaveLabel(VAR Info : labeltype; Dept : string);

Implementation
{**********************************************************************}

  PROCEDURE DateString(Month, Day, Year : word; VAR DateStr : string; YearDigits : byte; Delimiter : string);
  {YearDigits = 2 for '95'; 4 for '1995'}

    VAR
      Errcode : integer;
      DayStr, MonthStr, YearStr : string;
    BEGIN
      Str(Month, Monthstr);
      If Month < 10 THEN Monthstr := '0'+MonthStr;
      Str(Day, DayStr);
      IF Day < 10 THEN DayStr := '0'+DayStr;
      Str(Year, YearStr);
      IF YearDigits = 2 THEN
        YearStr := YearStr[3]+YearStr[4];
      DateStr := MonthStr+Delimiter+DayStr+Delimiter+YearStr;

    END;



PROCEDURE DoHelp(Xloc, Yloc : byte);
  VAR
    HoldMin, HoldMax : word;
    TAttr : byte;
    Ch : char;
    FunctionArea : areasavertype;
    Fmin, FMax : word;
    W, H : byte;


    PROCEDURE DisplayHelp (FileName, BoxLabel : string; Width, Height : byte);
      VAR
        Cntr : byte;
        Helpfile : text;
        Helpstring : string;
        HelpYloc : byte;
        DirInfo: SearchRec;
        EndCh, HelpCh : char;
        Quit : boolean; {Have they had enough?}
        Wrapped : boolean; {Is help file longer than one screen?}
        BlnkLine : string; {a blank line the width of the screen to do a
                    pseudo-clrscr after each page of info is displayed}
    BEGIN
        SaveArea(5, 4, Width, Height, FunctionArea); {72, 20}
        FMin := WindMin; FMax := WindMax;
        TextBackground(Cyan);
        BlnkLine := '';
        For Cntr := 1 to (Width - 5) DO
          BlnkLine := BlnkLine+' ';
        DrawBox(5, 4, Width - 3, Height - 2, DarkGray, Cyan, DarkGray, DarkGray, #4+BoxLabel+#4,  {68, 18}
          #4+' Press any key to exit '+#4,'',#218, #196, #191, #179, #192, #217, #0, #0);
        TextColor(White);
        HelpYloc := 2;
        Wrapped := FALSE;
        Quit := FALSE;
        FindFirst(FileName, Archive, DirInfo);
        IF DosError = 0 THEN BEGIN
           Assign(Helpfile, 'C:\package\help\'+FileName);
           Reset(Helpfile);
           TextBackground(Cyan);
           While (Not EOF(helpfile)) AND Not(Quit) DO BEGIN
             Readln(helpfile, Helpstring);
             Helpstring := Trim(Helpstring);
             IF Helpstring[1] = '*' THEN BEGIN   {* means print line in different color }
               Delete(HelpString, 1, 1);
               Center(Helpstring, HelpYloc, White);
             END ELSE
               Center(Helpstring, HelpYloc, Yellow);
             Inc(HelpYloc);
             IF HelpYloc = Height - 3 {16} THEN BEGIN
               Center('Press a key to continue, ESC to quit', HelpYloc, White);
               helpch := readkey;
               IF helpch <> #27 THEN BEGIN
                 Wrapped := TRUE;
                 For HelpYloc := 2 to (Height - 3) {17} DO  {Clrscr}
                   Say(2, HelpYloc, BlnkLine, Black);
                 HelpYloc := 2;
               END ELSE
                 Quit := TRUE;
             END;
           END;
           IF Wrapped THEN
             Center('Press ENTER to continue', HelpYloc, White);
           Close(helpfile);
        END ELSE IF DosError <> 0 THEN BEGIN
             Center('Help file is missing.', HelpYloc, White);
             Inc(HelpYloc, 1);
             Center('Help is not available for this key.', HelpYloc, LightGray);
        END;
        IF Not Quit THEN
          EndCh := Readkey;
        RestoreArea(5, 4, FunctionArea);
        WindMin := FMin; WindMax := FMax;
    END;

  BEGIN
    HoldMin := WindMin;
    HoldMax := WindMax;
    TAttr := TextAttr;
    W := 72; H := 20;
    SaveArea(2, 3, 77, 23, HelpBackground);
    (* TextAttr := Colorscheme; *)
    TextColor(Black);
    TextBackground(LightGray);    {@}
    DrawBox(2, 3, 75,21, DarkGray,Cyan, DarkGray, DarkGray, #4+' Help: Editing keys '+#4,
      #4+' press ENTER or ESC to exit help '+#4,'',#218, #196, #191, #179, #192, #217, #176, #177);
    TextBackground(LightGray);
    Center(' Press a key for help on that key ', 2, Black);

    TextBackground(Black);

    Center(' Ctrl Key Combinations ', 3, White);
    TextBackground(LightGray);
    Say(5,5, 'A', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Center Label', Yellow);
    TextBackground(White);
    Say(WhereX + 1, WhereY, 'B', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Blank Line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'C', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Copy Line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'D', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Delete Line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'E', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Erase Label', Yellow);
    TextBackground(LightGray);
    Say(6, 6, 'F', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Cut before cursor', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'I', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Insert Line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'J', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Right justify', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'K', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Copy and Cut', Yellow);

    TextBackground(LightGray);
    Say(4, 7, 'L', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Aux lines', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'P', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Paste line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'R', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Cut after cursor', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'S', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Display standard doses', Yellow);
    TextBackground(LightGray);
    Say(2,  8, 'U', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Uncenter', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'V', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'VCU/MCVH Pharmacy Services', Yellow);
    TextBackground(LightGray);
    Say(WhereX+1, WhereY, 'HOME', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Go to Line #', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'END', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'end of text', Yellow);

    TextBackground(Black);
    Center(' Alt Key Combinations ',10, White);
    TextBackground(Cyan);
    Center('Alt + Line Number : move to line number', 11, Yellow);

    TextBackground(Black);
    Center(' Single Key Commands ' , 13, White);
    TextBackground(LightGray);
    Say(5, 15, 'PAGE UP', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'First line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'PAGE DOWN', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Last line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'HOME', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Beginning of line', Yellow);
    TextBackground(LightGray);
    Say(27, 16, 'END', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'End of line', Yellow);
    TextBackground(LightGray);
    Say(WhereX + 1, WhereY, 'ESC', Black);
    TextBackground(Cyan);
    Say(WhereX + 1, WhereY, 'Abort', Yellow);

    TextBackground(Black);
    Center(' Function Keys ' , 17, White);
    TextBackground(LightGray);
    Say(5, 19, 'F1', Black);
    Say(12, 19, 'F2', Black);
    Say(19, 19, 'F3', Black);
    Say(26, 19, 'F4', Black);
    Say(33, 19, 'F5', Black);
    Say(40, 19, 'F6', Black);
    Say(47, 19, 'F7', Black);
    Say(54, 19, 'F8', Black);
    Say(61, 19, 'F9', Black);
    Say(68, 19, 'F10', Black);
    TextBackground(Black);
    Center(' Ctrl+H for additional information ', 20, White);


    REPEAT
      Ch := readkey;
      IF (Ch <> #13) AND (Ch <> #27) THEN BEGIN

      CASE CH OF
         #0 : BEGIN
                Ch := readkey;
                CASE Ch OF
                  #59 : DisplayHelp('F1.hlp', ' F1 ', W, H);
                  #60 : DisplayHelp('F2.hlp', ' F2 ', W, H);
                  #61 : DisplayHelp('F3.hlp', ' F3 ', W, H);
                  #62 : DisplayHelp('F4.hlp', ' F4 ', W, H);
                  #63 : DisplayHelp('F5.hlp', ' F5 ', W, H);
                  #64 : DisplayHelp('F6.hlp', ' F6 ', W, H);
                  #65 : DisplayHelp('F7.hlp', ' F7 ', W, H);
                  #66 : DisplayHelp('F8.hlp', ' F8 ', W, H);
                  #67 : DisplayHelp('F9.hlp', ' F9 ', W, H);
                  #68 : DisplayHelp('F10.hlp', ' F10 ', W, H);

{Home, End, CTRL End}
                  #71, #79, #117 : DisplayHelp('HOME_END.HLP', ' Home, End, Ctrl+End ', W, H);

{PAGE UP, PAGE DOWN}
                  #73, #81 : DisplayHelp('PGUPDOWN.HLP', ' Page Up/Page Down ', W, H);
{CTRLHOME, Alt 1 - 5}
                  #119..#124 : DisplayHelp('CTRLHOME.HLP' , ' Ctrl+Home, Alt+Line# ', W, H);

                  ELSE BEGIN
                    (*DisplayHelp('HELPLESS.HLP', '', W, H);*)
                  END;
              END;
      END; {CASE  Ch =  #0}
(*      #1  :  DisplayHelp('CTRL_A.HLP', ' Ctrl+A ', W, H); *)
      #2  :  DisplayHelp('CTRL_B.HLP', ' Ctrl+B ', W, H);
      #3  :  DisplayHelp('CTRL_C.HLP', ' Ctrl+C ', W, H);
      #4  :  DisplayHelp('CTRL_D.HLP', ' Ctrl+D ', W, H);
      #5  :  DisplayHelp('CTRL_E.HLP', ' Ctrl+E ', W, H);
      #6  :  DisplayHelp('CTRL_F.HLP', ' Ctrl+F ', W, H);
      #8  :  DisplayHelp('PM.HLP', ' HELP ', W, H);
      #9  :  DisplayHelp('CTRL_I.HLP', ' Ctrl+I ', W, H);
      #1, #10, #21 :  DisplayHelp('JUSTIFY.HLP', ' Justification ', W, H);
      #11 :  DisplayHelp('CTRL_K.HLP', ' Ctrl+K ', W, H);
      #12 :  DisplayHelp('CTRL_L.HLP', ' Ctrl+L ', W, H);
      #16 :  DisplayHelp('CTRL_P.HLP', ' Ctrl+P ', W, H);
      #18 :  DisplayHelp('CTRL_R.HLP', ' Ctrl+R ', W, H);
(*      #21 :  DisplayHelp('CTRL_U.HLP', ' Ctrl+U ', W, H); *)
      #22 :  DisplayHelp('CTRL_V.HLP', ' Ctrl+V ', W, H);
      ELSE (* DisplayHelp('HELPLESS.HLP', '', W, H); *)



    END; {CASE}


    END; {if not #13 or #27 - Return or esc}

    UNTIL (Ch = #13) OR (Ch = #27);
    RestoreArea(2, 3, HelpBackground);
    TextAttr := TAttr;
    WindMin := HoldMin;
    WindMax := HoldMax;


  END;


PROCEDURE Abort(Mode : string; VAR Abort : boolean);
  VAR
    ch : char;
    HoldMin, HoldMax : word;
    AbortBackground : AreaSaverType;

  BEGIN
    HoldMin := WindMin;
    HoldMax := WindMax;
    TextColor(Blue);
    SaveArea(35, 19, 42, 6, AbortBackground); {30, 16}
    TextAttr := Colorscheme;
    If Mode = 'S' Then
      DrawBox(35,19,40,4,White,Red,15,15,'',  {30, 16}
      '','End program? [Y/N]',#218, #196, #191, #179, #192, #217,{ #0, #176} #176, #177);
    REPEAT

      ch := Upcase(readkey);
      If Not(ch in AnswerSet) THEN BEGIN
         GoToXY(2,3);
         TextBackground(Red); TextColor(White);
         Write(' Please answer ''Y'' or ''N'' ');
      END;
    UNTIL ch in AnswerSet;
    If (ch = 'Y') OR (ch = #27) THEN BEGIN
       Window(1,1,80,25);
       TextBackground(Black);
       TextColor(LightGray);
       Clrscr;
       Abort := TRUE;
    END ELSE BEGIN
       Abort := FALSE;
       RestoreArea(35, 19, Abortbackground); {30, 16}
       WindMin := HoldMin;
       WindMax := HoldMax;
    END;
  END;

PROCEDURE Choosefromprompt(VAR menu:prompttype; menusize,xloc,yloc,Wdt,Hgt:INTEGER; VAR choice:INTEGER; VAR exitkey:STRING);
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
      TEXTBACKGROUND(Blue); TEXTCOLOR(LightGray);
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
          TEXTCOLOR(WHITE); WRITE(menu[Choice]);
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
    TEXTCOLOR(WHITE); TEXTBACKGROUND(Blue);
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
          TEXTCOLOR(WHITE); WRITE(menu[Choice]);
          CASE instr OF
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

  PROCEDURE RetrieveLabel(Dept : string; VAR FreeInfo : labeltype;
    VAR FreeEntered, Retrieved : boolean; VAR LabelFileName : string);

    VAR
      ErrorArea,
      RetrieveArea : areasavertype;
      ErrMax, ErrMin,
      RetMax, RetMin : word;
      MenuField, { MF2, MF3, MF4,} DescField : integer;
      RetrieveKey : string;

    FUNCTION GetRecCnt(DbfFileName : string) : string;
      VAR
        TempCntStr : string;
        DbfSpec : dbfheadertype;
        DbfResult : string;
      BEGIN
        TempCntStr := '';
        Opendbasefile(Dbfspec, DbfFilename, DbfResult);
        IF DbfResult = 'GOOD' THEN BEGIN
          Str(Dbfspec.numofrecs,TempCntStr);
          Closedbasefile(Dbfspec);
        END;
        GetRecCnt := TempCntStr;

      END;
    PROCEDURE GetFileName(VAR LabelFileName : string; VAR RecNum : integer; VAR FreeInfo : LabelType; VAR Entered : boolean);
      VAR
        Filespecs  : dbfheadertype;
        FileRecord : recordarraytype;
        Results    : string;
        Tempstring : string;
        i          : byte;

      BEGIN
        For i := 1 to 5 DO BEGIN
          FreeInfo[i] := '';
        END;
        RecNum := ((PageNum - 1) * Height) + ListMenuNum;
        Opendbasefile(Filespecs, LabelFilename, Results);
        IF Results = 'GOOD' THEN BEGIN
           Readdbfrecord(Filespecs, RecNum, Filerecord, Results);
           Closedbasefile(Filespecs);
         END;
         For i := 1 to 5 DO BEGIN
           FreeInfo[i] := Filerecord[i];
         END;
         Entered := TRUE;
       END;

    PROCEDURE DeleteMenuItem(VAR FreeInfo : labeltype);
      VAR
        Tempstring, Results, Tempkey : string;
        Filespec   : dbfheadertype;

      PROCEDURE DeleteARecord(FileName : STRING; Index : LONGINT);
      {************************************************************************}
      {*  Deletes a Dbase record and moves all following records up one.      *}
      {************************************************************************}
      VAR
        Results    : STRING;
        count      : LONGINT;
        Filespecs  : dbfheadertype;
        ByteBlock  : ByteList;   {Just an array of bytes.}
      BEGIN
        GoToXY(WhereX, WhereY + 1);
        Write('Deleting record. . .');
        Opendbasefile(Filespecs, Filename, Results);
        IF Results = 'GOOD' THEN BEGIN
          count := index;
          While count <= filespecs.numofrecs DO BEGIN
            ReadBlock(filespecs, count + 1, ByteBlock);
            WriteBlock(filespecs, count, ByteBlock);
            Inc(count);
          END;
          filespecs.numofrecs := filespecs.numofrecs - 1;
          {This "with" section is optional.}
          WITH FileSpecs DO BEGIN
            SEEK(FileHandle,HeaderLength+(numofrecs*RecordLength));
            TRUNCATE(FileHandle);
          END;
          {end of optional section.}
          DbfSort(Filespecs, 1);
          closedbasefile(filespecs);
        END;
      END;

  BEGIN
     TextColor(White); TextBackground(Blue);
     Xloc := 2; Yloc := 2;
     For i := 1 to 5 DO BEGIN
       Say(Xloc, Yloc, Freeinfo[i], White);
       inc(Yloc);
     END;

     Say(Xloc, Yloc + 1, 'Delete item? [Y/N]', White);
     Del := Upcase(readkey);

     IF Del = 'Y' THEN BEGIN
       GoToXY(Xloc, Yloc + 1);
       DeleteARecord(LabelFileName, RecordNum);
     END;
     FreeEntered := FALSE;
     GoToXY(Xloc, Yloc + 1);

  END;




     BEGIN
       InitPtrMenu(BigMenu);
       TextBackground(Blue);
       Dept := Trim(Dept);
       IF Dept = 'C' THEN
         LabelfileName := 'C:\package\cmp\cmpdlist.dbf'
       ELSE IF Dept = 'PS' THEN
         LabelfileName := 'C:\package\pca\pcalist.dbf'
       ELSE
         LabelfileName := 'C:\package\pack\lbllist.dbf';
       REPEAT
         Height := 15;
         Empty := TRUE;
         MenuField := 0; DescField := 2;
         (*       MF2 := 0; MF3 := 0; MF4 := 0;  {These are pretty much just placeholders} *)
         BuildBigPtrMenu(LabelFileName, BigMenu, Height, MenuField, 0, 0, 0, DescField, 0, 0, Empty, 'Y');
(*       BBPtrMenu(LabelFileName, BigMenu, Height, MenuField, 3, 4, 0, DescField, 0, 0, Empty); *)
(*       BuildBigMenu2(LabelFileName, BigMenu, Height,{ Fieldnum,} Empty, 'N'); *)
         IF Empty THEN BEGIN
           SaveArea(30, 6, 47, 10, ErrorArea);  {15, 8}
           ErrMin := WindMin; ErrMax := WindMax;
           TextBackground(Blue);
           DrawBox(30,6,44,7,White,Red,15,15,' **ERROR** ',
            #4+' Press ENTER to continue '+#4,'',#218, #196, #191, #179, #192, #217, #0, #177);
           TextBackground(Red);
           Center(LabelfileName, 2, White);
           Center('is missing or empty. Please', 3, White);
           Center('enter information in editing phase.', 4, White);
           InfoEntered := FALSE;
           Retrievekey := 'ESC';
           Readln;
           RestoreArea(30, 6, ErrorArea);
           WindMin := ErrMin; WindMax := ErrMax;
         END ELSE BEGIN
           SaveArea(4, 2, 76, 23, RetrieveArea);
           RetMax := WindMax; RetMin := WindMin;
           TextAttr := Colorscheme;
           DrawBox(4,2, 74, 22,LightGray, Blue, White,
             White,' '+LabelfileName+' contains '+GetRecCnt(LabelfileName)+' labels ',
             #4+' '+#24+' '+#25+' - choose, ENTER - select, ESC - abort, DEL - delete '+#4,
             '',#218 ,#196, #191, #179, #192, #217, #176, #177);
           ListMenuNum := 1;
           PageNum := 1;
           Item := '';
           EnhancedPtrMenu(BigMenu, PageNum, 7, 4, 70, Height + 3, ListMenuNum, Retrievekey,
              White, Blue, 'Y', 6, 17, 'ESC RETURN DEL');
(*           EnhPtrMenu(BigMenu, PageNum, 7, 4, 70, Height + 3, ListMenuNum, Retrievekey, White, Blue, 'N', 25, 14); *)
           IF Retrievekey = 'RETURN' THEN
             Retrieved := TRUE;
           IF (Retrievekey <> 'ESC') THEN BEGIN
             GetFileName(LabelfileName, RecordNum, FreeInfo, FreeEntered);
             IF RetrieveKey = 'DEL' THEN BEGIN
               InfoDeleted := FALSE;
               DrawBox(14,3,48,12,LightGray,Blue,15, Yellow, '',
                 '','',#218, #196, #191, #179, #192, #217, #0, #177);
               DeleteMenuItem(FreeInfo);
             END;
             END ELSE IF Retrievekey = 'ESC' THEN BEGIN
               FreeEntered := FALSE;
               (* Retrievekey := ''; *)
             END;
             RestoreArea(4, 2, RetrieveArea);
             WindMax := RetMax; WindMin := RetMin;
           END;
         UNTIL (RetrieveKey = 'RETURN') OR (RetrieveKey = 'ESC');
         DisposeMenu(BigMenu);
         END;


(* Test version -- commented out ----------------------- *)

(*  PROCEDURE RetrieveLabel(Dept : string; VAR FreeInfo : labeltype;
    VAR FreeEntered, Retrieved : boolean; VAR LabelFileName : string);

    VAR
      ErrorArea,
      RetrieveArea : areasavertype;
      ErrMax, ErrMin,
      RetMax, RetMin : word;
      MenuField, DescField : integer;
      RetrieveKey : string;
      DoSomething : boolean; { did the user press a key that requires action }

    FUNCTION GetRecCnt(DbfFileName : string) : string;
      VAR
        TempCntStr : string;
        DbfSpec : dbfheadertype;
        DbfResult : string;
      BEGIN
        TempCntStr := '';
        Opendbasefile(Dbfspec, DbfFilename, DbfResult);
        IF DbfResult = 'GOOD' THEN BEGIN
          Str(Dbfspec.numofrecs,TempCntStr);
          Closedbasefile(Dbfspec);
        END;
        GetRecCnt := TempCntStr;

      END;
    PROCEDURE GetFileName(VAR LabelFileName : string; VAR RecNum : integer; VAR FreeInfo : LabelType; VAR Entered : boolean);
      VAR
        Filespecs  : dbfheadertype;
        FileRecord : recordarraytype;
        Results    : string;
        Tempstring : string;
        i          : byte;

      BEGIN
        For i := 1 to 5 DO BEGIN
          FreeInfo[i] := '';
        END;
        RecNum := ((PageNum - 1) * Height) + ListMenuNum;
        Opendbasefile(Filespecs, LabelFilename, Results);
        IF Results = 'GOOD' THEN BEGIN
           Readdbfrecord(Filespecs, RecNum, Filerecord, Results);
           Closedbasefile(Filespecs);
         END;
         For i := 1 to 5 DO BEGIN
           FreeInfo[i] := Filerecord[i];
         END;
         Entered := TRUE;
       END;

    PROCEDURE DeleteMenuItem(VAR FreeInfo : labeltype);
      VAR
        Tempstring, Results, Tempkey : string;
        Filespec   : dbfheadertype;

      PROCEDURE DeleteARecord(FileName : STRING; Index : LONGINT);
      {************************************************************************}
      {*  Deletes a Dbase record and moves all following records up one.      *}
      {************************************************************************}
      VAR
        Results    : STRING;
        count      : LONGINT;
        Filespecs  : dbfheadertype;
        ByteBlock  : ByteList;   {Just an array of bytes.}
      BEGIN
        GoToXY(WhereX, WhereY + 1);
        Write('Deleting record. . .');
        Opendbasefile(Filespecs, Filename, Results);
        IF Results = 'GOOD' THEN BEGIN
          count := index;
          While count <= filespecs.numofrecs DO BEGIN
            ReadBlock(filespecs, count + 1, ByteBlock);
            WriteBlock(filespecs, count, ByteBlock);
            Inc(count);
          END;
          filespecs.numofrecs := filespecs.numofrecs - 1;
          {This "with" section is optional.}
          WITH FileSpecs DO BEGIN
            SEEK(FileHandle,HeaderLength+(numofrecs*RecordLength));
            TRUNCATE(FileHandle);
          END;
          {end of optional section.}
          DbfSort(Filespecs, 1);
          closedbasefile(filespecs);
        END;
      END;



  BEGIN
     TextColor(White); TextBackground(Blue);
     Xloc := 2; Yloc := 2;
     For i := 1 to 5 DO BEGIN
       Say(Xloc, Yloc, Freeinfo[i], White);
       inc(Yloc);
     END;

     Say(Xloc, Yloc + 1, 'Delete item? [Y/N]', White);
     Del := Upcase(readkey);

     IF Del = 'Y' THEN BEGIN
       GoToXY(Xloc, Yloc + 1);
       DeleteARecord(LabelFileName, RecordNum);
     END;
     FreeEntered := FALSE;
     GoToXY(Xloc, Yloc + 1);

  END;




     BEGIN
       InitPtrMenu(BigMenu);
       TextBackground(Blue);
       Dept := Trim(Dept);
{*       DoSomething := TRUE; }
       Empty := TRUE;
       SaveArea(4, 2, 76, 23, RetrieveArea);
       RetMax := WindMax; RetMin := WindMin;

       IF Dept = 'C' THEN
         LabelfileName := 'C:\package\cmp\cmpdlist.dbf'
       ELSE IF Dept = 'PS' THEN
         LabelfileName := 'C:\package\pca\pcalist.dbf'
       ELSE
         LabelfileName := 'C:\package\pack\lbllist.dbf';
       REPEAT
         Height := 15;
{         Empty := TRUE; }
         MenuField := 0; DescField := 2;
         {MF2 := 0; MF3 := 0; MF4 := 0;}  {These are pretty much just placeholders}

{         IF DoSomething THEN }
           BuildBigPtrMenu(LabelFileName, BigMenu, Height, MenuField, 0, 0, 0, DescField, 0, 0, Empty, 'Y');
         IF Empty THEN BEGIN
           SaveArea(30, 6, 47, 10, ErrorArea);  {15, 8}
           ErrMin := WindMin; ErrMax := WindMax;
           TextBackground(Blue);
           DrawBox(30,6,44,7,White,Red,15,15,' **ERROR** ',
            #4+' Press ENTER to continue '+#4,'',#218, #196, #191, #179, #192, #217, #0, #177);
           TextBackground(Red);
           Center(LabelfileName, 2, White);
           Center('is missing or empty. Please', 3, White);
           Center('enter information in editing phase.', 4, White);
           InfoEntered := FALSE;
           Retrievekey := 'ESC';
           Readln;
           RestoreArea(30, 6, ErrorArea);
           WindMin := ErrMin; WindMax := ErrMax;
         END ELSE BEGIN
           { SaveArea(4, 2, 76, 23, RetrieveArea);
           RetMax := WindMax; RetMin := WindMin; }
           TextAttr := Colorscheme;
           DrawBox(4,2, 74, 22,LightGray, Blue, White,
             White,' '+LabelfileName+' contains '+GetRecCnt(LabelfileName)+' labels ',
             #4+' '+#24+' '+#25+' - choose, ENTER - select, ESC - abort, DEL - delete '+#4,
             '',#218 ,#196, #191, #179, #192, #217, #176, #177);
           ListMenuNum := 1;
           PageNum := 1;
           Item := '';
           EnhancedPtrMenu(BigMenu, PageNum, 7, 4, 70, Height + 3, ListMenuNum, Retrievekey, White, Blue, 'Y',
             6, 17,'ESC RETURN DEL');
           { IF (RetrieveKey = 'F1') OR (RetrieveKey = 'F2') THEN
              DoSomething := FALSE
           ELSE
              DoSomething := TRUE; }
           IF Retrievekey = 'RETURN' THEN
             Retrieved := TRUE;
{           IF (Retrievekey <> 'ESC') THEN BEGIN }
             IF (RetrieveKey = 'RETURN') OR (RetrieveKey = 'DEL') THEN BEGIN
             GetFileName(LabelfileName, RecordNum, FreeInfo, FreeEntered);
             IF RetrieveKey = 'DEL' THEN BEGIN
               InfoDeleted := FALSE;
               DrawBox(14,3,48,12,LightGray,Blue,15, Yellow, '',
                 '','',#218, #196, #191, #179, #192, #217, #0, #177);
               DeleteMenuItem(FreeInfo);
             END;
           END ELSE IF Retrievekey = 'ESC' THEN BEGIN
               FreeEntered := FALSE;
               { Retrievekey := ''; }
           END;
  {           IF DoSomething THEN BEGIN }
             RestoreArea(4, 2, RetrieveArea);
             WindMax := RetMax; WindMin := RetMin;
{           END;  }
         END;
         UNTIL (RetrieveKey = 'RETURN') OR (RetrieveKey = 'ESC');
         DisposeMenu(BigMenu);
         END;  *)




FUNCTION BlankLine : string;
  VAR
    Tempstring : string;
    i : byte;

  BEGIN
    Tempstring := '';
    For i := 1 to CharsPerLine DO
      Tempstring := Tempstring + ' ';

      Blankline := Tempstring;

  END;

  PROCEDURE GetDrug(Filename : string; {VAR DrugRec : recordarraytype;}
    VAR Recnum : longint; VAR NumOfFields : integer; VAR Drug : drugtype; VAR DrugGotten : boolean);
    VAR
      Results : string;
      Dbfspec : dbfheadertype;
      DrugRec : recordarraytype;
      StorageType : string;
      TmpMenuNum : integer;
      TmpStr1, TmpStr2 : string;

      FUNCTION GlassOrPlastic(Var GorPNum : integer) : STRING;
        CONST
          GorPMenu : MenuType = (' GLASS ',
                                 'PLASTIC',
                            '','','','','','','','','','','','','','','','','','');

        VAR
          GorPArea : Areasavertype;
          GorPMin, GorPMax : word;
(*          GorPNum : integer; *)
          GorPKey : string;
        BEGIN
          SaveArea(32, 6, 44, 8, GorPArea);
          GorPMin := WindMin; GorPMax := WindMax;
          TextBackground(Blue);
          DrawBox(32, 6, 42,6,LightGray, Blue, White,Yellow,#4+' Drug will be packaged in. . .?'+#4,
              EnterEscape,'',#218 ,#196, #191, #179, #192, #217, #0, #177);
          TextBackground(Blue);
(*          GorPNum := 1; *)
          GorPKey := '';
          ChoosefromMenu(GorPMenu, 2, 19, 2, 8, 2, GorPNum, GorPKey, 15, Blue);
          GlassOrPlastic := GorPMenu[GorPNum];

          RestoreArea(32, 6, GorPArea);
          WindMin := GorPMin; WindMax := GorPMax;
        END;

      FUNCTION LongerExpDate(VAR ExpStr1, ExpStr2 : string) : integer;
        (*This function compares the expdate and the alt_expdate,
        the number it returns will be the menu number for the
        Glass or plastic function. If the drug is stable in plastic
        longer, that will be the default menu choice, otherwise,
        glass will be the default. *)
        VAR
          ExpNum1, ExpNum2 : byte;
          Errcode : integer;
        BEGIN
          Val(ExpStr1, ExpNum1, Errcode);
          Val(ExpStr2, ExpNum2, Errcode);
          IF ExpNum2 >= ExpNum1 THEN
            LongerExpDate := 2
          ELSE
            LongerExpDate := 1;
        END;

      BEGIN
        DrugGotten := FALSE;
        Drug.Name := '';
        Drug.Form := '';
        Drug.Conc := '';
        Drug.Units := '';
        Drug.Exp := '';
        Drug.Glass := '';
        Drug.PFL := '';
        DRUG.Freeze := '';
        DRUG.Aux1  := '';
        DRUG.Aux2 := '';
        Drug.TimeReqd := '';
        Drug.Ourlot := '';
        StorageType := '';
        Filename := Trim(FileName);
        Opendbasefile(Dbfspec, Filename, Results);
        IF Results = 'GOOD' THEN BEGIN
          Readdbfrecord(Dbfspec, Recnum, Drugrec, Results);
          Numoffields := Dbfspec.numoffields;
          Closedbasefile(Dbfspec);
          With DRUG DO BEGIN
             DrugGotten := TRUE;
             Name := Trim(DrugRec[1]);
             Form := Trim(DrugRec[2]);
             Units := Trim(DrugRec[4]);
             Glass := Trim(DrugRec[8]);
(*NEXT TWO LINES WERE MOVED OUT OF COMMENTED SECTION.
THERE IS NO GLASS OR PLASTIC CHOICE AT THE MOMENT.
EVERYTHING IS PACKAGED IN PLASTIC *)

             Exp  := Trim(DrugRec[5]);    (* <========== *)
             IF Drug.Glass = 'T' Then
               Drug.Storage := 'GLASS' ELSE
             Drug.Storage := 'PLASTIC';          (* <========== *)

             (* IF (FileName = 'liquids.dbf') AND (Glass <> 'T') THEN BEGIN
               TmpStr1 := Trim(DrugRec[5]); TmpStr2 := Trim(DrugRec[24]);
               TmpMenuNum := LongerExpDate(TmpStr1, TmpStr2);
               StorageType := GlassOrPlastic(TmpMenuNum);
               IF Trim(StorageType) = 'GLASS' THEN BEGIN
                 Drug.Storage := 'GLASS';
                 Exp := Trim(DrugRec[5])
               END ELSE BEGIN
                 Drug.Storage := 'PLASTIC';
                 Exp := Trim(DrugRec[24]);
               END
             END ELSE BEGIN
               Exp  := Trim(DrugRec[5]);
               Drug.Storage := '';
             END; *)
             OurLot := Config.Prefix+'-'+Config.ShortDate+Config.LotLetter;
             Aux1 := Trim(DrugRec[6]);
             Aux2 := Trim(DrugRec[7]);
             Company := Trim(DrugRec[12]);
             Company := Pad(Company, dbfspec.flds[12].fdlength);
             Lot := Trim(DrugRec[13]);
             Lot := Pad(Lot, dbfspec.flds[13].fdlength);
             Mfg_Exp := Trim(DrugRec[14]);
             Mfg_Exp := Pad(Mfg_Exp, dbfspec.flds[14].fdlength);
             ChkDiscardDate := Trim(DrugRec[22]);
             DrugInfo := Trim(DrugRec[21]);
             IF Filename <> 'solids.dbf' THEN BEGIN
               Conc := Trim(DrugRec[3]);
               Glass := Trim(DrugRec[8]);
               Min_Dose := Trim(DrugRec[15]);
               Stand_Dose := Trim(DrugRec[16]);
               Alt_Conc := Trim(DrugRec[17]);
               UnitDose := Trim(DrugRec[18]);
               Max_Dose := Trim(DrugRec[19]);
               Max_Conc := Trim(DrugRec[20]);

               DiscardDate := Trim(DrugRec[23]);
(*               IF Trim(DrugRec[22]) = 'T' THEN
                 ReconInfo.CheckDiscardDate := TRUE
               ELSE
                 ReconInfo.CheckDiscardDate := FALSE; *)
               IF (Filename = 'inject.dbf') THEN BEGIN
                 Pfl := Trim(DrugRec[9]);
                 Freeze := Trim(DrugRec[10]);
                 TimeReqd := Trim(DrugRec[11]);
               END;
             END;
           END;
        END ELSE BEGIN
          DrugGotten := FALSE;
          Writeln(Filename, ' is damaged. Result of opening = ', Results);
          Readln;
        END;
      END;

 FUNCTION GetDateInt (Datestring : string) : longint;
   (* Convert '12/01/1995' to '19951201' to 19951201 to compare
   to the proposed expiration date and today's date *)
   VAR
     ExpIntStr : string;
     ExpInt : Longint;
     Errcode : integer;

   BEGIN
     ExpIntStr := DateString[7]+DateString[8]+DateString[9]+DateString[10]+DateString[1]
       +DateString[2]+DateString[4]+DateString[5];
     Val(ExpIntStr, ExpInt, Errcode);
     IF Errcode = 0 THEN
       GetDateInt := ExpInt;
   END;

 PROCEDURE GetManuInfo(VAR ManuInfo : ManuType; VAR DrugInfo : Drugptrtype;
  VAR Entered : boolean; UseDrugInfo : char; X, Y : byte; ScreenMode : char (*; VAR ReconInfo : ReconType*));
   Const
     ManuMenu:MenuType=  ('   Company Name:',
                          '     Lot Number:',
                          '      Exp. Date:',
                          '    Packaged by:',
                          ' Reconst/Opened:','','','','','','','','','','','','','','','');


   VAR
     MenuLength : integer;
     TodayInt : longint;
     Year, Month, Day, DayOfWeek : word;
     Today : String;
     ManuBkgrnd : AreaSaverType;
     ManuMax, ManuMin : word;
     ManuMenuNum : integer;
     ManuKey,
     Returnkey : string;
     Aborted,
     Done : boolean;
     Errcode : integer;
     ExpIntStr : string;

   BEGIN
     With ManuInfo DO BEGIN
       Aborted := FALSE;
       SaveArea(X, Y, 53, 14, ManuBkgrnd); {52}
       ManuMax := WindMax; ManuMin := WindMin;
       CASE ScreenMode of
         'A' : BEGIN  {Overwrites the normal label box, matches that background}
                      {User pressed F5}
                 TextAttr := Colorscheme;
(* {%} DrawBox(27,5,51,12,LightGray,Blue,15,15,'','','',
       #218, #196, #191, #179, #192, #217, #176, #177); *)


                 DrawBox(X,Y,51,12,LightGray,Blue,White,Yellow,#4+' Manufacturer''s Information '+#4,
                   F2Escape,'',#218, #196, #191, #179, #192, #217, #176, #177);
                 TextBackground(Blue);
               END;
         'B' : BEGIN
                      {User pressed F3, box appears over drug info menu}
                 DrawBox(X,Y,51,12,LightGray,Blue,White,Yellow,#4+' Manufacturer''s Information '+#4,
                 F2Escape,'',#218, #196, #191, #179, #192, #217, #0, #177);
               END;
       END;

(*       DrawBox(27,5,51,10,LightGray,Blue,15,15,'','','',
       #218, #196, #191, #179, #192, #217, #176, #177); *)

                    {50, 12}
       IF UseDrugInfo = 'Y' THEN BEGIN
         CompanyName := DrugInfo^.Company;
         IF trim(CompanyName) = '' THEN CompanyName := '                         ';
         LotNumber := DrugInfo^.Lot;
         IF Trim(LotNumber) = '' THEN LotNumber :=   '                    ';
         MExpDate := DrugInfo^.Mfg_exp;
       END ELSE BEGIN
         CompanyName := '                         ';
         LotNumber :=   '                    ';
         MExpDate  := '00/00/0000';
       END;
       Packager  := '   ';


       Say(21, 3, CompanyName, White);
       Say(21, 4, LotNumber, White);
       Say(21, 5, MExpDate, White);
       IF DrugInfo^.ChkDiscardDate = 'T' THEN
         Say(21, 7, DrugInfo^.DiscardDate, White);


       REPEAT
         IF Trim(CompanyName) <> '' THEN
           ManuMenuNum := 4
         ELSE
           ManuMenuNum := 1;
         Entered := FALSE;
         Done := FALSE;
         ManuKey := '';
         Returnkey := '';
         IF DrugInfo^.ChkDiscardDate = 'T' THEN
           MenuLength := 5
         ELSE
           MenuLength := 4;


         REPEAT
           IF Not Done THEN BEGIN
             ChoosefromMenu(ManuMenu, MenuLength, 2, 2, 4, 20, ManuMenuNum, Returnkey, White, Blue);
             IF ((Returnkey <> 'F2') AND (Returnkey <> 'ESC')) AND Not(Done) THEN BEGIN
               CASE MANUMENUNUM OF
                 1 : BEGIN
                       IF Not DONE THEN BEGIN
                         ReadString(21, 3, 'C', CompanyName, Length(CompanyName), Manukey, White, Black, EchoInput);
                         Inc(ManuMenuNum);
                         DrugInfo^.Company := CompanyName;


                       END;
                     END;
                 2 : BEGIN
                       IF Not DONE THEN BEGIN
                         ReadString(21, 4, 'C', LotNumber, Length(LotNumber), Manukey, White, Black, EchoInput);
                         Inc(ManuMenuNum);
                         DrugInfo^.Lot := LotNumber;

                       END;
                     END;
                 3 : BEGIN
                       IF Not DONE THEN BEGIN
                         Getchunky(21, 5, MExpDate, Black, LightGray, DateFormat, Manukey);
                         Inc(ManuMenuNum);
                         DrugInfo^.Mfg_exp := MExpDate;

                       END;
                     END;
                 4 : BEGIN
                       IF Not DONE THEN BEGIN
                         ReadString(21, 6, 'C', Packager, Length(Packager), Manukey, White,Black, EchoInput);
                         IF DrugInfo^.ChkDiscardDate = 'T' THEN
                           ManuMenuNum := 5
                         ELSE
                           ManuMenuNum := 1;
                       END;
                     END;
                 5 : BEGIN
                         Say(4, 8, 'This drug is reconstituted prior to use or ', Yellow);
                         Say(4, 9, 'packaged from an "open" bottle. Enter the date', Yellow);
                         Say(4, 10, 'the drug was reconstituted or bottle opened.', Yellow);
                         Getchunky(21, 7, DrugInfo^.DiscardDate, Black, LightGray, DateFormat, Manukey);
                         Say(4, 8, '                                               ', White);
                         Say(4, 9, '                                               ', White);
                         Say(4, 10, '                                               ', White);
                         ManuMenuNum := 1;
                     END;
                END;
             END;
           END;
           IF (Manukey = 'F2') OR (Manukey = 'ESC') OR (Returnkey = 'ESC') OR (Returnkey = 'F2') THEN
              Done := TRUE;

         UNTIL DONE;
         IF (ManuKey = 'F2') OR (Returnkey = 'F2') THEN BEGIN
           IF (Length(Trim(CompanyName)) <> 0) AND (Length(Trim(LotNumber)) <> 0) AND
              (Length(Trim(MExpDate)) <> 0) AND (Length(Trim(Packager)) <> 0) THEN BEGIN
             ExpInt := GetDateInt(Trim(MExpDate));
             GetDate(Year, Month, Day, DayOfWeek);
             DateString(Month, Day, Year, Today, 4, '/');
             TodayInt := GetDateInt(Today);
             IF ExpInt >= TodayInt THEN BEGIN
               Entered := TRUE;
               CompanyName := Trim(CompanyName);
               LotNumber := Trim(LotNumber);
               MExpDate := Trim(MExpDate);
               Packager := Trim(Packager);
             END
             ELSE BEGIN
               Center('Product is expired.', 10, Red);
               Readln;
               TextColor(Blue);
               Say(2, 10, '                                            ', LightGray);
               Entered := FALSE;

             END;


           END
           ELSE BEGIN
             Center('You must complete each field.', 10, Yellow);
             Readln;
             TextColor(Blue);
             Say(2, 10, '                                            ', Blue);
             IF Length(Trim(CompanyName)) = 0 THEN
                CompanyName := '                         ';
             IF Length(Trim(LotNumber)) = 0 THEN
                LotNumber :=   '                    ';
             IF Length(Trim(Packager)) = 0 THEN
                Packager  := '   ';
           END;
         END ELSE IF (ManuKey = 'ESC') OR (Returnkey = 'ESC') THEN
             Aborted := TRUE;

       UNTIL Entered OR Aborted;
       RestoreArea(X, Y, ManuBkgrnd);
       WindMax := ManuMax; WindMin := ManuMin;
     END;

   END;





 PROCEDURE GetQty(VAR Dose : string;  X, Y : byte; VAR Done : boolean; VAR Timestr : string);


     FUNCTION RemoveCommas(Instring : String) : String;
       CONST
          Numeric : SET OF CHAR = ['0','1','2','3','4','5','6','7','8','9','.'];
       VAR
         i : integer;
         TempString : string;
         NonNumeric : integer;
       BEGIN
         TempString := '                ';
         NonNumeric := 0;
         For i := 1 to Length(Instring) DO BEGIN
           IF Instring[i] IN Numeric THEN
             TempString[i - NonNumeric] := Instring[i]
           ELSE  { IF NOT(Instring[i] IN Numeric) THEN}
             Inc(NonNumeric);
         END;      {FOR}
         RemoveCommas := Trim(TempString);
       END; {END of FUNCTION RemoveCommas}

   PROCEDURE InsertComma(VAR Dose : string; Dosenum : real);
    {*********************************************************************}
    {Inserts commas into the dose string to be printed to label.  If Dose is
     1000 mg, procedure outputs 1,000.}
    {*********************************************************************}
   BEGIN
     If (DoseNum > 999) AND (DoseNum < 9999) THEN
       Insert(',',Dose, 2)
     ELSE IF (DoseNum > 9999) AND (DoseNum < 99999) THEN
       Insert(',',Dose, 3)
     ELSE IF (DoseNum > 99999) AND (DoseNum < 999999) THEN
       Insert(',',Dose, 4)
     ELSE IF (DoseNum > 999999) AND (DoseNum < 9999999) THEN BEGIN
       Insert(',',Dose, 2);
       Insert(',',Dose, 6);
     END ELSE IF (DoseNum > 9999999) AND (DoseNum < 99999999) THEN BEGIN
       Insert(',',Dose,3);
       Insert(',',Dose,7);
     END;
   END; {END of PROCEDURE InsertCommas}




   FUNCTION TrimZeros(Instring : string) : string;
     VAR
       Done : boolean;
     BEGIN
       Instring := Trim(Instring);
       Done := FALSE;
       IF Pos('.', Instring) <> 0 THEN BEGIN
         While NOT Done DO BEGIN
           IF (Instring[length(instring)] = '0') OR (Instring[length(instring)] = '.') THEN BEGIN
             IF Instring[Length(instring)] = '.' THEN
               Done := TRUE;
             Delete(Instring, Length(instring), 1);

           END ELSE
             Done := TRUE;
         END;
         TrimZeros := Instring;
       END;
     END; {END of FUNCTION TrimZeros}


    PROCEDURE CheckDose(VAR DoseNum : real; VAR Drug : drugtype; VAR Nonstandard, CheckConc, CheckUD : boolean);
      Const
        MaxStand = 50;
        MaxUnitDose = 10;
      Type
        StandType = Array[1..MaxStand] OF real;
        UDType = Array[1..MaxUnitDose] of real;
      VAR
        UnitDoses : UDType;
        StandardDoses : StandType;
        Dosestr,
        Tempstring : string;
        Errcode : integer;
        unitdosecount, standarddosecount, h : byte;  {number of standard doses}
        Found : boolean;
        CheckDoseBackground : areasavertype;
        CDMin, CDMax : word;
        OldColor : Word;
        MaxDoseNum,
        MinDoseNum : real;
        CompChar : char; {I for inclusive, E for exclusive, the first character
                          of the MIN/MAX field.  E4 means >4 or <4, I4 means >=4, <=4}
      BEGIN
        SaveArea(10, 10, 55, 8, CheckDoseBackground);
        CDMin := WindMin; CDMax := WindMax;
        Nonstandard := FALSE;
        CheckConc := FALSE;
        CheckUD := FALSE;
        Drug.Warn1 := '';
        Drug.Warn2 := '';
        With Drug DO BEGIN
          Standarddosecount := 0;
          IF Trim(Stand_Dose) <> '' THEN BEGIN
            Tempstring := Stand_Dose;
            For h := 1 to MaxStand DO
              StandardDoses[h] := 0;
            While Length(Tempstring) > 0 DO BEGIN
              Dosestr := Copy(Tempstring, 1, (Pos(';', Tempstring) - 1));
              Inc(standarddosecount);
              Val(Dosestr, StandardDoses[standarddosecount], Errcode);
              Delete(Tempstring, 1, Pos(';', Tempstring));
            END;
            Found := FALSE;
            h := 1;
            While Not(Found) AND (h <= standarddosecount) DO BEGIN
              IF Dosenum = StandardDoses[h] THEN
                Found := TRUE
              ELSE
                inc(h);
            END;
            If Not Found then BEGIN
              DrawBox(10, 10, 53,6,White,Red,15,15,'',
                '','',#218, #196, #191, #179, #192, #217, #0, #177);
              OldColor := TextAttr;
              GoToXY(2, 2);
              TextColor(White);
              TextBackground(Red);
              Write(Dosenum:9:4, ' is not a standard dose.');
              Nonstandard := TRUE;
              Readln;
              TextAttr := OldColor;
            END;
          END;
          IF Trim(Min_Dose) <> '' THEN BEGIN
            Min_Dose := Trim(Min_Dose);
            CompChar := Min_Dose[1];
            Delete(Min_Dose, 1, 1);
            Val(Min_Dose, MinDoseNum, Errcode);
            IF CompChar = 'E' THEN BEGIN
              IF MinDoseNum > DoseNum THEN BEGIN
                DrawBox(10, 10, 53,6,White,Red,15,15,'',
                  '','',#218, #196, #191, #179, #192, #217, #0, #177);
                OldColor := TextAttr;
                GoToXY(2, 2);
                TextColor(White);
                TextBackground(Red);
                CheckConc := TRUE;
                Drug.Warn1 := 'Dose may be too small for this conc.';
                Write(Drug.Warn1);
                GoToXY(2, 3);
                Drug.Warn2 := 'You may need to use '+Trim(Alt_conc)+' conc.';
                Write(Drug.Warn2);
                Readln;
                TextAttr := OldColor;
              END;
              END ELSE BEGIN
                IF MinDoseNum >= DoseNum THEN BEGIN
                   DrawBox(10, 10, 53,6,White,Red,15,15,'',
                     '','',#218, #196, #191, #179, #192, #217, #0, #177);
                   OldColor := TextAttr;
                   GoToXY(2, 2);
                   TextColor(White);
                   TextBackground(Red);
                   CheckConc := TRUE;
                   Drug.Warn1 := 'Dose may be too small for this conc.';
                   Write(Drug.Warn1);
                   GoToXY(2, 3);
                   Drug.Warn2 := 'You may need to use '+Trim(Alt_conc)+' conc.';
                   Write(Drug.Warn2);
                   Readln;
                   TextAttr := OldColor;
                END;
              END;
          END;
          IF Trim(Max_Dose) <> '' THEN BEGIN
            Max_Dose := Trim(Max_Dose);
            CompChar := Max_Dose[1];
            Delete(Max_Dose, 1, 1);
            Val(Max_Dose, MaxDoseNum, Errcode);
            IF CompChar = 'E' THEN BEGIN
              IF MaxDoseNum <= DoseNum THEN BEGIN
                DrawBox(10, 10, 53,6,White,Red,15,15,'',
                  '','',#218, #196, #191, #179, #192, #217, #0, #177);
                OldColor := TextAttr;
                GoToXY(2, 2);
                TextColor(White);
                TextBackground(Red);
                CheckConc := TRUE;
                Drug.Warn1 := 'Dose may be too large for this conc.';
                Write(Drug.Warn1);
                GoToXY(2, 3);
                Drug.Warn2 := 'You may need to use '+Trim(Max_conc)+' conc.';
                Write(Drug.Warn2);
                Readln;
                TextAttr := OldColor;
              END;
            END ELSE BEGIN
              IF MaxDoseNum <= DoseNum THEN BEGIN
                DrawBox(10, 10, 53,6,White,Red,15,15,'',
                  '','',#218, #196, #191, #179, #192, #217, #0, #177);
                OldColor := TextAttr;
                GoToXY(2, 2);
                TextColor(White);
                TextBackground(Red);
                CheckConc := TRUE;
                Drug.Warn1 := 'Dose may be too large for this conc.';
                Write(Drug.Warn1);
                GoToXY(2, 3);
                Drug.Warn2 := 'You may need to use '+Trim(Max_conc)+' conc.';
                Write(Drug.Warn2);
                Readln;
                TextAttr := OldColor;
              END;
            END;
          END;
          IF Trim(UnitDose) <> '' THEN BEGIN
            Tempstring := UnitDose;
            unitdosecount := 0;
            For h := 1 to MaxUnitDose DO
              UnitDoses[h] := 0;
            While Length(Tempstring) > 0 DO BEGIN
              Dosestr := Copy(Tempstring, 1, (Pos(';', Tempstring) - 1));
              Inc(unitdosecount);
              Val(Dosestr, UnitDoses[unitdosecount], Errcode);
              Delete(Tempstring, 1, Pos(';', Tempstring));
            END;
            Found := FALSE;
            h := 1;
            While Not(Found) AND (h <= unitdosecount) DO BEGIN
              IF Dosenum = UnitDoses[h] THEN
                Found := TRUE
              ELSE
                inc(h);
            END;
            If Found then BEGIN
              DrawBox(10, 10, 53,6,White,Red,15,15,'',
                '','',#218, #196, #191, #179, #192, #217, #0, #177);
              OldColor := TextAttr;
              GoToXY(2, 2);
              TextColor(White);
              TextBackground(Red);
              Write('This strength may be commercially available.');
              CheckUD := TRUE;
              Readln;
              TextAttr := OldColor;
            END;
          END;
        END;
        RestoreArea(10, 10, CheckDoseBackground);
        WindMin := CDMin; WindMax := CDMax;
      END;    {END of PROCEDURE CheckDose}


    CONST {DECLARATIONS FOR PROCEDURE GetQty}
      DrugInfo : menutype = ('Name/Form','Exp','Conc','Aux1','Aux2','Dose',
      '','','','','','','','','','','','','',''{,'','','','','','','','','',''});
      AMPM : menutype = ('AM','PM','','','','','','','','','','','','','','','','','','');
      Offset = 10;

    VAR
      Quan, Volume, Concentration : real;
      Errcode : integer;
      Volstr : string;
      AMPMNum : integer;
      HoldMin, HoldMax : word;
      ClassBkgrnd : areasavertype;
      QtyKey : string;


    BEGIN


(*      HoldMin:=WINDMIN; HoldMax:=WINDMAX;
      SaveArea(2, 2, 65, 12, ClassBkgrnd);

      TextColor(Blue); *)
      TextAttr := Colorscheme;
                   {had been 76}
      DrawBox(2, 2, 75, 22,LightGray, Blue, White,Yellow,#4+' Enter Dose '+#4, EnterEscape,
            '',#218 ,#196, #191, #179, #192, #217, #176, #177);
      TextBackground(BLUE);



      QtyKey := '';
      Done := FALSE;
      Timestr := '';
      TextColor(Yellow);
      For i := 1 to 6 DO
        Say(X, Y + i, DrugInfo[i], Yellow);
      TextColor(White);

      With DrugHandle^ DO BEGIN
        Say(X + Offset, Y + 1, Name+' '+Form, White);
        Say(X + Offset, Y + 2, Exp+' days', White);
        IF Trim(Conc) <> '' THEN
          Say(X + Offset, Y + 3, Conc+' '+units+'/1 ML', White);
        Say(X + Offset, Y + 4, Aux1, White);
        Say(X + Offset, Y + 5, Aux2, White);
        IF (Trim(Glass) = 'T') OR (Trim(PFL) = 'T') OR (Trim(Freeze) = 'T') THEN BEGIN
          TextBackground(LightGray);
          IF Trim(Glass) = 'T' THEN
            Center('This drug must be packaged in glass.', Y + 12, Red);
          IF Trim(PFL) = 'T' THEN
            Center('This drug must be protected from light.', Y + 13, Red);
          IF Trim(Freeze) = 'T' THEN
            Center('This drug must be stored in the freezer.', Y + 14, Red);
          TextBackground(Blue);
        END;
          REPEAT
            Dose := '              ';
            Say(X + Offset + Length(Dose) + 2, Y + 6, Units, Yellow);
            Readstring(X + Offset, Y + 6, 'C', Dose, Length(Dose), Qtykey, White, Black, EchoInput);
            Dose := Trim(Dose);
            Dose := RemoveCommas(Dose);
            Dose := TrimZeros(Dose);
            Val(Dose, Quan, Errcode);
            IF Quan <> 0 THEN
              CheckDose(Quan, DrugHandle^, NonStand, ChkConc, ChkUD);
          UNTIL (Errcode = 0) OR (Qtykey = 'ESC');
          IF (Qtykey = 'RETURN') AND (Trim(Dose) <> '') THEN BEGIN
              Done := TRUE;
              Val(Conc, Concentration, Errcode);
              IF (Concentration <> 0) AND (Units <> 'ML') then BEGIN

                Volume := Quan / Concentration;
                Str(Volume:20:2, Volstr);
                Volstr := Trim(Volstr);
                Volstr := TrimZeros(Volstr);
                InsertComma(Dose, Quan);
                Dose := Trim(Dose);
                Dose := Dose+' '+Units+'/'+Volstr+' ML';
                GoToXY(X+Offset, 9);
                Write(' Dose: ', Dose);
              END ELSE BEGIN
               InsertComma(Dose, Quan);
               Dose := Trim(Dose);
               Dose := Dose+' '+Units;
             END;
           END;
         END;
         IF DONE AND (DrugHandle^.TimeReqd = 'T') THEN BEGIN
           Say(X, 10, 'Enter time drug expires', Yellow);
           GetChunky(WhereX + 2, WhereY, Timestr, Black, LightGray, '00:00', Qtykey);
           AMPMNum := 1;
           ChoosefromMenu(AMPM, 2, WhereX, 9, 2, 2, AMPMNum, Qtykey, White, Blue);
           Timestr := TimeStr + ' '+AMPM[AMPMNum];
         END;
         IF Done THEN BEGIN
           Center('Press Enter to continue', Y + 11, LightGray);
           Readln;
         END;
(*        RestoreArea(2, 2, Classbkgrnd);
{        Writeln('Restoring Classbkgrnd');
        Readln; }
        WINDMIN:=HoldMin; WINDMAX:=HoldMax; *)

       END;

  PROCEDURE GetDoseForm(VAR Filename, IndexName, DoseFormKey : string); { X, Y : byte); }
    Const
      Forms : menutype=('INJECTION  ',
                        'ORAL LIQUID',
                        'ORAL SOLID ','','','','','',
       '','','','','','','','','','','','');
    VAR
      FormNum : integer;
      FormBkgrnd : areasavertype;
      FormMin, FormMax : word;


    BEGIN
        TextBackground(Blue);
      (*  FormMin := WindMin; FormMax := WindMax;
        SaveArea(28, 6, 44, 10, FormBkgrnd); *) {28, 6}
        DrawBox(32, 6, 42, 7,LightGray, Blue, White,Yellow,#4+' Pick dosage form/storage '+#4, EnterEscape, '',
          #218 ,#196, #191, #179, #192, #217, #0, #177);
        FormNum := 1;
        DoseFormKey := '';
        FileName := '';
        IndexName := '';
        ChoosefromMenu(Forms, 3, 16, 2, 15, 5, FormNum, DoseFormKey, 15, Blue);
        If DoseFormKey = 'RETURN' THEN BEGIN
          CASE FormNum OF
            1 : BEGIN
                  Filename := 'inject.dbf';
                  IndexName := 'inject.abx';
                END;
            2 : BEGIN
                  Filename := 'liquids.dbf';
                  IndexName := 'liquids.abx';
                END;
            3 : BEGIN
                  Filename := 'solids.dbf';
                  IndexName := 'solids.abx';
                END;
          END;
        END;
(*        RestoreArea(28, 6, FormBkgrnd);
        WindMin := FormMin; WindMax := FormMax; *)
    END;

  PROCEDURE GetExpdate(VAR Expdate : string; JulianToday : word; DrugExp : String; Entered : boolean; Timestr : string (*;
    VAR ReconInfo : Recontype*); VAR DrugInfo : DrugPtrType);
    (* If entered, ie manu info, then compare the proposed date to a manufacturer's expiration date *)
    VAR
      Month, Year, Day, DayOfWeek,
      Expday,
      DrugLife : word;
      Errcode : integer;
      TempExpInt, DiscardExpInt : longint;
      ReconDate : string;
      Reconjuliandate : word;


     FUNCTION GetLesserDate(DrugDate, ManuDate : longint; YearPlaces : byte) : String;
       VAR
         Tempstring : string;
         DrugLesser : boolean;
       BEGIN
         IF DrugDate <= ManuDate THEN BEGIN
           Str(DrugDate, Tempstring);
           DrugLesser := TRUE;
         END ELSE BEGIN
           Str(ManuDate, Tempstring);
           DrugLesser := FALSE;
         END;
         Tempstring := Trim(Tempstring);
         IF YearPlaces = 2 THEN
           GetLesserDate := Tempstring[5]+Tempstring[6]+'/'+Tempstring[7]+Tempstring[8]+'/'+Tempstring[3]+Tempstring[4]
         ELSE
           GetLesserDate :=
Tempstring[5]+Tempstring[6]+'/'+Tempstring[7]+Tempstring[8]+'/'+Tempstring[1]+Tempstring[2]+Tempstring[3]+Tempstring[4];
         IF (DrugLesser) AND (Trim(Timestr) <> '') THEN
           GetLesserDate := Tempstring[5]+Tempstring[6]+'/'+Tempstring[7]+
             Tempstring[8]+'/'+Tempstring[3]+Tempstring[4]+'@'+Timestr;
       END;


     PROCEDURE getcalendarday(VAR julianday,calendaryear,month,day : word);
       {************************************************************}
       {*   Converts julian day of year into the calendar          *}
       {*   month,day.                                             *}
       {************************************************************}
     BEGIN
       IF year MOD 4 = 0 THEN BEGIN     { IF this is a leap year. }
         IF julianday<=366 THEN BEGIN
           year:=calendaryear;
         END ELSE BEGIN
           year:=calendaryear+1;
           julianday:=julianday-366;
         END;

       CASE julianday OF
         1..31   : BEGIN       { Jan }
           month:=1;
           day:=julianday-0;
         END;
         32..60  : BEGIN
           month:=2;
           day:=julianday-31;
         END;
         61..91  : BEGIN
           month:=3;
           day:=julianday-60;
         END;
         92..121 : BEGIN
           month:=4;
           day:=julianday-91;
         END;
         122..152  : BEGIN
           month:=5;
           day:=julianday-121;
         END;
         153..182  : BEGIN
           month:=6;
           day:=julianday-152;
         END;
         183..213  : BEGIN
           month:=7;
           day:=julianday-182;
         END;
         214..244  : BEGIN
           month:=8;
           day:=julianday-213;
         END;
         245..274  : BEGIN
           month:=9;
           day:=julianday-244;
         END;
         275..305  : BEGIN
           month:=10;
           day:=julianday-274;
         END;
         306..335  : BEGIN
           month:=11;
           day:=julianday-305;
         END;
         336..366  : BEGIN     { Dec }
           month:=12;
           day:=julianday-335;
         END;
       END; {End of case statement.}
     END;

     IF year MOD 4 <> 0 THEN BEGIN    { IF this is not a leap year. }
       IF julianday<=365 THEN BEGIN
         year:=calendaryear;
       END ELSE BEGIN
         year:=calendaryear+1;
         julianday:=julianday-365;
       END;
       CASE julianday OF
         1..31  : BEGIN        { Jan }
           month:=1;
           day:=julianday-0;
         END;
         32..59  : BEGIN
           month:=2;
           day:=julianday-31;
         END;
         60..90  : BEGIN
           month:=3;
           day:=julianday-59;
         END;
         91..120  : BEGIN
           month:=4;
           day:=julianday-90;
         END;
         121..151  : BEGIN
           month:=5;
           day:=julianday-120;
         END;
         152..181  : BEGIN
           month:=6;
           day:=julianday-151;
         END;
         182..212  : BEGIN
           month:=7;
           day:=julianday-181;
         END;
         213..243  : BEGIN
           month:=8;
           day:=julianday-212;
         END;
         244..273  : BEGIN
           month:=9;
           day:=julianday-243;
         END;
         274..304  : BEGIN
           month:=10;
           day:=julianday-273;
         END;
         305..334  : BEGIN
           month:=11;
           day:=julianday-304;
         END;
         335..365  : BEGIN     { Dec }
           month:=12;
           day:=julianday-334;
         END;
       END;  {End of case statement.}
     END;
  END;  { End of getcalendarday. }


    BEGIN

    (* 1/21/2000.  Changing this procedure.  If the
    drug is reconstituted it, base the expiration date
    not on TODAY but on the date the drug was reconstituted. *)

        DrugExp := Trim(DrugExp);
        Val(DrugExp, DrugLife, Errcode);
        IF DrugInfo^.ChkDiscardDate = 'T' THEN BEGIN
           ReconDate := DrugInfo^.DiscardDate;
           Val(ReconDate[1]+ReconDate[2], Month, errcode);
           Val(ReconDate[4]+ReconDate[5], Day, errcode);
           Val(ReconDate[7]+ReconDate[8]+ReconDate[9]+ReconDate[10], Year, errcode);
(*           DateString(Month, Day, Year, Date, 4, '/');
           DateString(Month, Day, Year, ShortDate, 2, ''); *)
           IF year MOD 4 <>0 THEN
             reconjuliandate := julianbase[month] + day;
           IF year MOD 4 = 0 THEN
             reconjuliandate:=leapbase[month] + day;
          expday := reconjuliandate + druglife;
          END ELSE BEGIN
          GetDate(Year, Month, Day, DayOfWeek);
          expday:=config.juliandate+druglife;
        END;

        IF year MOD 4 = 0 THEN BEGIN     { IF this is a leap year. }
          IF expday > 366 THEN BEGIN
            Inc(Year);
            expday := expday-366;
          END;
        END;
        IF year MOD 4 <> 0 THEN BEGIN    { IF this is not a leap year. }
          IF expday > 365 THEN BEGIN
            Inc(Year);
            expday:=expday-365;
          END;
        END;
        getcalendarday(expday,year,month,day);
        DateString(Month, Day, Year, ExpDate, 4, '/');
        IF Entered THEN BEGIN
          TempExpInt := GetDateInt(ExpDate);
{          IF DrugInfo^.ChkDiscardDate = 'T' THEN BEGIN
             DiscardExpInt := GetDateInt(Trim(DrugInfo^.DiscardDate));
(*  Following line doesn't make sense. . .changed it to the line below..
    eg, TempExpInt rather than Manu.ExpInt. . .
    ExpDate := GetLesserDate(DiscardExpInt, Manu.ExpInt,4); *)
             ExpDate := GetLesserDate(DiscardExpInt, TempExpInt,4);
             TempExpInt := GetDateInt(ExpDate);
          END; }
          ExpDate := GetLesserDate(TempExpInt, Manu.ExpInt,2);
        END;
        IF Length(ExpDate) = 10 THEN
          Delete(Expdate, 7, 2);


    END;

   PROCEDURE WriteManuInfo(VAR WMManu : manutype; WMFilename : string; WMDrugRec : integer; VAR DrugInfo : DrugPtrType);
    VAR
      WMSpec : dbfheadertype; {WM = WriteManu * Just trying for unique name}
      WMResults : string;
      WMTempstring : string;
      WMF : file;
      Logfile : text;

    BEGIN
      WMTempstring := '';
      Opendbasefile(WMSpec, WMFilename, WMResults);
      IF (WMDrugRec > 0) AND (WMDrugRec <= WMSpec.Numofrecs) THEN BEGIN
        IF WMResults = 'GOOD' THEN BEGIN
          WMTempstring := WMManu.CompanyName;
          Writeonefield(WMSpec, WMDrugRec, 12, WMTempstring);
          WMTempstring := WMManu.LotNumber;
          Writeonefield(WMSpec, WMDrugRec, 13, WMTempstring);
          WMTempstring := WMManu.MExpDate;
          Writeonefield(WMSpec, WMDrugRec, 14, WMTempstring);
          WMTempstring := DrugInfo^.ChkDiscardDate;
          Writeonefield(WMSpec, WMDrugRec, 22, WMTempstring);
          WMTempstring := DrugInfo^.DiscardDate;
          Writeonefield(WMSpec, WMDrugRec, 23, WMTempstring);
          Closedbasefile(WMSpec);
        END;
        Assign(WMF, WMFileName);   {reset attributes of drug file}
        SetFAttr(WMF, $0);
      END ELSE BEGIN
         Assign(Logfile, 'pm_err.log');
         Append(Logfile);
         Writeln(Logfile, 'File = ', WMFileName, ' contains ',WMSpec.numofrecs);
         Writeln(Logfile, 'Attempt to write to rec #', WMDrugRec);
         Writeln(Logfile, LabelInfo[1]);
         Writeln(Logfile, LabelInfo[2]);
         Writeln(Logfile, LabelInfo[3]);
         Writeln(Logfile, LabelInfo[4]);
         Writeln(Logfile);
         Close(Logfile);
      END;

    END;


  PROCEDURE GetDrugInfo (VAR Drug : DrugType; VAR DrugFile, DrugIndex : string; Xpos, Ypos : integer;
     VAR Entered, ManuInfo : boolean);
    VAR
      Timestr : string;  {time drugs with short expiration will expire}
{      RecNum : integer; }
      S : pathstr;
      Drugstr, Tempstr, Dose, Schedule, Route : string;
      i : integer;
      ClassBkgrnd,
      RouteBkgrnd : areasavertype;
      Druginfonum : integer;
      X, y : byte;
      Hmin, Hmax : word;
      Dikey : string;
      Holdmin, Holdmax : word;
      DrugKey : string;
      PriceStr : string;
      DirInfo : SearchRec;



  PROCEDURE BuildIndex(InFilename, OutfileName : string);
    VAR
      Inspec, Outspec : dbfheadertype;
      Results : string;
      i, j : integer;              {i is alph-index, j num of recs in infile}
      Initial : char;
      Tempstring : string;
      Temprec : recordarraytype;
      Count : integer;
      Rnum : integer;              {number of recs in outfile}
      Endpoint : integer;

    BEGIN
      Opendbasefile(Inspec,  Infilename,  Results);
      IF Results = 'GOOD' THEN BEGIN
        Opendbasefile(Outspec, Outfilename, Results);
        IF Results = 'GOOD' THEN BEGIN
          i := 1; j := 1;  Rnum := 0; Count := 0;
          Temprec[1] := '';
          Temprec[2] := '';
          Temprec[3] := '';
          While (i <= 26) AND (j <= inspec.numofrecs) DO BEGIN
            Readonefield(Inspec, j, 1, Tempstring);
            Temprec[1] := Alphabet[i];
            IF (tempstring[1] = Temprec[1]) THEN BEGIN
              If Temprec[2] = '' THEN
                Str(j, Temprec[2]);
                Inc(count);
                Inc(j);
              END
              ELSE BEGIN
                Str(Count, Temprec[3]);
                Inc(Rnum);
                Writedbfrecord(Outspec, Rnum, Temprec, Results);
                inc(i);
                count := 0;
                Temprec[2] := '';
              END;
            END;
            Str(Count, Temprec[3]);
            Inc(Rnum);
            Writedbfrecord(Outspec, Rnum, Temprec, Results); {write Z}
            IF i < 26 THEN BEGIN
              Endpoint := i + 1;
              For i := Endpoint to 26 DO BEGIN
                Temprec[1] := Alphabet[i];
                Temprec[2] := '0';
                Temprec[3] := '0';
                Inc(outspec.numofrecs);
                Writedbfrecord(Outspec, Outspec.numofrecs, Temprec, Results);
              END;
            END;
          Closedbasefile(outspec);
        END ELSE BEGIN
          (*Writeln('Result of opening ', Outfilename, ' = ', Results); *)
          (*Readln; *) {For debugging purposes}
        END;
        Closedbasefile(inspec);
      END ELSE BEGIN
        (*Writeln('Result of opening ', Infilename, ' = ', Results);*)
        (* Readln; *) {For debugging purposes}
      END;

    END;




    PROCEDURE InitializeAlphabet (VAR ABet : AlphabetType);
      VAR
        i : byte;
      BEGIN
        For i := 1 to 26 DO
          Abet[i] := Chr(64 + i);
      END;



  PROCEDURE NewDrug(VAR Drug : drugtype;VAR Drugkey : string; VAR TargRec :longint);
      VAR
        EBArea,
        DrugArea,
        AlphabetArea : areasavertype;
        HoldWindMin, HoldWindMax : word;
        ErrBoxMin, ErrBoxMax : word;
        Menunum,
        Cornerpos,
        Start, Stop,
        Height : integer;
        pagenum  : word;
        F : file;
        Attr : word;
        Fieldnum, FN2, FN3, FN4,
        Errcode, DF,
        Numoffields,
        AlphNum : integer;
        Abxspec : dbfheadertype;
        Results, PickALetterKey, FormSelectedKey : string;
        DrugRec,
        Abxrec  : recordarraytype;
        DrugMenu : bigmenuptrtype;
        Empty : boolean;
        DrugSuccess : boolean;

    PROCEDURE SortDatabase(Filename : string);
      VAR
        Spec : dbfheadertype;
        SortResults : string;
      BEGIN
        Opendbasefile(Spec, Filename, SortResults);
        Dbfsort(Spec, 1);
        Closedbasefile(Spec);

      END;


  {---beginning of this procedure ---}

  BEGIN
    InitPtrMenu(DrugMenu);

    REPEAT

      GetDoseForm(DrugFile, DrugIndex, FormSelectedKey); {, 28, 6);}

IF FormSelectedKey = 'RETURN' THEN BEGIN
  DrugKey := '';
  PickALetterKey := '';
  Height := 15;
  Start := 0;
  Stop := 0;
  InitializeAlphabet(Alphabet);
  S := '';
  S := FSearch(Drugfile, '');
  If S <> '' THEN BEGIN
    Assign(F, DrugFile);
    GetFAttr(F, Attr);
    If Attr <> $0 THEN BEGIN        {If the drug file has changed then}
      TextBackground(Blue);
      Center('  Building index/Sorting. . .', 5, Red);
      SortDatabase(DrugFile);
      DeleteFile(DrugIndex);             {delete old index}
      FindFirst('fdabx.dbf',Archive,DirInfo);
      If DosError <> 0 Then Begin
        DeleteFile('C:\package\pm.pid');
        Window(1,1,80,25);
        TextColor(LightGray);
        TextBackground(Black);
        Clrscr;
        Writeln('fdabx.dbf not found');
        Writeln('Program cannot continue.');
        Readln;
        Halt;
      END ELSE BEGIN
        CopyFile('fdabx.dbf', DrugIndex); {create new empty index}
        BuildIndex(DrugFile, DrugIndex);   {fill new index}
        Assign(F, DrugFile);                {reset attributes of drug file/index}
        SetFAttr(F, $0);
        Assign(F, DrugIndex);
        SetFAttr(F, $0);
      END;
    END;
    REPEAT
      AlphNum := 13;
      TextBackground(Blue); {28, 6}
      DrawBox(32, 6, 42,6,LightGray, Blue, White,Yellow,#4+' Pick a letter '+#4,
        EnterEscape,'',#218 ,#196, #191, #179, #192, #217, #0, #177);
      ChoosefromAlphabet(Alphabet, 26, 2, 2, 3, 2, AlphNum, PickALetterKey, 15, Blue);
      IF PickALetterKey = 'RETURN' THEN BEGIN
        REPEAT
          Opendbasefile(AbxSpec, DrugIndex, Results);
          Readdbfrecord(AbxSpec, AlphNum , Abxrec, Results);
          IF Results = 'GOOD' THEN BEGIN
            Val(AbxRec[2], Start, errcode);
            Val(AbxRec[3], Stop, errcode);
            Closedbasefile(AbxSpec);
            IF Start <> 0 THEN BEGIN
              HoldWindMin := WindMin; HoldWindMax := WindMax;
              SaveArea(2, 2, 78, 23, DrugArea);
              Start := Start - 1;
              Stop := Start + Stop;
              FieldNum := 1; FN2 := 2; FN3 := 3; FN4 := 4;
              DF := 21;
              CornerPos := Height + 4;
              BuildBigPtrMenu(DrugFile, DrugMenu, Height, FieldNum, FN2, FN3, FN4, DF, Start, Stop, Empty, 'Y');
              IF Not(Empty) THEN BEGIN
                PageNum := 1; MenuNum := 1;
                TextBackground(Blue);
                TextAttr := Colorscheme;
                {had been 76}
                DrawBox(2,2, 75, 22,LightGray, Blue, White,Yellow,#4+' Pick a drug '+#4, EnterEscape,
                  '',#218 ,#196, #191, #179, #192, #217, #176, #177);
                TextBackground(Blue);
                Center('All concentrations are in terms of units (mg, gm, mEq, etc) per 1 ml', 2, Yellow);
                DrugKey := '';
                EnhancedPtrMenu(DrugMenu, PageNum, 3, 4, 74, Height+3, MenuNum, Drugkey, 15, Blue, 'Y', 6, 17,
                  'ESC RETURN');
                IF (Drugkey = 'ESC') { OR (DrugKey = 'F1') OR (DrugKey = 'F2') OR (DrugKey = 'DEL') } THEN BEGIN
                  PickALetterKey := '';
                  DrugKey := 'ESC';
                  RestoreArea(2, 2, DrugArea);
                  WindMin := HoldWindmin; WindMax := HoldWindMax;
                END ELSE IF Drugkey = 'RETURN' THEN BEGIN
                  Entered := TRUE;
                  TargRec := Start + ((PageNum - 1) * Height) + Menunum;
                  GetDrug(DrugFile, TargRec, Numoffields, Drug, DrugSuccess);
                  IF DrugSuccess THEN BEGIN
                    GetQty(Dose, X, Y, Entered, Timestr);
                    IF Entered THEN BEGIN
                      ManuInfo := FALSE;
                      GetManuInfo(Manu, DrugHandle, ManuInfo, 'Y', 24, 5,'B'(*, ReconInfo*)); {7, 7}
                      IF Not ManuInfo THEN BEGIN
                        Entered := FALSE;
                        DrugKey := ''
                      END ELSE BEGIN
                        GetExpdate(Drug.Expdate, Config.JulianDate,
                        Drug.Exp, ManuInfo, Timestr(*, ReconInfo*), DrugHandle);
                      END;
                    END ELSE DrugKey := '';
                  END;
                END;
              END ELSE BEGIN {IF EMPTY}
                Center('No entries found. . .', 5, Yellow);
                Delay(500);
                DrugKey := 'ESC';
                PickALetterKey := '';
              END;
              {RestoreArea(2, 2, DrugArea);
              WindMin := HoldWindmin; WindMax := HoldWindMax;}
            END ELSE BEGIN {start = 0}
              Center('No entries found. . .', 5, Yellow);
              Delay(500);
              DrugKey := 'ESC';
              PickALetterKey := '';
            END;
          END;
        UNTIL (DrugKey = 'ESC') OR (DrugKey = 'RETURN');
      END ELSE BEGIN {pickaletterkey <> RETURN}
        FormSelectedKey := '';
        DrugKey := '';
      END;
    UNTIL (PickALetterKey = 'RETURN') OR (PickALetterKey = 'ESC');
  END ELSE BEGIN {if S = '', eg file not found}
    SaveArea(30, 6, 47, 10, EBArea);      {15, 8}
    ErrBoxMin := WindMin; ErrBoxMax := WindMax;
    TextBackground(Blue);
    DrawBox(30,6,44,7,White,Red,15,15,'**ERROR**',
      'Press ENTER to continue','File '+ DrugFile+ ' not found. . .',#218, #196, #191, #179, #192, #217, #0, #177);
    READLN;
    RestoreArea(30, 6, EBArea);
    WindMin := ErrBoxMin; WindMax := ErrBoxMax;
    TextColor(LightGray);
    FormSelectedKey := 'ESC';
    DrugKey := '';
  END;
END ELSE BEGIN {formselectedkey <> 'RETURN'}
  DrugKey := 'ESC';
END;
    UNTIL (FormSelectedKey = 'ESC') OR (FormSelectedKey = 'RETURN');
    DisposeMenu(DrugMenu);
  END;






  BEGIN
    REPEAT
      With Drug DO BEGIN
        Dikey := '';
        X := 2; Y := 2;
        DrugStr := '';
(*        Dose     := '                     ';
        Schedule := '                     ';
        Route    := '                     '; *)
(*        Drug.Name := '';
        Drug.Form := '';
        Drug.Conc := '';
        Drug.Exp := '';
        Drug.Aux1 := '';
        Drug.Aux2 := '';
        DRUG.Glass := '';
        DRUG.PFL := '';
        DRUG.Freeze  := ''; *)
        TextColor(LightGray);
        HMin:=WINDMIN; HMax:=WINDMAX;
        SaveArea(2, 2, 77, 23, DrugBkgrnd);
        TextAttr := Colorscheme;
        DrugKey := '';
        NewDrug(Drug, DrugKey, RecNum);
      END; {With}
      RestoreArea(2,2,Drugbkgrnd);
      WINDMIN:=HMin; WINDMAX:=HMax;
    UNTIL (DrugKey = 'RETURN') OR (DrugKey = 'ESC');

    END;

(*  PROCEDURE UpdateConfig(VAR Config : configtype; CurrentDept : integer; VAR DateChanged : boolean);
    VAR
      outfile : text;
      i : byte;
    BEGIN
      With Config DO BEGIN
        IF (NumberOfPrints <> 0) OR
          ((LoopCounter > 0) AND (DrugEntered)) OR DateChanged THEN BEGIN
          IF NOT DateChanged THEN
            Inc(LotNumbers[CurrentDept]);
          Assign(outfile, 'pm.cfg');
          Rewrite(outfile);
          Writeln(outfile, Date);
          For i := 1 to NumOfDepts DO
            Writeln(outfile, Lotnumbers[i]);
          Writeln(outfile, defaultdept);
          Writeln(outfile, labelsperrow);
          Close(outfile);
        END;
      END;
    END; *)

  PROCEDURE UpdateConfig(VAR Config : configtype; {CurrentDept : integer;} VAR DateChanged : boolean);
    VAR
      ConfigSpec : dbfheadertype;
      ConfigRec : recordarraytype;
      OpenResult : string;
      i : byte;
      TmpStr : string;

(*
 Field  Field Name  Type       Width    Dec    Index
      1  DATE        Character     10               N
      2  COUNTER_P   Numeric        3               N
      3  COUNTER_C   Numeric        3               N
      4  COUNTER_BP  Numeric        3               N
      5  COUNTER_PS  Numeric        3               N
      6  COUNTER_M4  Numeric        3               N
      7  COUNTER_M6  Numeric        3               N
      8  COUNTER_Q   Numeric        3               N
      9  COUNTER_M5  Numeric        3               N
      10  COUNTERM10  Numeric        3               N
      11  DEFAULT     Numeric        3               N
      12  LBLSPERROW  Numeric        3               N
      13  CURRENTDPT  Numeric        3               N
      14  LOTLETTER   Character      2               N
      15  SHORTDATE   Character      6               N
      16  JULIANDATE  Numeric        3               N
      17  PREFIX      Character      2               N
      18  MCVNAME     Character     35               N
   ** Total **                      95 *)

    BEGIN
      With Config DO BEGIN
        IF (NumberOfPrints <> 0) OR
          ((LoopCounter > 0) AND (DrugEntered)) OR DateChanged THEN BEGIN
          IF NOT DateChanged THEN
            Inc(LotNumbers[CurrentDept]);
            Opendbasefile(Configspec, 'pm_cfg.dbf', OpenResult);
          ConfigRec[1] := Date;
          For i := 1 to NumOfDepts DO BEGIN
              Str(LotNumbers[i], TmpStr);
              ConfigRec[i + 1] := Trim(TmpStr);
          END;
          Str(defaultDept, TmpStr);
{was 9, 10}   ConfigRec[11] := Trim(TmpStr);
          Str(labelsperrow, TmpStr);
{was 10, 11}  ConfigRec[12] := Trim(TmpStr);
          Str(CurrentDept, TmpStr);
{was 11, 12}  ConfigRec[13] := Trim(TmpStr);
{was 12, 13}  ConfigRec[14] := Trim(LotLetter);
{was 13, 14}  ConfigRec[15] := Trim(ShortDate);
          Str(JulianDate, TmpStr);
{was 14,15}  ConfigRec[16] := Trim(TmpStr);
          Prefix := Depts[CurrentDept];
{was 15,16}  ConfigRec[17] := Trim(Prefix);
          ConfigRec[18] := Trim(MCVName);
          Writedbfrecord(ConfigSpec, 1, ConfigRec, OpenResult);
          Closedbasefile(ConfigSpec);
        END;
      END;
    END;


{  PROCEDURE GetSetDate(VAR CurrentDept : integer; VAR ShortDate : string; VAR Prefix : prefixtype;
    VAR LotLetter : string; VAR JulianDate : word; VAR Config : ConfigType; Interactive : char);

    CONST

      julianbase:ARRAY[1..12] OF INTEGER=(0,31,59,90,120,151,181,212,243,273,
        304,334);
      leapbase:ARRAY[1..12] OF INTEGER=(0,31,60,91,121,152,182,213,244,274,
        305,335);

      Depts : Menutype = ('P','C','BP','PS','M4','M6','Q','None','','',
         '','','','','','','','','','');


    VAR
      OLDPREFIX : PREFIXTYPE;
      Year, Month, Day, DayOfWeek : word;
      Errcode : integer;
      YearString, Daystring, Monthstring,
      Returnkey : string;
      Infile : text;
      OldDate, SystemDate,
      Tempstr : string;
      GSDArea : areasavertype;
      OlMin, OlMax : word;
      NewLot : string;
      Found : boolean;
      GSDPromptHandle : PlusMenuPtrType;
      count : byte;
      GSDNum : integer;
      GSDKey : string;
      Xpos, Ypos : byte;
      MonthStr, DayStr, YearStr : string;
      Hour, Min, Sec, Sec100 : word;
      ResetDate,
      Time, Datekey,
      HourStr, Minstr : string;



    PROCEDURE ChangeLot(VAR Config : configtype; NewLot : string; VAR Found : boolean; LotToChange : byte);

      VAR
        i : word;


      BEGIN
        NewLot := Trim(NewLot);
        i := 1;
        While not(found) AND (i <= 78) do begin
          IF LotLetters[i] = NewLot THEN BEGIN
            Found := TRUE;
          END ELSE
            inc(i);
        END;
        IF Found THEN Config.LotNumbers[LotToChange] := i;

      END;

    FUNCTION GetLotLetter (LotNumber : byte) : string;
      BEGIN
        IF (LotNumber > 0) AND (LotNumber < 79) THEN
          GetLotLetter := Lotletters[lotnumber]
        ELSE
          GetLotLetter := #19;

      END;

  BEGIN
    With CONFIG DO BEGIN
      GSDPromptHandle := NIL;
      New(GSDPromptHandle);
      GSDPromptHandle^[1].MenuItem := 'Label date:';
      GSDPromptHandle^[1].Description := '';
      GSDPromptHandle^[2].MenuItem := 'System date:';
      GSDPromptHandle^[2].Description := '';
      GSDPromptHandle^[3].MenuItem := 'Current dept:';
      GSDPromptHandle^[3].Description := '';
      GSDPromptHandle^[4].MenuItem := 'Lot letter: ';
      GSDPromptHandle^[4].Description := '';
      For count := 5 to 20 DO BEGIN
        GSDPromptHandle^[count].MenuItem := '';
        GSDPromptHandle^[count].Description := '';
      END;

      Assign(infile, 'pm.cfg');
      Reset(infile);
      While NOT EOF(infile) DO BEGIN
        Readln(infile, OldDate);
        OldDate := Trim(OldDate);
        i := 1;
        While (i <= NumOfDepts) AND Not EOF(infile) DO BEGIN
          Readln(infile, Tempstr);
          Tempstr := Trim(Tempstr);
          Val(Tempstr, LotNumbers[i], Errcode);
          inc(i);
        END;
        IF Not EOF(INFILE) THEN BEGIN
          Readln(infile, Tempstr);
          Tempstr := Trim(Tempstr);
          Val(Tempstr, DefaultDept, Errcode);
          IF Errcode <> 0 THEN DefaultDept := 1;
        END ELSE
          DefaultDept := 1;
        IF NOT EOF(infile) THEN BEGIN
          Readln(infile, Tempstr);
          Tempstr := Trim(Tempstr);
          Val(Tempstr, LabelsPerRow, Errcode);
        END ELSE
          Labelsperrow := 1;
      END;
      Close(infile);
      Date := Olddate;
      ShortDate := Date[1]+Date[2]+Date[4]+Date[5]+Date[9]+Date[10];
      CurrentDept := DefaultDept;
      Prefix := Depts[CurrentDept];
      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
      Val(Date[1]+Date[2], Month, errcode);
      Val(Date[4]+Date[5], Day, errcode);
      Val(Date[7]+Date[8]+Date[9]+Date[10], Year, errcode);
      DateString(Month, Day, Year, Date, 4, '/');
      DateString(Month, Day, Year, ShortDate, 2, '');
      IF year MOD 4 <>0 THEN
        juliandate := julianbase[month] + day;
      IF year MOD 4 = 0 THEN
         juliandate:=leapbase[month] + day;
      IF (Interactive = 'Y') OR (Interactive = 'B') THEN BEGIN
        OlMin := WindMin; OlMax := WindMax;
        SaveArea(27, 5, 58, 13, GSDArea);
        GSDNum := 1;
        GSDKey := '';
        TextAttr := Colorscheme;
        IF Interactive = 'B' THEN

           DrawBox(27,5,51,12, White, Blue,15, Yellow,#4+' Adjust settings '+#4, F2Escape,'',
              #218 ,#196, #191, #179, #192, #217, #176, #177)
        ELSE
           DrawBox(27,5,51,12, White, Blue,15,Yellow,#4+' Adjust settings '+#4, F2Escape,'',
              #218 ,#196, #191, #179, #192, #217, #176, #177);
        Xpos := 2; Ypos := 1;
        TextBackground(Blue);
        Say(Xpos + 15, Ypos+3, Date, LightGray);
        GetDate(Year, Month, Day, DayOfWeek);
        DateString(Month, Day, Year, SystemDate, 4, '/');
        Say(Xpos + 15, Ypos+4, SystemDate, LightGray);
        Say(Xpos + 15, Ypos+5, Prefix, LightGray);
        Say(Xpos + 15, Ypos+6, LotLetter, LightGray);

        REPEAT
          ChoosefromPtrMenu(GSDPromptHandle^, 4, Xpos, Ypos + 2, 10,  4, GSDNum, GSDkey, White, Blue, Xpos+3, YPos);
          IF (GSDKey <> 'F2') AND (GSDKey <> 'ESC') THEN BEGIN
            CASE GSDNum OF
              1 : BEGIN
                    GetDate(Year, Month, Day, DayOfWeek);
                    DateString(Month, Day, Year, SystemDate, 4, '/');
                    Getchunky(Xpos + 15, Ypos + 3, Date, 1, 15, Date, Returnkey);
                    IF Date <> OldDate THEN BEGIN
                      DateChanged := TRUE;
                      For i := 1 to NumOfDepts DO
                        LotNumbers[i] := 1;
                      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                      Say(Xpos + 15, Ypos+6, LotLetter, LightGray);
                    END;
                    Val(Date[1]+Date[2], Month, errcode);
                    Val(Date[4]+Date[5], Day, errcode);
                    Val(Date[7]+Date[8]+Date[9]+Date[10], Year, errcode);
                    DateString(Month, Day, Year, Date, 4, '/');
                    DateString(Month, Day, Year, ShortDate, 2, '');
                    IF year MOD 4 <>0 THEN
                      juliandate := julianbase[month] + day;
                    IF year MOD 4 = 0 THEN
                      juliandate:=leapbase[month] + day;
                    GSDNum := 2;
                  END;
              2 : BEGIN
                    GetDate(Year, Month, Day, DayOfWeek);
                    DateString(Month, Day, Year, ResetDate, 4, '/');
                    Getchunky(Xpos + 15, Ypos + 4, ResetDate, 1, 15, ResetDate, Returnkey);
                    Monthstr := ResetDate[1] + ResetDate[2];
                    Val(Monthstr, Month, Errcode);
                    DayStr := ResetDate[4] + ResetDate[5];
                    Val(Daystr, Day, Errcode);
                    YearStr := ResetDate[7] + ResetDate[8] + ResetDate[9] + ResetDate[10];
                    Val(Yearstr, Year, Errcode);
                    SetDate(Year, Month, Day);
                    GetTime(Hour, Min, Sec, Sec100);
                    Str(Hour, HourStr);
                    If Hour < 10 THEN HourStr := '0' + Hourstr;
                      Str(Min, MinStr);
                    If Min < 10 THEN MinStr := '0' + Minstr;
                    Time := HourStr+':'+MinStr;
                    GetChunky(WhereX+2, WhereY, Time, 1, 15, Time, Datekey);
                    HourStr := Time[1] + Time[2];
                    Val(HourStr, Hour, Errcode);
                    MinStr := Time[4] + Time[5];
                    Val(Minstr, Min, Errcode);
                    SetTime(Hour, Min, 0, 0);
                    GSDNum := 3;

                  END;
              3 : BEGIN
                    CurrentDept := DefaultDept;
                    OldPrefix := Depts[CurrentDept];
                    Say(Xpos + 15, Ypos+5, '                    ', Blue);
                    ChoosefromMenu(Depts, 7, Xpos+14, Ypos+4, 3, 1, CurrentDept, Returnkey, 7, 1);
                    Prefix := Depts[CurrentDept];
                    Say(Xpos + 15, Ypos+5, '                    ', Blue);
                    Say(Xpos + 15, Ypos+5, Prefix, LightGray);
                    IF Prefix <> OldPrefix THEN BEGIN
                      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                      Say(Xpos + 15, Ypos+6, LotLetter, LightGray);
                    END;
                    GSDNum := 4;
                  END;
              4 : BEGIN
                    LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                    TextColor(White);
                    NewLot := Pad(LotLetter, 2);
                    ReadString(Xpos+15, Ypos+6, 'C', NewLot, Length(NewLot), Returnkey, LightGray);
                    NewLot := Trim(NewLot);
                    IF NewLot <> LotLetter THEN BEGIN
                      Found := FALSE;
                      LotLetter := NewLot;
                      ChangeLot(Config, NewLot, Found, CurrentDept);
                    END;
                  END;
              END;
            END;
        UNTIL (GSDkey = 'F2') OR (GSDKey = 'ESC');
        UpdateConfig(Config, CurrentDept, DateChanged);
        DateChanged := FALSE;
        Dispose(GSDPromptHandle);
        RestoreArea(27, 5, GSDArea);
        WindMax := OlMax; WindMin := OlMin;
      END;
    END;
  END; }



  PROCEDURE GetSetDate({VAR CurrentDept : integer; VAR ShortDate : string; VAR Prefix : prefixtype;
    VAR LotLetter : string; VAR JulianDate : word;} VAR Config : ConfigType; Interactive : char);

    CONST

(*      julianbase:ARRAY[1..12] OF INTEGER=(0,31,59,90,120,151,181,212,243,273,
        304,334);
      leapbase:ARRAY[1..12] OF INTEGER=(0,31,60,91,121,152,182,213,244,274,
        305,335); *)

(*      Depts : Menutype = ('P','C','BP','PS','M4','M6','Q','None','','',
         '','','','','','','','','',''); *)


{      GSDPromptHandle^[1].MenuItem := 'Label date:';
      GSDPromptHandle^[1].Description := '';
      GSDPromptHandle^[2].MenuItem := 'System date:';
      GSDPromptHandle^[2].Description := '';
      GSDPromptHandle^[3].MenuItem := 'Current dept:';
      GSDPromptHandle^[3].Description := '';
      GSDPromptHandle^[4].MenuItem := 'Lot letter: ';
      GSDPromptHandle^[4].Description := ''; }

     GSDMenu : MenuType = ('Label date:',
                           'System date:',
                           'Current dept:',
                           'Lot letter: ','','','','','','','','','','','','','','','','');





    VAR
      ConfigSpec : Dbfheadertype;
      ConfigRec : recordarraytype;
      OLDPREFIX : PREFIXTYPE;
      Year, Month, Day, DayOfWeek : word;
      Errcode : integer;
      YearString, Daystring, Monthstring,
      Returnkey : string;
(*      Infile : text; *)
      OldDate, SystemDate,
      Tempstr : string;
      GSDArea : areasavertype;
      OlMin, OlMax : word;
      NewLot : string;
      Found : boolean;
(*      GSDPromptHandle : PlusMenuPtrType;  *)
      count : byte;
      GSDNum : integer;
      GSDKey : string;
      Xpos, Ypos : byte;
      MonthStr, DayStr, YearStr : string;
      Hour, Min, Sec, Sec100 : word;
      ResetDate,
      Time, Datekey,
      HourStr, Minstr : string;
      OpenResult : string;
      DeptChosen : integer;



    PROCEDURE ChangeLot(VAR Config : configtype; NewLot : string; VAR Found : boolean (*; LotToChange : byte*) );

      VAR
        i : word;
      BEGIN
        NewLot := Trim(NewLot);
        i := 1;
        While not(found) AND (i <= 78) do begin
          IF LotLetters[i] = NewLot THEN BEGIN
            Found := TRUE;
            IF Found THEN
              Config.LotNumbers[Config.CurrentDept] := i;
          END ELSE
            inc(i);
        END;

      END;

    FUNCTION GetLotLetter (LotNumber : byte) : string;
      BEGIN
        IF (LotNumber > 0) AND (LotNumber < 79) THEN
          GetLotLetter := Lotletters[lotnumber]
        ELSE
          GetLotLetter := #19;
      END;

  BEGIN
    With CONFIG DO BEGIN
(*      GSDPromptHandle := NIL;
      New(GSDPromptHandle);
      GSDPromptHandle^[1].MenuItem := 'Label date:';
      GSDPromptHandle^[1].Description := '';
      GSDPromptHandle^[2].MenuItem := 'System date:';
      GSDPromptHandle^[2].Description := '';
      GSDPromptHandle^[3].MenuItem := 'Current dept:';
      GSDPromptHandle^[3].Description := '';
      GSDPromptHandle^[4].MenuItem := 'Lot letter: ';
      GSDPromptHandle^[4].Description := '';
      For count := 5 to 20 DO BEGIN
        GSDPromptHandle^[count].MenuItem := '';
        GSDPromptHandle^[count].Description := '';
      END; *)
      Opendbasefile(ConfigSpec, 'pm_cfg.dbf', OpenResult);
      Readdbfrecord(ConfigSpec, 1, ConfigRec, OpenResult);
      Closedbasefile(ConfigSpec);
      OldDate := ConfigRec[1];
(* was 17 *)      MCVName := Trim(ConfigRec[18]);
      i := 1;
      While (i <= NumOfDepts) DO BEGIN
          Tempstr := Trim(ConfigRec[i + 1]);
          Val(Tempstr, LotNumbers[i], Errcode);
          inc(i);
        END;
        { default department counter was 9, is now 10 - changed to 11 when M10 added
        Tempstr := Trim(ConfigRec[9]); }
        Tempstr := Trim(ConfigRec[11]);
        Val(Tempstr, DefaultDept, Errcode);
        IF Errcode <> 0 THEN DefaultDept := 1;

{        Tempstr := Trim(ConfigRec[10]); }
        Tempstr := Trim(ConfigRec[12]); {was 11}
        Val(Tempstr, LabelsPerRow, Errcode);
        IF Errcode <> 0 THEN Labelsperrow := 3;
(*      Assign(infile, 'pm.cfg');
        Reset(infile);
      While NOT EOF(infile) DO BEGIN
        Readln(infile, OldDate);
        OldDate := Trim(OldDate);
        i := 1;
        While (i <= NumOfDepts) AND Not EOF(infile) DO BEGIN
          Readln(infile, Tempstr);
          Tempstr := Trim(Tempstr);
          Val(Tempstr, LotNumbers[i], Errcode);
          inc(i);
        END;
        IF Not EOF(INFILE) THEN BEGIN
          Readln(infile, Tempstr);
          Tempstr := Trim(Tempstr);
          Val(Tempstr, DefaultDept, Errcode);
          IF Errcode <> 0 THEN DefaultDept := 1;
        END ELSE
          DefaultDept := 1;
        IF NOT EOF(infile) THEN BEGIN
          Readln(infile, Tempstr);
          Tempstr := Trim(Tempstr);
          Val(Tempstr, LabelsPerRow, Errcode);
        END ELSE
          Labelsperrow := 1;
      END;
      Close(infile); *)
      Date := Olddate;
      ShortDate := Date[1]+Date[2]+Date[4]+Date[5]+Date[9]+Date[10];
      CurrentDept := DefaultDept;
      Prefix := Depts[CurrentDept];
      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
      Val(Date[1]+Date[2], Month, errcode);
      Val(Date[4]+Date[5], Day, errcode);
      Val(Date[7]+Date[8]+Date[9]+Date[10], Year, errcode);
      DateString(Month, Day, Year, Date, 4, '/');
      DateString(Month, Day, Year, ShortDate, 2, '');
      IF year MOD 4 <>0 THEN
        juliandate := julianbase[month] + day;
      IF year MOD 4 = 0 THEN
         juliandate:=leapbase[month] + day;
      IF (Interactive = 'Y') OR (Interactive = 'B') THEN BEGIN
        OlMin := WindMin; OlMax := WindMax;
        SaveArea(27, 5, 58, 13, GSDArea);
        IF DefaultDept = 1 then
           GSDNum := 3
        ELSE
           GSDNum := 1;
        GSDKey := '';
        TextAttr := Colorscheme;
        IF Interactive = 'B' THEN

           DrawBox(27,5,51,12, White, Blue,15, Yellow,#4+' Adjust settings '+#4, F2Escape,'',
              #218 ,#196, #191, #179, #192, #217, #176, #177)
        ELSE
           DrawBox(27,5,51,12, White, Blue,15,Yellow,#4+' Adjust settings '+#4, F2Escape,'',
              #218 ,#196, #191, #179, #192, #217, #176, #177);
        Xpos := 2; Ypos := 1;
        TextBackground(Blue);
        Say(Xpos + 15, Ypos+3, Date, LightGray);
        GetDate(Year, Month, Day, DayOfWeek);
        DateString(Month, Day, Year, SystemDate, 4, '/');
        Say(Xpos + 15, Ypos+4, SystemDate, LightGray);
        Say(Xpos + 15, Ypos+5, Prefix, LightGray);
        Say(Xpos + 15, Ypos+6, LotLetter, LightGray);

        REPEAT
{Was ChooseFromPtrMenu}
(*          ChoosefromMenu(ManuMenu,               MenuLength, 2,    2,        4,  20, ManuMenuNum, Returnkey, White, Blue); *)
          ChoosefromMenu(GSDMenu{PromptHandle^}, 4,          Xpos, Ypos + 2, 10,  4, GSDNum,      GSDkey,    White, Blue)
           (*, Xpos+3, YPos)*);
          IF (GSDKey <> 'F2') AND (GSDKey <> 'ESC') THEN BEGIN
            CASE GSDNum OF
              1 : BEGIN
                    GetDate(Year, Month, Day, DayOfWeek);
                    DateString(Month, Day, Year, SystemDate, 4, '/');
                    Getchunky(Xpos + 15, Ypos + 3, Date, 1, 15, DateFormat, Returnkey);
                    IF Date <> OldDate THEN BEGIN
                      DateChanged := TRUE;
                      For i := 1 to NumOfDepts DO
                        LotNumbers[i] := 1;
                      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                      Say(Xpos + 15, Ypos+6, LotLetter, LightGray);
                    END;
                    Val(Date[1]+Date[2], Month, errcode);
                    Val(Date[4]+Date[5], Day, errcode);
                    Val(Date[7]+Date[8]+Date[9]+Date[10], Year, errcode);
                    DateString(Month, Day, Year, Date, 4, '/');
                    DateString(Month, Day, Year, ShortDate, 2, '');
                    IF year MOD 4 <>0 THEN
                      juliandate := julianbase[month] + day;
                    IF year MOD 4 = 0 THEN
                      juliandate:=leapbase[month] + day;
                    GSDNum := 2;
                  END;
              2 : BEGIN
                    GetDate(Year, Month, Day, DayOfWeek);
                    DateString(Month, Day, Year, ResetDate, 4, '/');
                    Getchunky(Xpos + 15, Ypos + 4, ResetDate, 1, 15, DateFormat, Returnkey);
                    Monthstr := ResetDate[1] + ResetDate[2];
                    Val(Monthstr, Month, Errcode);
                    DayStr := ResetDate[4] + ResetDate[5];
                    Val(Daystr, Day, Errcode);
                    YearStr := ResetDate[7] + ResetDate[8] + ResetDate[9] + ResetDate[10];
                    Val(Yearstr, Year, Errcode);
                    SetDate(Year, Month, Day);
                    GetTime(Hour, Min, Sec, Sec100);
                    Str(Hour, HourStr);
                    If Hour < 10 THEN HourStr := '0' + Hourstr;
                      Str(Min, MinStr);
                    If Min < 10 THEN MinStr := '0' + Minstr;
                    Time := HourStr+':'+MinStr;
                    GetChunky(WhereX+2, WhereY, Time, 1, 15, Time, Datekey);
                    HourStr := Time[1] + Time[2];
                    Val(HourStr, Hour, Errcode);
                    MinStr := Time[4] + Time[5];
                    Val(Minstr, Min, Errcode);
                    SetTime(Hour, Min, 0, 0);
                    GSDNum := 3;

                  END;
              3 : BEGIN
                    CurrentDept := DefaultDept;
                    OldPrefix := Depts[CurrentDept];
                    Say(Xpos + 15, Ypos+5, '                         ', Blue);
                    DeptChosen := CurrentDept;
                    ChoosefromMenu(Depts, NumOfDepts, Xpos+14, Ypos+4, 3, 1, DeptChosen, Returnkey, 7, 1);
                    CurrentDept := DeptChosen;
                    Prefix := Depts[DeptChosen];
                    Say(Xpos + 15, Ypos+5, '                         ', Blue);
                    Say(Xpos + 15, Ypos+5, Prefix, LightGray);
                    IF Prefix <> OldPrefix THEN BEGIN
                      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                      Say(Xpos + 15, Ypos+6, LotLetter, LightGray);
                    END;
                    GSDNum := 4;
                  END;
              4 : BEGIN
                    LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                    TextColor(White);
                    NewLot := Pad(LotLetter, 2);
                    ReadString(Xpos+15, Ypos+6, 'C', NewLot, Length(NewLot), Returnkey, LightGray,Black, EchoInput);
                    NewLot := Trim(NewLot);
                    IF NewLot <> LotLetter THEN BEGIN
                      Found := FALSE;
                      LotLetter := NewLot;
                      ChangeLot(Config, NewLot, Found (*, CurrentDept*));
                    END;
                  END;
              END;
            END;
        UNTIL (GSDkey = 'F2') OR (GSDKey = 'ESC');
        IF DateChanged THEN
          UpdateConfig(Config,DateChanged);
        DateChanged := FALSE;
(*        Dispose(GSDPromptHandle); *)
        RestoreArea(27, 5, GSDArea);
        WindMax := OlMax; WindMin := OlMin;
      END;
    END;
  END;


(*  PROCEDURE GetConfig(VAR Config : ConfigType);

    CONST

      julianbase:ARRAY[1..12] OF INTEGER=(0,31,59,90,120,151,181,212,243,273,
        304,334);
      leapbase:ARRAY[1..12] OF INTEGER=(0,31,60,91,121,152,182,213,244,274,
        305,335);

    VAR
      ConfigSpec : Dbfheadertype;
      ConfigRec : recordarraytype;
      OLDPREFIX : PREFIXTYPE;
      Year, Month, Day, DayOfWeek : word;
      Errcode : integer;
      YearString, Daystring, Monthstring,
      Returnkey : string;
      OldDate, SystemDate,
      Tempstr : string;
      GSDArea : areasavertype;
      OlMin, OlMax : word;
      NewLot : string;
      Found : boolean;
      GSDPromptHandle : PlusMenuPtrType;
      count : byte;
      GSDNum : integer;
      GSDKey : string;
      Xpos, Ypos : byte;
      MonthStr, DayStr, YearStr : string;
      Hour, Min, Sec, Sec100 : word;
      ResetDate,
      Time, Datekey,
      HourStr, Minstr : string;
      OpenResult : string;
      DeptChosen : integer;





  BEGIN
    With CONFIG DO BEGIN
      Opendbasefile(ConfigSpec, 'pm_cfg.dbf', OpenResult);
      Readdbfrecord(ConfigSpec, 1, ConfigRec, OpenResult);
      Date := Trim(ConfigRec[1]);
      i := 1;
      While (i <= NumOfDepts) DO BEGIN
          Tempstr := Trim(ConfigRec[i + 1]);
          Val(Tempstr, LotNumbers[i], Errcode);
          inc(i);
      END;
      Tempstr := Trim(ConfigRec[9]);
      Val(Tempstr, DefaultDept, Errcode);
      IF Errcode <> 0 THEN DefaultDept := 1;
      Tempstr := Trim(ConfigRec[10]);
      Val(Tempstr, LabelsPerRow, Errcode);
      IF Errcode <> 0 THEN Labelsperrow := 3;
      ShortDate := Date[1]+Date[2]+Date[4]+Date[5]+Date[9]+Date[10];
      CurrentDept := DefaultDept;
      Prefix := Depts[CurrentDept];
      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
      Val(Date[1]+Date[2], Month, errcode);
      Val(Date[4]+Date[5], Day, errcode);
      Val(Date[7]+Date[8]+Date[9]+Date[10], Year, errcode);
      DateString(Month, Day, Year, Date, 4, '/');
      DateString(Month, Day, Year, ShortDate, 2, '');
      IF year MOD 4 <>0 THEN
        juliandate := julianbase[month] + day;
      IF year MOD 4 = 0 THEN
         juliandate:=leapbase[month] + day;
      IF (Interactive = 'Y') OR (Interactive = 'B') THEN BEGIN
        OlMin := WindMin; OlMax := WindMax;
        SaveArea(27, 5, 58, 13, GSDArea);
        GSDNum := 1;
        GSDKey := '';
        TextAttr := Colorscheme;
        IF Interactive = 'B' THEN

           DrawBox(27,5,51,12, White, Blue,15, Yellow,#4+' Adjust settings '+#4, F2Escape,'',
              #218 ,#196, #191, #179, #192, #217, #176, #177)
        ELSE
           DrawBox(27,5,51,12, White, Blue,15,Yellow,#4+' Adjust settings '+#4, F2Escape,'',
              #218 ,#196, #191, #179, #192, #217, #176, #177);
        Xpos := 2; Ypos := 1;
        TextBackground(Blue);
        Say(Xpos + 15, Ypos+3, Date, LightGray);
        GetDate(Year, Month, Day, DayOfWeek);
        DateString(Month, Day, Year, SystemDate, 4, '/');
        Say(Xpos + 15, Ypos+4, SystemDate, LightGray);
        Say(Xpos + 15, Ypos+5, Prefix, LightGray);
        Say(Xpos + 15, Ypos+6, LotLetter, LightGray);

        REPEAT
          ChoosefromPtrMenu(GSDPromptHandle^, 4, Xpos, Ypos + 2, 10,  4, GSDNum, GSDkey, White, Blue, Xpos+3, YPos);
          IF (GSDKey <> 'F2') AND (GSDKey <> 'ESC') THEN BEGIN
            CASE GSDNum OF
              1 : BEGIN
                    GetDate(Year, Month, Day, DayOfWeek);
                    DateString(Month, Day, Year, SystemDate, 4, '/');
                    Getchunky(Xpos + 15, Ypos + 3, Date, 1, 15, Date, Returnkey);
                    IF Date <> OldDate THEN BEGIN
                      DateChanged := TRUE;
                      For i := 1 to NumOfDepts DO
                        LotNumbers[i] := 1;
                      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                      Say(Xpos + 15, Ypos+6, LotLetter, LightGray);
                    END;
                    Val(Date[1]+Date[2], Month, errcode);
                    Val(Date[4]+Date[5], Day, errcode);
                    Val(Date[7]+Date[8]+Date[9]+Date[10], Year, errcode);
                    DateString(Month, Day, Year, Date, 4, '/');
                    DateString(Month, Day, Year, ShortDate, 2, '');
                    IF year MOD 4 <>0 THEN
                      juliandate := julianbase[month] + day;
                    IF year MOD 4 = 0 THEN
                      juliandate:=leapbase[month] + day;
                    GSDNum := 2;
                  END;
              2 : BEGIN
                    GetDate(Year, Month, Day, DayOfWeek);
                    DateString(Month, Day, Year, ResetDate, 4, '/');
                    Getchunky(Xpos + 15, Ypos + 4, ResetDate, 1, 15, ResetDate, Returnkey);
                    Monthstr := ResetDate[1] + ResetDate[2];
                    Val(Monthstr, Month, Errcode);
                    DayStr := ResetDate[4] + ResetDate[5];
                    Val(Daystr, Day, Errcode);
                    YearStr := ResetDate[7] + ResetDate[8] + ResetDate[9] + ResetDate[10];
                    Val(Yearstr, Year, Errcode);
                    SetDate(Year, Month, Day);
                    GetTime(Hour, Min, Sec, Sec100);
                    Str(Hour, HourStr);
                    If Hour < 10 THEN HourStr := '0' + Hourstr;
                      Str(Min, MinStr);
                    If Min < 10 THEN MinStr := '0' + Minstr;
                    Time := HourStr+':'+MinStr;
                    GetChunky(WhereX+2, WhereY, Time, 1, 15, Time, Datekey);
                    HourStr := Time[1] + Time[2];
                    Val(HourStr, Hour, Errcode);
                    MinStr := Time[4] + Time[5];
                    Val(Minstr, Min, Errcode);
                    SetTime(Hour, Min, 0, 0);
                    GSDNum := 3;

                  END;
              3 : BEGIN
                    CurrentDept := DefaultDept;
                    OldPrefix := Depts[CurrentDept];
                    Say(Xpos + 15, Ypos+5, '                    ', Blue);
                    DeptChosen := CurrentDept;
                    ChoosefromMenu(Depts, 7, Xpos+14, Ypos+4, 3, 1, DeptChosen, Returnkey, 7, 1);
                    CurrentDept := DeptChosen;
                    Prefix := Depts[DeptChosen];
                    Say(Xpos + 15, Ypos+5, '                    ', Blue);
                    Say(Xpos + 15, Ypos+5, Prefix, LightGray);
                    IF Prefix <> OldPrefix THEN BEGIN
                      LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                      Say(Xpos + 15, Ypos+6, LotLetter, LightGray);
                    END;
                    GSDNum := 4;
                  END;
              4 : BEGIN
                    LotLetter := GetLotLetter(Config.LotNumbers[CurrentDept]);
                    TextColor(White);
                    NewLot := Pad(LotLetter, 2);
                    ReadString(Xpos+15, Ypos+6, 'C', NewLot, Length(NewLot), Returnkey, LightGray);
                    NewLot := Trim(NewLot);
                    IF NewLot <> LotLetter THEN BEGIN
                      Found := FALSE;
                      LotLetter := NewLot;
                      ChangeLot(Config, NewLot, Found );
                    END;
                  END;
              END;
            END;
        UNTIL (GSDkey = 'F2') OR (GSDKey = 'ESC');
        UpdateConfig(Config,{ CurrentDept,} DateChanged);
        DateChanged := FALSE;
        Dispose(GSDPromptHandle);
        RestoreArea(27, 5, GSDArea);
        WindMax := OlMax; WindMin := OlMin;
      END;
    END;
  END;  *)




PROCEDURE EnterInfo(VAR Info : Labeltype; VAR Entered : boolean ; LoopVar : char;
  VAR DrugEntered, ManuInfoEntered, Retrieved, LotUsed : boolean; VAR LabelFileName : string;
  VAR Recnum : longint; VAR Quit : boolean;  VAR DrugFile : string (*; CheckDiscardDate : boolean*));

  CONST
    MaxEntries = 5;

  TYPE
    ClipboardType = Record
                      Entries : byte;
                      Clips : Array[1..MaxEntries] OF string[41];
                    END;


  VAR
    Ch : char;
    HoldMin, HoldMax : word;
    PasteArea, AuxArea : areasavertype;
    Tempstring2,
    Tempstring : string;
    Clipboard : ClipboardType;
    MenuNum : integer;
    Menukey, Returnkey : string;
    FreeInfo : Labeltype;
    FreeEntered : boolean;
    InsertMode : boolean;
    Done : Boolean;
    Xloc, Yloc : integer;
    WrapAllowed : boolean;
    HotZone : byte;
    OldLength,
    Cursorplace : byte;
    Errcode : integer;
    ExpDateEntered : boolean;


  PROCEDURE ShowConfig(Xloc, Yloc : byte);
  {*************************************************************}

      VAR
        ConfigMin, ConfigMax : word;
      BEGIN
        ConfigMin := WindMin; ConfigMax := WindMax;
{        DrawBox(Xloc, Yloc,40,4,LightGray,Blue,15,15,'','','',
          #218, #196, #191, #179, #192, #217, #0, #176);}
        TextAttr := Colorscheme;
        DrawBox(Xloc, Yloc,40,4,LightGray,Blue,15,15,'','','',
          #218, #196, #191, #179, #192, #217, #176, #177);
        TextBackground(Blue);
        Say(2, 2, 'Date: ', White);
        Say(WhereX, WhereY, Config.Date, Yellow);
        Say(WhereX, WhereY, '   Dept: ', White);
        Say(WhereX, WhereY, Config.Prefix, Yellow);
        Say(WhereX, WhereY, '  Lot: ', White);
        Say(WhereX, WhereY, Config.LotLetter, Yellow);
        Say(2, 3, 'Press ', White);
        Say(WhereX, WhereY, 'F8', Yellow);
        Say(WhereX, WhereY, ' to change these settings.', White);
        WindMin := ConfigMin; WindMax := ConfigMax;
      END;
  {****END PROCEDURE SHOWCONFIG*********************************}



  PROCEDURE Refresh (Xloc, Yloc : byte);
  {*************************************************************}
    VAR
      istr : string;
      i : integer;

    BEGIN
      GoToXY(Xloc, Yloc);
      TextBackground(Blue);
      For i := 1 to NumOfLines DO BEGIN
        Str(i, istr);
        Istr := Trim(istr);
        If Length(Istr) = 1 THEN
          Istr := Istr + ' ';
        Say(Xloc, Yloc, 'Line '+ istr+''+Info[i], White);
        Inc(Yloc);
      END;
    END;
  {****END PROCEDURE Refresh************************************}


  PROCEDURE GetGoTo(VAR LineNum : integer);
 {*************************************************************}
    VAR
      GetGoToKey,
      Answer : string;
      GoToArea : Areasavertype;
      HoldMin, HoldMax : word;
      Errcode : integer;


    BEGIN
      SaveArea(42, 7, 22, 8, GoToArea);
      HoldMin := Windmin; HoldMax := WindMax;
      GetGoToKey := '';
      DrawBox(42,7,20,6,LightGray, Blue, White,Yellow,'Go to','','',#218 ,#196, #191, #179, #192, #217, #0, #177);
      Center('line number? ', 2, White);
      Answer := '  ';
      ReadString(10, 3, 'C', Answer, Length(Answer), GetGoToKey, White, Black, EchoInput);
      Answer := Trim(Answer);
      Val(Answer, Linenum, Errcode);
      IF LineNum > NumofLines THEN LineNum := NumOfLines;
      IF LineNum < 1 THEN LineNum := 1;
      RestoreArea(42, 7, GoToArea);
      WindMin := HoldMin; WindMax := HoldMax;
    END;
  {**END PROCEDURE GetGoTo**************************************}

  PROCEDURE AddClip(VAR Clipboard : clipboardtype; VAR NewClip : string);
  {*************************************************************}

    VAR
      g : byte;
    BEGIN
      IF Trim(NewClip) <> '' THEN BEGIN
        IF Clipboard.Entries < MaxEntries THEN BEGIN
          Inc(Clipboard.Entries);
          Clipboard.Clips[Clipboard.Entries] := NewClip;
        END ELSE BEGIN
          For g := 1 to (MaxEntries - 1) DO
          Clipboard.Clips[g] := Clipboard.Clips[g + 1];
          Clipboard.Clips[MaxEntries] := NewClip;
        END;
      END;
    END;
  {***END PROCEDURE AddClip*************************************}

    PROCEDURE DoAuxlines(FileName : string; VAR AuxLine : string);
  {*************************************************************}
      CONST
         MaxAuxLines = 10;

      VAR

        AuxLineSpec : dbfheadertype;
        AuxLines : Array[1..MaxAuxlines] OF String;
        AuxLineIndex : byte;
        Results : String;
        AuxIndex : LongInt;
        TAttr : byte;
        AuxLineStr : string;
        Ch : char;
        Choice, Errcode : integer;
        X, Y : byte;

      BEGIN
        TAttr := TextAttr;
        Results := '';
        Opendbasefile(AuxLineSpec, Filename, Results);
        If Results = 'GOOD' THEN BEGIN
          IF AuxLineSpec.numofrecs > 0 THEN BEGIN
            For AuxIndex := 1 to AuxLineSpec.numofrecs DO BEGIN
              Readonefield(AuxLineSpec, AuxIndex, 1, AuxLines[AuxIndex]);
              AuxLines[AuxIndex] := Trim(AuxLines[AuxIndex]);
            END;
            Closedbasefile(AuxlineSpec);
            X := 5; Y := 2;
            For AuxLineIndex := 1 to AuxIndex DO BEGIN
              Str(AuxLineIndex, AuxLineStr);
              Say(X, Y, AuxLineStr+'  '+AuxLines[AuxLineIndex], White);
              Inc(Y, 1);
            END;
            Ch := Readkey;
            Val(Ch, Choice, Errcode);
            If (Choice > 0) AND (Choice <= AuxIndex) THEN
              AuxLine := AuxLines[Choice]
            ELSE
              AuxLine := '';
            If Ch = #0 THEN
              Ch := READKEY;
          END;
        END;
      END;
  {***END PROCEDURE DoAuxLines**********************************}

    PROCEDURE Prune(VAR Instring, Extra : string; TargLength : byte);
      VAR
        CutPoint,
        i : byte;
        Done : boolean;

      BEGIN
        Extra := '';
        Done := FALSE;
        Instring := Trim(Instring);
        IF Length(Instring) > TargLength THEN BEGIN
          i := Length(Instring);
          While (i > 1) AND NOT Done DO BEGIN
             If Instring[i] = ' ' THEN BEGIN
               CutPoint := i;
               IF CutPoint < TargLength THEN
                 Done := TRUE
               ELSE
                 Dec(i);
             END ELSE
               Dec(i);
          END;
          IF Done THEN BEGIN
            Extra := Copy(Instring, CutPoint, Length(Instring) - (CutPoint - 1));
            Extra := Trim(Extra);
            Extra := Pad(Extra, Length(extra) + 1);
            Delete(Instring, Cutpoint, (Length(Instring) - (CutPoint - 1)));
            Instring := Trim(Instring);


          END;
        END;
      END;

      PROCEDURE InfoScreen;
        CONST
          days : array [0..6] of String[9] =
            ('Sunday','Monday','Tuesday',
             'Wednesday','Thursday','Friday',
             'Saturday');
          Months : array[1..12] OF String[4] =
            ('Jan','Feb','Mar','Apr','May','June',
             'July','Aug','Sept','Oct','Nov','Dec');
          julianbase:ARRAY[1..12] OF INTEGER=(0,31,59,90,120,151,181,212,243,273,
            304,334);
          leapbase:ARRAY[1..12] OF INTEGER=(0,31,60,91,121,152,182,213,244,274,
            305,335);
        VAR
          year, month, day, dayofweek, juliandate : Word;
          Date, SystemDate, OldDate, ShortDate : string;
          Errcode : integer;
          InfoMin, InfoMax : word;
          InfoArea : areasavertype;
          h, m, s, hund, NonMilH : Word;
          JulDayStr,
          TempString : string;
          OldColors : Word;
          HourStr : string;

        FUNCTION AmPm(Hour : word) : string;
          BEGIN
            CASE Hour of
              0..11 : AmPm := 'am';
              12..23 : AmPM := 'pm';
            END;
          END;

        FUNCTION GetSuffix(Day : word) : string;
          VAR
            DayStr : string;
          BEGIN
            IF (Day = 11) OR (Day = 12) OR (Day = 13) THEN BEGIN
              GetSuffix := 'th';
            END ELSE BEGIN
              Str(Day, Daystr);
              Daystr := Trim(Daystr);
              CASE Daystr[Length(Daystr)] OF
                '0', '4'..'9' : GetSuffix := 'th';
                '1' : GetSuffix := 'st';
                '2' : GetSuffix := 'nd';
                '3' : GetSuffix := 'rd';
              END;
            END;
          END;

        FUNCTION CommentStr (Month, Day, DayOfWeek : word) : string;
          BEGIN
            IF (Month = 1) AND (Day = 1) THEN
              CommentStr := 'Happy New Year!'
            Else IF (Month = 2) AND (Day = 2) THEN
              CommentStr := 'It''s Groundhog Day!'
            Else IF (Month = 3) AND (Day = 15) THEN
              CommentStr := 'Et tu, brute?'
            Else IF (Month = 4) And (Day = 1) THEN
              CommentStr := 'Your hard drive has just been reformatted.'
            Else IF (Month = 4) And (Day = 15) THEN
              CommentStr := 'Death and taxes, death and taxes...'
            Else IF (Month = 5) AND (Day = 14) THEN
              CommentStr := 'Happy b-day, P!'
            Else IF (Month = 7) AND (Day = 4) THEN
              CommentStr := 'I thought fireworks were illegal...'
            Else IF (Month = 9) AND (Day = 12) THEN
              CommentStr := 'Happy birthday, Amanda!'
            Else IF (Month = 10) AND (Day = 31) THEN
              CommentStr := 'Happy Halloween!'
            Else If (Month = 12) AND (Day = 25) THEN
              CommentStr := 'Merry Christmas!'
            Else IF (DayOfWeek = 1) THEN
              CommentStr := 'Ugh. . .Monday.'
            Else IF (DayOfWeek = 5) THEN
              CommentStr := 'Thank God it''s Friday!'
            Else CommentStr := '';
          END;

        Function LeadingZero(w : Word) : String;
          VAR
            s : String;
          BEGIN
            Str(w:0,s);
            IF Length(s) = 1 then
              s := '0' + s;
            LeadingZero := s;
          END;

        Function TimeOfDay (Hour : word) : string;
          BEGIN
            Case Hour of
              0..6 : TimeOfDay := 'grief';
              7..11 : TimeOfDay := 'morning';
              12..17 : TimeOfDay := 'afternoon';
              18..23 : TimeOfDay := 'evening';
            END;
          END;

        BEGIN
          OldColors := TextAttr;
          TextAttr := Colorscheme;
          Tempstring := '';
          InfoMin := WindMin; InfoMax := WindMax;
          SaveArea(27, 5, 53, 14, InfoArea);
          DrawBox(27,5,51,12,LightGray,Blue,15,Yellow,'',' press ENTER ','',
            #218, #196, #191, #179, #192, #217, #176, #177);
          TextColor(White);
          TextBackground(Blue);
          NonMilH := 0;
          GetTime(h,m,s,hund);
          CASE h OF
            0 : NonMilH := 12;
            1..12 : NonMilH := h;
            13..23 : NonMilH := h - 12;
          END;
          Str(NonMilH, HourStr);
          Tempstring := 'Good '+TimeofDay(h)+', '+'it is '+HourStr+':'+LeadingZero(m)+' '+AmPm(h)+'.';
          Center(Tempstring, 2, Yellow);
          Center('Give or take an hour or so. . .', 3, LightGray);
          GetDate(Year, Month, Day, DayOfWeek);
          Tempstring := 'Today is '+Days[dayofweek]+', '+Months[month]+' '+LeadingZero(day)+' '+LeadingZero(Year);
          Center(Tempstring, 5, White);
          If Trim(CommentStr(Month, Day, DayOfWeek)) <> '' THEN
            Center(CommentStr(Month, Day, DayOfWeek), 6, White);
          IF year MOD 4 <>0 THEN
            juliandate := julianbase[month] + day;
          IF year MOD 4 = 0 THEN
            juliandate:=leapbase[month] + day;
          Center(#196+#196+#196+' '+#4+' '+#196+#196+#196, 7, LightGray);
          Str(JulianDate, JulDayStr);
          Tempstring := 'It''s the '+JuldayStr+GetSuffix(Juliandate)+' day of the year.';
          Center(Tempstring, 8, Yellow);
          Tempstring := Title+ ', was written';
          Center(Tempstring, 10, White);
          Center('by John Atkeson & Scott Simon.', 11, White);
          Readln;
          RestoreArea(27, 5, InfoArea);
          WindMin := InfoMin; WindMax := InfoMax;
          TextAttr := OldColors;
        END;



      PROCEDURE DisplaySDoses;
        CONST
          StringLength = 45;
        VAR

          SDoseMin, SDoseMax : word;
          SDoseArea : areasavertype;
          OldColors : Word;
          SDString : string;
          WhStr : string;
          Sloc : byte;


        BEGIN
          OldColors := TextAttr;
          TextAttr := Colorscheme;
          WhStr := '';  (*String to whittle down *)
          SDoseMin := WindMin; SDoseMax := WindMax;
          SaveArea(27, 5, 53, 14, SDoseArea);
          DrawBox(27,5,51,12,LightGray,Blue,15,Yellow,' Standard Doses ',' press ENTER ','',
            #218, #196, #191, #179, #192, #217, #176, #177);
          TextColor(White);
          TextBackground(Blue);
          SDString := '';
          Sloc := 3;
          WhStr := DrugHandle^.Stand_Dose;
          IF Length(Trim(DrugHandle^.Stand_Dose)) > 0 THEN BEGIN
            While Length(WhStr) > 0 DO BEGIN
              While (Length(SDString) < StringLength) AND (Length(WHStr) > 0) DO BEGIN
                SDString := SDString + Copy(WhStr, 1, (Pos(';', Whstr) - 1)) +Trim(DrugHandle^.Units)+' ';
                Delete(Whstr, 1, Pos(';', Whstr));
              END;
              Say(2, Sloc, Trim(SDString),Yellow);
              SDString := '';
              Inc(Sloc);
            END;

            (* SDString := Copy(DrugHandle^.Stand_Dose,1, 48);
            Say(2,5, SDString, Yellow);
            SDString := Copy(DrugHandle^.Stand_Dose,49, 48);
            Say(2,6, SDString, Yellow);
            SDString := Copy(DrugHandle^.Stand_Dose,97, 48);
            Say(2,7, SDString, Yellow);
            SDString := 'Doses in terms of '+DrugHandle^.Units+'.';
            Say(2,8, SDString, White); *)
          END ELSE BEGIN
            SDString := 'Drug not selected or No standard doses on file';
            Center(SDString, 5, Yellow);
          END;
          Readln;
          RestoreArea(27, 5, SDoseArea);
          WindMin := SDoseMin; WindMax := SDoseMax;
          TextAttr := OldColors;
        END;


      PROCEDURE CalcExpDate(VAR EDate : string; TodayJulian : word; VAR F6DateEntered : boolean);
        VAR
          Errcode : integer;
          ExpArea : areasavertype;
          DaysGoodStr : string;
          DaysGood : word;
          ExpMin, ExpMax : word;
          ExpKey : string;

        BEGIN
          ExpMin := WindMin; ExpMax := WindMax;
          SaveArea(33, 6, 39, 8, ExpArea);  {12, 8}
          DrawBox(33,6,37,6,LightGray, Blue, White,White,#4+' Calculate expiration '+#4,'','',
            #218 ,#196, #191, #179, #192, #217, #0, #177);
          EDate := '';
          Center('How many days is drug stable?', 3, White);
          DaysGoodStr := '   ';
          Readstring(18, 4, 'C', DaysGoodStr, Length(DaysGoodStr), ExpKey, White, Black, EchoInput);
          DaysGoodStr := Trim(DaysGoodStr);
          IF DaysGoodStr <> '' THEN BEGIN
            GetExpDate(EDate, TodayJulian, DaysGoodStr, ManuInfoEntered, '', DrugHandle(*, ReconInfo*));
            F6DateEntered := TRUE;
          END ELSE
            F6DateEntered := FALSE;
          RestoreArea(33, 6, ExpArea);
          WindMin := ExpMin; WindMax := ExpMax;
        END;


      FUNCTION CenterLine(Instring : string; TargLength : byte) : string;
        VAR
          AddFront : byte;
          FrontPad : string;
          i : byte;

        BEGIN
          Instring := Trim(Instring);
          IF Length(Instring) < TargLength THEN BEGIN
            AddFront := (TargLength - Length(Instring)) DIV 2;
            FrontPad := '';
            For i := 1 to AddFront DO
              FrontPad := FrontPad + ' ';
            Instring := FrontPad + Instring;
            Instring := Pad(Instring, TargLength);
          END;
          CenterLine := Instring;




        END;

        FUNCTION RightJustify(InputString : string; TargLength : byte) : string;
          VAR
            Addcount,
            AddLength : byte;
          BEGIN
            InputString := Trim(InputString);
            AddLength := TargLength - Length(InputString);
            For Addcount := 1 to Addlength DO
               InputString := ' '+InputString;
            RightJustify := InputString;

          END;

        PROCEDURE FetchReprint(VAR OldInfo : LabelType);
          VAR
            Again : char;
            PackNum, numcnt : integer;
            LotToLookFor, LotString : string;
            PackageResult : string;
            PackageSpec : dbfheadertype;
            ReprintMin, ReprintMax : word;
            ReprintArea : areasavertype;
            ReprintKey : string;
            LotFound : boolean;
          BEGIN
            ReprintMin := WindMin; ReprintMax := WindMax;
            SaveArea(30, 6, 47, 8, ReprintArea); {24, 7}
            REPEAT
              TextBackground(BLUE);
              DrawBox(30,6,45,6,LightGray, Blue, White,White,#4+' Reprint label '+#4,
                '','',#218 ,#196, #191, #179, #192, #217, #0, #177);
              Center('Lot number of label to be reprinted?', 3, White);
              LotToLookFor := '              ';
              Readstring(18, 4, 'C', LotToLookFor, Length(LotToLookFor), ReprintKey, White, Black, EchoInput);
              If (ReprintKey = 'RETURN') AND (Trim(LotToLookFor) <> '') THEN BEGIN
                LotToLookFor := Trim(LotToLookFor);
                LotFound := FALSE;
                PackageResult := '';
                LotString := '';
                PackNum := 1;
                Opendbasefile(PackageSpec, 'package.dbf', PackageResult);
                IF PackageResult = 'GOOD' THEN BEGIN
                  While (PackNum <= PackageSpec.numofrecs) AND Not LotFound DO BEGIN
                    readonefield(PackageSpec, Packnum, 3, LotString);
                    IF Pos(LotToLookFor, LotString) > 0 THEN BEGIN
                       LotFound := TRUE;
                       For numcnt := 1 to 5 DO
                         Readonefield(PackageSpec, Packnum, numcnt, Info[numcnt]);
                       Again := 'N';
                    END ELSE
                       Inc(Packnum);
                  END;
                  IF Not LotFound THEN BEGIN
                    DrawBox(30,6,45,6,LightGray, Red, White,White,'','','',#218 ,#196, #191, #179, #192, #217, #0, #177);
                    TextBackground(Red);
                    Center('Lot number ['+LotToLookFor+'] not found.', 3, White);
                    Center('Try again? [Y/N]', 4, White);
                    Again := Upcase(Readkey);
                  END;
                END;
              END ELSE Again := 'N';
            UNTIL Again <> 'Y';
            RestoreArea(30, 6, ReprintArea);
            WindMin := ReprintMin; WindMax := ReprintMax;
          END;


(*          PROCEDURE SaveScreen;
            VAR
              ScreenArea : areasavertype;
              CurrentMin, CurrentMax : word;
              Restart : char;
            BEGIN
              CurrentMin := WindMin; CurrentMax := WindMax;
              Window(1, 1, 80, 25);
              SaveArea(1, 1, 80, 25, ScreenArea);
              TextBackground(Black);
              Clrscr;
              Writeln('Press ENTER to return to program.');
              Restart := readkey;
              RestoreArea(1, 1, ScreenArea);
              WindMin := CurrentMin; WindMax := CurrentMax;
            END; *)



  BEGIN



    IF LoopCounter = 0 THEN
      DrugEntered := FALSE;
    {    For i := 1 to Numoflines DO
      Style[i] := 0; }
    Clipboard.Entries := 0;
    For i := 1 to 5 DO
      Clipboard.Clips[i] := Blankline;
    Menukey := '';
    Returnkey := '';
    MenuNum := 1;
    HotZone := 37;
    WrapAllowed := TRUE;
    LotUsed := FALSE;
    InsertMode := TRUE;
    Cursorplace := 1;


    ShowConfig(35, 19);     {30, 16}
    TextAttr := Colorscheme;
{    DrawBox(20,5,53,10,LightGray,Blue,15,15,'','','',
       #218, #196, #191, #179, #192, #217, #0, #0); }
{%} DrawBox(27,5,51,12,LightGray,Blue,15,15,'','','',
       #218, #196, #191, #179, #192, #217, #176, #177);


    TextColor(White);
    TextBackground(Blue);

    Refresh(2, 2);



    Xloc := 1; Yloc := 1;

REPEAT
  If InsertMode THEN Say(9,7,'INSERT',Yellow)
  ELSE Say(9, 7, '      ', Blue);
  newreadstring(Xloc + 8,Yloc + menunum,'D',Info[menunum],  {'D' doesn't mean anything, not 'C', lower case is allowed}
    Length(Info[menunum]),ReturnKey, CharsPerLine, WrapAllowed, HotZone, Cursorplace, InsertMode
    (*, 'Press any key to return to Pac-Manager'*));
  If (Returnkey = 'DOWN') OR (Returnkey = 'RETURN') OR (Returnkey = 'WRAP') THEN
    Inc(menunum)
  ELSE If (Returnkey = 'UP') THEN
    Dec(menunum)
  ELSE If (Returnkey = 'CTRLA') THEN BEGIN
    For i := 1 to Numoflines DO
      Info[i] := CenterLine(Info[i], CharsPerLine);
  END


  ELSE If (Returnkey = 'CTRLB') THEN
    Info[Menunum] := Blankline
  ELSE IF (Returnkey = 'CTRLDEL') THEN BEGIN
     AddClip(Clipboard, Info[MenuNum]);
    For i := MenuNum to NumOfLines - 1 DO
      Info[i] := Info[i + 1];
    Info[NumOfLines] := Blankline;
  END
  ELSE IF (Returnkey = 'CTRLE') THEN BEGIN
    For i := 1 to NumOfLines DO
      Info[i] := Blankline;
    DrugEntered := FALSE;
    ManuInfoEntered := FALSE;
  END
  ELSE If (Returnkey = 'CTRLINS') THEN BEGIN
    AddClip(Clipboard, Info[NumOfLines]);
    Info[NumOfLines] := BlankLine;
    For i := NumOfLines - 1 DOWNTO MenuNum DO
      Info[i + 1] := Info[i];
    Info[MenuNum] := BlankLine;
  END

  ELSE If (Returnkey = 'CTRLC') THEN
    AddClip(Clipboard, Info[MenuNum])

  ELSE IF (Returnkey = 'PASTE') THEN BEGIN
      SaveArea(27, 5, 54, 12, PasteArea);
      HoldMin := WindMin; HoldMax := WindMax;
      TextAttr := Colorscheme;
      DrawBox(27,5,51,12, LightGray,Blue,White, LightGray,#4+' Paste '+#4,'','',
        #218, #196, #191, #179, #192, #217, #176, #177);
      TextBackground(Blue);

      TextColor(White);
      For i := 1 to 5 DO BEGIN
        GotoXY(4, 1 + i);
        Write(i,' ', Clipboard.Clips[i]);
      END;
      TextColor(Yellow);
      Say(2, 7, 'Enter line number to paste, any other key to exit', Yellow);
      TextColor(Black);
      Ch := Readkey;
      Val(Ch, i, errcode);
      CASE i OF
       1..5 :  BEGIN
                 Insert(Trim(Clipboard.Clips[i])+' ', Info[MenuNum], Cursorplace);
                 IF Length(Info[Menunum]) > CharsPerline THEN BEGIN
                   Tempstring := Copy(Info[Menunum], CharsPerline + 1, Length(Info[MenuNum]));
                   Tempstring := Pad(Tempstring, CharsPerline);
                   AddClip(Clipboard, Tempstring);
                   Delete(Info[Menunum], CharsPerline + 1, Length(Info[MenuNum]));
                 END;
               END;
      END;
      IF Ch = #0 THEN
        Ch := Readkey;
      TextColor(White);
      RestoreArea(27, 5, PasteArea);
      WindMin := HoldMin; WindMax := HoldMax;
  END

  ELSE IF (Returnkey = 'CUT') THEN BEGIN
    AddClip(Clipboard, Info[MenuNum]);
    Info[MenuNum] := BlankLine;
  END

  ELSE IF (Returnkey = 'CTRLF') THEN BEGIN
    Tempstring := Copy(Info[menunum], 1, cursorplace - 1);
    Tempstring := Pad(Tempstring, CharsPerLine);
    AddClip(Clipboard, Tempstring);
    OldLength := Length(Info[menunum]);
    For i := 1 to cursorplace - 1 DO
      info[menunum][i] := ' ';
    Info[menunum] := trim(Info[menunum]);
    Info[menunum] := Pad(Info[menunum], Oldlength);
  END
  ELSE IF (ReturnKey = 'CTRLG') THEN BEGIN
    Delete(Info[Menunum], Length(Info[MenuNum]), 1);
    Insert(Char(#230), Info[Menunum], Cursorplace);
    Inc(Cursorplace);
  END
  ELSE IF Returnkey = 'CTRLJ' THEN BEGIN
    For i := 1 to 5 DO
      Info[i] := RightJustify(Info[i], CharsPerLine);
  END
  ELSE IF Returnkey = 'CTRLL' THEN BEGIN
     Tempstring := '';
(*  {%} DrawBox(27,5,51,10,LightGray,Blue,15,15,'','','',
       #218, #196, #191, #179, #192, #217, #176, #177); *)

     SaveArea(27, 5, 54, 15, AuxArea);
     HoldMin := WindMin; HoldMax := WindMax;
     TextAttr := Colorscheme;
     DrawBox(27, 5, 51,12,White,Blue,15,White,'Enter line number',
       '','',#218, #196, #191, #179, #192, #217, #176, #177);
     TextBackground(Blue);
     TextColor(White);
     AddClip(Clipboard, Info[MenuNum]);
     IF Config.Prefix = 'C' THEN
       DoAuxlines('cauxline.dbf', Tempstring)
     ELSE
       DoAuxlines('Auxlines.dbf',Tempstring);
     IF Trim(Tempstring) <> '' THEN
       Insert(Trim(Tempstring)+' ', Info[MenuNum], Cursorplace);
     IF Length(Info[Menunum]) > CharsPerline THEN BEGIN
       Tempstring := Copy(Info[Menunum], CharsPerline + 1, Length(Info[MenuNum]));
       Tempstring := Pad(Tempstring, CharsPerline);
       AddClip(Clipboard, Tempstring);
       Delete(Info[Menunum], CharsPerline + 1, Length(Info[MenuNum]));
     END;
     Info[MenuNum] := Pad(Info[MenuNum], CharsPerLine);
     RestoreArea(27, 5, AuxArea);
     WindMin := HoldMin; WindMax := HoldMax;

  END

  ELSE IF (Returnkey = 'CTRLR') THEN BEGIN
    Tempstring := Copy(Info[menunum], cursorplace, Length(info[menunum]));
    Tempstring := Pad(Tempstring, CharsPerLine);
    AddClip(Clipboard, Tempstring);
    For i := cursorplace to length(Info[MenuNum]) DO
      Info[Menunum][i] := ' ';
  END
  ELSE IF (Returnkey = 'CTRLS') THEN BEGIN
    DisplaySDoses;
  END

  ELSE IF (Returnkey = 'CTRLU') THEN BEGIN  {UN-CENTER}
    For i := 1 to numoflines DO BEGIN
      Info[i] := Trim(Info[i]);
      Info[i] := Pad(Info[i], CharsPerLine);
    END;
  END

  ELSE IF (RETURNKEY = 'CTRLV') THEN BEGIN
    MenuNum := 5;
    Insert(Config.MCVName+' ', Info[MenuNum], 1);
    IF Length(Info[MenuNum]) > CharsPerline THEN BEGIN
      Tempstring := Copy(Info[Menunum], CharsPerline + 1, Length(Info[MenuNum]));
      Tempstring := Pad(Tempstring, CharsPerline);
      AddClip(Clipboard, Tempstring);
      Delete(Info[Menunum], CharsPerline + 1, Length(Info[MenuNum]));
    END;

  END
(*  ELSE IF (RETURNKEY = 'CTRLX') THEN BEGIN
    Writeln(DrugHandle^.DrugInfo);
    Readln;
  END *)
(*  ELSE IF (Returnkey = 'CTRLW') THEN BEGIN
    PrintString(0, PrinterStatus, '',' ',' ','Y');
  END *)
  ELSE IF (Returnkey = 'CTRLZ') THEN BEGIN
    InfoScreen;
  END
  ELSE IF (Returnkey = 'F1') THEN
    DoHelp(2, 2)

  ELSE IF (Returnkey = 'F3') THEN BEGIN
    Menunum := 1;
(*    GetDoseForm(DrugFile, DrugIndex); {, 28, 6);}
    IF (DrugFile <> '') THEN BEGIN *)
      GetDrugInfo(DrugHandle^, DrugFile, DrugIndex, 5, 4, DrugEntered, ManuInfoEntered);

(*      IF DRUGENTERED THEN BEGIN
        With DrugHandle^ DO BEGIN
          IF DrugFile = 'solids.dbf' THEN BEGIN
            CharsPerLine := 20;
            NumOfLines := 7;
            AddClip(Clipboard, Info[menunum]);
            Info[Menunum] := Name;
            Prune(Info[MenuNum], Tempstring, CharsPerLine);
            Info[Menunum] := Pad(Info[MenuNum], CharsPerLine);
            AddClip(Clipboard, Info[menunum + 1]);
            Info[Menunum + 1] := Form+' '+Dose;
            Insert(Tempstring, Info[MenuNum + 1], 1);
            Prune(Info[MenuNum + 1], Tempstring2, CharsPerLine);
            Info[Menunum+1] := Pad(Info[MenuNum+1], CharsPerLine);

            AddClip(Clipboard, Info[Menunum + 2]);
            Info[Menunum + 2] := Pad('LOT: '+Config.Prefix+'-'+Config.ShortDate+Config.LotLetter, CharsPerLine);

            Insert(Tempstring2, Info[MenuNum + 2], 1);
            Info[MenuNum+2] := Trim(Info[Menunum+2]);
            Info[Menunum+2] := Pad(Info[MenuNum+2], CharsPerLine);

            AddClip(Clipboard, Info[Numoflines - 3]);
            Info[Numoflines - 1] := Pad(Aux1, CharsPerLine);
            AddClip(Clipboard, Info[Numoflines - 2]);
            Info[Numoflines] := Pad(Aux2, CharsPerLine);

            AddClip(Clipboard, Info[Numoflines - 1]);
            Info[Numoflines - 1] := Pad('MCVH Pharmacy', CharsPerLine);
            AddClip(Clipboard, Info[Numoflines]);
            Info[Numoflines] := Pad('EXP: '+ExpDate, CharsPerLine);
          END ELSE BEGIN
            AddClip(Clipboard, Info[menunum]);
            Info[Menunum] := Name+' '+Form;
            Prune(Info[MenuNum], Tempstring, CharsPerLine);
            Info[Menunum] := Pad(Info[MenuNum], CharsPerLine);

            AddClip(Clipboard, Info[menunum + 1]);
            Info[Menunum + 1] := Dose;
            Insert(Tempstring, Info[MenuNum + 1], 1);
            Prune(Info[MenuNum + 1], Tempstring2, CharsPerLine);
            Info[Menunum+1] := Pad(Info[MenuNum+1], CharsPerLine);

            AddClip(Clipboard, Info[Menunum + 2]);
            Info[Menunum + 2] :=
              Pad('LOT: '+Config.Prefix+'-'+Config.ShortDate+Config.LotLetter+' EXP: '+ExpDate, CharsPerLine);
            Insert(Tempstring2, Info[MenuNum + 2], 1);
            Info[MenuNum+2] := Trim(Info[Menunum+2]);
            Info[Menunum+2] := Pad(Info[MenuNum+2], CharsPerLine);

            AddClip(Clipboard, Info[Numoflines - 1]);
            Info[Numoflines - 1] := Pad(Aux1, CharsPerLine);
            AddClip(Clipboard, Info[Numoflines]);
            Info[Numoflines] := Pad(Aux2, CharsPerLine);
          END;
        END;
      END;  *)


     {This is the working version, commenting it out to save an unaltered copy}
      IF DRUGENTERED THEN BEGIN
        With DrugHandle^ DO BEGIN
          AddClip(Clipboard, Info[menunum]);
          Info[Menunum] := Name+' '+Form;
          Prune(Info[MenuNum], Tempstring, CharsPerLine);
          Info[Menunum] := Pad(Info[MenuNum], CharsPerLine);

          AddClip(Clipboard, Info[menunum + 1]);
          Info[Menunum + 1] := Dose;
          Insert(Tempstring, Info[MenuNum + 1], 1);
          Prune(Info[MenuNum + 1], Tempstring2, CharsPerLine);
          Info[Menunum+1] := Pad(Info[MenuNum+1], CharsPerLine);

          AddClip(Clipboard, Info[Menunum + 2]);
          (* originally this was : Config.Prefix+'-'+Config.ShortDate+Config.LotLetter
          instead of Drug.Ourlot *)
          Info[Menunum + 2] := Pad('LOT: '+DrugHandle^.OurLot+' EXP: '+ExpDate, CharsPerLine);
          Insert(Tempstring2, Info[MenuNum + 2], 1);
          Info[MenuNum+2] := Trim(Info[Menunum+2]);
          Info[Menunum+2] := Pad(Info[MenuNum+2], CharsPerLine);


          AddClip(Clipboard, Info[Numoflines - 1]);
          Info[Numoflines - 1] := Pad(Aux1, CharsPerLine);
          AddClip(Clipboard, Info[Numoflines]);
          Info[Numoflines] := Pad(Aux2, CharsPerLine);
        END;
      END;
    { END; }
  END

  ELSE IF Returnkey = 'F4' THEN BEGIN
    AddClip(Clipboard, Info[MenuNum]);
    Info[MenuNum] := Trim(info[Menunum]);
    Info[MenuNum] := Pad(info[MenuNum], Length(Info[MenuNum]) + 1);
    Insert('LOT: '+Config.Prefix+'-'+Config.ShortDate+Config.LotLetter+' ', Info[Menunum], CursorPlace);
    LotUsed := TRUE;
    Prune(Info[MenuNum], Tempstring, CharsPerLine);
    Info[Menunum] := Pad(Info[menunum], 40);

  END
  ELSE IF Returnkey = 'F5' THEN BEGIN
    GetManuInfo(Manu, DrugHandle, ManuInfoEntered, 'Y',{ 7, 5} 27, 5,'A'(*, ReconInfo*));
  END

  ELSE IF Returnkey = 'F6' THEN BEGIN
    CalcExpDate(ExpDate, Config.JulianDate, ExpDateEntered);
    IF ExpdateEntered THEN BEGIN
      AddClip(Clipboard, Info[MenuNum]);
      Info[MenuNum] := Trim(info[Menunum]);
      Info[MenuNum] := Pad(info[MenuNum], Length(Info[MenuNum]) + 1);
      Insert('EXP: '+ExpDate+' ', Info[MenuNum], CursorPlace);
      Prune(Info[MenuNum], Tempstring, CharsPerLine);
      Info[Menunum] := Pad(Info[menunum], CharsPerLine);
    END;
  END

  ELSE IF Returnkey = 'F7' THEN BEGIN
    FreeEntered := FALSE;
    IF Loop = 'N' THEN
      Retrieved := FALSE;
    RetrieveLabel(Config.Prefix, FreeInfo, FreeEntered, Retrieved, LabelFileName);
    IF FreeEntered THEN BEGIN
      Info := FreeInfo;
      DrugFile := '';
      RecNum := 0;
      DrugEntered := FALSE;
    END;
  END
  ELSE IF Returnkey = 'F8' THEN BEGIN
    GetSetDate(Config, 'Y');
    ShowConfig(35, 19);  {30, 16}
  END
  ELSE IF Returnkey = 'F9' THEN BEGIN
    FetchReprint(Info);
  END

  ELSE IF (Returnkey = 'F10') THEN
    Quit := TRUE

  ELSE IF (Returnkey = 'PAGEUP') THEN
    MenuNum := 1

  ELSE IF (Returnkey = 'PAGEDOWN') THEN
    MenuNum := NumOfLines

  ELSE IF Returnkey = 'ALT1' THEN Menunum := 1
  ELSE IF Returnkey = 'ALT2' THEN Menunum := 2
  ELSE IF Returnkey = 'ALT3' THEN Menunum := 3
  ELSE IF Returnkey = 'ALT4' THEN Menunum := 4
  ELSE IF Returnkey = 'ALT5' THEN Menunum := 5
  (*  IF Returnkey = 'ALT6' THEN Menunum := 6;
  IF Returnkey = 'ALT7' THEN Menunum := 7;
  IF Returnkey = 'ALT8' THEN Menunum := 8;
  IF Returnkey = 'ALT9' THEN Menunum := 9;
  IF Returnkey = 'ALT0' THEN Menunum := 10; *)

  ELSE IF Returnkey = 'ESC' THEN BEGIN
    Abort('S', Quit);
  END


  ELSE IF Returnkey = 'CTRLHOME' THEN BEGIN
    GetGoTo(MenuNum);
  END;



  If Menunum > NumofLines THEN
    Menunum := 1;
  If Menunum < 1 THEN
    Menunum := NumofLines;


  Refresh(2, 2);
  (* ShowConfig(35, 16); *)


UNTIL (Returnkey = 'F2') OR (Quit);


IF Returnkey = 'F2' THEN
  Entered := TRUE;
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

PROCEDURE println(VAR printerstatus : statustype; outstring:STRING);
    {*********************************************************************}
    {*  IF the printer has already been proven not to work, then this    *}
    {*  won't try to send any more to the printer until NoPrint has   *}
    {*  been deliberately reset.                                         *}
    {*********************************************************************}
    VAR
      Continue : char;
      PErrorArea : areasavertype;
      ErrorMin, ErrorMax : word;
      OldAttr : word;


    BEGIN
      SaveArea(35, 19, 42, 6, PErrorArea); {30, 16}
      ErrorMin := WindMin; ErrorMax := WindMax;
      OldAttr := TextAttr;
      Continue := ' ';
      While NOT(PrinterStatus.PrinterError) AND (Continue <> 'N') DO BEGIN
          PrintString(0, PrinterStatus, Outstring, LF);
          IF PrinterStatus.PrinterError THEN BEGIN
            TextColor(Blue); {30, 16}
            TextAttr := Colorscheme;
            DrawBox(35,19,40,4,White,Red,15,15,' Printer Error ',
               ' '+PrinterStatus.StatusString+' ','Continue printing? [Y/N]',
               #218, #196, #191, #179, #192, #217,{ #0, #176} #176, #177);
            Continue := Upcase(Readkey);
            IF Continue = 'Y' THEN
              PrinterStatus.PrinterError := FALSE;
          END ELSE
            Continue := 'N';
      END;

        RestoreArea(35, 19, PErrorArea); {30, 16}
        WindMax := ErrorMax; WindMin := ErrorMin;
        TextAttr := OldAttr;


    END;



PROCEDURE printlabels(VAR Line: labeltype; destination:STRING);
  {************************************************************}
  {* Actually prints the labels.                              *}
  {************************************************************}

  CONST
    Bidirectional : string = #27 + #85 + #48;
    PrintStyle    : string = #15+#27+#80;

  VAR
    remainder  : INTEGER;
    counter    : INTEGER;
    outlabels  : TEXT;
    RowsOfPrints, err : INTEGER;
    Outline : string;
    j : byte;


  PROCEDURE FormFeed(VAR PrinterStatus : statustype);
    {VAR BlankLabel : Text;}

  BEGIN
      {Assign(Blanklabel, destination);
      Rewrite(Blanklabel);}
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));

      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));
      println(PrinterStatus, St(' ',38) +St(' ',43) +St(' ',43));

      {Close(Blanklabel);}
   END;

   PROCEDURE PrintManu(Manu : ManuType; Drug : DrugType; VAR PrinterStatus : statustype);
     VAR
       NoteText,
       NoteText2,
       ManuText : string;

     FUNCTION GetStorage : string;
       VAR
         FleetingString : string;
       BEGIN
         FleetingString := '';
         IF DrugFile = 'liqglass.dbf' THEN
           FleetingString := 'GLASS'
         ELSE IF DrugFile = 'liqplast.dbf' THEN
           FleetingString := 'PLASTIC'
         ELSE
           FleetingString := '';
         GetStorage := FleetingString;
       END;

     PROCEDURE UNDERLINE (Message : string; LFeed : char);
      {IF LF = 'Y' THEN Do a writeln at the end}
      VAR
        i,
        MessageLength : byte;
      BEGIN
        Message := Trim(Message);
        PrintString(0, PrinterStatus, Message, NONE);
        MessageLength := Length(Message);
        For i := 1 to MessageLength DO
          PrintString(0, PrinterStatus, #8, NONE); {backspace}
        For i := 1 to MessageLength DO
          PrintString(0, PrinterStatus, #95, NONE); {underline}
       IF LFeed = 'Y' THEN
          PrintString(0, PrinterStatus, '', LF); {line feed}
      END;

     BEGIN
       With Manu DO BEGIN
         {PRINT FIRST LINE}
         ManuText := '';
         NoteText := '';
         NoteText2 := '';

       {Write(outfile, PrinterConfig.Reset, PrinterConfig.FontSelect, PrinterConfig.Compressed);}
         ManuText := 'Number of Doses Packaged: '+NumberofLabels;
         ManuText := St(ManuText, 45);
         PrintString(0, PrinterStatus, ManuText, NONE);
{        PrintString(0, PrinterStatus, 'Number of Doses Packaged: ', NONE);
         PrintString(0, PrinterStatus, NumberofLabels, NONE);
         (* UnderLine(NumberOfLabels, 'N'); *)
         IF (Drug.PFL = 'T') OR (Drug.Freeze = 'T') OR (Drug.Glass = 'T') THEN BEGIN
           PrintString(0, PrinterStatus, ' Code: ', NONE);
           IF Drug.PFL = 'T' THEN Underline('P', 'N');
           IF Drug.Glass = 'T' THEN Underline('G','N');
           IF Drug.Freeze = 'T' THEN Underline('F', 'N');
         END;
         PrintString(0, PrinterStatus, '', LF);} {line feed}

         NoteText := Drug.Name + ' LOT: '+Drug.OurLot;(* +Config.Prefix+'-'+Config.ShortDate+Config.LotLetter; *)
         NoteText := St(NoteText, 45);
         PrintString(0, PrinterStatus, NoteText, None);

         NoteText2 := 'DOSE: '+Drug.Dose + ' LOT: '+Drug.Ourlot; (*Config.Prefix+'-'+Config.ShortDate+Config.LotLetter; *)
         NoteText2 := St(NoteText2, 45);
         PrintString(0, PrinterStatus, NoteText2, LF);


         {PRINT SECOND LINE}
         ManuText := '';
         NoteText := '';
         NoteText2 := '';
         ManuText := 'Company Name: '+CompanyName;
         ManuText := St(ManuText, 45);
         PrintString(0, PrinterStatus, ManuText, NONE);
         IF (Drug.Units <> 'ML') AND (DrugFile <> 'solids.dbf') THEN BEGIN
           NoteText := 'CONC: '+Drug.Conc + Drug.Units+'/ML -  Stable for '+Drug.Exp+' days';
         END ELSE BEGIN
           NoteText := 'Stable for '+Drug.Exp+' days';
         END;
         NoteText := St(NoteText, 45);
         PrintString(0, PrinterStatus, NoteText, NONE);

         NoteText2 := Drug.DrugInfo;
         NoteText2 := St(NoteText2, 45);
         PrintString(0, PrinterStatus, NoteText2, LF);


         {======PRINT THIRD LINE=========}

(*         PrintString(0, PrinterStatus, 'Company Name: ', NONE);
         PrintString(0, PrinterStatus, CompanyName, LF); *)
         (* Underline(CompanyName, 'Y'); *)
         NoteText := '';
         ManuText := '';
         NoteText2 := '';
         ManuText := 'Mfg Lot: '+LotNumber+' Mfg Exp: '+MExpDate;
         ManuText := St(ManuText, 45);
         PrintString(0, PrinterStatus, ManuText, NONE);
{         PrintString(0, PrinterStatus, 'Mfg Lot: ', NONE);
         PrintString(0, PrinterStatus, LotNumber, NONE);
         (* Underline(LotNumber, 'N'); *)
         PrintString(0, PrinterStatus, ' Mfg Exp: ', NONE);
         PrintString(0, PrinterStatus, MExpDate, LF);
         (* Underline(MExpDate, 'Y'); *) }
         NoteText := '';
         IF Drug.PFL = 'T' THEN NoteText := 'PROTECT FROM LIGHT';
         NoteText := St(NoteText, 45);
         PrintString(0, PrinterStatus, NoteText, NONE);

         NoteText2 := '';
         IF ChkConc THEN NoteText2 := Drug.Warn1;
         NoteText2 := St(NoteText2, 45);
         PrintString(0, PrinterStatus, NoteText2, LF);


         {======PRINT FOURTH LINE=========}

{         IF (Drug.PFL = 'T') OR (Drug.Freeze = 'T') OR (Drug.Glass = 'T') THEN BEGIN
           (* PrintString(0, PrinterStatus, ' Code: ', NONE); *)
           IF Drug.PFL = 'T' THEN NoteText := NoteText + ' Protect from Light';
           IF Drug.Glass = 'T' THEN NoteText := NoteText + ' Package in Glass';
           IF Drug.Freeze = 'T' THEN NoteText := NoteText + ' Store in Freezer';
           NoteText := Trim(NoteText);
         END;  }
{         NoteText := St(NoteText, 45);
(*         IF Length(Trim(NoteText)) > 0 THEN *)
         PrintString(0, PrinterStatus, NoteText, NONE);
(*         ELSE
           PrintString(0, PrinterStatus, '', LF); *) }




         ManuText := '';
         NoteText := '';
         NoteText2 := '';

         IF Trim(Drug.Storage) <> '' THEN BEGIN
           ManuText := ManuText + 'Pkgd in '+Drug.Storage;
           (* PrintString(0, PrinterStatus, 'Pkgd in ', NONE); {line feed}
           PrintString(0, PrinterStatus, Drug.Storage, NONE); *)
           (* UnderLine(Drug.Storage, 'N'); *)
         END;
         IF Drug.ChkDiscardDate = 'T' THEN BEGIN
           ManuText := ManuText + ' Reconst: '+ Drug.DiscardDate;
           (* PrintString(0, PrinterStatus, ' Reconst: ', NONE);
           PrintString(0, PrinterStatus, Drug.DiscardDate, NONE); *)
           (* UnderLine(Drug.DiscardDate, 'N'); *)
         END;
         ManuText := St(ManuText, 45);
         PrintString(0, PrinterStatus, ManuText, NONE);
         IF Drug.FREEZE = 'T' THEN NoteText := 'STORE IN FREEZER';
         NoteText := St(NoteText, 45);
         PrintString(0, PrinterStatus, NoteText, NONE);
         NoteText2 := '';
         IF ChkConc THEN NoteText2 := Drug.Warn2;
         NoteText2 := St(NoteText2, 45);
         PrintString(0, PrinterStatus, NoteText2, LF);


         {======PRINT FIFTH LINE=========}




(*         IF NonStand THEN NoteText := 'Non-standard dose';
           PrintString(0, PrinterStatus, NoteText, LF); *)
(*         ELSE
           PrintString(0, PrinterStatus, '', LF); {line feed} *)

         NoteText := '';
         ManuText := '';
         NoteText2 := '';
         ManuText := 'Packaged by: '+Packager+' Initials:__________';
         ManuText := St(ManuText, 45);
{         PrintString(0, PrinterStatus, 'Packaged by: ', NONE);
         PrintString(0, PrinterStatus, Packager, NONE); }
         (* Underline(Packager, 'N'); *)
{         PrintString(0, PrinterStatus, ' Initials:__________',  LF); }
         PrintString(0, PrinterStatus, ManuText, NONE);
         IF Drug.GLASS = 'T' THEN NoteText := 'THIS DRUG MUST BE PACKAGED IN GLASS';
         NoteText := St(NoteText, 45);
         PrintString(0, PrinterStatus, NoteText, NONE);
         NoteText2 := '';
         IF NonStand THEN NoteText2 := 'This is not a standard dose';
         NoteText2 := St(NoteText2, 45);
         PrintString(0, PrinterStatus, NoteText2, LF);




         PrintString(0, PrinterStatus, '', LF); {line feed}
       END;
     END;


BEGIN

      Window(1,1,80,25);
      TextColor(Blue);    {30, 16}
      TextAttr:= Colorscheme;
      DrawBox(35,19,40,4,LightGray,Blue,15,White,'','','',#218, #196, #191, #179, #192, #217, {#0, #177} #176, #177);
      TextBackground(Blue);
      TextColor(White);
      Repeat
        Say(2, 2, 'How many labels do you want? ', White);
        READLN(numberoflabels);
        VAL(numberoflabels {string},numberofprints {integer},err);
        If err <> 0 THEN BEGIN
          TextColor(Red);
          GoToXY(2,3);
          Write('Please enter number of labels.');
          TextColor(LightGray);
        END;
      Until err = 0;

      If numberofprints <> 0 THEN BEGIN
        rowsofprints := (numberofprints div config.labelsperrow) + 1;
        IF DrugHandle^.pfl = 'T' THEN Rowsofprints := Rowsofprints * 2;  {Print labels for UVLI Bags}
        remainder:=numberofprints MOD config.labelsperrow;
        IF remainder>0 THEN  rowsofprints:=rowsofprints+1;

        IF (destination='CRT') THEN BEGIN  { Print to screen. }
          ASSIGNCRT(outlabels);
        END ELSE BEGIN      { Else print to printer, or file, or whatever. }
        END;
        With Printerconfig DO BEGIN

{         Write(outlabels, Fontselect);
          Write(outlabels, Compressed); }

          counter:=0;
          PrinterStatus.PrinterError := FALSE;

          REPEAT

            For i := 1 to Numoflines DO BEGIN
              outline := '';
              For j := 1 to Config.Labelsperrow DO
                Outline := Outline + St(Line[i], 45);
              println(printerstatus, outline);
            END;
            println(printerstatus, '');
            counter:=counter+1;
            outline := '';
          UNTIL counter>=rowsofprints;

          IF ManuInfoEntered THEN
            PrintManu(Manu, DrugHandle^, PrinterStatus);
          FormFeed(PrinterStatus);
        END;
        END ELSE BEGIN

          GoToXY(2,3);
          Say(2,3,'                              ',Blue);
          Center('Zero aborts printing.        ', 3, Yellow);
          Delay(500);
          Say(2,3,'                              ',Blue);
          TextColor(LightGray);
        END;


  END;  { End of printlabels. }



PROCEDURE SaveLabel(VAR Info : labeltype; Dept : string);

{  CONST
    Illegal : Set Of Char=[' ','/','.',',','\','[',']','*','+','=','|',':',';','"','?','<','>']; }

  VAR

    Save : char;
    Returnkey : string;
    i : integer;
    S : PathStr;
    DirInfo : searchrec;
    TempString : String;
    Counter : integer;
    Valid : boolean;
    CurrentNumofrecs : integer;
    LabelFileSpec : DBFheadertype;
    LabelFileResults : string;
    FullArea : areasavertype;
    FullMin, FullMax : word;



PROCEDURE UpdateListFile(FName : string);
{**************************************************************************}
{A misguided attempt to write packaging label info to a dBase file.}
{**************************************************************************}
VAR
  listfilespecs                    : DBFheadertype;
  listfilerecord                   : recordarraytype;
  listfileresults : string;
  PrintString : string;
  j : integer;
  targetrec : integer;



BEGIN

 opendbasefile(listfilespecs, FName, listfileresults);
 IF listfileresults = 'GOOD' THEN BEGIN
   IF RETRIEVED THEN
     Targetrec := recordnum
   ELSE BEGIN
     listfilespecs.numofrecs := listfilespecs.numofrecs + 1;
     targetrec := listfilespecs.numofrecs;
   END;
   For j := 1 to 5 DO
     listfilerecord[j] := Trim(info[j]);
   WriteDBFrecord(listfilespecs, targetrec, listfilerecord,listfileresults);
   Say(3, WhereY + 2, 'Sorting database. Please wait. . .', Yellow);
   DBFSort(listfilespecs, 1);
   closedbasefile(listfilespecs);
 END
 ELSE BEGIN
   TextColor(Red);
   Say(3, WhereY, 'Error - information not written to database.', Red);
   Say(3, WhereY + 1, 'File = '+FName, Red);
   Say(3, WhereY + 1, 'Results = '+ListFileResults, Red);
   Say(3, WhereY + 1, 'Press ENTER to continue.', Red);
   TextColor(LightGray);
   READLN;
 END;

END;

  BEGIN
{~} (*   DrawBox(20,5,53,10,LightGray,Blue,15,White,'','','',
       #218, #196, #191, #179, #192, #217, #0, #176); *)
    TextAttr := Colorscheme;
    DrawBox(27,5,51,12,LightGray,Blue,15,15,'','','',
       #218, #196, #191, #179, #192, #217, #176, #177);


    TextColor(White);
    TextBackground(Blue);
    IF Dept = 'C' THEN
      LabelfileName := 'C:\package\cmp\cmpdlist.dbf'
    ELSE IF Dept = 'PS' THEN
      LabelfileName := 'C:\package\pca\pcalist.dbf'
    ELSE
      LabelfileName := 'C:\package\pack\lbllist.dbf';
    LabelFIleResults := '';
    Opendbasefile(LabelfileSpec, LabelFileName, LabelFileResults);
    IF LabelFileResults = 'GOOD' THEN BEGIN
      CurrentNumOfRecs := LabelFileSpec.numofrecs;
      Closedbasefile(LabelfileSpec);
(*    END; *)
    IF CurrentNumOfRecs < MaxLabelListRecs THEN BEGIN
     {If there are less than max number of recs allowed, ask if they
     want to save the label, else TS!}
      Say(3, 2, 'Save label information? [Y/N]', White);
      REPEAT
        Save := Upcase(Readkey);
        If Not(Save In AnswerSet) THEN BEGIN
           Say(3,3,'Please answer ''Y'' or ''N'' ', Yellow);
        END;
      UNTIL Save In AnswerSet;
      Say(3,3,'                          ', LightGray);
      IF Save = 'Y' THEN BEGIN
        UpdateListFile(LabelFileName);
      END;
    END ELSE BEGIN
        SaveArea(30, 6, 47, 10, FullArea);  {15, 8}
        FullMin := WindMin; FullMax := WindMax;
        TextBackground(Blue);
        DrawBox(30,6,44,7,White,Cyan,15,15,' - Note - ',
         #4+' Press ENTER to continue '+#4,'',#218, #196, #191, #179, #192, #217, #0, #177);
        TextBackground(Cyan);
        Center('The label database is full. Some records ', 3, White);
        Center('must be deleted before', 4, White);
        Center('additional labels can be saved.', 5, White);
        Readln;
        RestoreArea(30, 6, FullArea);
        WindMin := FullMin; WindMax := FullMax;

(*      Say(3, 2, 'The label database is full. Some records must be', Yellow);
      Say(3, 3, 'deleted before additional labels can be saved.', Yellow);
      Say(3, 4, 'Press any key to continue.', Yellow);
      Save := Readkey; *)
      END;
    END ELSE BEGIN
        SaveArea(30, 6, 47, 10, FullArea);  {15, 8}
        FullMin := WindMin; FullMax := WindMax;
        TextBackground(Blue);
        DrawBox(30,6,44,7,White,Red,15,15,' **ERROR** ',
         #4+' Press ENTER to continue '+#4,'',#218, #196, #191, #179, #192, #217, #0, #177);
        TextBackground(Red);
        Center('There has been a problem opening the ', 3, White);
        Center('label database.  Result of open operation ', 4, White);
        Center('was '+LabelFileResults, 5, White);
        Readln;
        RestoreArea(30, 6, FullArea);
        WindMin := FullMin; WindMax := FullMax;
        (*result <> good*)
    END;
  END;

PROCEDURE WriteOutput(VAR Line : labeltype; Numoflabels : integer; Filename : string; MEntered : boolean;
  VAR DrugInfo : DrugPtrType);
{**************************************************************************}
{A misguided attempt to write packaging label info to a dBase file.}
{**************************************************************************}
VAR
  outrecordspecs                    : DBFheadertype;
  outrecord                         : recordarraytype;
  results : string;
  PrintString : string;
  ErrorBoxArea : areasavertype;
  EBMin, EBMax : word;


BEGIN

 str(numoflabels, printstring);
 Filename := Trim(Filename);
 opendbasefile(outrecordspecs, Filename,results);
 IF results = 'GOOD' THEN BEGIN
   outrecordspecs.numofrecs := outrecordspecs.numofrecs + 1;
   outrecord[1] := trim(Line[1]);
   outrecord[2] := trim(Line[2]);
   outrecord[3] := trim(Line[3]);
   outrecord[4] := trim(Line[4]);
   outrecord[5] := trim(Line[5]);
   outrecord[6] := printstring;
(*   IF MEntered THEN BEGIN *)
     outrecord[7] := Trim(Manu.CompanyName);
     outrecord[8] := Trim(Manu.LotNumber);
     outrecord[9] := Trim(Manu.MExpDate);
     outrecord[10] := Trim(Manu.Packager);
     outrecord[11] := Trim(DrugInfo^.DiscardDate);
(*   END; *)
   WriteDBFrecord(outrecordspecs, outrecordspecs.numofrecs, outrecord,results);
   closedbasefile(outrecordspecs);
 END
 ELSE BEGIN
   SaveArea(15, 8, 47, 10, ErrorBoxArea);
   EBMin := WindMin; EBMax := WindMax;
   TextBackground(Blue);
   DrawBox(15,8,45,8,White,Red,15,15,'**ERROR**',
   'Press ENTER to continue','Information not written to database.',#218, #196, #191, #179, #192, #217, #0, #177);
   READLN;
   RestoreArea(15, 8, ErrorBoxArea);
   WindMin := EBMin; WindMax := EBMax;
   TextColor(LightGray);

 END;

END;






PROCEDURE FunctionKey(Xplace, Yplace : byte);
  Const
    FunctionKeys : Array[1..10] OF String =
      ('HELP', 'PRINT', 'DRUG', 'LOT NO.', 'MFG. INFO',
       'EXP. DATE','RETRIEVE LABEL','DATE, DEPT, LOT',
       'REPRINT LABEL','EXIT');
  VAR
    cntr : byte;
  BEGIN
    For cntr := 1 to 10 do BEGIN
      GoToXY(Xplace, Yplace);
      TextColor(White);
      Write('F', cntr,': ');
      TextColor(Yellow);
      Write(FunctionKeys[cntr]);
      Inc(YPlace, 2);
    END;
  END;


PROCEDURE InitDrugHandle (VAR Drug : drugtype);
   BEGIN
     With Drug DO BEGIN
       Name := ''; (*  DrugType = Record *)
       Form := ''; (* Name, Form, Conc, Units, Exp, Aux1, Aux2, Glass, *)
       Conc := ''; (* Freeze, PFL, TimeReqd, Dose, ExpDate, Company, *)
       Units := '';(* Lot, Mfg_Exp, Min_Dose, Stand_Dose, Alt_Conc, UnitDose,*)
       Exp := '';  (* Max_Dose, Max_Conc, ChkDiscardDate, DiscardDate,*)
       Aux1 := ''; (* Storage, Warn1, Warn2, DrugInfo, OurLot END;*)
       Aux2 := '';
       Glass := '';
       Freeze := '';
       PFL := '';
       TimeReqd := '';
       Dose := '';
       ExpDate := '';
       Company := '';
       Lot := '';
       Mfg_Exp := '';
       Min_Dose := '';
       Stand_Dose := '';
       Alt_Conc := '';
       UnitDose := '';
       Max_Dose := '';
       Max_Conc := '';
       ChkDiscardDate := '';
       DiscardDate := '';
       Storage := '';
       Warn1 := '';
       Warn2 := '';
       DrugInfo := '';
       OurLot := '';
     END;
   END;

  PROCEDURE InitManu(VAR ManuInfo : Manutype);
     BEGIN
        With ManuInfo DO BEGIN
          CompanyName := '                         ';
          LotNumber :=   '                    ';
          MExpDate :=    '          ';
          Packager :=    '   ';
          ExpInt := 0;
        END;
     END;



BEGIN                 {MAIN PROGRAM}
  (* They have been running several copies of Pac-Manager in
  Packaging, not sure if this is causing problems or not.
  As soon as the program opens, it looks for a file, pm.pid
  if the file exists, it assumes that the program is already
  running, gives an error message.
  If it doesn't exist, it creates it and runs the program.
  Last thing it does before exiting is delete the pm.pid
  file. *)
  FindFirst('C:\package\pm.pid', Archive, DirInfo);
  IF DosError = 0 THEN BEGIN
    Writeln('Pac-Manager is already running in another window.');
    Writeln('Please switch to that window.');
    Writeln('Press ENTER to return to Dos.');
    Readln;
  END ELSE BEGIN
  Assign(PidFile, 'C:\package\pm.pid');
  Rewrite(PidFile);
  GetDate(y,mon,day,dow);
  GetTime(h,min,s,hund);
  Writeln(PidFile, 'Started  ',Mon,'/',Day,'/',Y,' at ',h,':',min,'.',s);
  Close(PidFile);

  GetPrinterConfig('printdrv.dbf', PrinterConfig);
  TextBackground(LightGray);
  TextColor(Black);
  ColorScheme := TextAttr;  
  Quit := FALSE;
  (* NumOfLines := 5; CharsPerLine := 40; *)
  REPEAT
    DrugHandle := NIL;
    New(DrugHandle);
    InitDrugHandle(DrugHandle^);
    InitManu(Manu);
(*    With ReconInfo DO BEGIN
     CheckDiscardDate := FALSE;
     DiscardDate := '00/00/0000';
    END; *)
    For i := 1 to (NumofLines - 1) DO BEGIN
      LabelInfo[i] := Blankline;
    END;
(*    LabelInfo[NumOfLines] := Pad(Config.MCVName, CharsPerLine); *)
    InfoEntered := FALSE;
    Screen(Title, Version, Black, LightGray, Blue);
    TextAttr := Colorscheme;

(*Interactive = 'Y', prompt for department, this is for Inpatient, other areas always use same dept.*)
(*    If Paramstr(1) <> ''
       Then Interactive := 'B'   {B = beginning. N for not, Y for interactive}
     Else *)
    Interactive := 'N';
    GetSetDate({CurrentDept, ShortDate, Prefix, LotLetter, JulianDate,} Config, Interactive);
    LabelInfo[NumOfLines] := Pad(Config.MCVName, CharsPerLine);
    LoopCounter := 0;
    REPEAT
       ManuInfoEntered := FALSE;
       REPEAT
         TextAttr := Colorscheme;
         DrawBox(2,3,21,21,DarkGray,Cyan,White,White,'','',
           '',#218, #196, #191, #179, #192, #217, #176, #177);
         TextBackground(Cyan);
         FunctionKey(2, 2);
         TextBackground(Blue);
         EnterInfo(LabelInfo, InfoEntered, 'Y', DrugEntered, ManuInfoEntered, Retrieved,
            LotUsed, LabelFileName, Recnum, Quit, DrugFile (*, ReconInfo.CheckDiscardDate*));
         IF Not QUIT THEN BEGIN
           IF DrugEntered THEN Inc(LoopCounter);
             IF (InfoEntered) THEN BEGIN
               Center('Edit info? [Y/N]', 8, Yellow);
               Edit := Upcase(readkey);
             END ELSE
               Edit := 'N';
         END ELSE
           Edit := 'N';

       UNTIL Edit = 'N';
       IF NOT Quit THEN BEGIN
         PrintLabels(LabelInfo, 'LPT1');
         If NumberOfPrints <> 0 THEN BEGIN
           Say(2,2,'Print this label again? [Y/N]   ', White);
           MoreLabels := Upcase(Readkey);
         END ELSE
           MoreLabels := 'N';
       END ELSE
         MoreLabels := 'N';

    UNTIL MoreLabels = 'N';

    IF Not QUIT THEN BEGIN

      IF DrugEntered OR LotUsed OR (LoopCounter > 0) THEN BEGIN
        UpdateConfig(Config, {Config.CurrentDept,} DateChanged);
        WriteOutput(LabelInfo, numberofprints, 'package.dbf', DrugEntered, DrugHandle);
        IF DrugEntered THEN BEGIN
          WriteManuInfo(Manu, DrugFile, RecNum, DrugHandle);
        END;
      END;
     IF Not DrugEntered THEN BEGIN
        SaveLabel(LabelInfo, Config.Prefix);
(*        Say(3, 5,'Add to statistics database? [Y/N]        ', White);
        Dbfinfo := Upcase(readkey);
        If Dbfinfo = 'Y' THEN BEGIN
          WriteOutput(LabelInfo, numberofprints, 'freeform.dbf', Manuinfoentered, DrugHandle);
        END; *)
      END;
    END;
    Dispose(DrugHandle);
  UNTIL Quit;
  DeleteFile('C:\package\pm.pid');
  Window(1,1,80,25);
  TextColor(LightGray);
  TextBackground(Black);
  Clrscr;
  END;

END.


(* Talked to Clifton. . .program needs to calculate expiration date
based on plastic or glass container.  Not sure what the best way to do
this is. . .one option

  When you press F3, you get a dosageform menu, change it to look something
  like this
       Injection    - glass
       Injection    - plastic
       Oral liquids - glass
       Oral liquids - plastic
       Oral solids
  Then the only thing different, really, is the database name/abx file
  being passed to the rest of the procedures. . .this should probably be
  the easiest way to deal with the problem.  *)

(*4/15/97:
  1. I added the additional fields from inject/liquids.dbfs to solids.dbf.
  not really any reason not to have them there, makes things easier if all
  the dbfs have the same structure.  The difference has been causing some
  runtime errors [runtime error 100, disk read error] when accessings the
  solids.dbf. An intermittent error.  Need to restructure production dbfs.
  2. Add field "chkdiscarddate, discarddate" to all three dbfs.
  3. These fields will let the packager enter the date reconstituted drugs
  will expire, this info needs to be entered.  Program needs to compare
  discard date, manufacturer's exp. date and the proposed date [today +
  number of days drug is stable]
  At the moment, I am prompting the user for the discard date when he/she
  enters the manufacturer's information.
  4. Add discard date to package.dbf record?
  5. Add a month field to package.dbf record?  Would help keep this file
  smaller. Files could be pruned and copied over to separated dbfs per
  month. . .pack0497.dbf, pack0597.dbf, etc. . .maybe a dbase program to
  do this?*)


(*I have changed readstring to take another color as a parameter, and
printstring to take the TYPE Action {NONE, INIT, LF, FF}. . .will have
to recompile PM soon and make changes. *)

(**NOTES, 8/13/96**)
(* I think I found the dbf shredding error, I was trying to
write the manu info to the database even in the case of freeform
labels. . .if a lot was used, it tried to write to the database,
the rec num was out of range, so it wrote to rec 0. . .corrected
this, also wrote a procedure to write attempts to write to an
out of range rec to a log file.  It seems to work, knock wood.

New features in this version, reprinting labels, a database of
auxiliary lines, printing is handled by Bios calls now, seems
a little bit more robust.  The cursor keys work differently now.
The procedure of entering date, department, lot letter is controlled
by a command line parameter.  If paramstr(1) = '', it doesn't prompt
for info [M4, M6], if paramstr(1) <> '', it does prompt.  The
procedure updates the lot letter field if date or department is changed,
saves the user some keystrokes.

The auxline feature has two different databases, one for compounding,
another for anyone else.

Ctrl+W does a form feed, mainly for my purposes.

Ctrl+G does the Greek letter "mu," for micro grams.

At the moment, the EnterInfo allows lower case, this could change.

Also, the contents of the help screen are now stored in ascii files
to keep the size of the exe down.  There are a bunch of *.hlp files,
explaining how the editing keys work, plus some general help on
the program.*)

(**NOTES, 6/30/95**)
{Should add a field for the minimum (or maximum) measurable dose from
a liquid.  Compare dose entered to this minimum value, if entered dose
is less than minimum, give a message of some kind. . .

Also, should add a field of standard doses, could be delimited, for gent
might be 2; 3; 4; 5; 6; 7.2; 8; 9; 10 --
first check if dose is greater than the minimum, if it is, check to see if
it is in the standard dose field, if it isn't, give a warning.

These values could be blank, then wouldn't check}
