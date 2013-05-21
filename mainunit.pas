unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, UniqueInstanceRaw,
  ABUtil2, NEODBFIO, mouser, Print, Dos, Crt, PacManager;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}


procedure TMainForm.FormCreate(Sender: TObject);
begin                 {MAIN PROGRAM}
  (* They have been running several copies of Pac-Manager in
  Packaging, not sure if this is causing problems or not.
  As soon as the program opens, it looks for a file, pm.pid
  if the file exists, it assumes that the program is already
  running, gives an error message.
  If it doesn't exist, it creates it and runs the program.
  Last thing it does before exiting is delete the pm.pid
  file. *)

  { FindFirst('C:\package\pm.pid', Archive, DirInfo);
  IF DosError = 0 THEN BEGIN}
  if InstanceRunning then begin
    Writeln('Pac-Manager is already running in another window.');
    Writeln('Please switch to that window.');
    Writeln('Press ENTER to return to Dos.');
    Readln;
  END ELSE BEGIN
  {AssignFile(PidFile, 'C:\package\pm.pid');
  Rewrite(PidFile);
  GetDate(y,mon,day,dow);
  GetTime(h,min,s,hund);
  Writeln(PidFile, 'Started  ',Mon,'/',Day,'/',Y,' at ',h,':',min,'.',s);
  CloseFile(PidFile);
  }
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
end;

end.

