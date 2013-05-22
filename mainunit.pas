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

procedure Init;
begin
  GetPrinterConfig('printdrv.dbf', PrinterConfig);
  TextBackground(LightGray);
  TextColor(Black);
  ColorScheme := TextAttr;
end;

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
  if InstanceRunning then begin
    Writeln('Pac-Manager is already running in another window.');
    Writeln('Please switch to that window.');
    Writeln('Press ENTER to return to Dos.');
    Readln;
  END ELSE BEGIN
    GetPrinterConfig('printdrv.dbf', PrinterConfig);
    TextBackground(LightGray);
    TextColor(Black);
    ColorScheme := TextAttr;
    Quit := FALSE;
    REPEAT
      DrugHandle := NIL;
      New(DrugHandle);
      InitDrugHandle(DrugHandle^);
      InitManu(Manu);
      For i := 1 to (NumofLines - 1) DO BEGIN
        LabelInfo[i] := Blankline;
      END;
      InfoEntered := FALSE;
      Screen(Title, Version, Black, LightGray, Blue);
      TextAttr := Colorscheme;
      Interactive := 'N';
      GetSetDate(Config, Interactive);
      LabelInfo[NumOfLines] := Pad(Config.MCVName, CharsPerLine);
      LoopCounter := 0;
      REPEAT
         ManuInfoEntered := FALSE;
         REPEAT
           TextAttr := Colorscheme;
           DrawBox(2,3,21,21,DarkGray,Cyan,White,White,'','', '',#218, #196, #191, #179, #192, #217, #176, #177);
           TextBackground(Cyan);
           FunctionKey(2, 2);
           TextBackground(Blue);
           EnterInfo(LabelInfo, InfoEntered, 'Y', DrugEntered, ManuInfoEntered, Retrieved,
              LotUsed, LabelFileName, Recnum, Quit, DrugFile);
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
          UpdateConfig(Config, DateChanged);
          WriteOutput(LabelInfo, numberofprints, 'package.dbf', DrugEntered, DrugHandle);
          IF DrugEntered THEN BEGIN
            WriteManuInfo(Manu, DrugFile, RecNum, DrugHandle);
          END;
        END;
        IF Not DrugEntered THEN BEGIN
          SaveLabel(LabelInfo, Config.Prefix);
        END;
      END;
      Dispose(DrugHandle);
    UNTIL Quit;
    Window(1,1,80,25);
    TextColor(LightGray);
    TextBackground(Black);
    Clrscr;
  END;
end;

end.

