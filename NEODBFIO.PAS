(* {$N+} *)
{$E+}
{$R+}
UNIT NEODBFIO;
{************************************************************}
{* This is John's most recent, as of 6/13/94, version of    *}
{* dbfio.pas.  Renamed for my purposes as neodbfio.pas      *}
{* Not the best name, perhaps, but "a rose by any other..." *}
{*  Procedures for interfacing with dbase DBF files.        *}
{* Written by JCA 1992-93.                                  *}
{*   All problems are not my fault.   Just kidding.         *}
{************************************************************}

INTERFACE
USES DOS;

(* ADR program requires Maxfields 65, Maxbytes 1850 *)
(* this must be reduced for some programs *)

CONST
(*  maxfields        = 100;  { Maximum number of fields allowed per record. } *)
  Maxfields        = 50;   {for nupacman was 50 prior to 1/26/99 }
(* Maxfields = 65;  for labvalue dbf in ADR project *)
(* !!!! maxfields        = 70; *)
(*  maxbytes         = 900; was 900 prior to 1/26/99 *)  { Maximum number of bytes allowed per record. } {was 1400}
MaxBytes = 1850; (* for labvalue dbf in ADR project *)
(*this is the most recent!!!  maxbytes         = 1900;  { Maximum number of bytes allowed per record. } {was 1400} *)
(*  MaxBytes         = 300;  {for nupacman/freeform} *)
(*  MaxBytes         = 700; {for adr!}  *)
(*  MaxFieldLength   = 80;   { Maximum number of bytes allowed in a field. } *)
   MaxFieldLength = 202; {for compound.pas}
(*  MaxFieldLength = 254; {for Mederror.pas} *)
  FieldNameSize    = 10;
  DbfUnitId:STRING='Dbfio Dbase I/O unit by J. C. Atkeson 1994.';

TYPE
  FieldNameType = STRING[FieldNameSize];
  DBFheaderType = RECORD
    FileHandle     : FILE;
    FileOpen       : BOOLEAN; { Flag if file is currently open.}
    FileName       : STRING[80];
    VersionNumber  : INTEGER; { DBASE version which created DBF file.}
    year,month,day : WORD;    { Date DBF file was last changed. }
    NumofRecs      : LONGINT; { Number of records in this DBF file.}
    HeaderLength   : INTEGER; { Header length in bytes. }
    RecordLength   : INTEGER; { Length of each record, in bytes. }
    numoffields    : INTEGER; { Number of fields in each record. }
    FileSize       : LONGINT; { in bytes. }
    Filechanged    : BOOLEAN; { Flag to update header when closing file. }
    flds           : ARRAY[1..maxfields] OF RECORD { Info on data fields. }
      FdName       : FieldNameType;
      FdType       : CHAR;  { 'D'=date, 'N'=numeric, 'C'=char, types. }
      FdLength     : BYTE;  { Field length in bytes. }
      FdDec        : BYTE;  { Decimals after point, if numeric. }
      FdLoc        : WORD;  { Offset from start of record, in bytes. }
    END;
  END;  {End of  DBFheaderType declaration. }

  bytelist     = ARRAY[0..maxbytes] OF BYTE;  { File input buffer. }
  fieldstring  = STRING[MaxFieldLength];   { Just for recordarraytype. }
  recordarraytype = ARRAY[1..maxfields] OF fieldstring; { separate fields into a string array. }

  PROCEDURE OpenDbaseFile(VAR spec : DBFheaderType;NewFileName : STRING;VAR status : STRING);

  PROCEDURE OpenDbaseFile2(VAR spec : DBFheaderType;NewFileName : STRING;VAR status : STRING);

  PROCEDURE NeatenFields(VAR spec:DBFheadertype; VAR outblock:bytelist; VAR status:STRING);

  PROCEDURE ReadDBFrecord(VAR spec:DBFheaderType; recordnum:LONGINT; VAR reclist:recordarraytype; VAR status:STRING);

  PROCEDURE WriteDBFrecord(VAR spec:DBFheaderType; recordnum:LONGINT; VAR reclist:recordarraytype; VAR status:STRING);

  PROCEDURE UpdateDBFHeader(VAR spec : DBFheaderType);

  PROCEDURE CloseDbaseFile(VAR spec : DBFheaderType);

  PROCEDURE ReadOneField(VAR spec:DBFheaderType;recnum:LONGINT;fieldnum:LONGINT;VAR infield:STRING);

  PROCEDURE WriteOneField(VAR spec:DBFheaderType;recnum:LONGINT;fieldnum:LONGINT;outfield:STRING);

  PROCEDURE CopyDbfHeader(VAR source:DBFheaderType; newname:STRING);

  PROCEDURE DspDbfInfo(VAR spec:DBFheaderType);

  PROCEDURE treesearchdbf(VAR sortedfile:DBFheaderType;keyfield:WORD;
   target:STRING;range1,range2:LONGINT;VAR index:INTEGER;VAR found:BOOLEAN);


  PROCEDURE Searchdbf(VAR DbaseFile:DBFheaderType;keyfield:WORD;
   target:STRING;range1,range2:LONGINT;VAR index:LONGINT;VAR found, trimtest:BOOLEAN);

  PROCEDURE ReadBlock(VAR spec:DBFheaderType; recnum:LONGINT; VAR inblock:bytelist);

  PROCEDURE WriteBlock(VAR spec:DBFheaderType; recnum:LONGINT; VAR outblock:bytelist);

  PROCEDURE Dbfsort(VAR spec:DBFheaderType; keyfield:WORD);


{**************************************************************}
{>>>>>>>>>>>>>>>>>>>>}  IMPLEMENTATION  {<<<<<<<<<<<<<<<<<<<<<<}
{**************************************************************}
CONST
  holdblocklimit = 6000;
TYPE
  HoldBlockPtr  = ^HoldBlockType;
  HoldblockType = ARRAY[0..holdblocklimit] OF BYTE;


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


  FUNCTION copystr(VAR rawbytes:bytelist; index,veclength:INTEGER) : STRING;
  {************************************************************}
  {* Copies a block of chars from a bytelist into a string.   *}
  {************************************************************}
  VAR cnt1 : INTEGER; outstr : STRING;
  BEGIN
    MOVE(rawbytes[index],outstr[1],veclength);
    setlength(outstr, veclength); copystr:=outstr;
  END;


  FUNCTION number(VAR rawbytes:bytelist; index,veclength:INTEGER) : LONGINT;
  {***************************************************************}
  {* Copies a block of bytes from a bytelist into binary number. *}
  {***************************************************************}
  VAR converter : RECORD CASE INTEGER OF   { Maps bytes to longint. }
                    0 : (inbytes : bytelist);
                    1 : (tolong : LONGINT);
                  END;
    byteindex   : INTEGER;
  BEGIN
    converter.tolong:=0; veclength:=veclength-1;
    FOR byteindex:=0 TO veclength DO BEGIN
      converter.inbytes[byteindex]:=rawbytes[index+byteindex];
    END;
    number:=converter.tolong;
  END;   { End of number. }

  FUNCTION number2(VAR rawbytes:bytelist; index,veclength:INTEGER) : LONGINT;
  {************************************************************}
  {* Alternative code for number - simpler.                   *}
  VAR tempnum : LONGINT;
  BEGIN
    tempnum:=0;
    MOVE(rawbytes[index],tempnum,veclength);
    number2:=tempnum;
  END;   { End of number2. }

  PROCEDURE OpenDbaseFile2(VAR spec : DBFheaderType;NewFileName : STRING;VAR status : STRING);
  {************************************************************}
  {* Purpose: Open a DBASE file and read in its specs.        *}
  {************************************************************}
  CONST
    TagSize = 32;  { Date, num of fields, etc, is first 32 bytes. }
    FieldInfoSize = 32;  { 32 bytes per field description. }
  VAR
    numpassed,fieldcount,recsize            : INTEGER;
    datasize                                : LONGINT;
    headerterm                              : BYTE;
    inblock                                 : bytelist;
    fileinfo                                : SEARCHREC;
    Fieldptr                                : WORD;
  BEGIN
    WITH Spec DO BEGIN
      FileOpen:=FALSE;
      FileName:=NewFileName;
      FileName:=NewFileName;
    END;
    FINDFIRST(NewFileName,AnyFile,FileInfo);
    IF DOSERROR=0 THEN BEGIN
      ASSIGN(spec.FileHandle,NewFileName);
      RESET(spec.FileHandle,1);     { Read dBASE Header, 1 byte indexing. }
      SEEK(spec.FileHandle,0);  { Start at 0th byte. }
      BLOCKREAD(spec.FileHandle,inblock,TagSize,numpassed);  { Read in the first 32 bytes. }
      spec.VersionNumber:=(inblock[0] AND 7);
      spec.year:=number(inblock, 1, 1);   { Date last updated. }
      spec.month:=number(inblock, 2, 1);
      spec.day:=number(inblock, 3, 1);
      spec.NumofRecs:=number(inblock, 4, 4); { Number of records in this file.}
      spec.HeaderLength:=number(inblock, 8, 2);
      spec.RecordLength:=number(inblock, 10, 2);
      spec.numoffields:=(spec.HeaderLength - 33) DIV FieldInfoSize;
      datasize:=spec.RecordLength * spec.NumofRecs + 1;
      spec.FileSize:=spec.HeaderLength + datasize;
      spec.Filechanged:=FALSE;
      IF spec.VersionNumber <> 3 THEN BEGIN
         status:='NOT DBASE 3';                { Not a DBASE 3 file. }
      END ELSE BEGIN
        Fieldptr:=1; { First field starts at 2nd byte of inblock. }
        RESET(spec.FileHandle,1);    { Read DBASE field specs. }
        FOR fieldcount:=1 TO spec.numoffields DO BEGIN
          {Get field info string.}
          SEEK(spec.FileHandle,TagSize+((fieldcount-1)*FieldInfoSize));
          BLOCKREAD(spec.FileHandle,inblock,FieldInfoSize,numpassed);
          WITH spec.flds[fieldcount] DO BEGIN
            FdName:=copystr(inblock,0,FieldNameSize);
            FdType:=CHR(inblock[11]);
            FdLength:=number(inblock,16,1);
            FdDec:=number(inblock,17,1);
            Fdloc:=Fieldptr;
            INC(Fieldptr,Fdlength);
          END;
        END;
        BLOCKREAD(spec.FileHandle,headerterm,1,numpassed); {Last header byte}
        IF spec.NumofRecs=0 THEN BEGIN   { No records. }
          status:='NO RECORDS';
        END;
        IF headerterm <> 13 THEN  { 13 is carriage return. }
          status:='BAD HEADER'       {Bad Dbf header }
        ELSE
          status:='GOOD';    { No errors. }
      END;
      IF status<>'GOOD' THEN BEGIN
        CLOSE(Spec.FileHandle);
      END ELSE BEGIN
        Spec.FileOpen:=TRUE;
      END;
    END ELSE BEGIN
      status:='FILE NOT FOUND';
    END;
  END;  { End of OpenDbaseFile2. }

  PROCEDURE OpenDbaseFile(VAR spec : DBFheaderType;NewFileName : STRING;VAR status : STRING);
  {************************************************************}
  {* Purpose: Open a DBASE file and read in its specs.        *}
  {************************************************************}
  CONST
    TagSize = 32;  { Date, num of fields, etc, is first 32 bytes. }
    FieldInfoSize = 32;  { 32 bytes per field description. }

    PROCEDURE CleanFieldName(VAR Name:FieldNameType);
    VAR Cnt:BYTE;
    BEGIN
      Cnt:=1;
      WHILE (Cnt<=FieldNameSize) AND (Name[Cnt]<>#00) AND
       (Name[Cnt]<>' ') DO BEGIN
        INC(Cnt,1);
      END;
      WHILE Cnt<=FieldNameSize DO BEGIN
        Name[Cnt]:=' ';
        INC(Cnt,1);
      END;
    END;

  VAR
    numpassed,fieldcount,recsize            : INTEGER;
    datasize                                : LONGINT;
    headerterm                              : BYTE;
    inblock                                 : bytelist;
    fileinfo                                : SEARCHREC;
    Fieldptr                                : WORD;
    holdblock : holdblockptr;
    getsize   : LONGINT;
  BEGIN
    WITH Spec DO BEGIN
      FileOpen:=FALSE;
      FileName:=NewFileName;
      FINDFIRST(NewFileName,AnyFile,FileInfo);
      IF DOSERROR=0 THEN BEGIN
{        FileName:=FileInfo.Name;}
        NEW(holdblock);
        FILLCHAR(holdblock^,SIZEOF(holdblocktype),0);
        {Getsize will get as many bytes as it can without going past EOF.}
        IF FileInfo.size<holdblocklimit+1 THEN BEGIN
          getsize:=fileinfo.size;
        END ELSE BEGIN
          getsize:=holdblocklimit+1;
        END;
        ASSIGN(spec.FileHandle,FileName);
        RESET(spec.FileHandle,1);     { Read dBASE Header, 1 byte indexing. }
        SEEK(spec.FileHandle,0);  { Start at 0th byte. }
        BLOCKREAD(spec.FileHandle,holdblock^,getsize,numpassed);  { Read in the first 32 bytes. }
        MOVE(holdblock^,inblock,tagsize);
        VersionNumber:=(inblock[0] AND 7);
        year:=number(inblock, 1, 1);   { Date last updated. }
        month:=number(inblock, 2, 1);
        day:=number(inblock, 3, 1);
        NumofRecs:=number(inblock, 4, 4); { Number of records in this file.}
        HeaderLength:=number(inblock, 8, 2);
  {Check HeaderLength!!!!}
        RecordLength:=number(inblock, 10, 2);
        numoffields:=(spec.HeaderLength - 33) DIV FieldInfoSize;
        datasize:=RecordLength * NumofRecs + 1;
        FileSize:=HeaderLength + datasize;
        Filechanged:=FALSE;
        IF spec.VersionNumber <> 3 THEN BEGIN
           status:='NOT DBASE 3';                { Not a DBASE 3 file. }
        END ELSE BEGIN
          Fieldptr:=1; { First field starts at 2nd byte of inblock. }
          { Read DBASE field specs. }
          FOR fieldcount:=1 TO spec.numoffields DO BEGIN
            {Get field info string.}
            MOVE(HoldBlock^[TagSize+((fieldcount-1)*FieldInfoSize)],
            Inblock,FieldInfoSize);

  {          FldSpecStart:=TagSize+((fieldcount-1)*FieldInfoSize);}

            WITH flds[fieldcount] DO BEGIN
              FdName:=copystr(inblock,0,FieldNameSize);
              CleanFieldName(FdName);
              FdType:=CHR(inblock[11]);
              FdLength:=number(inblock,16,1);
              FdDec:=number(inblock,17,1);
              Fdloc:=Fieldptr;
              INC(Fieldptr,Fdlength);
            END;
          END;
          {Last header byte}
          MOVE(Holdblock^[spec.HeaderLength-1],headerterm,1);
          IF spec.NumofRecs=0 THEN BEGIN   { No records. }
            status:='NO RECORDS';
          END;
          IF headerterm <> 13 THEN  { 13 is carriage return. }
            status:='BAD HEADER'       {Bad Dbf header }
          ELSE
            status:='GOOD';    { No errors. }
        END;
        DISPOSE(holdblock);
        IF status<>'GOOD' THEN BEGIN
          CLOSE(spec.FileHandle);
        END ELSE BEGIN
          FileOpen:=TRUE;
        END;
      END ELSE BEGIN
        status:='FILE NOT FOUND';
      END;
    END; {End of with Spec do.}
  END;  { End of OpenDbaseFile. }


  PROCEDURE NeatenFields(VAR spec:DBFheadertype; VAR outblock:bytelist; VAR status:STRING);
  {************************************************************}
  {* Check for numeric Dbase fields and right-justify them.   *}
  {* If fields are character, then make them fit their length.*}
  {************************************************************}
   {IF spec.Flds[fieldindex].FdType='D' THEN BEGIN  date type = YYYYMMDD END;
   Right justify numbers; type 'NF'. }
  VAR
    fieldindex,charindex,byteindex,err :INTEGER;
    tempreal      : REAL;
    tempstr       : STRING[100];
    strlength     : BYTE;
  BEGIN
    { Prepare outblock for writing to file. }
    FOR fieldindex:=1 TO spec.numoffields DO BEGIN
      WITH spec.flds[fieldindex] DO BEGIN
      { Numeric field - right justify numbers. }
        tempstr:=copystr(outblock,fdloc,fdlength);
        CASE Fdtype OF
         { Character field -  pad with blanks. }
          'C' : BEGIN
              strlength:=LENGTH(tempstr);
              IF strlength<fdlength THEN BEGIN
                tempstr[0]:=CHR(fdlength);
                FILLCHAR(tempstr[strlength+1],fdlength-strlength,' ');
              END;
            END;
          'N' : BEGIN
            VAL(trim(tempstr),tempreal,err);
            IF FALSE THEN BEGIN {For debugging only.}
              IF err>0 THEN BEGIN
                WRITELN('Right justify error:',err);
                WRITELN('Problem data field is:',fieldindex);
                WRITELN('Problem data record is:');
                READLN;
              END;
            END;
            STR(tempreal:fdlength:fddec,tempstr);
          END;
          'F' : BEGIN  {floating point}
            END;
          'D' : BEGIN  {date field}
            END;
          'L' : BEGIN  {logical (boolean) field}
            END;
          'M' : BEGIN  {memo field}
            END;
          ELSE BEGIN {case else}
          END;
        END;  {End Case statement.}
        MOVE(tempstr[1],outblock[fdloc],fdlength);
      END;
    END;  { End of for loop. }
  END;  {End of NeatenFields.}


  PROCEDURE ReadDBFrecord(VAR spec:DBFheaderType; recordnum:LONGINT; VAR reclist:recordarraytype; VAR status:STRING);
  {************************************************************}
  {*     Read a DBASE record from a pre-opened file.          *}
  {************************************************************}
  VAR
    inblock      : bytelist;
    fieldindex,numpassed : INTEGER;
  BEGIN
    WITH spec DO BEGIN
      IF (recordnum>=1) AND (recordnum<=spec.NumofRecs) THEN BEGIN
        SEEK(FileHandle,HeaderLength+((recordnum-1)*RecordLength));
        BLOCKREAD(FileHandle,inblock,RecordLength,numpassed);
        IF numpassed=RecordLength THEN BEGIN
          { First field starts at 2nd byte of inblock. }
          FOR fieldindex:=1 TO numoffields DO BEGIN
            WITH flds[fieldindex] DO BEGIN
              reclist[fieldindex][0]:=CHR(fdlength);
              MOVE(inblock[Fdloc],reclist[fieldindex][1],fdlength);
            END;
          END;
          status:='GOOD';
        END
        ELSE status:='UNEXPECTED END OF FILE';
      END
      ELSE status:='OUT OF RANGE';
    END;
  END;   { End of ReadDBFrecord. }


  PROCEDURE WriteDBFrecord(VAR spec:DBFheaderType; recordnum:LONGINT; VAR reclist:recordarraytype; VAR status:STRING);
  {************************************************************}
  {*  Purpose: Write record to DBF file                       *}
  {************************************************************}
   {IF spec.Flds[fieldindex].FdType='D' THEN BEGIN  date type = YYYYMMDD END;
   Right justify numbers; type 'NF',  Left justify everything else. }
  VAR
    eofmarker     : CHAR;
    dayofweek     : WORD;
    outblock      : bytelist;  appending : BOOLEAN;
    fieldindex,charindex,strlength,err :INTEGER;
    tempreal      : REAL;
    blanks        : STRING;
    numwritten    : WORD;
  BEGIN
    eofmarker:=#26;   { Set End of File character }
    FILLCHAR(blanks,255,' '); setlength(blanks,255);
    IF recordnum > spec.NumofRecs THEN BEGIN   { Appending rec }
      INC(spec.NumofRecs,1);
      recordnum:=spec.NumofRecs;
      SEEK(spec.FileHandle,spec.HeaderLength+(recordnum*spec.RecordLength));
      BLOCKWRITE(spec.FileHandle,eofmarker,1);  {Add End of file marker }
    END;
    { Prepare outblock for writing to file. }
    outblock[0]:=ORD(' ');  {  Init to 1 space for the status flag  }
    FOR fieldindex:=1 TO spec.numoffields DO BEGIN
      WITH spec.flds[fieldindex] DO BEGIN
      { Numeric field - right justify numbers. }
        IF Fdtype='N' THEN BEGIN
          VAL(trim(reclist[fieldindex]),tempreal,err);
          STR(tempreal:fdlength:fddec,reclist[fieldindex]);
        END ELSE BEGIN
          { Character field -  pad with blanks. }
          IF Fdtype='C' THEN BEGIN
            strlength:=LENGTH(reclist[fieldindex]);
            IF strlength<fdlength THEN BEGIN
              reclist[fieldindex][0]:=CHR(fdlength);
              FILLCHAR(reclist[fieldindex][strlength+1],fdlength-strlength,' ');
            END;
          END;
        END;
        { Put record string into bytes. }
        MOVE(reclist[fieldindex][1],outblock[Fdloc],Fdlength);
      END;
    END;  { End of for loop. }
    {  Actual output of record to file. }
    SEEK(spec.FileHandle,spec.HeaderLength+((recordnum-1)*spec.RecordLength));
    BLOCKWRITE(spec.FileHandle,outblock,spec.RecordLength,numwritten);
    spec.Filechanged:=TRUE;
    IF numwritten=spec.RecordLength THEN status:='GOOD'
    ELSE status:='WRITE PROBLEM';
    GETDATE(spec.year,spec.month,spec.day,dayofweek);  { Update update date.}
    spec.year:=ROUND(FRAC(spec.year/100)*100);
  END;  {End of WriteDBFrecord.}


  PROCEDURE UpdateDBFHeader(VAR spec : DBFheaderType);
  {************************************************************}
  {* Updates DBASE file header to match the most recent       *}
  {* changes made to the records.                             *}
  {************************************************************}
  CONST eofmarker : CHAR = #26;
  VAR 
    datebytes : bytelist;
  BEGIN
    { Update update date. }
    WITH spec DO BEGIN
      datebytes[0]:=year; datebytes[1]:=month; datebytes[2]:=day;
      SEEK(FileHandle,1); BLOCKWRITE(FileHandle,datebytes,3);
      SEEK(FileHandle,4);
      BLOCKWRITE(FileHandle,NumofRecs,4); { Update record count. }
      SEEK(FileHandle,HeaderLength+(NumofRecs*RecordLength));
      BLOCKWRITE(FileHandle,eofmarker,1); { Write end of file marker. }
    END;
  END;


  PROCEDURE CloseDbaseFile(VAR spec : DBFheaderType);
  {************************************************************}
  {* Updates DBASE file header and closes the file.           *}
  {* You MUST close your DBASE file with this procedure or    *}
  {* the DBASE file may get screwed up.                       *}
  {************************************************************}
  BEGIN
    IF Spec.FileOpen THEN BEGIN
      IF spec.Filechanged THEN UpdateDBFHeader(spec);
      CLOSE(spec.FileHandle);
    END;
  END;

  PROCEDURE Getfield(VAR spec:DBFheaderType;VAR inblock:bytelist;fieldnum:LONGINT;VAR infield:STRING);
  {************************************************************}
  {* Extracts a single field from a raw input block.          *}
  {************************************************************}
  BEGIN
    WITH spec DO BEGIN
      WITH flds[fieldnum] DO BEGIN
        MOVE(inblock[fdloc],infield[1],fdlength);
        setlength(infield, fdlength);
      END;
    END;
  END;

  PROCEDURE Putfield(VAR spec:DBFheaderType;VAR outblock:bytelist;fieldnum:LONGINT;VAR outfield:STRING);
  {************************************************************}
  {* Puts a single field into a raw input block.              *}
  {************************************************************}
  VAR tempreal:REAL; err:INTEGER; strlength:BYTE;
  BEGIN
    WITH spec DO BEGIN
      WITH flds[fieldnum] DO BEGIN

      { Numeric field - right justify numbers. }
        IF Fdtype='N' THEN BEGIN
          VAL(trim(outfield),tempreal,err);
          STR(tempreal:fdlength:fddec,outfield);
        END ELSE BEGIN
          { Character field -  pad with blanks. }
          IF Fdtype='C' THEN BEGIN
            strlength:=LENGTH(outfield);
            IF strlength<fdlength THEN BEGIN
              setlength(outfield, fdlength);
              FILLCHAR(outfield[strlength+1],fdlength-strlength,' ');
            END;
          END;
        END;
        MOVE(outfield[1],outblock[fdloc],fdlength);
      END;
    END;
  END;


  PROCEDURE ReadOneField(VAR spec:DBFheaderType;recnum:LONGINT;fieldnum:LONGINT;VAR infield:STRING);
  {************************************************************}
  {* Gets just one field from a record, for speed.            *}
  {************************************************************}
  VAR  recloc:LONGINT;
  BEGIN
    WITH spec DO BEGIN
      recloc:=HeaderLength+((recnum-1)*RecordLength);
      WITH flds[fieldnum] DO BEGIN
        SEEK(FileHandle,recloc+Fdloc);
        BLOCKREAD(FileHandle,infield[1],Fdlength);
        setlength(infield,Fdlength);
      END;
    END;
  END;

  PROCEDURE WriteOneField(VAR spec:DBFheaderType;recnum:LONGINT;fieldnum:LONGINT;outfield:STRING);
  {************************************************************}
  {* Writes just one field from a record, for speed.          *}
  {************************************************************}
  VAR  recloc:LONGINT; tempreal:REAL; err:INTEGER; strlength:BYTE;
  BEGIN
    WITH spec DO BEGIN
      WITH flds[Fieldnum] DO BEGIN
      { Numeric field - right justify numbers. }
        IF Fdtype='N' THEN BEGIN
          VAL(trim(outfield),tempreal,err);
          STR(tempreal:fdlength:fddec,outfield);
        END ELSE BEGIN
          { Character field -  pad with blanks. }
          IF Fdtype='C' THEN BEGIN
            strlength:=LENGTH(outfield);
            IF strlength<fdlength THEN BEGIN
              setlength(outfield,fdlength);
              FILLCHAR(outfield[strlength+1],fdlength-strlength,' ');
            END;
          END;
        END;
        recloc:=HeaderLength+((recnum-1)*RecordLength);
        SEEK(FileHandle,recloc+Fdloc);
        BLOCKWRITE(FileHandle,outfield[1],Fdlength);
      END;
    END;
  END;


  PROCEDURE CopyDbfHeader(VAR source:DBFheaderType; newname:STRING);
  {************************************************************}
  {* Clones the header from one DBF file into a new file with *}
  {* no records.                                              *}
  {************************************************************}
  VAR  numpassed,byteindex : INTEGER;
    holdblock : ARRAY[1..6000] OF BYTE;
    dest : FILE;
  BEGIN
    SEEK(source.FileHandle,0);  { Start at 0th byte. }
    BLOCKREAD(source.FileHandle,holdblock,source.HeaderLength,numpassed);
    IF numpassed=source.HeaderLength THEN BEGIN
      holdblock[source.HeaderLength+1]:=26;  { End of file marker. }
      FOR byteindex:=5 TO 8 DO holdblock[byteindex]:=$00; { Set to 0 records. }
      ASSIGN(dest,newname);
      REWRITE(dest,1);
      SEEK(dest,0);  { Start at 0th byte. }
      BLOCKWRITE(dest,holdblock,source.HeaderLength+1);
      CLOSE(dest);
    END ELSE BEGIN
      WRITELN('Header too big:',source.HeaderLength); READLN;
    END;
  END;


  PROCEDURE searchdbasefile(VAR spec:DBFheaderType; VAR target:STRING);
  {**************************************************************}
  {* Will be just a sequential search through an unsorted file. *}
  {**************************************************************}
  BEGIN
  END;


  PROCEDURE DspDbfInfo(VAR spec:DBFheaderType);
  {************************************************************}
  {*  Display DBASE file header information - for debugging.  *}
  {************************************************************}
  VAR fieldcount,recordcount : INTEGER;
  BEGIN
    WRITELN('DBASE Version         : ',spec.VersionNumber:1);
    WRITELN('Database in use       : ',spec.FileName);
    WRITELN('Number of data records: ',spec.NumofRecs:8);
    WRITELN('Header length         : ',spec.HeaderLength:4);
    WRITELN('Record length         : ',spec.RecordLength:4);
    WRITELN('Number of fields      : ',spec.numoffields:3);
    WRITELN('File size             : ',spec.FileSize:8);
{    READLN;}
    WITH Spec DO BEGIN
      FOR FieldCount:=1 TO numoffields DO BEGIN
        WITH Flds[FieldCount] DO BEGIN
          WRITELN(FieldCount:3,FdName,' ',FdLength,' ',FdDec,' ');
        END;
      END;
    END;
  END;


  PROCEDURE treesearchdbf(VAR sortedfile:DBFheaderType;keyfield:WORD;
   target:STRING;range1,range2:LONGINT;VAR index:INTEGER;VAR found:BOOLEAN);
  {***************************************************************}
  {* Tree searches a DBF file. File MUST be sorted first, by     *}
  {* the field specified by 'keyfield'.  Sort must be ascending. *}
  {***************************************************************}
  VAR
    sample:STRING;

    PROCEDURE tree(minloc,maxloc:LONGINT);
    {**************************************************************}
    BEGIN
      Index:=(maxloc+minloc+1) DIV 2;
      ReadOneField(sortedfile,index,keyfield,sample);
      IF maxloc-minloc>1 THEN BEGIN
        IF target<=sample THEN BEGIN
          tree(minloc,index);
        END ELSE BEGIN
          tree(index,maxloc);
        END; { End if name<sample - else. }
      END;
    END;  { End of tree. }

  BEGIN
    found:=FALSE;
    tree(range1,range2);
    IF target=sample THEN found:=TRUE;
  END;  { End of treesearchdbf.  }

  PROCEDURE Searchdbf(VAR DbaseFile:DBFheaderType;keyfield:WORD;
   target:STRING;range1,range2:LONGINT;VAR index:LONGINT;VAR found, trimtest:BOOLEAN);
  {*******************************************************************}
  {* Sequentially searches a DBF file.                               *}
  {*******************************************************************}
  VAR
    sample:STRING;

  BEGIN
    found:=FALSE; Index:=range1;
    WHILE NOT(Found) AND (index<=range2) DO BEGIN
      ReadOneField(DbaseFile,index,keyfield,sample);
      IF TrimTest THEN BEGIN
         Target := Trim(Target);
         Sample := Trim(sample);
      END;
      IF target=sample THEN BEGIN
        found:=TRUE;
      END ELSE BEGIN
        INC(Index,1);
      END;
    END;
  END;  { End of Searchdbf.  }

  PROCEDURE ReadBlock(VAR spec:DBFheaderType; recnum:LONGINT; VAR inblock:bytelist);
  {************************************************************}
  VAR numpassed : WORD;
  BEGIN
    SEEK(spec.FileHandle,spec.HeaderLength+((recnum-1)*spec.RecordLength));
    BLOCKREAD(spec.FileHandle,inblock,spec.RecordLength,numpassed);
  END;

  PROCEDURE WriteBlock(VAR spec:DBFheaderType; recnum:LONGINT; VAR outblock:bytelist);
  {************************************************************}
  VAR numpassed : WORD;
  BEGIN
    WITH Spec DO BEGIN
      SEEK(FileHandle,HeaderLength+((recnum-1)*RecordLength));
      BLOCKWRITE(FileHandle,outblock,RecordLength,numpassed);
      FileChanged:=TRUE;
    END;
  END;

(*  FUNCTION Extract(VAR spec:DBFheaderType;FieldNum:BYTE; VAR inblock:bytelist):DOUBLE;
  {************************************************************}
  {* Extract one field value from a block of bytes.           *}
  {************************************************************}
  VAR TempNum:DOUBLE; TempStr:fieldstring; err:INTEGER;
  BEGIN
    WITH Spec DO BEGIN
      WITH Flds[FieldNum] DO BEGIN
        MOVE(Inblock[FdLoc],TempStr[1],FdLength);
        TempStr[0]:=CHR(FdLength);
        VAL(TempStr,TempNum,err);
        Extract:=TempNum;
      END;
    END;
  END;  *)

  PROCEDURE Dbfsort(VAR spec:DBFheaderType; keyfield:WORD);
  {************************************************************}
  {* Dbase sorting. - Not tested.  Preforms QuickSort.        *}
  {* Should work as is, but slowly.                           *}
  {************************************************************}
  VAR
    inblock : bytelist;
{    temprecs: recordarraytype;}
{    results : STRING;}
    err     : INTEGER;
    hour,minute,sec,sec100 : WORD;

(*
    FUNCTION fieldval(recnum:LONGINT):STRING;
    {************************************************************}
    BEGIN
      ReadDBFrecord(spec,recnum,temprecs,results);
      fieldval:=temprecs[keyfield];
    END;

    FUNCTION onefield(recnum:LONGINT):STRING;
    {************************************************************}
    VAR  recloc:LONGINT; outstr:STRING;
    BEGIN
      WITH spec DO BEGIN
        recloc:=HeaderLength+((recnum-1)*RecordLength);
        WITH flds[keyfield] DO BEGIN
          SEEK(FileHandle,recloc+Fdloc);
          BLOCKREAD(FileHandle,outstr[1],Fdlength);
          outstr[0]:=CHR(Fdlength);
          onefield:=outstr;
        END;
      END;
    END;
*)
    PROCEDURE swapit(firstloc,secondloc:LONGINT);
    {************************************************************}
    VAR firstval,secondval:bytelist;
    BEGIN
      ReadBlock(spec,firstloc,firstval);
      ReadBlock(spec,secondloc,secondval);
      WriteBlock(spec,firstloc,secondval);
      WriteBlock(spec,secondloc,firstval);
    END;

    PROCEDURE partition(low,high:LONGINT; VAR pivotloc:LONGINT);
    {************************************************************}
    {* Sorts a section of the point array into <, > pivotval.   *}
    {************************************************************}
    VAR pivotval,testval:STRING; dataindex,lastsmall:LONGINT;
    BEGIN
      swapit(low,(low+high) DIV 2);  { Swap first record with middle. }
      ReadOneField(spec,low,keyfield,pivotval);
      lastsmall:=low;
      FOR dataindex:=low+1 TO high DO BEGIN
        ReadOneField(spec,dataindex,keyfield,testval);
        IF testval<pivotval THEN BEGIN
          lastsmall:=lastsmall+1;
          swapit(lastsmall,dataindex);
        END;
      END;
      swapit(low,lastsmall);  { Restore pivot record to middle. }
      pivotloc:=lastsmall;
    END;

    PROCEDURE Sort(low,high:LONGINT);
    {************************************************************}
    {* Non-recursive version of sort.                           *}
    {* Sorts the given range (low to high, inclusive) of the    *}
    {* vector.                                                  *}
    {************************************************************}
    CONST
      StackMax = 10000;
    TYPE
      StackType = ARRAY[1..StackMax] OF LONGINT;
    VAR Pivotloc,Stacklevel :LONGINT;
      PivotStack : ^StackType;
      Last   : LONGINT; {Last }
    BEGIN
      NEW(PivotStack);
      FILLCHAR(pivotstack^,SIZEOF(StackType),0);
      Last:=High;
      Stacklevel:=1;
      PivotStack^[Stacklevel]:=high+1;
      REPEAT
        WHILE Low<High DO BEGIN {Dive into zone.}
          INC(Stacklevel,1);
          Partition(Low,High,PivotLoc);
          PivotStack^[Stacklevel]:=Pivotloc;
          High:=Pivotloc-1;
        END;
        Low:=PivotStack^[Stacklevel]+1;
        DEC(Stacklevel,1);                 { Rise out of zone. }
        High:=PivotStack^[Stacklevel]-1;
      UNTIL Low>=Last;
      DISPOSE(PivotStack);
    END;
(*
    PROCEDURE Sort(low,high:LONGINT);
    {************************************************************}
    {* Traditional recursive version of sort sub.               *}
    {************************************************************}
    VAR pivotloc:LONGINT;
    BEGIN
      IF low<high THEN BEGIN
        partition(low,high,pivotloc);
        Sort(low,pivotloc-1);
        Sort(pivotloc+1,high);
      END;
    END;
*)
  BEGIN
    GETTIME(hour,minute,sec,sec100);
    Sort(1,spec.NumofRecs);
    spec.Filechanged:=TRUE;
{
    WRITELN('sort time:');
    WRITELN(hour,' ',minute,' ',sec,' ',sec100);
    GETTIME(hour,minute,sec,sec100);
    WRITELN(hour,' ',minute,' ',sec,' ',sec100);
    WRITE('Press enter:'); READLN;
}
  END;  {End of Dbfsort. }





END.  { End of Dbfio unit. }

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


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

