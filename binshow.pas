{************************************************************************}
{* Program:     BinShow                                                 *}
{************************************************************************}
{* F”rfattare:  Peter Karlsson                                          *}
{* Datum:       95-09-02                                                *}
{* Version:     1.20                                                    *}
{************************************************************************}
{* Moduler:     binshow.pas                                             *}
{*              colors.inc                                              *}
{*              mkcrt.pas                                               *}
{************************************************************************}
{************************************************************************}
{* Modul:       binshow.pas                                             *}
{************************************************************************}
{* Inneh†ll:    BinShows huvudprogram                                   *}
{************************************************************************}
{* Funktion:    Visar en fil i bin„r-, oktal-, decimal- och hexadecimal-*}
{*              format                                                  *}
{************************************************************************}
{* Rutiner:     UpStr           Octal           HelpScreen              *}
{*              Hex             Decimal         ShowTime                *}
{*              Hexlong         ASCII           Readkey6                *}
{*              Binary          Codepage        ShowFile                *}
{*              GetCursor       SetCursor                               *}
{************************************************************************}
{* Revision:                                                            *}
{* v1.0 BETA (p†b”rjat 08-17)                                           *}
{*       - 1994-09-03 - f”rsta publika betan                            *}
{* v1.1 BETA (p†b”rjat 09-10)                                           *}
{*       - 1994-09-13 - fixade x-koordinat i ChrOut                     *}
{*                      valfritt sk„rml„ge (s† l„nge det „r 80xnn)      *}
{* v1.15 - 1995-01-28 - sl„cker mark”ren, anv„nder Martin Kal‚ns MKCRT, *}
{*                      klarar fler sk„rml„gen                          *}
{* v1.16 - 1995-08-20 - visar filnamnet, och kan v„xla den visningen    *}
{*                      med mellanslag                                  *}
{* v1.20 - 1995-09-02 - kan visa tv† filer samtidigt, /REF              *}
{* v1.21 - 1995-09-22 - visar b†da filnamnen p† titelraden              *}
{************************************************************************}

{$M 49152,0,655360}
{$G+}

Program BinShow;

Uses Dos, MkCrt;

Const
  Hidden     = $2FFF;

  CursorUp   = Chr(72);
  CursorDown = Chr(80);
  PageUp     = Chr(73);
  PageDown   = Chr(81);
  ControlPgUp= Chr(132);
  ControlPgDn= Chr(118);
  Home       = Chr(71);
  KEnd       = Chr(79);

  Esc        = Chr(27);
  TAB        = Chr(9);
  Enter      = Chr(13);
  Space      = Chr(32);

  HexChars: Array[0..$F] of Char = '0123456789ABCDEF';
  Special: Array[0..32] of String[3] =
              ('NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL',
               'BS ', 'HT ', 'LF ', 'VT ', 'FF ', 'CR ', 'SO ', 'SI ',
               'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB',
               'CAN', 'EM ', 'SUB', 'ESC', 'FS ', 'GS ', 'RS ', 'US ',
               'SPA');
  version    = '1.21';
  { File Mode }
  fmReadOnly  = $00;  { Only one of these should be used }
  fmWriteOnly = $01;
  fmReadWrite = $02;

  fmDenyAll   = $10;  { together with only one of these  }
  fmDenyWrite = $20;
  fmDenyRead  = $30;
  fmDenyNone  = $40;

  fmNoInherit = $70;  { Set for "No inheritance"
      0= Child program of the current program can access file handle.
      1= Current program can access file handle only }

{$I COLOR.INC}

Var
  BinDigits: Array[0..2] of char;

Type
  buftype = array[0..32767] of byte;

{************************************************************************}
{* Rutin:       GetCursor                                               *}
{************************************************************************}
{* Inneh†ll:    Returnerar aktuell status f”r mark”ren                  *}
{* Copyright:   Martin Kal‚n. Totally free to use and/or modify for your*}
{*              own programs.                                           *}
{* Definition:  Function GetCursor: Word; Assembler;                    *}
{************************************************************************}

Function GetCursor: Word; Assembler;
Asm
  mov ah,3
  mov bh,0
{ mov bh,TextPage }
  int 10h
  mov ah,ch
  mov al,cl
End; {GetCursor}

{************************************************************************}
{* Rutin:       SetCursor                                               *}
{************************************************************************}
{* Inneh†ll:    S„tter status f”r mark”ren.                             *}
{* Copyright:   Martin Kal‚n. Totally free to use and/or modify for your*}
{*              own programs.                                           *}
{* Definition:  Procedure SetCursor(State: Word); Assembler;            *}
{************************************************************************}

Procedure SetCursor(State:Word); Assembler;
Asm
  mov ah,1
  mov cx,State
  int 10h
End; {SetCursor}

{************************************************************************}
{* Rutin:       ClrScr                                                  *}
{************************************************************************}
{* Inneh†ll:    Rensar sk„rmen via BIOS-skrollningsfunktion.            *}
{* Copright:    Martin Kal‚n                                            *}
{* Definition:  Procedure ClrScr(attr:byte); Assembler;                 *}
{************************************************************************}

Procedure ClrScr(attr:byte); Assembler;
Asm
  mov ax,$40  {BIOS segment}
  mov es,ax
  mov ah,6    {Scroll Window Up}
  xor al,al   {Clear entire window}
  mov bh,attr {Display attributes for blank lines}
  xor cx,cx
  mov dh,es:ScreenLines
  inc dh
  mov dl,es:ScreenCols
  int 10h     {BIOS function call, video services}
End; {ClrScr}

{************************************************************************}
{* Rutin:       PaintScr                                                *}
{************************************************************************}
{* Inneh†ll:    Som ClrScre, men rensar en specifik area p† sk„rmen     *}
{* Copright:    Martin Kal‚n                                            *}
{* Definition:  Procedure PaintScr(x1,y1,x2,y2,attr:byte); Assembler;   *}
{************************************************************************}

Procedure PaintScr(x1,y1,x2,y2,attr:byte); Assembler;
Asm
  mov ah,6    {Scroll Window Up}
  xor al,al   {Clear entire window}
  mov bh,attr {Display attributes for blank lines}
  mov cl,x1
  mov ch,y1
  mov dl,x2
  mov dh,y2
  int 10h     {BIOS function call, video services}
End; {PaintScr}

{************************************************************************}
{* Rutin:       UpStr                                                   *}
{************************************************************************}
{* Inneh†ll:    Kapitaliserar en str„ng                                 *}
{* Definition:  Function UpStr(s:string):String;                        *}
{************************************************************************}

Function UpStr(s:string):String;
var
  i: byte;
begin
  for i:=1 to length(s) do
    s[i]:=UpCase(s[i]);
  UpStr:=s;
end;

{************************************************************************}
{* Rutin:       Hex                                                     *}
{************************************************************************}
{* Inneh†ll:    Framst„ller en hexadecimalstr„ng                        *}
{* Definition:  Function Hex(X:Byte):String;                            *}
{************************************************************************}

function Hex(X: Byte): String;
begin
  Hex := hexChars[X shr 4]+ hexChars[X and 15];
end;

{************************************************************************}
{* Rutin:       Hexlong                                                 *}
{************************************************************************}
{* Inneh†ll:    Framst„ller en hexadecimalstr„ng                        *}
{* Definition:  Function Hexlong(w:Word):String;                        *}
{************************************************************************}

Function Hexlong(w: Longint): String;
Var
  Temp: string;
  i:    byte;
begin
  Temp := '';
  for i:=7 downto 0 do
  begin
    Temp := HexChars[w and 15] + Temp;
    w := w shr 4;
  end;
  HexLong := Temp;
end;

{************************************************************************}
{* Rutin:       Binary                                                  *}
{************************************************************************}
{* Inneh†ll:    Framst„ller en bin„rstr„ng                              *}
{* Definition:  Function Binary(w:byte):String;                         *}
{************************************************************************}

Function Binary(w: byte): String;
Var
  Temp: string;
  i:    byte;
begin
  Temp := '';
  for i:=7 downto 0 do
  begin
    Temp := BinDigits[w and 1] + Temp; {Chr((w and 1)+48) + Temp;}
    w := w shr 1;
  end;
  Binary := BinDigits[2] + Temp;
end;

{************************************************************************}
{* Rutin:       Octal                                                   *}
{************************************************************************}
{* Inneh†ll:    Framst„ller en oktalstr„ng                              *}
{* Definition:  Function Octal(w:byte):String;                          *}
{************************************************************************}

Function Octal(w: byte): String;
Var
  Temp: string;
  i:    byte;
begin
  Temp := '';
  for i:=2 downto 0 do
  begin
    Temp := Chr((w and 7)+48) + Temp;
    w := w shr 3;
  end;
  Octal := Temp;
end;

{************************************************************************}
{* Rutin:       Decimal                                                 *}
{************************************************************************}
{* Inneh†ll:    Framst„ller en decimalstr„ng                            *}
{* Definition:  Function Decimal(w:word;l: byte):String;                *}
{************************************************************************}

Function Decimal(w: word;l: byte): String;
Var
  Temp: string;
  i:    byte;
begin
  Temp := '';
  for i:=l downto 0 do
  begin
    Temp := Chr((w mod 10)+48) + Temp;
    w := w div 10;
  end;
  Decimal := Temp;
end;

{************************************************************************}
{* Rutin:       ASCII                                                   *}
{************************************************************************}
{* Inneh†ll:    Namnger ASCII-specialkoder                              *}
{* Definition:  Function ASCII(w:byte):String;                          *}
{************************************************************************}

Function ASCII(w:byte):String;
begin
  case w of
    0..32: ASCII := special[w];
    127:   ASCII := 'DEL';
    else   ASCII := '   ';
  end;
end;

{************************************************************************}
{* Rutin:       Codepage                                                *}
{************************************************************************}
{* Inneh†ll:    L„ser aktuell teckentabell                              *}
{* Definition:  Function CodePage: Word;                                *}
{************************************************************************}

Function CodePage: Word;
Var
  Ver   : Word;
  Reg   : Registers;
begin
   Ver:=DosVersion;
   if (Lo(Ver) > 3) or ((Lo(Ver) = 3) and (Hi(Ver) >= 30)) then begin
     Reg.AX:=$6601;
     MsDos(Reg);
     CodePage:=Reg.BX;
   end
   else begin
     CodePage:=437;
   end;
end;

{************************************************************************}
{* Rutin:       HelpScreen                                              *}
{************************************************************************}
{* Inneh†ll:    Visar hj„lpsk„rmen                                      *}
{* Definition:  Function HelpScreen;                                    *}
{************************************************************************}

Procedure HelpScreen;
begin
  Writeln('Shows files in binary format v',version);
  Writeln('(c) 1994, 1995 Peter Karlsson');
  Writeln;
  Writeln('  BINSHOW [drive:][path]filename [[drive.][path]filename2]');
  Writeln('  BINSHOW /REF');
  Writeln;
  Writeln('[drive:][path]filename    The file to display');
  Writeln('[drive:][path]filename2   The second file to display, if any');
  Writeln('/REF                      Show a reference table');
  Halt;
end;

{************************************************************************}
{* Rutin:       ShowTime                                                *}
{************************************************************************}
{* Inneh†ll:    Visar klockan i h”gra sk„rmh”rnet                       *}
{* Definition:  Procedure ShowTime;                                     *}
{************************************************************************}

var
  h, m, s, d: word;
Procedure ShowTime;
begin
  Gettime(h,m,s,d);
  TextXYC(ScreenCols-9,0,Decimal(h,1)+'.'+Decimal(m,1)+'.'+Decimal(s,1),BGray+Black);
end;

{************************************************************************}
{* Rutin:       ReadKey6                                                *}
{************************************************************************}
{* Inneh†ll:    Implementerar ett ReadKey via DOS-funktion 6.           *}
{* Copyright:   Thomas Mainka. Ur filen ADANSI.PAS                      *}
{* Definition:  Procedure ReadKey6(Var c:Char;Var P:Boolean)            *
{* Parametrar:  c       - Ut tecken                                     *}
{*              P       - Key not Pressed-information                   *}
{************************************************************************}

Procedure ReadKey6(Var c:Char;Var P:Boolean);
var
  Reg:  Registers;
begin
   Reg.AX:=$0600;
   Reg.DL:=$ff;
   MsDos(Reg);
   c:=char(Reg.AL);
   P:=(Reg.Flags and FZero)=FZero;
end;

{************************************************************************}
{* Rutin:       ShowFile                                                *}
{************************************************************************}
{* Inneh†ll:    Visar filen                                             *}
{* Definition:  Procedure Showfile;                                     *}
{************************************************************************}

Procedure ShowFile;
var
  fil, fil2:                                                    file;
  buf, buf2:                                                    ^buftype;
  curlow, curmax, filemax, filemax2, newlow, displayline:       longint;
  ReadBytes, i, codepag, oldattr, oldcurs:                      word;
  exit, truekey, key, twofiles, displaylabels, filenameshow:    boolean;
  c, c2:                                                        char;
  j, color:                                                     byte;
  fname1, fname2, showname1, showname2:                         string;
  fdir:                                                         DirStr;
  fnamestr:                                                     NameStr;
  fext:                                                         ExtStr;
begin
  Filenameshow := true;
  InitSeg;
  OldAttr := TextAttr;
  OldCurs := GetCursor;
  If ScreenCols < 80 then begin
    Writeln(#7'A minimum of 80 screen columns are required.');
    Halt;
  end;
  BinDigits := '01%';
  {$I-}
  If not (UpStr(ParamStr(1)) = '/REF') then begin
    FileMode := fmReadOnly + fmDenyNone;
    If (Pos('*', ParamStr(1)) <> 0) or (Pos('?', ParamStr(1)) <> 0) then begin
      Writeln('Wildcards not allowed: ', ParamStr(1));
      Halt(1);
    end else If (Pos('*', ParamStr(2)) <> 0) or (Pos('?', ParamStr(2)) <> 0) then begin
      Writeln('Wildcards not allowed: ', ParamStr(2));
      Halt(2);
    end;
    Writeln('Loading ', FExpand(ParamStr(1)));
    Assign(fil, ParamStr(1));
    Reset(fil, 1);
    If IOResult <> 0 then begin
      Writeln('Can''t load ', FExpand(ParamStr(1)));
      Halt(1);
    end; {If}
    New(buf);
    If ParamCount=2 then begin
      Writeln('Loading ', FExpand(ParamStr(2)));
      Assign(fil2, ParamStr(2));
      Reset(fil2, 1);
      If IOResult <> 0 then begin
        Writeln('Can''t load ', FExpand(ParamStr(2)));
        Halt(1);
      end; {If}
      twofiles := true;
      New(buf2);
    end else
      twofiles := false;
  end;
  {$I+}

  If UpStr(ParamStr(1)) = '/REF' then begin
    fname1 := 'Reference table';
    twofiles := false;
  end else begin
    fname1 := FExpand(ParamStr(1));
    fname2 := FExpand(ParamStr(2));
    FSplit(fname1, fdir, fnamestr, fext);
    showname1 := fnamestr + fext;
    FSplit(fname2, fdir, fnamestr, fext);
    showname2 := fnamestr + fext;
  end;

  displaylabels := (not twofiles) or (ScreenCols > 110);

  ClrScr(Gray+BBlue);
  SetCursor(Hidden);
  PaintScr(0, 0, ScreenCols-1, 0, BGray+Black);
  If twofiles then begin
    TextXY(0,  0, ' File 1: ' + showname1);
    TextXY(45, 0, ' File 2: ' + showname2);
  end else
    TextXY(0, 0, ' File: ' + fname1);

  PaintScr(0, ScreenLines, ScreenCols-1, ScreenLines, BGray+Black);
  TextXY((ScreenCols div 2)-38, ScreenLines,
         'Keys: <Arrows, PageUp/Down, Ctrl-PageUp/Down, Tab, Home, End, Spacebar, Esc>');

  Codepag := Codepage;

  If displaylabels then begin
    TextXY(ScreenCols-31, 4, 'BinShow v'+version);
    TextXY(ScreenCols-31, 6, '(c) 1994, 1995 Peter Karlsson');
    TextXY(ScreenCols-31, 8, 'Softwolves Software (TM)');
    TextXY(ScreenCols-31, 9, 'c/o Peter Karlsson');
    if (Codepage = 437) or (Codepage = 850) or (Codepage = 865) then begin
      TextXY(ScreenCols-31, 10, 'V„rnsta, Ullers„ter');
      TextXY(ScreenCols-31, 11, 'S-718 92  FR™VI');
    end else begin
      TextXY(ScreenCols-31, 10, 'Vaernsta, Ullersaeter');
      TextXY(ScreenCols-31, 11, 'S-718 92  FROEVI');
    end;
    TextXY(ScreenCols-31, 12, 'Sweden');
  end;

  exit := false;

  curlow        := 0;           { Inl„st l„gsta adress }
  curmax        := 0;           { Inl„st h”gsta adress }
  filemax       := 2147483647;  { Filslut }
  filemax2      := filemax;     { Filslut, fil 2 }
  displayline   := 0;           { Rad som ska visas; adress = disp.line+curlow }

  { L„s in b”rjan av filen }

  If fname1 = 'Reference table' then begin
    New(buf);
    filemax := 256;
    curmax := 65535;
    for i := 0 to 255 do buf^[i] := i;
  end else begin
    Seek(fil, 0);
    BlockRead(fil, buf^, SizeOf(buftype), ReadBytes);
    curmax := curlow + SizeOf(buftype) - 1; { H”gsta inl„sta adress }
    if ReadBytes <> SizeOf(buftype) then begin
      filemax := ReadBytes;       { Filslutet har hittats }
      If displaylabels then
        TextXY(Screencols-31, 14, 'File length: $' + HexLong(filemax))
      else If filenameshow then
        TextXY(21, 0, '<$' + HexLong(filemax));
    end;
  end;

  If twofiles then begin
    Seek(fil2, 0);
    BlockRead(fil2, buf2^, SizeOf(buftype), ReadBytes);
    If ReadBytes <> SizeOf(buftype) then begin
      filemax2 := ReadBytes;    { Filslutet har hittats }
      If displaylabels then
        TextXY(ScreenCols-31, 15, 'File 2 length: $'+Hexlong(filemax2))
      else If filenameshow then
        TextXY(35, 0, '$' + HexLong(filemax2) + '>');
    end;
  end;

  Repeat
    { Om vi f”rs”ker visa efter filslut }
    If not twofiles then begin
      While (curlow + displayline + ScreenLines - 2 >= filemax) do
        Dec(displayline);
    end else begin
      While (curlow + displayline + ScreenLines - 2 >= filemax) and
            (curlow + displayline + ScreenLines - 2 >= filemax2) do
        Dec(displayline);
    end; { If }

    { Om vi f”rs”ker visa f”re filb”rjan }
    While (displayline + curlow) < 0 do
      Inc(displayline);

    { Om vi f”rs”ker visa efter buffertens slut l„ser vi in en ny
      buffert och st„ller om visarpekaren                         }
    While (curlow + displayline + ScreenLines - 1) >= curmax do
    begin
      If displaylabels then
        TextXYC(ScreenCols - 31, 16, 'Searching', BBlue + Gray + Flash);
      newlow := curlow + SizeOf(buftype) - 1024;
      displayline := displayline - newlow + curlow;

      If newlow <= filemax then begin   { F”rs”k inte l„sa efter EOF }
        Seek(fil, newlow);
        BlockRead(fil, buf^, SizeOf(buftype), ReadBytes);
        If ReadBytes < SizeOf(buftype) then begin
          filemax := ReadBytes + newlow;  { Filslutet har hittats }
          If displaylabels then
            TextXY(ScreenCols - 31, 14, 'File length: $' + Hexlong(filemax))
          else If filenameshow then
            TextXY(21, 0, '<$' + HexLong(filemax));
        end; { If }
      end;

      If twofiles and (newlow <= filemax2) then begin
        Seek(fil2, newlow);
        BlockRead(fil2, buf2^, SizeOf(buftype), ReadBytes);
        If ReadBytes < SizeOf(buftype) then begin
          filemax2 := ReadBytes + newlow;  { Filslutet har hittats }
          If displaylabels then
            TextXY(ScreenCols - 31, 15, 'File 2 length: $' + Hexlong(filemax2))
          else If filenameshow then
            TextXY(35, 0, '$' + HexLong(filemax2) + '>');
        end;
      end; { If }

      curlow := newlow;
      curmax := curlow + SizeOf(buftype);

    end; { While }

    { Om vi f”rs”ker visa f”re buffertens b”rjan + 256 bytes l„ser vi
      in en ny buffer och st„ller om visarpekaren                     }
    While (displayline < 256) and (curlow > 0) do
    begin
      If displaylabels then TextXYC(ScreenCols - 31, 16, 'Searching', $97);
      newlow := curlow - SizeOf(buftype) + 1024;
      displayline := displayline + SizeOf(buftype) - 1024;

      Seek(fil, newlow);
      BlockRead(fil, buf^, SizeOf(buftype), ReadBytes);

      If twofiles then begin
        Seek(fil2, newlow);
        BlockRead(fil2, buf2^, SizeOf(buftype), ReadBytes);
      end;

      curlow := newlow;
      curmax := curlow + SizeOf(buftype);
    end; { While }

    If displaylabels then TextXY(ScreenCols - 31, 16, '         ');
                            { Rensa ev. Loading-meddelande }

    { Visa data }


    for i := displayline to displayline + ScreenLines - 2 do begin
      color := Gray + BBlue;
      If twofiles then
        If (buf^[i] <> buf2^[i]) and (i+curlow < filemax) and
           (i + curlow < filemax2) then
          color := Gray + BRed;
      If i + curlow < filemax then
        TextXYC(1, i - displayline + 1, '$' + HexLong(i+curlow) + ': ' +
                                               Binary(buf^[i]) +
                                        ' @' + Octal(buf^[i]) +
                                        ' #' + Decimal(buf^[i],2) +
                                        ' $' + Hex(buf^[i]) +
                                       '  "' + Chr(buf^[i]) + '": '+ ASCII(buf^[i]) + ' ', color)
      else
        TextXYC(1, i - displayline + 1, '$'+Hexlong(i+curlow)+
                                        ': (past end of file)                ', Gray + BBlue);
      If twofiles then
        If i+curlow < filemax2 then
          TextXYC(46, i - displayline + 1,       Binary(buf2^[i]) +
                                           ' @' + Octal(buf2^[i]) +
                                           ' #' + Decimal(buf2^[i],2) +
                                           ' $' + Hex(buf2^[i]) +
                                          '  "' + Chr(buf2^[i]) + '": ' + ASCII(buf2^[i]), color)
        else
          TextXYC(46, i - displayline + 1, '(past end of file)               ', Gray + BBlue);
    end; { For }

    Repeat
      Repeat
        ShowTime;
        Readkey6(C,key);
      Until key = false;
      Truekey := true;

      { Reagera p† tangenttrycken }
      Case C of
      Chr(0):   { Ut”kad tangentkod }
        begin
          Repeat
            ShowTime;
            Readkey6(C2,key);
          Until key = false;
          Case C2 of
          CursorDown:  Inc(Displayline);
          CursorUp:    Dec(Displayline);
          PageDown:    Inc(Displayline,ScreenLines-1);
          PageUp:      Dec(Displayline,ScreenLines-1);
          ControlPgDn: Inc(Displayline,1024);
          ControlPgUp: Dec(Displayline,1024);
          Home:        Displayline:=0-curlow;
          KEnd:
            Case twofiles of
            false: begin
              If Filemax <> 2147483647 then
                Displayline:=filemax-curlow-23
              else
              begin
                TextXYC(ScreenCols-31,16,'Searching',BBlue+Gray+Flash);
                filemax:=FileSize(fil);
                TextXY(ScreenCols-31,14,'File length: $'+Hexlong(filemax));
                Displayline:=filemax-curlow-23;
              end;
            end;
            true:  begin
              If displaylabels then
                TextXYC(ScreenCols-31, 16, 'Searching', BBlue+Gray+Flash);
              filemax := Filesize(fil);
              filemax2 := Filesize(fil2);
              If displaylabels then begin
                TextXY(ScreenCols-31, 14, 'File length: $' + Hexlong(filemax));
                TextXY(ScreenCols-31, 15, 'File 2 length: $' + Hexlong(filemax2));
              end else If filenameshow then
                TextXY(21, 0, '<$' + HexLong(filemax) + '    $' + HexLong(filemax2) + '>');
              If filemax > filemax2 then
                Displayline := filemax - curlow - 23
              else
                Displayline := filemax2 - curlow - 23;
              end;
            end; { Case twofiles }
            else begin
              Write(#7);
              Truekey := false;
            end; { else }
          end; { Case C2 }
        end;
        Esc:    Exit := True;
        Enter:  Inc(Displayline,ScreenLines-1);
        Tab:    If BinDigits = '01%' then
                  BinDigits := ' * '
                else
                  BinDigits := '01%';
        Space:
          Case filenameshow of
          TRUE:
            begin
              filenameshow := false;
              TextXY(0,0 ,'                                                                       ');
              TextXY(0,0 ,' Address    Binary    Oct. Dec. Hex. ASCII');
            end;
          FALSE:
            begin
              filenameshow := true;
              TextXY(0,0 ,'                                                                       ');
              If twofiles then begin
                TextXY(0,  0, ' File 1: ' + showname1);
                TextXY(45, 0, ' File 2: ' + showname2);
                if (not displaylabels) and filenameshow then begin
                  If filemax <> 2147483647 then
                    TextXY(21, 0, '<$' + HexLong(filemax));
                  If filemax2 <> 2147483647 then
                    TextXY(35, 0, '$' + HexLong(filemax2) + '>');
                end;
              end else
                TextXY(0, 0, ' File: ' + fname1);
            end;
          end; { Case filenameshow }
        else begin
          Write(#7);
          Truekey := false;
        end;
      end; { Case C }
    Until Truekey = true;
  Until Exit = True;
  Dispose(buf);                    { Sl„ng ut databufferten }
  If twofiles then Dispose(buf2);
  ClrScr(oldAttr);
  SetCursor(oldCurs);
  GotoXY(0, 0);
end;

{************************************************************************}
{* Rutin:       main                                                    *}
{************************************************************************}
{* Inneh†ll:    V„ljer delprogram                                       *}
{* Definition:  -                                                       *}
{************************************************************************}

begin
  If (ParamCount=0) or (ParamCount>2) or (ParamStr(1)='/?') then
    HelpScreen
  else
    Showfile;
end.
