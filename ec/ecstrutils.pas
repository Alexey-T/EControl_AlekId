{ *************************************************************************** }
{                                                                             }
{ EControl Common Library                                                     }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}
{$define EC_UNICODE}

unit ecStrUtils;

interface

uses Classes, Graphics, SysUtils, ecUnicode;

const
  { Search flags }
  FF_CASESENSITIVE  = 1;
  FF_WHOLEWORD      = 2;
  FF_BACKWARD       = 4;
  FF_REGEXPRESSION  = 8;
  sLineBreak = #13#10;


type
 {$IFDEF EC_UNICODE}
  ecString = WideString;
  ecChar = WideChar;
  PecChar = PWideChar;
  TecStrings = TWideStrings;
  TecStringList = TWideStringList;
 {$ELSE}
  ecString = AnsiString;
  ecChar = Char;
  PecChar = PChar;
  TecStrings = TStrings;
  TecStringList = TStringList;
 {$ENDIF}

 {$IFDEF EC_STRING_UNICODE}
  UCString = string;
  UCChar = Char;
 {$ELSE}
  UCString = WideString;
  UCChar = WideChar;
 {$ENDIF}

  ecPointer = Pointer;
  ecPChar = PChar;

  // Encoding
  TTextCoding = (tcAnsi,
                 tcUnicode,
                 tcSwapUnicode,
                 tcUTF8);

  // Line break format
  TTextFormat = (tfDefault,       // Line breaks is unchanged
                 tfCR_NL,         // Windows
                 tfCR,            // Mac
                 tfNL);           // Unix

  // Change case commands
  TChangeCase = (ccNone, ccUpper, ccLower, ccToggle, ccTitle);

  // Event to define "word" characters
  // Used by several components and may be shared
  TCheckCharEvent = procedure(Sender: TObject; C: Word; var IsWord: Boolean) of object;

  TzStringList = class(TStringList)
  private
    FDelimiter: Char;
    FCaseSensitive: Boolean;
    function GetDelimitedText: string;
    procedure SetDelimitedText(const Value: string);
    procedure SetCaseSensitive(const Value: Boolean);
  public
    function Find(const S: string; out Index: Integer): Boolean; override;
    procedure Sort; override;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
  private
    function GetValueFromIndex(Index: Integer): string;
    procedure SetValueFromIndex(Index: Integer; const Value: string);
  public
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
  end;


function GetStreamFormat(Stream: TStream): TTextCoding;
procedure WriteTextSignature(Stream: TStream; TextCoding: TTextCoding);
procedure ReadStringFromStream(var Text: ecString; Stream: TStream;
                                    TextCoding: TTextCoding = tcAnsi);
procedure WriteStringToStream(const Text: ecString; Stream: TStream;
                                    TextCoding: TTextCoding = tcAnsi);

function IsDigitChar(const c: UCChar): Boolean; overload;
function IsHexDigitChar(const c: UCChar): Boolean; overload;
function IsLineBreakChar(const c: UCChar): Boolean; overload;
function IsWordChar(const c: UCChar): Boolean; overload;
function IsSpaceChar(const c: UCChar): Boolean; overload;
function IsAlphaChar(const c: UCChar): Boolean; overload;

function IsIdentChar(const C: UCChar): Boolean; overload;
function IsIdentDigitChar(const C: UCChar): Boolean; overload;
function IsIdentLetterChar(const C: UCChar): Boolean; overload;
function IsWordBreak(aPos: integer; const Text: UCString): Boolean; overload;

function ecUpCase(const C: UCChar): UCChar; overload;

function SkipSpaces(const Source: ecString; var APos: integer): integer;
function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): integer;
function IsStringEmpty(const S: ecString): Boolean;
function GetLineIndex(const S: ecString; aPos: integer): integer;
function GetLines(const AText: ecString; List: TList = nil): integer; // Calculates line number in the Text, fills indexes List
function ecEncodeString(const S: string): string;
function ecDecodeString(const S: string): string;

function ecPosEx(const SubStr, S: ecString; Offset: Cardinal = 1): Integer;

function EncodeINIString(const S: ecString): AnsiString;
function DecodeINIString(const S: AnsiString): ecString;

function ChangeComponentReference(This, NewRef: TComponent; var RefVar: TComponent): Boolean;

const
  SecListIndexError = 'Index %d is out of range';

implementation

uses
  Controls, Forms;

function TzStringList.GetDelimitedText: string;
var
  S: string;
  P: PChar;
  I, NCount: Integer; //renamed Count
begin
  NCount := GetCount;
  if (NCount = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to NCount - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not (P^ in [#0..' ', QuoteChar, Delimiter]) do
        Inc(P);
      if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
      System.Delete(Result, Length(Result), 1);
  end;
end;

procedure TzStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

procedure TzStringList.SetDelimitedText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    while P^ in [#1..' '] do
      Inc(P);
    while P^ <> #0 do
    begin
      P1 := P;
      while (P^ > ' ') and (P^ <> Delimiter) do
        Inc(P);
      SetString(S, P1, P - P1);
      Add(S);
      while P^ in [#1..' '] do
        Inc(P);
      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (P^ in [#1..' ']);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function StrAnsiCompare(List: TStringList; const S1, S2: string): Integer;
begin
  with TzStringList(List) do
   if CaseSensitive then
     Result := AnsiCompareStr(S1, S2)
   else
     Result := AnsiCompareText(S1, S2);
end;

function StringListAnsiCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrAnsiCompare(List, List[Index1], List[Index2]);
end;

function TzStringList.Find(const S: string; out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := StrAnsiCompare(Self, Strings[I], S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TzStringList.Sort;
begin
  CustomSort(StringListAnsiCompare);
end;

function TzStringList.GetValueFromIndex(Index: Integer): string;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TzStringList.SetValueFromIndex(Index: Integer; const Value: string);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + '=' + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

function GetStreamFormat(Stream: TStream): TTextCoding;
var BytesRead: Integer;
    Sign: WORD;
    C: BYTE;
begin
  Result := tcAnsi;
  Sign := 0;
  C := 0;
  if Stream.Size - Stream.Position < 2 then Exit;
  BytesRead := Stream.Read(Sign, 2);
  if (Sign = $FEFF) or (Sign = $FFFE) then
   begin
    if Sign = $FFFE then Result := tcSwapUnicode
     else Result := tcUnicode;
    Exit;
   end;
  if Sign = $BBEF then
   begin
    Inc(BytesRead, Stream.Read(C, 1));
    if C = $BF then
     begin
      Result := tcUTF8;
      Exit;
     end;
   end;
  Stream.Seek(-BytesRead, soFromCurrent);
end;

procedure WriteTextSignature(Stream: TStream; TextCoding: TTextCoding);
  procedure Write(const Bytes: array of Byte);
  begin
    Stream.Write(Bytes[0], Length(Bytes));
  end;
begin
  case TextCoding of
    tcUnicode:    Write([$FF, $FE]);
    tcSwapUnicode:Write([$FE, $FF]);
    tcUTF8:       Write([$EF, $BB, $BF]);
  end;
end;

procedure WriteStringToStream(const Text: ecString; Stream: TStream;
                                    TextCoding: TTextCoding);
var S: AnsiString;
    W: UCString;
begin
  // Write text
  case TextCoding of
    tcAnsi:
        begin
          {$IFDEF EC_UNICODE}
          S := Text; //todo //UnicodeToAnsi(Text, CharSet, CodePage);
          Stream.WriteBuffer(Pointer(S)^, Length(S));
          {$ELSE}
          if CodePage <> CP_ACP then
            begin
              W := AnsiToUnicode(Text, CharSet);
              S := UnicodeToAnsiCP(W, CodePage);
            end
          else
            S := Text;
          Stream.WriteBuffer(Pointer(S)^, Length(S));
          {$ENDIF}
        end;
    tcUnicode:
        begin
          {$IFDEF EC_UNICODE}
          Stream.WriteBuffer(Pointer(Text)^, Length(Text) * 2);
          {$ELSE}
          W := AnsiToUnicode(Text, CharSet, CodePage);
          Stream.WriteBuffer(Pointer(W)^, Length(W) * 2);
          {$ENDIF}
        end;
    tcSwapUnicode:
        begin
          {$IFDEF EC_UNICODE}
          W := Text;
          {$ELSE}
          W := AnsiToUnicode(Text, CharSet, CodePage);
          {$ENDIF}
          raise Exception.Create('Cannot yet save Unicode BE string');
          {
          StrSwapByteOrder(PWideChar(W));
          Stream.WriteBuffer(Pointer(W)^, Length(W) * 2);
          }
        end;
    tcUTF8:
        begin
          {$IFDEF EC_UNICODE}
          W := Text;
          {$ELSE}
          W := AnsiToUnicode(Text, CharSet, CodePage);
          {$ENDIF}

          S := Utf8Encode(W);
          Stream.WriteBuffer(Pointer(S)^, Length(S));
        end;
  end;
end;

procedure ReadStringFromStream(var Text: ecString; Stream: TStream;
                               TextCoding: TTextCoding);
var Size: Cardinal;
    S: AnsiString;
    W: UCString;
begin
  Size := Stream.Size - Stream.Position;
  try
    case TextCoding of
      tcAnsi:
         begin
          {$IFDEF EC_UNICODE}
          SetLength(S, Size);
          Stream.Read(S[1], Size);
          Text := S; //todo //AnsiToUnicode(S, CharSet, CodePage);
          {$ELSE}
          SetLength(Text, Size);
          Stream.Read(Text[1], Size);
          if CodePage <> CP_ACP then
            begin
              W := AnsiToUnicodeCP(Text, CodePage);
              Text := UnicodeToAnsi(W, CharSet);
            end;
          {$ENDIF}
         end;
      tcUnicode:
         begin
          Size := Size shr 1;
          {$IFNDEF EC_UNICODE}
          SetLength(W, Size);
          Stream.Read(W[1], Size shl 1);
          Text := UnicodeToAnsi(W, CharSet, CodePage);
          {$ELSE}
          SetLength(Text, Size);
          Stream.Read(Text[1], Size shl 1);
          {$ENDIF}
         end;
      tcSwapUnicode:
         begin
          Size := Size shr 1;
          {$IFNDEF EC_UNICODE}
          SetLength(W, Size);
          Stream.Read(W[1], Size shl 1);
          StrSwapByteOrder(PWideChar(W));
          Text := UnicodeToAnsi(W, CharSet, CodePage);
          {$ELSE}
          raise Exception.Create('Cannot yet load Unicode BE string');
          {
          SetLength(Text, Size);
          Stream.Read(Text[1], Size shl 1);
          StrSwapByteOrder(PWideChar(Text));
          }
          {$ENDIF}
         end;
      tcUTF8:
         begin
          SetLength(S, Size);
          Stream.Read(S[1], Size);
          W := UTF8Decode(S);

          {$IFDEF EC_UNICODE}
          Text := W;
          {$ELSE}
          Text := UnicodeToAnsi(W, CharSet, CodePage);
          {$ENDIF}
         end;
    end;
  except
  end;
end;


//==============================================================================
//  Routines
//==============================================================================
function IsSpaceChar(const c: UCChar): Boolean;
begin
  Result := c = ' ';
end;

function IsLineBreakChar(const c: UCChar): Boolean;
begin
   case C of
     #$000A, #$000D,
     #$2028, #$2029, #$0085: Result := True;
     else Result := False;
   end;
end;

function IsDigitChar(const C: UCChar): Boolean;
begin
  Result := Pos(c, '1234567890') > 0;
end;

function IsHexDigitChar(const C: UCChar): Boolean;
begin
  Result := Pos(c, '1234567890abcdefABCDEF') > 0;
end;

function IsWordChar(const C: UCChar): Boolean;
begin
  if IsDigitChar(C) then Result := True
  else
  if (C >= 'a') and (C <= 'z') then Result := True
  else
  if (C >= 'A') and (C <= 'Z') then Result := True
  else
  if (C = '_')
    or (C = #$0301) //AT
    or (C = #$00B4) //AT
    or (C = #$02B9) //AT
    or (C = #$02CA) //AT
    or (C = #$0384) then Result := True
  else
    Result := False;
end;

function IsAlphaChar(const C: UCChar): Boolean;
begin
  Result :=
    ((C >= 'a') and (C <= 'z')) or
    ((C >= 'A') and (C <= 'Z'));
end;

function IsIdentChar(const C: UCChar): Boolean;
begin
  Result := IsIdentLetterChar(C) or IsIdentDigitChar(C);
end;

function IsIdentLetterChar(const C: UCChar): Boolean;
begin
  Result :=
    ((C >= 'a') and (C <= 'z')) or
    ((C >= 'A') and (C <= 'Z')) or
    (C = '_');
end;

function IsIdentDigitChar(const C: UCChar): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function IsWordBreak(aPos: integer; const Text: UCString): Boolean;
begin
  Result := (aPos = 1) or (aPos > Length(Text)) or
            (IsWordChar(Text[aPos]) xor IsWordChar(Text[aPos - 1]));
end;

function ecUpCase(const C: UCChar): UCChar;
begin
  Result := UpCase(C);
end;

function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): integer;
var N: integer;
begin
  Result := 0;
  N := Length(Source);
  while (APos <= N) and IsSpaceChar(Source[APos]) and not IsLineBreakChar(Source[APos]) do
    inc(APos);
  if APos > N then Result := -1;
end;

function SkipSpaces(const Source: ecString; var APos: integer): integer;
var N: integer;
begin
  Result := 0;
  N := Length(Source);
  while (APos <= N) and IsSpaceChar(Source[APos]) do
   begin
    if Source[APos] = #10 then inc(Result);
    inc(APos);
   end;
  if APos > N then Result := -1;
end;

function IsStringEmpty(const S: ecString): Boolean;
var i: integer;
begin
  for i := 1 to Length(s) do
   if not IsSpaceChar(s[i]) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function GetLineIndex(const S: ecString; aPos: integer): integer;
var i, N: integer;
begin
  Result := 0;
  N := Length(S);
  Inc(aPos);
  if N > aPos then N := aPos;
  for i := 1 to N do
   if S[i] = #10 then inc(Result);
end;

function GetLines(const AText: ecString; List: TList): integer;
var i, L, st, res: Integer;
    procedure Add;
    begin
        if List <> nil then List.Add(TObject(st));
        st := i;
        Inc(res);
    end;
begin
    if List <> nil then List.Clear;
    res := 0;
    L := Length(AText);
    if L > 0 then begin
        st := 0;
        i := 1;
        while i <= L do begin
            case AText[i] of
            #13:begin
                    if (i < L) and (AText[i + 1] = #10) then Inc(i);
                    Add;
                end;
            #10:Add;
            end;
            inc(i);
        end;
        Add;
    end;
    Result := res;
end;

function ecEncodeString(const S: string): string;
var I, L, K: integer;
begin
  Result := '';
  L := Length(S);
  I := 1;
  while I <= L do
    if (S[I] >= ' ') and (S[I] <> '''') then
     begin
       K := I;
       repeat
         Inc(I)
       until (I > L) or (S[I] < ' ') or (S[I] = '''');
       Result := Result + '''' + Copy(S, K, I - K) + '''';
     end else
     begin
      Result := Result + '#' + IntToStr(Ord(S[I]));
      Inc(I);
     end;
end;

function ecDecodeString(const S: string): string;
var I, L, K: integer;
begin
  Result := '';
  L := Length(S);
  I := 1;
  while I <= L do
   if S[I] = '''' then
     begin
       K := I;
       repeat
         Inc(I);
       until (I > L) or (S[i] = '''');
       Result := Result + Copy(S, K + 1, I - K - 1);
       Inc(I);
     end else
   if S[I] = '#' then
     begin
       K := I + 1;
       repeat
         Inc(I)
       until (I > L) or not IsIdentDigitChar(S[I]);
       if (K = I) or ((I - K) > 3) then
         raise Exception.Create('Invalid character code');
       Result := Result + Chr(StrToInt(Copy(S, K, I - K)));
     end else Exit;
//   else raise Exception.Create('Invalid property data');
end;

function ecPosEx(const SubStr, S: ecString; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function EncodeINIString(const S: ecString): AnsiString;
var i, N: integer;
    C: ecChar;
begin
  N := Length(S);
  SetLength(Result, N);
  for i := N downto 1 do
    begin
      C := S[i];
      if (Ord(C) < 32) or (C = '#') or (C = '$') then
        begin
          Result[i] := '#';
          Insert(AnsiString(IntToHex(Ord(C), 2)), Result, I + 1);
        end else
{$IFDEF EC_UNICODE}
      if Ord(C) > 255 then
        begin
          Result[i] := '$';
          Insert(AnsiString(IntToHex(Ord(C), 4)), Result, I + 1);
        end else
{$ENDIF}
      Result[i] := AnsiChar(C);
    end;
end;

function DecodeINIString(const S: AnsiString): ecString;
var i, N: integer;
    W: UCChar;
begin
  Result := ecString(S);
  N := Length(S);
  for i := N downto 1 do
    begin
      case Result[i] of
        '#': begin
               Result[i] := ecChar(StrToInt('$' + Copy(Result, i+1, 2)));
               Delete(Result, i+1, 2);
             end;
        '$': begin
               W := UCChar(StrToInt('$' + Copy(Result, i+1, 4)));
{$IFDEF EC_UNICODE}
               Result[i] := W;
               Delete(Result, i+1, 4);
{$ELSE}
               Delete(Result, i, 5);
               Insert(UnicodeToAnsi(W), Result, i);
{$ENDIF}
             end;
      end;
    end;
end;

function ChangeComponentReference(This, NewRef: TComponent; var RefVar: TComponent): Boolean;
begin
  Result := (RefVar <> NewRef) and Assigned(This);
  if Result then
    begin
      if Assigned(RefVar) then
        RefVar.RemoveFreeNotification(This);
      RefVar := NewRef;
      if Assigned(RefVar) then
        RefVar.FreeNotification(This);
    end;
end;


end.
