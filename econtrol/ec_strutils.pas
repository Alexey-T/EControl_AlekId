{ *************************************************************************** }
{                                                                             }
{ EControl Common Library                                                     }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{                                                                             }
{ *************************************************************************** }
//Much code stripped from Lazarus port, by Alexey T.

{$mode delphi}

unit ec_StrUtils;

interface

uses SysUtils, Classes, Graphics;

type
  ecString = UnicodeString;
  ecChar = WideChar;
  UCString = UnicodeString;
  UCChar = WideChar;

function IsDigitChar(C: UCChar): Boolean; inline;
function IsHexDigitChar(C: UCChar): Boolean;
function IsLineBreakChar(c: UCChar): Boolean;
function IsWordChar(c: UCChar): Boolean;
function IsSpaceChar(c: UCChar): Boolean; inline;
function IsAlphaChar(c: UCChar): Boolean;

function IsIdentChar(C: UCChar): Boolean;
function IsIdentDigitChar(C: UCChar): Boolean; inline;
function IsIdentLetterChar(C: UCChar): Boolean;
function IsWordBreak(aPos: integer; const Text: UCString): Boolean; overload;

function ecUpCase(C: UCChar): UCChar; inline;
function SkipSpaces(const Source: ecString; var APos: integer): integer;
function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): integer;
function ecEncodeString(const S: string): string;
function ecDecodeString(const S: string): string;
function ecPosEx(const SubStr, S: ecString; Offset: Cardinal = 1): Integer;

implementation

uses
  Controls, Forms;

//==============================================================================
//  Routines
//==============================================================================
function IsSpaceChar(c: UCChar): Boolean; inline;
begin
  Result := (c=' ') or (c=#9);
end;

function IsLineBreakChar(c: UCChar): Boolean;
begin
   case C of
     #$000A, #$000D,
     #$2028, #$2029, #$0085:
       Result := True;
     else
       Result := False;
   end;
end;

function IsDigitChar(C: UCChar): Boolean; inline;
begin
  Result := (C>='0') and (C<='9');
end;

function IsHexDigitChar(C: UCChar): Boolean;
begin
  case C of
    '0'..'9':
       Result := true;
    'a'..'f':
      Result := true;
    'A'..'F':
      Result := true;
    else
      Result := false;
  end;
end;

function IsWordChar(c: UCChar): Boolean;
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

function IsAlphaChar(c: UCChar): Boolean;
begin
  Result :=
    ((C >= 'a') and (C <= 'z')) or
    ((C >= 'A') and (C <= 'Z'));
end;

function IsIdentChar(C: UCChar): Boolean;
begin
  Result := IsIdentLetterChar(C) or IsIdentDigitChar(C);
end;

function IsIdentLetterChar(C: UCChar): Boolean;
begin
  Result :=
    ((C >= 'a') and (C <= 'z')) or
    ((C >= 'A') and (C <= 'Z')) or
    (C = '_');
end;

function IsIdentDigitChar(C: UCChar): Boolean; inline;
begin
  Result := (C >= '0') and (C <= '9');
end;

function IsWordBreak(aPos: integer; const Text: UCString): Boolean;
begin
  Result := (aPos = 1) or (aPos > Length(Text)) or
            (IsWordChar(Text[aPos]) xor IsWordChar(Text[aPos - 1]));
end;

function ecUpCase(C: UCChar): UCChar; inline;
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
