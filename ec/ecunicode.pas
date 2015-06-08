// Copyright (c) 1999, 2000 Mike Lischke (public@lischke-online.de)
// Portions Copyright (c) 1999, 2000 Azret Botash (az)

{$mode delphi}
unit ecUnicode;

interface

uses
  Classes;

const
  // can't use identifier "Null" here as this is already in a special Variant identifier
  WideNull = WideChar(#0);
  Tabulator = WideChar(#9);
  Space = WideChar(#32);

  // logical line breaks
  LF = WideChar($A);
  LineFeed = WideChar($A);
  VerticalTab = WideChar($B);
  FormFeed = WideChar($C);
  CR = WideChar($D);
  CarriageReturn = WideChar($D);
  CRLF: WideString = #$D#$A;
  LineSeparator = WideChar($2028);
  ParagraphSeparator = WideChar($2029);

  // byte order marks for strings
  // Unicode text files should contain $FFFE as first character to identify such a file clearly. Depending on the system
  // where the file was created on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF); // this is how the BOM appears on x86 systems when written by a x86 system
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TWideStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FSaved,                            // set in SaveToStream, True in case saving was successfull otherwise False
    FSaveUnicode: Boolean;             // flag set on loading to keep track in which format to save
                                       // (can be set explicitely, but expect losses if there's true Unicode content
                                       // and this flag is set to False)
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure ReadAnsiData(Reader: TReader);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetValueFromIndex(Index: Integer): WideString;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: String; Data: Integer);
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(Strings: TWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWideStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: WideString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: WideString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PWideChar); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Saved: Boolean read FSaved;
    property SaveUnicode: Boolean read FSaveUnicode write FSaveUnicode;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
  end;

  // TWideStringList class
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TWideStringItemList = array of TWideStringItem;

  TWideStringList = class(TWideStrings)
  private
    FList: TWideStringItemList;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FCaseSensitive: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: WideString);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure Sort; virtual;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  RTLConsts, SysUtils;

//----------------- TWideStrings ---------------------------------------------------------------------------------------

function TWideStrings.Add(const S: WideString): Integer;

begin
  Result := GetCount;
  Insert(Result, S);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;

begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Append(const S: WideString);

begin
  Add(S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.AddStrings(Strings: TStrings);

var
  I: Integer;

begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.AddStrings(Strings: TWideStrings);

var
  I: Integer;

begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Assign(Source: TPersistent);

// usual assignment routine, but able to assign wide and small strings

var
  I: Integer;

begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to TStrings(Source).Count - 1 do AddObject(TStrings(Source)[I], TStrings(Source).Objects[I]);
      finally
        EndUpdate;
      end;
    end
    else inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.AssignTo(Dest: TPersistent);

// need to do also assignment to old style TStrings, but this class doesn't know TWideStrings, so
// we need to do it from here

var
  I: Integer;

begin
  if Dest is TStrings then
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Self.Count - 1 do AddObject(Self[I], Self.Objects[I]);
      finally
        EndUpdate;
      end;
    end
    else
      if Dest is TWideStrings then
        with Dest as TWideStrings do
        begin
          BeginUpdate;
          try
            Clear;
            AddStrings(Self);
          finally
            EndUpdate;
          end;
        end
        else inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.BeginUpdate;

begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;

  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then Result := not Equals(TWideStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('WideStrings', ReadData, WriteData, DoWrite);
  Filer.DefineProperty('Strings', ReadAnsiData, nil, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.EndUpdate;

begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.Equals(Strings: TWideStrings): Boolean;

var
  I, Count: Integer;

begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Error(const Msg: String; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Exchange(Index1, Index2: Integer);

var
  TempObject: TObject;
  TempString: WideString;

begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.GetCapacity: Integer;

begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.GetName(Index: Integer): WideString;

var
  P: Integer;

begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P > 0 then SetLength(Result, P - 1)
           else Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.GetObject(Index: Integer): TObject;

begin
  Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

{
function TWideStrings.GetText: PWideChar;

begin
  Result := StrNewW(PWideChar(GetTextStr));
end;
}
//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.GetTextStr: WideString;

var
  I, L,
  Size,
  Count: Integer;
  P: PWideChar;
  S: WideString;

begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, 2 * L);
      Inc(P, L);
    end;
    P^ := CarriageReturn;
    Inc(P);
    P^ := LineFeed;
    Inc(P);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.GetValue(const Name: WideString): WideString;

var
  I: Integer;

begin
  I := IndexOfName(Name);
  if I >= 0 then Result := Copy(Get(I), Length(Name) + 2, MaxInt)
            else Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.IndexOf(const S: WideString): Integer;

begin
  for Result := 0 to GetCount - 1 do
    if UnicodeCompareStr(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.IndexOfName(const Name: WideString): Integer;

var
  P: Integer;
  S: WideString;

begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P > 0) and (UnicodeCompareStr(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStrings.IndexOfObject(AObject: TObject): Integer;

begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);

begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.LoadFromFile(const FileName: WideString);
var
  Stream: TStream;
begin
  try
    Stream := {$ifdef TNT}TTntFileStream{$else}TFileStream{$endif}.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
   {$IFDEF VER130}
    RaiseLastWin32Error;
   {$ELSE}
    RaiseLastOSError;
   {$ENDIF}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.LoadFromStream(Stream: TStream);

// usual loader routine, but enhanced to handle byte order marks in stream

var
  Size,
  BytesRead: Integer;
  Order: WideChar;
  SW: WideString;
  SA: String;

begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    BytesRead := Stream.Read(Order, 2);
    if (Order = BOM_LSB_FIRST) or (Order = BOM_MSB_FIRST) then
    begin
      FSaveUnicode := True;
      SetLength(SW, (Size - 2) div 2);
      Stream.Read(PWideChar(SW)^, Size - 2);
      if Order = BOM_MSB_FIRST then raise Exception.Create('Cannot yet load BOM MSB string'); //StrSwapByteOrder(PWideChar(SW));
      SetTextStr(SW);
    end
    else
    begin
      // without byte order mark it is assumed that we are loading ANSI text
      FSaveUnicode := False;
      Stream.Seek(-BytesRead, soFromCurrent);
      SetLength(SA, Size);
      Stream.Read(PChar(SA)^, Size);
      SetTextStr(SA);
    end;
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Move(CurIndex, NewIndex: Integer);

var
  TempObject: TObject;
  TempString: WideString;

begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.Put(Index: Integer; const S: WideString);

var
  TempObject: TObject;

begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.PutObject(Index: Integer; AObject: TObject);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
    Add(Reader.ReadWideString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TWideStrings.ReadAnsiData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
    Add(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SaveToFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := {$ifdef TNT}TTntFileStream{$else}TFileStream{$endif}.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SaveToStream(Stream: TStream);

var
  SW, BOM: WideString;
  SA: String;
  Allowed: Boolean;
  Run: PWideChar;

begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of information
  // when converting Unicode to ANSI strings.
  SW := GetTextStr;
  Allowed := True;
  FSaved := False; // be pessimistic
  // check for potential information loss makes only sense if the application has set
  // an event to be used as call back to ask about the conversion

  if Allowed then
  begin
    // only save if allowed
    if FSaveUnicode then
    begin
      BOM := BOM_LSB_FIRST;
      Stream.WriteBuffer(PWideChar(BOM)^, 2);
      // SW has already been filled
      Stream.WriteBuffer(PWideChar(SW)^, 2 * Length(SW));
    end
    else
    begin
      // implicit conversion to ANSI
      SA := SW;
      if Allowed then Stream.WriteBuffer(PChar(SA)^, Length(SA));
    end;
    FSaved := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetCapacity(NewCapacity: Integer);

begin
  // do nothing - descendants may optionally implement this method
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetText(Text: PWideChar);

begin
  SetTextStr(Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetTextStr(const Value: WideString);

var
  Head,
  Tail: PWideChar;
  S: WideString;

begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, LineFeed, CarriageReturn, VerticalTab, FormFeed]) and
            (Tail^ <> LineSeparator) and
            (Tail^ <> ParagraphSeparator) do Inc(Tail);
      SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = CarriageReturn) and
           (Head^ = LineFeed) then Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetUpdateState(Updating: Boolean);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.SetValue(const Name, Value: WideString);

var
  I : Integer;

begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
    if I >= 0 then Delete(I);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStrings.WriteData(Writer: TWriter);

var
  I: Integer;

begin
  Writer.WriteListBegin;
  for I := 0 to Count-1 do
  Writer.WriteWideString(Get(I));
  Writer.WriteListEnd;
end;

function TWideStrings.GetValueFromIndex(Index: Integer): WideString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TWideStrings.SetValueFromIndex(Index: Integer; const Value: WideString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + '=' + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

//----------------- TWideStringList ------------------------------------------------------------------------------------

destructor TWideStringList.Destroy;

begin
  FOnChange := nil;
  FOnChanging := nil;
  FCount := 0;
  FList := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.Add(const S: WideString): Integer;

begin
  if not Sorted then Result := FCount
                else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore:
          Exit;
        dupError:
          Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Changed;

begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Changing;

begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Clear;

begin
  if FCount <> 0 then
  begin
    Changing;
    // this will automatically finalize the array
    FList := nil;
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Delete(Index: Integer);

begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList[Index].FString := '';
  Dec(FCount);
  if Index < FCount then System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TWideStringItem));
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Exchange(Index1, Index2: Integer);

begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.ExchangeItems(Index1, Index2: Integer);

var
  Temp: TWideStringItem;

begin
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.Find(const S: WideString; var Index: Integer): Boolean;

var
  L, H, I, C: Integer;

begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := UnicodeCompareStr(FList[I].FString, S);
    if C < 0 then L := I+1
             else
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

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.Get(Index: Integer): WideString;

begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList[Index].FString;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.GetCapacity: Integer;

begin
  Result := Length(FList);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.GetCount: Integer;

begin
  Result := FCount;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.GetObject(Index: Integer): TObject;

begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList[Index].FObject;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Grow;

var
  Delta,
  Len: Integer;

begin
  Len := Length(FList);
  if Len > 64 then Delta := Len div 4
              else
    if Len > 8 then Delta := 16
               else Delta := 4;
  SetCapacity(Len + Delta);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWideStringList.IndexOf(const S: WideString): Integer;

begin
  if not Sorted then Result := inherited IndexOf(S)
                else
    if not Find(S, Result) then Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Insert(Index: Integer; const S: WideString);

begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString);

begin
  Changing;
  if FCount = Length(FList) then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TWideStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Put(Index: Integer; const S: WideString);

begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList[Index].FString := S;
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);

begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.QuickSort(L, R: Integer);

var
  I, J: Integer;
  P: WideString;

begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      while UnicodeCompareStr(FList[I].FString, P) < 0 do Inc(I);
      while UnicodeCompareStr(FList[J].FString, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.SetCapacity(NewCapacity: Integer);

begin
  SetLength(FList, NewCapacity);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.SetSorted(Value: Boolean);

begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.SetUpdateState(Updating: Boolean);

begin
  if Updating then Changing
              else Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWideStringList.Sort;

begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1);
    Changed;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TWideStringList.Create;
begin
  inherited;
end;

procedure TWideStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

