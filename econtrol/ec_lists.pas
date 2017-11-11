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

unit ec_Lists;

interface

uses Classes;

type
  TSortedItem = class
  protected
    function GetKey: integer; virtual; abstract;
  end;

  TSortedList = class
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TSortedItem;
  public
    constructor Create(OwnObjects: Boolean);
    destructor Destroy; override;

    function Add(Item: TSortedItem): integer;
    procedure Delete(Index: integer);
    procedure Remove(Item: TSortedItem);
    procedure Clear;
    function PriorAt(Pos: integer): integer;
    function GetAt(Pos: integer): TSortedItem;
    function GetIndexAt(Pos: integer): integer;

    property Items[Index: integer]: TSortedItem read GetItem; default;
    property Count: integer read GetCount;
  end;

  TRange = class(TSortedItem)
  private
    function GetLength: integer;
  {$IFDEF EC_DOTNET}
  public
  {$ELSE}
  protected
  {$ENDIF}
    FStartPos: integer;
    FEndPos: integer;
  protected
    function GetKey: integer; override;
  public
    constructor Create(AStartPos, AEndPos: integer);
    property StartPos: integer read FStartPos;
    property EndPos: integer read FEndPos;
    property Size: integer read GetLength;
  end;

  // Array of sorted ranges
  TRangeList = class
  private
    FList: TList;
    FUnionSiblings: Boolean;
    FPrevIdx: integer;
    function GetCount: integer;
    function GetItems(Index: integer): TRange;
  protected
    // Union ranges with the [Index] and [Index + 1]
    // returns new range index (or union result)
    function UnionRanges(Index: integer): integer; virtual;
    function IsGreater(I1, I2: integer): Boolean;
    // Try to add range if there are no intersections
    function TryAdd(Range: TRange): Boolean;
  public
    constructor Create(UnionSiblings: Boolean = True);
    destructor Destroy; override;
    function Add(Range: TRange): integer; virtual;
    procedure Delete(Index: integer);
    procedure Clear; virtual;
    function ClearFromPos(APos: integer; CopyTo: TRangeList = nil): integer; virtual;
    // Deletes ranges that intersect the bounds, returns number of deleted items
    function DeleteIntersected(AStart, AEnd: integer): integer;
    function SplitRange(RangeIdx, SplitPos: integer): Boolean;

    // Content has been changed, updates ranges upper Pos
    // Removes affected ranges
    function ContentChanged(Pos, Count: integer): Boolean;
    // Exactly at position
    function RangeAt(APos: integer): integer;
    // At position or next
    function NextAt(APos: integer): integer;
    // At position or prior
    function PriorAt(APos: integer): integer;

    property Count: integer read GetCount;
    property Items[Index: integer]: TRange read GetItems; default;
  end;

  // collection of overlapped ranges
  // all ranges are sorted for quick search
  TRangeCollection = class
  private
    FRangeArrays: TList;
    function GetLevelCount: integer;
    function GetLevels(Index: integer): TRangeList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Range: TRange);
    procedure Clear;
    procedure ClearFromPos(APos: integer);
    function GetRangesAtPos(List: TList; Pos: integer): integer;
    function GetRangesAtRange(List: TList; StartPos, EndPos: integer): integer;
    property LevelCount: integer read GetLevelCount;
    property Levels[Index: integer]: TRangeList read GetLevels; default;
  end;

// List[Index] > Key  =>  Result > 0
// List[Index] = Key  =>  Result = 0
// List[Index] < Key  =>  Result < 0
TCompareProc = function(const List: TList; Index: integer; Key: TObject): integer;
// Search in sorted list
function QuickSearch(const List: TList; CompProc: TCompareProc;
                     Key: TObject; var Index: integer): Boolean;

implementation

uses SysUtils, Contnrs;

function QuickSearch(const List: TList; CompProc: TCompareProc;
                     Key: TObject; var Index: integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  if List.Count = 0 then
   begin
    Index := -1;
    Exit;
   end;

  L := 0;
  H := List.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompProc(List, I, Key);
    if C < 0 then L := I + 1 else
    begin
      if C = 0 then
      begin
        Result := True;
        Index := I;
        Exit;
      end;
      H := I - 1;
    end;
  end;
  Index := L;
  if Index >= List.Count then Index := List.Count - 1;
  if Index >= 0 then
   if CompProc(List, Index, Key) > 0 then
    dec(Index);
end;

{ TRange }

constructor TRange.Create(AStartPos, AEndPos: integer);
begin
  inherited Create;
  FStartPos := AStartPos;
  FEndPos := AEndPos;
end;

function TRange.GetKey: integer;
begin
  Result := FStartPos;
end;

function TRange.GetLength: integer;
begin
  Result := FEndPos - FStartPos;
end;

{ TRangeList }

constructor TRangeList.Create(UnionSiblings: Boolean);
begin
  inherited Create;
  FList := TObjectList.Create;
  FUnionSiblings := UnionSiblings;
end;

destructor TRangeList.Destroy;
begin
  FreeAndNil(FList); //AT
  inherited;
end;

procedure TRangeList.Clear;
begin
  FList.Clear;
end;

function TRangeList.UnionRanges(Index: integer): integer;
begin
  // Default action - union of ranges
  if Items[Index].FEndPos < Items[Index + 1].FEndPos then
    Items[Index].FEndPos := Items[Index + 1].FEndPos;
  FList.Delete(Index + 1);
  Result := Index;
end;

function TRangeList.IsGreater(I1, I2: integer): Boolean;
begin
  if FUnionSiblings then
    Result := I1 >= I2
  else
    Result := I1 > I2;
end;

function TRangeList.TryAdd(Range: TRange): Boolean;
var idx: integer;
begin
  Result := True;
  if (Count = 0) or (Items[Count - 1].EndPos <= Range.StartPos) then
    FList.Add(Range)
  else
  if Items[Count - 1].StartPos <= Range.StartPos then
   Result := False
  else
   begin
     idx := NextAt(Range.StartPos);
     if idx = -1 then FList.Add(Range) else
     if (Items[idx].StartPos >= Range.EndPos) and
        ((idx = 0) or (Items[idx - 1].EndPos <= Range.StartPos)) then
        FList.Insert(idx, Range)
     else
       Result := False;
   end;
end;

function TRangeList.Add(Range: TRange): integer;
var idx, k: integer;
begin
  // Stream adding
  if (Count = 0) or (Items[Count - 1].EndPos < Range.StartPos) then
   begin
    Result := Count;
    FList.Add(Range);
    Exit;
   end;

  idx := PriorAt(Range.StartPos);
  if idx = Count - 1 then FList.Add(Range)
   else FList.Insert(idx + 1, Range);
  // Lower range check
  if (idx <> -1) and IsGreater(Items[idx].EndPos, Range.StartPos) then
    idx := UnionRanges(idx)
  else
    idx := idx + 1;
  k := idx + 1;
  while (k < Count) and IsGreater(Items[idx].EndPos, Items[k].StartPos) do
   begin
    idx := UnionRanges(idx);
    k := idx + 1;
   end;

  Result := idx;
end;

procedure TRangeList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

function TRangeList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TRangeList.GetItems(Index: integer): TRange;
begin
  Result := TRange(FList[Index]);
end;

function TRangeList.RangeAt(APos: integer): integer;
begin
  Result := PriorAt(APos);
  if (Result <> -1) and (Items[Result].EndPos <= APos) then
    Result := -1;
end;

function TRangeList.NextAt(APos: integer): integer;
begin
  Result := PriorAt(APos);
  if Result = -1 then
   begin
     if Count > 0 then Result := 0
   end else
   if Items[Result].EndPos <= APos then
    if Result < Count - 1 then Inc(Result)
     else Result := -1;
end;

function RangeCmp(const List: TList; Index: integer; Key: TObject): integer;
begin
 with TRange(List[Index]) do
  if FStartPos > integer(Key) then Result := 1 else
   if (FStartPos <= integer(Key)) and (FEndPos > integer(Key)) then Result:= 0
     else Result := -1;
end;

function TRangeList.PriorAt(APos: integer): integer;
begin
  if (FPrevIdx >= 0) and (FPrevIdx < FList.Count - 1) then
   begin
     if TRange(FList[FPrevIdx]).StartPos <= APos then
      if (FPrevIdx >= FList.Count - 1) or
         (TRange(FList[FPrevIdx + 1]).StartPos > APos) then
       begin
        Result := FPrevIdx;
        Exit;
       end else
      if (FPrevIdx >= FList.Count - 2) or
         (TRange(FList[FPrevIdx + 2]).StartPos > APos) then
       begin
        Result := FPrevIdx + 1;
        Exit;
       end;
   end;
  QuickSearch(FList, RangeCmp, TObject(APos), Result);
  FPrevIdx := Result;
end;

function TRangeList.ContentChanged(Pos, Count: integer): Boolean;
var idx: integer;
begin
  idx := PriorAt(Pos);
  if (idx <> -1) and (Items[idx].EndPos >= Pos) then Delete(idx)
   else
    begin
     Inc(idx);
     if idx >= FList.Count then // No change
      begin
       Result := False;
       Exit;
      end;
    end;

  if Count < 0 then
   while (idx < FList.Count) and (Items[idx].StartPos <= Pos - Count) do
    Delete(idx);

  while idx < FList.Count do
    begin
      Inc(Items[idx].FStartPos, Count);
      Inc(Items[idx].FEndPos, Count);
      Inc(idx);
    end;
  Result := True;
end;

function TRangeList.ClearFromPos(APos: integer; CopyTo: TRangeList): integer;
var idx, i: integer;
begin
  Result := APos;
  idx := NextAt(APos);
  if idx <> -1 then
   begin
     if Items[idx].StartPos < APos then
       Result := Items[idx].StartPos;
     if CopyTo <> nil then
      begin
        CopyTo.Clear;
        CopyTo.FList.Capacity := Count - idx;
        for i := idx to Count - 1 do
         begin
           CopyTo.FList.Add(FList[i]);
           FList.List[i] := nil;
         end;
//        N := Count - idx;
//        CopyTo.FList.Count := N;
//        Move(FList.List[idx], CopyTo.FList.List[0], N * sizeof(pointer));
//        for i := Count - 1 downto idx do
//          FList.List[idx] := nil;
      end;
     for i := Count - 1 downto idx do
       Delete(i);
   end;
end;

function TRangeList.DeleteIntersected(AStart, AEnd: integer): integer;
var idx: integer;
begin
  idx := NextAt(AStart);
  if idx = -1 then idx := Count - 1 else
   if Items[idx].StartPos >= AEnd then Dec(idx);
  Result := 0;
  while (idx >= 0) and (idx < Count) and (Items[idx].EndPos > Astart) do
   begin
    Inc(Result);
    Delete(idx);
   end;
end;

type
  TRangeClass = class of TRange;

function TRangeList.SplitRange(RangeIdx, SplitPos: integer): Boolean;
var R: TRange;
    sp: integer;
begin
  R := Items[RangeIdx];
  Result := (SplitPos > R.StartPos) and (SplitPos < R.EndPos);
  if Result then
    begin
      sp := R.StartPos;
      R.FStartPos := SplitPos;
      R := TRangeClass(R.ClassType).Create(sp, SplitPos);
      FList.Insert(RangeIdx, R);
    end;
end;

{ TRangeCollection }

constructor TRangeCollection.Create;
begin
  inherited;
  FRangeArrays := TObjectList.Create;
end;

destructor TRangeCollection.Destroy;
begin
  Clear;
  FreeAndNil(FRangeArrays);
end;

procedure TRangeCollection.Clear;
begin
  FRangeArrays.Clear;
end;

procedure TRangeCollection.Add(Range: TRange);
var i: integer;
    R: TRangeList;
begin
  for i := 0 to FRangeArrays.Count - 1 do
    if TRangeList(FRangeArrays[i]).TryAdd(Range) then Exit;
  R := TRangeList.Create(False);
  FRangeArrays.Add(R);
  R.Add(Range);
end;

procedure TRangeCollection.ClearFromPos(APos: integer);
var i: integer;
    R: TRangeList;
begin
  for i := FRangeArrays.Count - 1 downto 0 do
   begin
     R := TRangeList(FRangeArrays[i]);
     R.ClearFromPos(APos);
     if R.Count = 0 then
       FRangeArrays.Delete(i);
   end;
end;

function TRangeCollection.GetRangesAtPos(List: TList; Pos: integer): integer;
var i, idx: integer;
    R: TRangeList;
begin
  Result := -1;
  for i := 0 to FRangeArrays.Count - 1 do
   begin
     R := TRangeList(FRangeArrays[i]);
     idx := R.NextAt(Pos);
     if idx <> -1 then
      begin
       if R[idx].StartPos <= Pos then
        begin
         List.Add(R[idx]);
         if (Result = -1) or (Result > R[idx].EndPos) then
           Result := R[idx].EndPos;
        end else
         if (Result = -1) or (Result > R[idx].StartPos) then
           Result := R[idx].StartPos;
      end;
   end;
end;

function TRangeCollection.GetRangesAtRange(List: TList; StartPos,
  EndPos: integer): integer;
var i, idx: integer;
    R: TRangeList;
begin
  Result := -1;
  for i := 0 to FRangeArrays.Count - 1 do
   begin
     R := TRangeList(FRangeArrays[i]);
     idx := R.NextAt(StartPos);
     if (idx <> -1) then
      while (Idx < R.Count) and (R[idx].StartPos < EndPos) do
       begin
        List.Add(R[idx]);
        Inc(Idx);
       end;
   end;
end;

function TRangeCollection.GetLevelCount: integer;
begin
  Result := FRangeArrays.Count;
end;

function TRangeCollection.GetLevels(Index: integer): TRangeList;
begin
  Result := TRangeList(FRangeArrays[Index]);
end;

{ TSortedList }

function TSortedList.Add(Item: TSortedItem): integer;
begin
  if (Count = 0) or (Items[Count - 1].GetKey <= Item.GetKey) then
   begin
    Result := Count;
    FList.Add(Item);
   end else
   begin
    Result := PriorAt(Item.GetKey);
    Inc(Result);
    if Result = Count then FList.Add(Item)
     else FList.Insert(Result, Item);
   end;
end;

procedure TSortedList.Clear;
begin
  FList.Clear;
end;

constructor TSortedList.Create(OwnObjects: Boolean);
begin
  inherited Create;
  if OwnObjects then
    FList := TObjectList.Create
  else
    FList := TList.Create;
end;

procedure TSortedList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

destructor TSortedList.Destroy;
begin
  FreeAndNil(FList);//AT
  inherited;
end;

function TSortedList.GetAt(Pos: integer): TSortedItem;
var idx: integer;
begin
  idx := GetIndexAt(Pos);
  if idx = -1 then Result := nil
   else Result := Items[idx];
end;

function TSortedList.GetIndexAt(Pos: integer): integer;
begin
  Result := PriorAt(Pos);
  if (Result <> -1) and (Items[Result].GetKey <> Pos) then
    Result := -1;
end;

function TSortedList.GetCount: integer;
begin
  if FList<>nil then//AT
    Result := FList.Count
  else
    Result:= 0;//AT
end;

function TSortedList.GetItem(Index: integer): TSortedItem;
begin
  Result := TSortedItem(FList[Index]);
end;

function ItemCmp(const List: TList; Index: integer; Key: TObject): integer;
begin
  Result := TSortedItem(List[Index]).GetKey - integer(Key);
end;

function TSortedList.PriorAt(Pos: integer): integer;
begin
  QuickSearch(FList, ItemCmp, TObject(Pos), Result);
end;

procedure TSortedList.Remove(Item: TSortedItem);
begin
  FList.Remove(Item);
end;

end.
