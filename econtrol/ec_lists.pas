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

uses
  Classes,FGL;

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

  { TRange }

  TRange = packed record
  private
    function GetLength: integer;
  public                                              
    StartPos, EndPos: integer;
    PointStart, PointEnd: TPoint;
    constructor Create(AStartPos, AEndPos: integer);
    constructor Create(AStartPos, AEndPos: integer; const APointStart, APointEnd: TPoint);
    property Size: integer read GetLength;
    class operator=(const a, b: TRange): boolean;
  end;

  { GRangeList }

  // Array of sorted ranges
  GRangeList<GRange> = class (TFPGList<GRange>)
  private
    FUnionSiblings: Boolean;
    FPrevIdx: integer;
  protected
    // Union ranges with the [Index] and [Index + 1]
    // returns new range index (or union result)
    function UnionRanges(Index: integer): integer; virtual;
    function IsGreater(I1, I2: integer): Boolean;
    function CompProc(const Val:TRange; Key: integer): integer;
  public
    constructor Create(UnionSiblings: Boolean = True);
    destructor Destroy; override;
    function Add(const Range: GRange): integer;virtual;
    function ClearFromPos(APos: integer; CopyTo: GRangeList<GRange> = nil): integer; virtual;
    // Deletes ranges that intersect the bounds, returns number of deleted items
    function DeleteIntersected(AStart, AEnd: integer): integer;
    //function SplitRange(RangeIdx, SplitPos: integer): Boolean;

    // Content has been changed, updates ranges upper Pos
    // Removes affected ranges
    function ContentChanged(Pos, Count: integer): Boolean;
    // Exactly at position
    function RangeAt(APos: integer): integer;
    // At position or next
    function NextAt(APos: integer): integer;
    // At position or prior
    function PriorAt(APos: integer): integer;
  end;

  TRangeList = GRangeList<TRange>;

implementation

uses
  //Math, Dialogs,
  SysUtils, Contnrs;

{ TRange }

constructor TRange.Create(AStartPos, AEndPos: integer; const APointStart, APointEnd: TPoint);
begin
  StartPos := AStartPos;
  EndPos := AEndPos;
  PointStart := APointStart;
  PointEnd := APointEnd;
end;

constructor TRange.Create(AStartPos, AEndPos: integer);
begin
  StartPos := AStartPos;
  EndPos := AEndPos;
  PointStart.X := -1;
  PointStart.Y := -1;
  PointEnd.X := -1;
  PointEnd.Y := -1;
end;

function TRange.GetLength: integer;
begin
  Result := EndPos-StartPos;
end;

class operator TRange.=(const a,b: TRange):boolean;
begin                   
  Result:=
    (a.StartPos=b.StartPos) and
    (a.EndPos=b.EndPos) and
    (a.PointStart=b.PointStart) and
    (a.PointEnd=b.PointEnd);
end;

{ TRangeList }

constructor GRangeList<GRange>.Create(UnionSiblings: Boolean);
begin
  inherited Create;
  FUnionSiblings := UnionSiblings;
end;

destructor GRangeList<GRange>.Destroy;
begin
  inherited;
end;

function GRangeList<GRange>.UnionRanges(Index: integer): integer;
begin
  // Default action - union of ranges
  if TRange(InternalItems[Index]^).EndPos < TRange(InternalItems[Index + 1]^).EndPos then
    TRange(InternalItems[Index]^).EndPos := TRange(InternalItems[Index + 1]^).EndPos;
  inherited Delete(Index + 1);
  Result := Index;
end;

function GRangeList<GRange>.IsGreater(I1, I2: integer): Boolean;
begin
  if FUnionSiblings then
    Result := I1 >= I2
  else
    Result := I1 > I2;
end;

function GRangeList<GRange>.Add(const Range: GRange): integer;
var idx, k: integer;
  _Range: TRange absolute Range;
begin
  // Stream adding
  if (Count = 0) or ( TRange(InternalItems[Count - 1]^).EndPos <= _Range.StartPos) then
  begin
    Result := Count;
    inherited Add(Range);
  end
  else
  begin
    idx := PriorAt(_Range.StartPos);
    if idx = Count - 1 then
       inherited Add(Range)
    else
       inherited Insert(idx + 1, Range);
    // Lower range check
    if (idx <> -1) and IsGreater(TRange(InternalItems[idx]^).EndPos, _Range.StartPos) then
      idx := UnionRanges(idx)
    else
      idx := idx + 1;
    k := idx + 1;
    while (k < Count) and IsGreater(TRange(InternalItems[idx]^).EndPos, TRange(InternalItems[k]^).StartPos) do
    begin
      idx := UnionRanges(idx);
      k := idx + 1;
    end;

    Result := idx;
  end;
end;

function GRangeList<GRange>.RangeAt(APos: integer): integer;
begin
  Result := PriorAt(APos);
  if (Result <> -1) and (TRange(InternalItems[Result]^).EndPos <= APos) then
    Result := -1;
end;

function GRangeList<GRange>.NextAt(APos: integer): integer;
begin
  Result := PriorAt(APos);
  if Result = -1 then
   begin
     if Count > 0 then Result := 0
   end else
   if TRange(InternalItems[Result]^).EndPos <= APos then
    if Result < Count - 1 then Inc(Result)
     else Result := -1;
end;


function GRangeList<GRange>.CompProc(const Val:TRange; Key: integer): integer;
begin
  with Val do
    if StartPos > Key then
      Result := 1
    else if (StartPos <= Key)and(EndPos > Key) then
      Result:= 0
    else
      Result := -1;
end;

function GRangeList<GRange>.PriorAt(APos: integer): integer; 
var
  H, I, C: Integer;
begin
  if (FPrevIdx >= 0) and (FPrevIdx < Count - 1) and (TRange(InternalItems[FPrevIdx]^).StartPos <= APos)then
  begin
      if (FPrevIdx >= Count - 1) or (TRange(InternalItems[FPrevIdx + 1]^).StartPos > APos) then
      begin
        Result := FPrevIdx;
        Exit;
      end
      else if (FPrevIdx >= Count - 2) or (TRange(InternalItems[FPrevIdx + 2]^).StartPos > APos) then
      begin
        Result := FPrevIdx + 1;
        Exit;
      end;
  end;
  if Count = 0 then
  begin
    FPrevIdx := -1;
    Exit(-1);
  end
  else
  begin
    Result := 0;
    H := Count - 1;
    while Result <= H do
    begin
      I := (Result + H) shr 1;
      C := CompProc(TRange(InternalItems[i]^), APos);
      if C < 0 then
        Result := I + 1
      else
      begin
        if C = 0 then
        begin
          FPrevIdx := I;
          Exit(I);
        end;
        H := I - 1;
      end;
    end;
    if Result >= Count then
      Result := Count - 1;
    if Result >= 0 then
      if CompProc(TRange(InternalItems[i]^), APos) > 0 then
        dec(Result);

    FPrevIdx := Result;
  end;
end;

function GRangeList<GRange>.ContentChanged(Pos, Count: integer): Boolean;
var idx: integer;
begin
  idx := PriorAt(Pos);
  if (idx <> -1) and (TRange(InternalItems[idx]^).EndPos >= Pos) then Delete(idx)
   else
    begin
     Inc(idx);
     if idx >= Count then // No change
      begin
       Result := False;
       Exit;
      end;
    end;

  if Count < 0 then
    while (idx < Count) and (TRange(InternalItems[idx]^).StartPos <= Pos - Count) do
      Delete(idx);

  while idx < Count do
  begin
    Inc(TRange(InternalItems[idx]^).StartPos, Count);
    Inc(TRange(InternalItems[idx]^).EndPos, Count);
    Inc(idx);
  end;
  Result := True;
end;

function GRangeList<GRange>.ClearFromPos(APos: integer; CopyTo: GRangeList<GRange>): integer;
var idx, i: integer;
begin
  Result := APos;
  idx := NextAt(APos);
  if idx <> -1 then
  begin
    if TRange(InternalItems[idx]^).StartPos < APos then
      Result := TRange(InternalItems[idx]^).StartPos;
    if CopyTo <> nil then
    begin
      CopyTo.Clear;
      CopyTo.Capacity := Count - idx;
      for i := idx to Count - 1 do
        CopyTo.Add(Items[i]);
    end;
    for i := Count - 1 downto idx do
      Delete(i);
  end;
end;

function GRangeList<GRange>.DeleteIntersected(AStart, AEnd: integer): integer;
var idx: integer;
begin
  idx := NextAt(AStart);
  if idx = -1 then idx := Count - 1 else
   if TRange(InternalItems[idx]^).StartPos >= AEnd then Dec(idx);
  Result := 0;
  while (idx >= 0) and (idx < Count) and (TRange(InternalItems[idx]^).EndPos > Astart) do
   begin
    Inc(Result);
    Delete(idx);
   end;
end;

//type
//  TRangeClass = class of TRange;

(*
function GRangeList<GRange>.SplitRange(RangeIdx, SplitPos: integer): Boolean;
var R: GRange;
    sp: integer;
begin
  R := Items[RangeIdx];
  Result := (SplitPos > R.StartPos) and (SplitPos < R.EndPos);
  if Result then
    begin
      sp := R.StartPos;
      R.StartPos := SplitPos;
      Items[RangeIdx]:=R;
      R := TRange.Create(sp, SplitPos);
      Insert(RangeIdx, R);
    end;
end;
*)

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

function TSortedList.PriorAt(Pos: integer): integer;
  function CompProc(Item: pointer; Key: integer): integer;
  begin
    Result := TSortedItem(Item).GetKey - Key;
  end;

  function QuickSearch(const List: TList; Key: integer; var Index: integer): Boolean;
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
      C := CompProc(List[I], Key);
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
    if Index >= List.Count then
      Index := List.Count - 1;
    if Index >= 0 then
      if CompProc(List[Index], Key) > 0 then
        dec(Index);
  end;
begin
  QuickSearch(FList, Pos, Result);
end;

procedure TSortedList.Remove(Item: TSortedItem);
begin
  FList.Remove(Item);
end;

end.
