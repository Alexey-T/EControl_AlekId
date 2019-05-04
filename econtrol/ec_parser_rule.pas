unit ec_parser_rule;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
    TParserRule=class;
    TParserRuleItem=class;

    { TParserRuleBranch }

  TParserRuleBranch = class
  private
    FItems: TList;
    FRule: TParserRule;
    function GetCount: integer;
    function GetItems(Index: integer): TParserRuleItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure _SetRule(rule:TParserRule);
    function IsValid: Boolean;
    procedure AddItem(const rule:TParserRuleItem);
    procedure Delete(atIx:integer);
    property Count: integer read GetCount;
    property Items[Index: integer]: TParserRuleItem read GetItems;
    property Rule: TParserRule read FRule;
  end;

  TParserRule = class
  private
    FBranches: TList;
    FName: string;
    FIndex: integer;
    function GetCount: integer;
    function GetBranches(Index: integer): TParserRuleBranch;
  public
    constructor Create;
    destructor Destroy; override;
    procedure _SetName(const name:string);
    procedure AddBranch( br: TParserRuleBranch);
    procedure _SetIndex(ix:integer);
    function IsValid: Boolean;

    property Count: integer read GetCount;
    property Branches[Index: integer]: TParserRuleBranch read GetBranches;
    property Name: string read FName;
    property Index:integer read FIndex;
  end;

    TParserItemType = (itTerminal,           // "aaa"
                     itTerminalNoCase,     // 'aaa'
                     itTokenRule,          // <aaa>
                     itParserRule);        //  aaa


  { TParserRuleItem }

  TParserRuleItem = class
  private
    FItemType: TParserItemType;
    FTerminal: string;
    FTokenType: integer;
    FParserRule: TParserRule;
    FBranch: TParserRuleBranch;
    FRepMin: integer;
    FRepMax: integer;
    FOwnRule: Boolean;
  public
    procedure _SetReptMax(max:integer);
    procedure _SetReptMin(min:integer);
    procedure _SetBranch(branch:TParserRuleBranch);
    procedure _Setup(typ:TParserItemType; const term:string; rule:TParserRule; ownRule:boolean);
    procedure _SetTokenType(typ:integer);
    procedure _SetRule(rule:TParserRule);
    constructor Create;
    destructor Destroy; override;
    function IsValid: Boolean;

    property ItemType: TParserItemType read FItemType;
    property Terminal: string read FTerminal;
    property TokenType: integer read FTokenType;
    property ParserRule: TParserRule read FParserRule;
    property Branch: TParserRuleBranch read FBranch;
    property RepMin: integer read FRepMin;
    property RepMax: integer read FRepMax;
    property IsSubRule: Boolean read FOwnRule;
  end;

implementation
uses Contnrs;

{ TParserRuleBranch }

constructor TParserRuleBranch.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TParserRuleBranch.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TParserRuleBranch._SetRule(rule: TParserRule);
begin
  FRule:=rule;
end;

function TParserRuleBranch.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TParserRuleBranch.GetItems(Index: integer): TParserRuleItem;
begin
  Result := TParserRuleItem(FItems[Index]);
end;

function TParserRuleBranch.IsValid: Boolean;
var i: integer;
begin
  for i := 0 to FItems.Count - 1 do
   if not Items[i].IsValid then
     begin
      Result := False;
      Exit;
     end;
  Result := True;
end;

procedure TParserRuleBranch.AddItem(const rule: TParserRuleItem);
begin
  FItems.Add(rule);
end;

procedure TParserRuleBranch.Delete(atIx: integer);
begin
  FItems.Delete(atIx);
end;

{ TParserRule }

constructor TParserRule.Create;
begin
  inherited Create;
  FBranches := TObjectList.Create;
end;

destructor TParserRule.Destroy;
begin
  FBranches.Free;
  inherited;
end;

procedure TParserRule._SetName(const name: string);
begin
  FName:=name;
end;

procedure TParserRule.AddBranch(br: TParserRuleBranch);
begin
  FBranches.Add(br);
end;

procedure TParserRule._SetIndex(ix: integer);
begin
  FIndex:=ix;
end;

function TParserRule.GetBranches(Index: integer): TParserRuleBranch;
begin
  Result := TParserRuleBranch(FBranches[Index]);
end;

function TParserRule.GetCount: integer;
begin
  Result := FBranches.Count;
end;

function TParserRule.IsValid: Boolean;
var i: integer;
begin
  for i := 0 to Count - 1 do
   if not Branches[i].IsValid then
    begin
     Result := False;
     Exit;
    end;
  Result := (Count > 1) or (Count = 1) and (Branches[0].Count > 0);
end;

{ TParserRuleItem }

procedure TParserRuleItem._SetReptMax(max: integer);
begin
  FRepMax:=max;
end;

procedure TParserRuleItem._SetReptMin(min: integer);
begin
 FRepMin:=min;
end;

procedure TParserRuleItem._SetBranch(branch: TParserRuleBranch);
begin
  FBranch:=branch;
end;

procedure TParserRuleItem._Setup(typ: TParserItemType; const term: string;
  rule: TParserRule; ownRule: boolean);
begin
  FItemType:=typ; FTerminal:=term; FParserRule:=rule; FOwnRule:=ownRule;
end;

procedure TParserRuleItem._SetTokenType(typ: integer);
begin
  FTokenType:=typ;
end;

procedure TParserRuleItem._SetRule(rule: TParserRule);
begin
  FParserRule:=rule;
end;

constructor TParserRuleItem.Create;
begin
  inherited Create;
  FTokenType := -1;
  FRepMin := 1;
  FRepMax := 1;
  FOwnRule := False;
end;

destructor TParserRuleItem.Destroy;
begin
  if FOwnRule then
    FParserRule.Free;
  inherited;
end;

function TParserRuleItem.IsValid: Boolean;
begin
  case FItemType of
    itTerminal,
    itTerminalNoCase: Result := FTerminal <> '';
    itTokenRule: Result := FTokenType <> -1;
    itParserRule: Result := FParserRule <> nil;
    else Result := False;
  end;
end;


end.

