unit ec_rules;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils,
  graphics,

  ec_StrUtils,
  ec_Lists,
  ec_syntax_item,
  ec_syntax_format,
  ec_RegExpr,
  ec_parser_rule ;


type
  TecTagBlockCondition=class;//fwd

  TRuleCollectionItem = class(TSyntCollectionItem)
  private
    FStyleName: string;
    FBlockName: string;
    FFormat: TecSyntaxFormat;
    FBlock: TecTagBlockCondition;
    FStrictParent: Boolean;
    FNotParent: Boolean;
    FAlwaysEnabled: Boolean;
    FStatesAbsent: integer;
    FStatesAdd: integer;
    FStatesRemove: integer;
    FStatesPresent: integer;
    function GetStyleName: string;
    procedure SetStyleName(const Value: string);
    function GetBlockName: string;
    procedure SetBlockName(const Value: string);
    procedure SetNotParent(const Value: Boolean);
    procedure SetStrictParent(const Value: Boolean);
    procedure SetAlwaysEnabled(const Value: Boolean);
    function GetSyntOwner: TPersistent;
    procedure SetStatesAdd(const Value: integer);
    procedure SetStatesAbsent(const Value: integer);
    procedure SetStatesRemove(const Value: integer);
    procedure SetStatesPresent(const Value: integer);
  protected
    function ValidStyleName(const AStyleName: string; AStyle: TecSyntaxFormat): string;
    function ValidSetStyle(const AStyleName: string; var AStyleField: string; var AStyle: TecSyntaxFormat): string;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Loaded; override;
  public
    property Style: TecSyntaxFormat read FFormat write FFormat;
    property Block: TecTagBlockCondition read FBlock write FBlock;
    property SyntOwner: TPersistent read GetSyntOwner;
  published
    property StyleName: string read GetStyleName write SetStyleName;
    property BlockName: string read GetBlockName write SetBlockName;
    property StrictParent: Boolean read FStrictParent write SetStrictParent default False;
    property NotParent: Boolean read FNotParent write SetNotParent default False;
    property AlwaysEnabled: Boolean read FAlwaysEnabled write SetAlwaysEnabled default False;
    property StatesAdd: integer read FStatesAdd write SetStatesAdd default 0;
    property StatesRemove: integer read FStatesRemove write SetStatesRemove default 0;
    property StatesPresent: integer read FStatesPresent write SetStatesPresent default 0;
    property StatesAbsent: integer read FStatesAbsent write SetStatesAbsent default 0;
  end;



  TOnMatchToken = procedure(Sender: TObject; Client: TObject;
        const Text: ecString; APos: integer; var MatchLen: integer) of object;

  TecTokenRule = class(TRuleCollectionItem)
  private
    FRegExpr: TecRegExpr;
    FTokenType: integer;
    FOnMatchToken: TOnMatchToken;
    FColumnTo: integer;
    FColumnFrom: integer;
    function GetExpression: ecString;
    procedure SetExpression(const Value: ecString);
    procedure SetTokenType(const Value: integer);
    procedure SetColumnFrom(const Value: integer);
    procedure SetColumnTo(const Value: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    function GetIsInvalid: Boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Match(const Source: ecString; Pos: integer): integer;
  published
    property TokenType: integer read FTokenType write SetTokenType default 0;
    property Expression: ecString read GetExpression write SetExpression;
    property ColumnFrom: integer read FColumnFrom write SetColumnFrom;
    property ColumnTo: integer read FColumnTo write SetColumnTo;
    property OnMatchToken: TOnMatchToken read FOnMatchToken write FOnMatchToken;
  end;


  TecConditionCollection=class;//


  TecLineBreakPos = (lbTop, lbBottom);

  TecLineBreakBound = set of TecLineBreakPos; // for user blocks

  TecTagBlockType = (btTagDetect, btLineBreak, btRangeStart, btRangeEnd);

  TecDynamicHighlight = (dhNone, dhBound, dhRangeNoBound, dhRange);

  TecHighlightPos = (cpAny, cpBound, cpBoundTag, cpRange, cpBoundTagBegin, cpOutOfRange);

  TOnBlockCheck = procedure(Sender: TObject; Client: TObject;
      const Text: ecString; var RefIdx: integer; var Accept: Boolean) of object;

  TecAutoCloseMode = (acmDisabled, acmCloseNearest, acmCloseOpened);

  { TecTagBlockCondition }

  TecTagBlockCondition = class(TRuleCollectionItem)
  private
    FConditions: TecConditionCollection;
    FIdentIndex: integer;
    FLinePos: TecLineBreakPos;
    FBlockOffset: integer;
    FBlockEndCond: TecTagBlockCondition;
    FBlockType: TecTagBlockType;
    FBlockEndName: string;
    FEndOfTextClose: Boolean;
    FNotCollapsed: Boolean;
    FSameIdent: Boolean;
    FInvertColors: Boolean;
    FHighlight: Boolean;
    FDisplayInTree: Boolean;
    FNameFmt: ecString;
    FGroupFmt: ecString;
    FRefToCondEnd: Boolean;
    FDynHighlight: TecDynamicHighlight;
    FHighlightPos: TecHighlightPos;
    FDynSelectMin: Boolean;
    FCancelNextRules: Boolean;
    FOnBlockCheck: TOnBlockCheck;
    FDrawStaple: Boolean;
    FGroupIndex: integer;
    FCollapseFmt: ecString;
    FSelfClose: Boolean;
    FNoEndRule: Boolean;
    FGrammaRuleName: string;
    FGrammaRule: TParserRule;
    FTokenType: integer;
    FTreeItemStyle: string;
    FTreeItemStyleObj: TecSyntaxFormat;
    FTreeGroupStyle: string;
    FTreeGroupStyleObj: TecSyntaxFormat;
    FTreeGroupImage: integer;
    FTreeItemImage: integer;
    FUseCustomPen: Boolean;
    FPen: TPen;
    FIgnoreAsParent: Boolean;
    FAutoCloseText: ecString;
    FAutoCloseMode: TecAutoCloseMode;
    procedure ConditionsChanged(Sender: TObject);
    function GetBlockEndName: string;
    procedure SetBlockEndName(const Value: string);
    procedure SetBlockType(const Value: TecTagBlockType);
    procedure SetConditions(const Value: TecConditionCollection);
    procedure SetBlockEndCond(const Value: TecTagBlockCondition);
    procedure SetLinePos(const Value: TecLineBreakPos);
    procedure SetIdentIndex(const Value: integer);
    procedure SetBlockOffset(const Value: integer);
    procedure SetEndOfTextClose(const Value: Boolean);
    procedure SetNotCollapsed(const Value: Boolean);
    procedure SetSameIdent(const Value: Boolean);
    procedure SetHighlight(const Value: Boolean);
    procedure SetInvertColors(const Value: Boolean);
    procedure SetDisplayInTree(const Value: Boolean);
    procedure SetCancelNextRules(const Value: Boolean);
    procedure SetDynHighlight(const Value: TecDynamicHighlight);
    procedure SetDynSelectMin(const Value: Boolean);
    procedure SetGroupFmt(const Value: ecString);
    procedure SetHighlightPos(const Value: TecHighlightPos);
    procedure SetNameFmt(const Value: ecString);
    procedure SetRefToCondEnd(const Value: Boolean);
    procedure SetDrawStaple(const Value: Boolean);
    procedure SetCollapseFmt(const Value: ecString);
    procedure SetSelfClose(const Value: Boolean);
    procedure SetNoEndRule(const Value: Boolean);
    procedure SetGrammaRuleName(const Value: string);
    procedure SetTokenType(const Value: integer);
    function GetTreeItemStyle: string;
    procedure SetTreeItemStyle(const Value: string);
    function GetTreeGroupStyle: string;
    procedure SetTreeGroupStyle(const Value: string);
    procedure SetTreeGroupImage(const Value: integer);
    procedure SetTreeItemImage(const Value: integer);
    procedure SetPen(const Value: TPen);
    procedure SetUseCustomPen(const Value: Boolean);
    procedure SetIgnoreAsParent(const Value: Boolean);
    procedure SetAutoCloseText(Value: ecString);
    procedure SetAutoCloseMode(const Value: TecAutoCloseMode);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    procedure Loaded; override;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure SetRule(parserRule:TParserRule);
    function Check(const Source: ecString; Tags: TObject;
                   N: integer;  var RefIdx: integer): Boolean;
    function CheckOffset: integer;
    procedure RemoveInvalid(byFormat:TecStylesCollection);
    property BlockEndCond: TecTagBlockCondition read FBlockEndCond write SetBlockEndCond;
    property TreeItemStyleObj: TecSyntaxFormat read FTreeItemStyleObj;
    property TreeGroupStyleObj: TecSyntaxFormat read FTreeGroupStyleObj;
  published
    property BlockType: TecTagBlockType read FBlockType write SetBlockType default btRangeStart;
    property ConditionList: TecConditionCollection read FConditions write SetConditions;
    property IdentIndex: integer read FIdentIndex write SetIdentIndex default 0;
    property LinePos: TecLineBreakPos read FLinePos write SetLinePos default lbTop;
    property BlockOffset: integer read FBlockOffset write SetBlockOffset default 0;
    property BlockEnd: string read GetBlockEndName write SetBlockEndName;
    property EndOfTextClose: Boolean read FEndOfTextClose write SetEndOfTextClose default False;
    property NotCollapsed: Boolean read FNotCollapsed write SetNotCollapsed default False;
    property SameIdent: Boolean read FSameIdent write SetSameIdent default False;
    property Highlight: Boolean read FHighlight write SetHighlight default False;
    property InvertColors: Boolean read FInvertColors write SetInvertColors default False;
    property DisplayInTree: Boolean read FDisplayInTree write SetDisplayInTree default True;
    property NameFmt: ecString read FNameFmt write SetNameFmt;
    property GroupFmt: ecString read FGroupFmt write SetGroupFmt;
    property RefToCondEnd: Boolean read FRefToCondEnd write SetRefToCondEnd default False;
    property DynHighlight: TecDynamicHighlight read FDynHighlight write SetDynHighlight default dhNone;
    property HighlightPos: TecHighlightPos read FHighlightPos write SetHighlightPos;
    property DynSelectMin: Boolean read FDynSelectMin write SetDynSelectMin default False;
    property CancelNextRules: Boolean read FCancelNextRules write SetCancelNextRules default False;
    property DrawStaple: Boolean read FDrawStaple write SetDrawStaple default False;
    property GroupIndex: integer read FGroupIndex write FGroupIndex default 0;
    property CollapseFmt: ecString read FCollapseFmt write SetCollapseFmt;
    property OnBlockCheck: TOnBlockCheck read FOnBlockCheck write FOnBlockCheck;
    property SelfClose: Boolean read FSelfClose write SetSelfClose default False;
    // New in v2.20
    property NoEndRule: Boolean read FNoEndRule write SetNoEndRule default False;
    property GrammaRuleName: string read FGrammaRuleName write SetGrammaRuleName;
    property GrammaRule:TParserRule read FGrammaRule;
    property TokenType: integer read FTokenType write SetTokenType default -1;
    property TreeItemStyle: string read GetTreeItemStyle write SetTreeItemStyle;
    property TreeGroupStyle: string read GetTreeGroupStyle write SetTreeGroupStyle;
    property TreeItemImage: integer read FTreeItemImage write SetTreeItemImage default -1;
    property TreeGroupImage: integer read FTreeGroupImage write SetTreeGroupImage default -1;
    // New in 2.40
    property Pen: TPen read FPen write SetPen;
    property UseCustomPen: Boolean read FUseCustomPen write SetUseCustomPen default False;
    property IgnoreAsParent: Boolean read FIgnoreAsParent write SetIgnoreAsParent;
    // New in 2.50
    property AutoCloseMode: TecAutoCloseMode read FAutoCloseMode write SetAutoCloseMode default acmDisabled;
    property AutoCloseText: ecString read FAutoCloseText write SetAutoCloseText;
  end;




  { TecSyntToken }
PecSyntToken=^TecSyntToken;
TecSyntToken = record
strict private
  FRange: TRange;
  FTokenType: integer;
  FRule: TRuleCollectionItem;
  private
  function GetStyle: TecSyntaxFormat;
public

   procedure  Make(ARule: TRuleCollectionItem;
    AStartPos, AEndPos: integer;
    constref APointStart, APointEnd: TPoint);
  procedure CorrectEndRange(aEndPos:integer;constref aPointEnd:TPoint);
  procedure SetRule(rule:TRuleCollectionItem);
  procedure SetTokenType(&type:integer);
  function GetStr(const Source: ecString): ecString;inline;
  class operator Equal(constref A,B: TecSyntToken): boolean;
  property Style: TecSyntaxFormat read GetStyle;
  property Range:TRange read FRange;
  property TokenType:integer read FTokenType;
  property Rule :TRuleCollectionItem read FRule;

end;

TecTagConditionType = (tcEqual, tcNotEqual, tcMask, tcSkip, tcStrictMask);
  TecSingleTagCondition = class(TCollectionItem)
  private
    FTagList: TStrings;
    FCondType: TecTagConditionType;
    FTokenTypes: DWORD;
    procedure SetTagList(const Value: TStrings);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetTokenTypes(const Value: DWORD);
    procedure SetCondType(const Value: TecTagConditionType);
    procedure TagListChanged(Sender: TObject);
    function GetIgnoreCase: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CheckToken(const Source: ecString; constref Token: TecSyntToken): Boolean;
  published
    property TagList: TStrings read FTagList write SetTagList;
    property CondType: TecTagConditionType read FCondType write SetCondType default tcEqual;
    property TokenTypes: DWORD read FTokenTypes write SetTokenTypes default 0;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase default False;
  end;




    TecConditionCollection = class(TCollection)
  private
    FOwner: TecTagBlockCondition;
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TecSingleTagCondition;
  protected
    procedure Update(Item: TCollectionItem); override;
    function  GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TecTagBlockCondition);
    function Add: TecSingleTagCondition;
    property Items[Index: integer]: TecSingleTagCondition read GetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;



procedure SetDefaultModifiers(RE: TecRegExpr);

implementation
uses
  forms,
  ec_token_holder,
  ec_synt_collection,
  ec_SyntAnal,
  ec_SyntaxClient;


procedure SetDefaultModifiers(RE: TecRegExpr);
begin
  RE.ModifierI := True;
  RE.ModifierG := True;
  RE.ModifierS := False;
  RE.ModifierM := True;
  RE.ModifierX := True;
  RE.ModifierR := False;
end;


{ TecTokenRule }

procedure TecTokenRule.AssignTo(Dest: TPersistent);
var dst: TecTokenRule;
begin
  inherited;
  if Dest is TecTokenRule then
   begin
    dst := Dest as TecTokenRule;
    dst.FTokenType := FTokenType;
    dst.FRegExpr.Expression := Expression;
    dst.OnMatchToken := OnMatchToken;
    dst.ColumnFrom := ColumnFrom;
    dst.ColumnTo := ColumnTo;
   end;
end;



constructor TecTokenRule.Create(Collection: TCollection);
begin
  inherited;
  FBlock := nil;
  FFormat := nil;
  FRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FRegExpr);
end;

destructor TecTokenRule.Destroy;
begin
  FreeAndNil(FRegExpr);
  inherited;
end;

function TecTokenRule.GetExpression: ecString;
begin
  Result := FRegExpr.Expression;
end;

function TecTokenRule.GetIsInvalid: Boolean;
begin
  Result := FRegExpr.IsInvalid;
end;

function TecTokenRule.GetItemBaseName: string;
begin
  Result := 'Token rule';
end;

function TecTokenRule.Match(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

procedure TecTokenRule.SetColumnFrom(const Value: integer);
begin
  if FColumnFrom <> Value then
    begin
      FColumnFrom := Value;
      Changed(False);
    end;
end;

procedure TecTokenRule.SetColumnTo(const Value: integer);
begin
  if FColumnTo <> Value then
    begin
      FColumnTo := Value;
      Changed(False);
    end;
end;

procedure TecTokenRule.SetExpression(const Value: ecString);
begin
  try
    FRegExpr.Expression := Value;
  except
    Application.HandleException(Self);
  end;
  Changed(False);
end;

procedure TecTokenRule.SetTokenType(const Value: integer);
begin
  if FTokenType <> Value then
    begin
      FTokenType := Value;
      Changed(False);
    end;
end;




{ TecTagBlockCondition }

constructor TecTagBlockCondition.Create(Collection: TCollection);
begin
  inherited;
  FConditions := TecConditionCollection.Create(Self);
  FConditions.OnChange := ConditionsChanged;
  FBlockType := btRangeStart;
  FLinePos := lbTop;
  FDisplayInTree := True;
//  FHighlightPos := cpBound;
  FTokenType := -1;
  FTreeItemImage := -1;
  FTreeGroupImage := -1;
  FPen := TPen.Create;
  FPen.OnChange := ConditionsChanged;
end;

procedure TecTagBlockCondition.AssignTo(Dest: TPersistent);
var dst: TecTagBlockCondition;
begin
  inherited;
  if Dest is TecTagBlockCondition then
   begin
     dst := Dest as TecTagBlockCondition;
     dst.ConditionList := ConditionList;
     dst.FIdentIndex := IdentIndex;
     dst.FLinePos := LinePos;
     dst.FBlockOffset := BlockOffset;
     dst.FBlockType := BlockType;
     dst.BlockEnd := BlockEnd;
     dst.FEndOfTextClose := FEndOfTextClose;
     dst.FNotCollapsed := FNotCollapsed;
     dst.FSameIdent := FSameIdent;
     dst.Highlight := Highlight;
     dst.InvertColors := InvertColors;
     dst.DisplayInTree := DisplayInTree;
     dst.NameFmt := NameFmt;
     dst.GroupFmt := GroupFmt;
     dst.RefToCondEnd := RefToCondEnd;
     dst.DynHighlight := DynHighlight;
     dst.HighlightPos := HighlightPos;
     dst.DynSelectMin := DynSelectMin;
     dst.CancelNextRules := CancelNextRules;
     dst.DrawStaple := DrawStaple;
     dst.GroupIndex := GroupIndex;
     dst.OnBlockCheck := OnBlockCheck;
     dst.CollapseFmt := CollapseFmt;
     dst.FSelfClose := SelfClose;
     dst.FNoEndRule := NoEndRule;
     dst.GrammaRuleName := GrammaRuleName;
     dst.TokenType := TokenType;
     dst.TreeItemStyle := TreeItemStyle;
     dst.TreeGroupStyle := TreeGroupStyle;
     dst.TreeItemImage := TreeItemImage;
     dst.TreeGroupImage := TreeGroupImage;
     dst.Pen := Pen;
     dst.UseCustomPen := UseCustomPen;
     dst.IgnoreAsParent := IgnoreAsParent;
     dst.AutoCloseText := AutoCloseText;
     dst.AutoCloseMode := AutoCloseMode;
   end;
end;

function TecTagBlockCondition.Check(const Source: ecString;
  Tags: TObject; N: integer; var RefIdx: integer): Boolean;

var
   condIx, offs, idx, skipped, skip_cond: integer;
   tokens:TecParserResults;
   pToken:PecSyntToken;
   tagCondition, nextCondition:TecSingleTagCondition;
begin
  Result := False;
  tokens := TecParserResults(tags);
  offs := CheckOffset;
  skipped := 0;
  skip_cond := 0;
  condIx := 0;
  while condIx < ConditionList.Count do begin
   tagCondition:= ConditionList[condIx];
   idx := N - 1 - condIx - offs - skipped + skip_cond;
   if (tagCondition.CondType = tcSkip) and (condIx < ConditionList.Count - 1)
      and (ConditionList[condIx+1].CondType <> tcSkip) then
    begin
        inc(condIx);
        nextCondition:= ConditionList[condIx];
        inc(skip_cond);
        while (idx >= 0) do // and not ConditionList[condIx].CheckToken(Source, tokens[idx]) do
         begin
          pToken:=tokens.__UnsafeGetTagPtr(idx);
          if nextCondition.CheckToken(Source, pToken^) then break;

           if not tagCondition.CheckToken(Source, pToken^) then  exit(false);
           dec(idx);
           inc(skipped);
         end;
        if idx < 0 then exit(false);
        tagCondition:=nextCondition;
     end;

   with tagCondition do
    if (idx < 0) or not CheckToken(Source, tokens.__UnsafeGetTagPtr(idx)^) then exit(false);
   inc(condIx);
  end;//while

  Result := ConditionList.Count > 0;
//  if FRefToCondEnd then
  RefIdx := N - ConditionList.Count - offs - skipped + skip_cond;
//  else
//    RefIdx := N - 1 - offs;
end;

destructor TecTagBlockCondition.Destroy;
begin
  FreeAndNil(FConditions);
  FreeAndNil(FPen);
  inherited;
end;

procedure TecTagBlockCondition.SetRule(parserRule: TParserRule);
begin
  FGrammaRule:=parserRule;
end;

function TecTagBlockCondition.GetItemBaseName: string;
begin
  Result := 'Tag block rule';
end;

function TecTagBlockCondition.GetBlockEndName: string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := FBlockEndName
  else
   if Assigned(FBlockEndCond) then
    Result := FBlockEndCond.DisplayName
   else
    Result := '';
end;

procedure TecTagBlockCondition.SetBlockEndName(const Value: string);
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    FBlockEndName := Value
  else
    FBlockEndCond := TecTagBlockCondition(TecBlockRuleCollection(Collection).ItemByName(Value));
  Changed(False);
end;

procedure TecTagBlockCondition.Loaded;
var FSynt: TecSyntAnalyzer;
begin
  inherited;
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;
  if FBlockEndName <> '' then
    FBlockEndCond := TecTagBlockCondition(FSynt.BlockRules.ItemByName(FBlockEndName));
  if FTreeItemStyle <> '' then
    FTreeItemStyleObj := TecSyntaxFormat(FSynt.Formats.ItemByName(FTreeItemStyle));
  if FTreeGroupStyle <> '' then
    FTreeGroupStyleObj := TecSyntaxFormat(FSynt.Formats.ItemByName(FTreeGroupStyle));
end;

function TecTagBlockCondition.CheckOffset: integer;
begin
  Result := 0;
  if FRefToCondEnd then Exit;
  if FIdentIndex < 0 then Result := -FIdentIndex;
  if (FBlockOffset < 0) and (FBlockOffset < FIdentIndex) then
    Result := -FBlockOffset;
end;

procedure TecTagBlockCondition.RemoveInvalid(byFormat: TecStylesCollection);
begin

  if not byFormat.ValidItem(Style) then
     Style := nil;

 if not byFormat.ValidItem(TreeItemStyleObj) then
         FTreeItemStyleObj := nil;

      if not byFormat.ValidItem(TreeGroupStyleObj) then
         FTreeGroupStyleObj := nil;
end;

procedure TecTagBlockCondition.SetBlockType(const Value: TecTagBlockType);
begin
  FBlockType := Value;
  if FBlockType in [btTagDetect, btLineBreak] then
   begin
     FBlockOffset := 0;
     FBlockEndCond := nil;
   end;
  Changed(False);
end;

procedure TecTagBlockCondition.SetConditions(
  const Value: TecConditionCollection);
begin
  FConditions.Assign(Value);
end;

procedure TecTagBlockCondition.ConditionsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TecTagBlockCondition.SetBlockEndCond(
  const Value: TecTagBlockCondition);
begin
  if FBlockEndCond <> Value then
    begin
      FBlockEndCond := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetLinePos(const Value: TecLineBreakPos);
begin
  if FLinePos <> Value then
    begin
      FLinePos := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetIdentIndex(const Value: integer);
begin
  if FIdentIndex <> Value then
    begin
      FIdentIndex := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetBlockOffset(const Value: integer);
begin
  if FBlockOffset <> Value then
    begin
      FBlockOffset := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetEndOfTextClose(const Value: Boolean);
begin
  if FEndOfTextClose <> Value then
    begin
      FEndOfTextClose := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetNotCollapsed(const Value: Boolean);
begin
  if FNotCollapsed <> Value then
    begin
      FNotCollapsed := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetSameIdent(const Value: Boolean);
begin
  if FSameIdent <> Value then
    begin
      FSameIdent := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetHighlight(const Value: Boolean);
begin
  if FHighlight <> Value then
    begin
      FHighlight := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetInvertColors(const Value: Boolean);
begin
  if FInvertColors <> Value then
    begin
      FInvertColors := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDisplayInTree(const Value: Boolean);
begin
  if FDisplayInTree <> Value then
    begin
      FDisplayInTree := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetCancelNextRules(const Value: Boolean);
begin
  if FCancelNextRules <> Value then
    begin
      FCancelNextRules := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDynHighlight(
  const Value: TecDynamicHighlight);
begin
  if FDynHighlight <> Value then
    begin
      FDynHighlight := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDynSelectMin(const Value: Boolean);
begin
  if FDynSelectMin <> Value then
    begin
      FDynSelectMin := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetGroupFmt(const Value: ecString);
begin
  if FGroupFmt <> Value then
    begin
      FGroupFmt := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetHighlightPos(const Value: TecHighlightPos);
begin
  if FHighlightPos <> Value then
    begin
      FHighlightPos := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetNameFmt(const Value: ecString);
begin
  if FNameFmt <> Value then
    begin
      FNameFmt := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetRefToCondEnd(const Value: Boolean);
begin
  if FRefToCondEnd <> Value then
    begin
      FRefToCondEnd := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDrawStaple(const Value: Boolean);
begin
  if FDrawStaple <> Value then
    begin
      FDrawStaple := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetCollapseFmt(const Value: ecString);
begin
  if FCollapseFmt <> Value then
    begin
      FCollapseFmt := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetSelfClose(const Value: Boolean);
begin
  if FSelfClose <> Value then
    begin
      FSelfClose := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetNoEndRule(const Value: Boolean);
begin
  if FNoEndRule <> Value then
    begin
      FNoEndRule := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetGrammaRuleName(const Value: string);
begin
  if FGrammaRuleName <> Value then
   begin
    FGrammaRuleName := Value;
    FGrammaRule :=
      TecSyntAnalyzer(TSyntCollection(Collection).SyntOwner).
                Gramma.ParserRuleByName(Value);
   end;
end;

procedure TecTagBlockCondition.SetTokenType(const Value: integer);
begin
  if FTokenType <> Value then
    begin
      FTokenType := Value;
      Changed(False);
    end;
end;

function TecTagBlockCondition.GetTreeItemStyle: string;
begin
  Result := ValidStyleName(FTreeItemStyle, FTreeItemStyleObj);
end;

procedure TecTagBlockCondition.SetTreeItemStyle(const Value: string);
begin
  ValidSetStyle(Value, FTreeItemStyle, FTreeItemStyleObj);
end;

function TecTagBlockCondition.GetTreeGroupStyle: string;
begin
  Result := ValidStyleName(FTreeGroupStyle, FTreeGroupStyleObj);
end;

procedure TecTagBlockCondition.SetTreeGroupStyle(const Value: string);
begin
  ValidSetStyle(Value, FTreeGroupStyle, FTreeGroupStyleObj);
end;

procedure TecTagBlockCondition.SetTreeGroupImage(const Value: integer);
begin
  if FTreeGroupImage <> Value then
    begin
      FTreeGroupImage := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetTreeItemImage(const Value: integer);
begin
  if FTreeItemImage <> Value then
    begin
      FTreeItemImage := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TecTagBlockCondition.SetUseCustomPen(const Value: Boolean);
begin
  if FUseCustomPen <> Value then
    begin
      FUseCustomPen := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetIgnoreAsParent(const Value: Boolean);
begin
  if FIgnoreAsParent <> Value then
    begin
      FIgnoreAsParent := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetAutoCloseText(Value: ecString);
begin
  if Value = sLineBreak then
    Value := '';
  if FAutoCloseText <> Value then
    begin
      FAutoCloseText := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetAutoCloseMode(const Value: TecAutoCloseMode);
begin
  if FAutoCloseMode <> Value then
    begin
      FAutoCloseMode := Value;
      Changed(False);
    end;
end;



{ TRuleCollectionItem }

function TRuleCollectionItem.ValidStyleName(const AStyleName: string;
  AStyle: TecSyntaxFormat): string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  Result := '';
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := AStyleName
  else
   if Assigned(AStyle) then
    Result := AStyle.DisplayName;
end;

function TRuleCollectionItem.ValidSetStyle(const AStyleName: string;
  var AStyleField: string; var AStyle: TecSyntaxFormat): string;
var FSynt: TecSyntAnalyzer;
begin
  Result := '';
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    AStyleField := AStyleName
  else
    AStyle := TecSyntaxFormat(FSynt.Formats.ItemByName(AStyleName));
  Changed(False);
end;

function TRuleCollectionItem.GetStyleName: string;
begin
  Result := ValidStyleName(FStyleName, FFormat);
end;

procedure TRuleCollectionItem.SetStyleName(const Value: string);
begin
  ValidSetStyle(Value, FStyleName, FFormat);
end;

procedure TRuleCollectionItem.Loaded;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;
  if FStyleName <> '' then
    FFormat := TecSyntaxFormat(FSynt.Formats.ItemByName(FStyleName));
  if FBlockName <> '' then
    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(FBlockName));
end;

function TRuleCollectionItem.GetBlockName: string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := FBlockName
  else
   if Assigned(FBlock) then
    Result := FBlock.DisplayName
   else
    Result := '';
end;

procedure TRuleCollectionItem.SetBlockName(const Value: string);
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    FBlockName := Value
  else
   begin
//    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(Value));
    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(Value));
    Changed(False);
   end;
end;

procedure TRuleCollectionItem.AssignTo(Dest: TPersistent);
var dst: TRuleCollectionItem;
begin
  inherited;
  if Dest is TRuleCollectionItem then
    begin
      dst := Dest as TRuleCollectionItem;
      dst.StyleName := StyleName;
      dst.BlockName := BlockName;
      dst.StrictParent := StrictParent;
      dst.NotParent := NotParent;
      dst.AlwaysEnabled := AlwaysEnabled;
      dst.StatesAbsent := StatesAbsent;
      dst.StatesAdd := StatesAdd;
      dst.StatesRemove := StatesRemove;
      dst.StatesPresent := StatesPresent;
    end;
end;

procedure TRuleCollectionItem.SetNotParent(const Value: Boolean);
begin
  if FNotParent <> Value then
    begin
      FNotParent := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStrictParent(const Value: Boolean);
begin
  if FStrictParent <> Value then
    begin
      FStrictParent := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetAlwaysEnabled(const Value: Boolean);
begin
  if FAlwaysEnabled <> Value then
    begin
      FAlwaysEnabled := Value;
      Changed(False);
    end;
end;

function TRuleCollectionItem.GetSyntOwner: TPersistent;
begin
  Result := TSyntCollection(Collection).SyntOwner;
end;

procedure TRuleCollectionItem.SetStatesAdd(const Value: integer);
begin
  if FStatesAdd <> Value then
    begin
      FStatesAdd := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStatesAbsent(const Value: integer);
begin
  if FStatesAbsent <> Value then
    begin
      FStatesAbsent := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStatesRemove(const Value: integer);
begin
  if FStatesRemove <> Value then
    begin
      FStatesRemove := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStatesPresent(const Value: integer);
begin
  if FStatesPresent <> Value then
    begin
      FStatesPresent := Value;
      Changed(False);
    end;
end;


{ TecConditionCollection }

function TecConditionCollection.Add: TecSingleTagCondition;
begin
  Result := (inherited Add) as TecSingleTagCondition;
end;

constructor TecConditionCollection.Create(AOwner: TecTagBlockCondition);
begin
  inherited Create(TecSingleTagCondition);
  FOwner := AOwner;
  PropName := 'Conditions';
end;

function TecConditionCollection.GetItem(Index: integer): TecSingleTagCondition;
begin
  Result := (inherited Items[Index]) as TecSingleTagCondition;
end;

function TecConditionCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TecConditionCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TecSyntToken }

procedure TecSyntToken.Make(ARule: TRuleCollectionItem;
  AStartPos, AEndPos: integer;
  constref APointStart, APointEnd: TPoint);
begin
  FRange.StartPos := AStartPos;
  FRange.EndPos := AEndPos;
  FRange.PointStart := APointStart;
  FRange.PointEnd := APointEnd;
  FRule := ARule;
  if Assigned(ARule) then
    FTokenType := TecTokenRule(ARule).TokenType
  else
    FTokenType := 0;
end;

procedure TecSyntToken.CorrectEndRange(aEndPos: integer; constref aPointEnd: TPoint
  );
begin
  FRange.EndPos:= aEndPos;
  FRange.PointEnd:=aPointEnd;
end;

procedure TecSyntToken.SetRule(rule: TRuleCollectionItem);
begin
  FRule:=rule;
end;

procedure TecSyntToken.SetTokenType(&type: integer);
begin
  FTokenType:=&type;
end;

function TecSyntToken.GetStr(const Source: ecString): ecString;
begin
  with Range do
    Result := Copy(Source, StartPos + 1, EndPos - StartPos);
end;

class operator TecSyntToken.Equal(constref A, B: TecSyntToken): boolean;
begin
  Result:= false;
end;

function TecSyntToken.GetStyle: TecSyntaxFormat;
begin
  if Rule = nil then
    Result := nil
  else
    Result := Rule.Style;
end;


{ TecSingleTagCondition }

procedure TecSingleTagCondition.AssignTo(Dest: TPersistent);
var dst: TecSingleTagCondition;
begin
  if Dest is TecSingleTagCondition then
   begin
     dst := Dest as TecSingleTagCondition;
     dst.CondType := CondType;
     dst.TokenTypes := TokenTypes;
     dst.FTagList.Assign(FTagList);
     dst.IgnoreCase := IgnoreCase;
   end;
end;

function TecSingleTagCondition.CheckToken(const Source: ecString; constref Token: TecSyntToken): Boolean;
var s: ecString;
    i, N: integer;
    RE: TecRegExpr;
begin
  Result := False;
  if FTokenTypes <> 0 then
   begin
    Result := ((1 shl Token.TokenType) and FTokenTypes) <> 0;
    if FCondType = tcSkip then Exit;
    if not Result then
    case FCondType of
      tcStrictMask, tcMask, tcEqual: Exit;
      tcNotEqual:
        begin
          Result := True;
          Exit;
        end;
    end;
   end;
  if FTagList.Count > 0 then begin
    s := Token.GetStr(Source);
    s := Trim(s); //AT
    if FCondType in [tcMask, tcStrictMask] then
     begin
       RE := TecRegExpr.Create;
       SetDefaultModifiers(RE);
       try
         for i := 0 to FTagList.Count - 1 do
          begin
            RE.Expression := FTagList[i];
            if FCondType = tcMask then
              Result := RE.MatchLength(s, 1) > 0
            else
              begin
                N := 1;
                Result := RE.Match(s, N);
                if Result then
                  Result := N > Length(S);
              end;

            if Result then break;
          end;
       except
       end;
       FreeAndNil(RE);
     end else
     begin
       Result := FTagList.IndexOf(s) <> -1;
       if FCondType = tcNotEqual then Result := not Result;
     end;
   end else Result := FCondType <> tcNotEqual;
end;

constructor TecSingleTagCondition.Create(Collection: TCollection);
begin
  inherited;
  FCondType := tcEqual;
  FTagList := TStringList.Create;
  TStringList(FTagList).Sorted := true;
  TStringList(FTagList).Delimiter := ' ';
  TStringList(FTagList).Duplicates := dupIgnore;
  TStringList(FTagList).CaseSensitive := True;
  TStringList(FTagList).OnChange := TagListChanged;
  TStringList(FTagList).QuoteChar := ' ';
end;

destructor TecSingleTagCondition.Destroy;
begin
  FreeAndNil(FTagList);
  inherited;
end;

procedure TecSingleTagCondition.SetIgnoreCase(const Value: Boolean);
begin
  TStringList(FTagList).CaseSensitive := not Value;
end;

function TecSingleTagCondition.GetIgnoreCase: Boolean;
begin
  Result := not TStringList(FTagList).CaseSensitive;
end;

procedure TecSingleTagCondition.SetTagList(const Value: TStrings);
begin
  TStringList(FTagList).DelimitedText := Value.Text;
  Changed(False);
end;

procedure TecSingleTagCondition.SetTokenTypes(const Value: DWORD);
begin
  FTokenTypes := Value;
  Changed(False);
end;

procedure TecSingleTagCondition.SetCondType(const Value: TecTagConditionType);
begin
  FCondType := Value;
  Changed(False);
end;

procedure TecSingleTagCondition.TagListChanged(Sender: TObject);
begin
  Changed(False);
end;



end.

