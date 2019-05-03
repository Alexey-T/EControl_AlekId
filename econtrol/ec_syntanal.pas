{ *************************************************************************** }
{                                                                             }
{ EControl Syntax Editor SDK                                                  }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{ Ported to Lazarus:                                                          }
{     Alexey Torgashin, UVviewsoft.com                                        }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}

unit ec_SyntAnal;

interface
{$IFDEF DEBUGLOG}
{$DEFINE DEBUG}
{$ENDIF}
uses
  SysUtils,
  Classes,
  syncobjs,
  Graphics,
  Controls,
  ExtCtrls,
  //Contnrs,
  LazUTF8Classes, //TFileStreamUTF8
  ec_RegExpr,
  ec_StrUtils,
  ec_Lists,
  ec_token_holder,
  ec_syntax_item,
  ec_synt_collection,
  ec_rules,
  ec_syntax_format,
  ec_SyntGramma,
  ec_Async,
  ec_proc_StreamComponent;

type
  IecSyntClient = interface
    ['{045EAD6D-5584-4A60-849E-6B8994AA5B8F}']
    procedure FormatChanged; // Lexer properties changed (update without clear)
    procedure Finished;      // Compleat analysis
  end;






  TecSyntAnalyzer       = class;
  //TecParserResults      = class;
  //TecClientSyntAnalyzer = class;

  TecSyntaxManager      = class;
  TecSubAnalyzerRule    = class;
  TecTextRange        = class;




  TBoundDefEvent = procedure(Sender: TObject; Range: TecTextRange; var sIdx, eIdx: integer) of object;



// *******************************************************************
// description classes of text contents
// *******************************************************************





  { TecTextRange }

  TecTextRange = class(TSortedItem)
  private
    FCondIndex: integer;
    FEndCondIndex: integer;
    function GetLevel: integer;
    function GetIsClosed: Boolean;
  protected
    function GetKey: integer; override;
  public
    StartPos: integer;
    StartIdx, EndIdx: integer;
    IdentIdx: integer;
    Rule: TecTagBlockCondition;
    Parent: TecTextRange;
    Index: integer;
    procedure _SetEndConditionIndex(iValue:integer);
    constructor Create(AStartIdx, AStartPos: integer);
    property Level: integer read GetLevel;
    property IsClosed: Boolean read GetIsClosed;
    property CondIndex:integer read FCondIndex;
    property EndCondIndex:integer read FEndCondIndex;
  end;

  { TecSubLexerRange }

  TecSubLexerRange = record
  public
    Range: TRange;
    Rule: TecSubAnalyzerRule;   // Rule reference
    CondEndPos: integer;      // Start pos of the start condition
    CondStartPos: integer;    // End pos of the end condition
    class operator Equal(constref a, b: TecSubLexerRange): boolean;
  end;

// *******************************************************************
//                Rules for syntax interpretation
// *******************************************************************

TecSubLexerRanges =class (GRangeList<TecSubLexerRange>);

TecBlockRuleCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecTagBlockCondition;
  public
    constructor Create;
    function Add: TecTagBlockCondition;
    property Items[Index: integer]: TecTagBlockCondition read GetItem; default;
  end;

  // Token identification rule


  TecTokenRuleCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecTokenRule;
  public
    constructor Create;
    function Add: TecTokenRule;
    property Items[Index: integer]: TecTokenRule read GetItem; default;
  end;

  TecSubAnalyzerRule = class(TRuleCollectionItem)
  private
    FStartRegExpr: TecRegExpr;
    FEndRegExpr: TecRegExpr;
    FSyntAnalyzer: TecSyntAnalyzer;
    FFromTextBegin: Boolean;
    FToTextEnd: Boolean;
    FIncludeBounds: Boolean;
    function GetEndExpression: string;
    function GetStartExpression: string;
    procedure SetEndExpression(const Value: string);
    procedure SetStartExpression(const Value: string);
    procedure SetSyntAnalyzer(const Value: TecSyntAnalyzer);
    procedure SetFromTextBegin(const Value: Boolean);
    procedure SetToTextEnd(const Value: Boolean);
    procedure SetIncludeBounds(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function MatchStart(const Source: ecString; Pos: integer): integer;
    function MatchEnd(const Source: ecString; Pos: integer): integer;
  published
    property StartExpression: string read GetStartExpression write SetStartExpression;
    property EndExpression: string read GetEndExpression write SetEndExpression;
    property SyntAnalyzer: TecSyntAnalyzer read FSyntAnalyzer write SetSyntAnalyzer;
    property FromTextBegin: Boolean read FFromTextBegin write SetFromTextBegin default False;
    property ToTextEnd: Boolean read FToTextEnd write SetToTextEnd default False;
    property IncludeBounds: Boolean read FIncludeBounds write SetIncludeBounds default False;
  end;

  TecSubAnalyzerRules = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecSubAnalyzerRule;
  public
    constructor Create;
    function Add: TecSubAnalyzerRule;
    property Items[Index: integer]: TecSubAnalyzerRule read GetItem; default;
  end;

  { TecCodeTemplate }

  TecCodeTemplate = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FAdvanced: boolean;
    FCode: TStrings;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Advanced: Boolean read FAdvanced write FAdvanced;
    property Code: TStrings read FCode;
  end;

  TecCodeTemplates = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TecCodeTemplate;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TecCodeTemplate;
    property Items[Index: integer]: TecCodeTemplate read GetItem; default;
  end;

// *******************************************************************
//  Parser classes
//
// *******************************************************************


// *******************************************************************
//  Syntax analizer for single client
//            container of description objects
// *******************************************************************

// *******************************************************************
//  Syntax analizer
//            container of syntax rules
// *******************************************************************

  TLoadableComponent = class(TComponent)
  private
    FSkipNewName: Boolean;
    FFileName: string;
    FIgnoreAll: Boolean;
    FSaving: Boolean;
  protected
    procedure OnReadError(Reader: TReader; const Message: string;
                          var Handled: Boolean); virtual;
    function NotStored: Boolean;
  public
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromResourceID(Instance: Cardinal; ResID: Integer; ResType: string); virtual;
    procedure LoadFromResourceName(Instance: Cardinal; const ResName: string; ResType: string); virtual;
    procedure LoadFromStream(const Stream: TStream); virtual;
  protected
    procedure SetName(const NewName: TComponentName); override;
    property FileName: string read FFileName write LoadFromFile;
  end;

  TParseTokenEvent = procedure(Client: TTokenHolder; const Text: ecString; Pos: integer;
      var TokenLength: integer; var Rule: TecTokenRule) of object;

  TecParseProgressEvent = procedure(Sender: TObject; AProgress: integer) of object;

  { TecSyntAnalyzer }

  TecSyntAnalyzer = class(TLoadableComponent)
  private
    FClientList: TList;
    FMasters: TList;      // Master lexer, i.e. lexers that uses it
    FOnChange: TNotifyEvent;
    FSampleText: TStrings;
    FFormats: TecStylesCollection;
    FTokenRules: TecTokenRuleCollection;
    FBlockRules: TecBlockRuleCollection;
    FCodeTemplates: TecCodeTemplates;
    FExtentions: string;
    FLexerName: string;
    FCoping: Boolean;
    FSkipSpaces: Boolean;
    FSubAnalyzers: TecSubAnalyzerRules;
    FTokenTypeNames: TStrings;
    FFullRefreshSize: integer;

    FMarkedBlock: TecSyntaxFormat;
    FMarkedBlockName: string;
    FSearchMatch: TecSyntaxFormat;
    FSearchMatchName: string;
    FCurrentLine: TecSyntaxFormat;
    FCurrentLineName: string;
    FDefStyle: TecSyntaxFormat;
    FDefStyleName: string;
    FCollapseStyle: TecSyntaxFormat;
    FCollapseStyleName: string;
    FNotes: TStrings;
    FInternal: boolean;
    FRestartFromLineStart: Boolean;
    FParseEndOfLine: Boolean;
    FGrammaParser: TGrammaAnalyzer;
    FLineComment: ecString;
    FCharset: TFontCharSet;
    FSeparateBlocks: integer;
    FAlwaysSyncBlockAnal: Boolean;   // Indicates that blocks analysis may after tokens
    FOnGetCollapseRange: TBoundDefEvent;
    FOnCloseTextRange: TBoundDefEvent;
    FIdleAppendDelayInit: Cardinal;
    FIdleAppendDelay: Cardinal;
    FOnParseToken: TParseTokenEvent;

    procedure SetSampleText(const Value: TStrings);
    procedure FormatsChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure TokenRuleChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure BlocksChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure SubLexRuleChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure SetBlockRules(const Value: TecBlockRuleCollection);
    procedure SetCodeTemplates(const Value: TecCodeTemplates);
    procedure SetTokenRules(const Value: TecTokenRuleCollection);
    procedure SetFormats(const Value: TecStylesCollection);
    function GetUniqueName(const Base: string): string;
    procedure SetSkipSpaces(const Value: Boolean);
    procedure SetSubAnalyzers(const Value: TecSubAnalyzerRules);
    procedure SetTokenTypeNames(const Value: TStrings);

    function GetStyleName(const AName: string; const AStyle: TecSyntaxFormat): string;
    procedure SetMarkedBlock(const Value: TecSyntaxFormat);
    function GetMarkedBlockName: string;
    procedure SetMarkedBlockName(const Value: string);
    procedure SetSearchMatch(const Value: TecSyntaxFormat);
    function GetSearchMatchStyle: string;
    procedure SetSearchMatchStyle(const Value: string);
    procedure SetCurrentLine(const Value: TecSyntaxFormat);
    function GetCurrentLineStyle: string;
    procedure SetCurrentLineStyle(const Value: string);
    procedure SetNotes(const Value: TStrings);
    procedure SetInternal(const Value: boolean);
    procedure SetRestartFromLineStart(const Value: Boolean);
    procedure SetParseEndOfLine(const Value: Boolean);
    procedure TokenNamesChanged(Sender: TObject);
    procedure CompileGramma;
    procedure SetGrammar(const Value: TGrammaAnalyzer);
    procedure GrammaChanged(Sender: TObject);
    procedure SetDefStyle(const Value: TecSyntaxFormat);
    function GetDefaultStyleName: string;
    procedure SetDefaultStyleName(const Value: string);
    procedure SetLineComment(const Value: ecString);
    procedure DetectBlockSeparate;
    procedure SetAlwaysSyncBlockAnal(const Value: Boolean);
    function GetCollapseStyleName: string;
    procedure SetCollapseStyleName(const Value: string);
    procedure SetCollapseStyle(const Value: TecSyntaxFormat);
    function GetSeparateBlocks: Boolean;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; dynamic;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //function AddClient(const Client: IecSyntClient; ABuffer: TATStringBuffer): TecClientSyntAnalyzer;
    procedure ClearClientContents;
    procedure UpdateClients;
    function GetClients:TList;
    procedure AddMasterLexer(SyntAnal: TecSyntAnalyzer);
    procedure RemoveMasterLexer(SyntAnal: TecSyntAnalyzer);

    function GetToken(Client: TTokenHolder; const Source: ecString;
                       APos: integer; OnlyGlobal: Boolean): TecSyntToken; virtual;

    procedure SelectTokenFormat(Client: TTokenHolder; const Source: ecString;
                       OnlyGlobal: Boolean; N: integer = -1); virtual;

    procedure HighlightKeywords(Client: TTokenHolder; const Source: ecString;
                       OnlyGlobal: Boolean); virtual;

    property MarkedBlock: TecSyntaxFormat read FMarkedBlock write SetMarkedBlock;
    property SearchMatch: TecSyntaxFormat read FSearchMatch write SetSearchMatch;
    property CurrentLine: TecSyntaxFormat read FCurrentLine write SetCurrentLine;
    property DefStyle: TecSyntaxFormat read FDefStyle write SetDefStyle;
    property CollapseStyle: TecSyntaxFormat read FCollapseStyle write SetCollapseStyle;
    property SeparateBlockAnalysis: Boolean read GetSeparateBlocks;
    property _SeparateBlocks:integer read FSeparateBlocks write FSeparateBlocks;
  published
    property Formats: TecStylesCollection read FFormats write SetFormats;
    property TokenRules: TecTokenRuleCollection read FTokenRules write SetTokenRules;
    property BlockRules: TecBlockRuleCollection read FBlockRules write SetBlockRules;
    property CodeTemplates: TecCodeTemplates read FCodeTemplates write SetCodeTemplates;
    property SubAnalyzers: TecSubAnalyzerRules read FSubAnalyzers write SetSubAnalyzers;
    property SampleText: TStrings read FSampleText write SetSampleText;

    property TokenTypeNames: TStrings read FTokenTypeNames write SetTokenTypeNames;
    property Gramma: TGrammaAnalyzer read FGrammaParser write SetGrammar;

    property MarkedBlockStyle: string read GetMarkedBlockName write SetMarkedBlockName;
    property SearchMatchStyle: string read GetSearchMatchStyle write SetSearchMatchStyle;
    property CurrentLineStyle: string read GetCurrentLineStyle write SetCurrentLineStyle;
    property DefaultStyleName: string read GetDefaultStyleName write SetDefaultStyleName;
    property CollapseStyleName: string read GetCollapseStyleName write SetCollapseStyleName;

    property Extentions: string read FExtentions write FExtentions;
    property LexerName: string read FLexerName write FLexerName;

    property SkipSpaces: Boolean read FSkipSpaces write SetSkipSpaces default True;
    property FullRefreshSize: integer read FFullRefreshSize write FFullRefreshSize default 0;
    property Notes: TStrings read FNotes write SetNotes;
    property Internal: boolean read FInternal write SetInternal default False;
    property RestartFromLineStart: Boolean read FRestartFromLineStart write SetRestartFromLineStart default False;
    property ParseEndOfLine: Boolean read FParseEndOfLine write SetParseEndOfLine default False;
    property LineComment: ecString read FLineComment write SetLineComment;
    property Charset: TFontCharSet read FCharset write FCharset; //AT
    property AlwaysSyncBlockAnal: Boolean read FAlwaysSyncBlockAnal write SetAlwaysSyncBlockAnal default False;
    property IdleAppendDelay: Cardinal read FIdleAppendDelay write FIdleAppendDelay default 200;
    property IdleAppendDelayInit: Cardinal read FIdleAppendDelayInit write FIdleAppendDelayInit default 50;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetCollapseRange: TBoundDefEvent read FOnGetCollapseRange write FOnGetCollapseRange;
    property OnCloseTextRange: TBoundDefEvent read FOnCloseTextRange write FOnCloseTextRange;
    property OnParseToken: TParseTokenEvent read FOnParseToken write FOnParseToken;
  end;

  TLibSyntAnalyzer = class(TecSyntAnalyzer)
  protected
    FParent: TecSyntaxManager;
//    function GetChildParent: TComponent; override;
//    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(const Stream: TStream); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  end;

  TecSyntaxManager = class(TLoadableComponent)
  private
    FOnChange: TNotifyEvent;
    FList: TList;
    FCurrentLexer: TecSyntAnalyzer;
    FOnLexerChanged: TNotifyEvent;
    FModified: Boolean;
    function GeItem(Index: integer): TecSyntAnalyzer;
    function GetCount: integer;
    procedure SetCurrentLexer(const Value: TecSyntAnalyzer);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Changed; dynamic;
    procedure OnReadError(Reader: TReader; const Message: string;
                          var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    function FindAnalyzer(const LexerName: string): TecSyntAnalyzer;
    function AddAnalyzer: TecSyntAnalyzer;
    procedure Clear;
    procedure Move(CurIndex, NewIndex: Integer);

    property AnalyzerCount: integer read GetCount;
    property Analyzers[Index: integer]: TecSyntAnalyzer read GeItem;
    property FileName;
    property CurrentLexer: TecSyntAnalyzer read FCurrentLexer write SetCurrentLexer;
    property Modified: Boolean read FModified write FModified;
  published
    property OnLexerChanged: TNotifyEvent read FOnLexerChanged write FOnLexerChanged stored NotStored;
    property OnChange: TNotifyEvent read FOnChange write FOnChange stored NotStored;
  end;

  TecSyntStyles = class(TLoadableComponent)
  private
    FStyles: TecStylesCollection;
    procedure SetStyles(const Value: TecStylesCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Styles: TecStylesCollection read FStyles write SetStyles;
  end;


var
  OnLexerParseProgress: TecParseProgressEvent;


implementation

uses  Forms, Dialogs,
  Math,
  ec_SyntaxClient,
  ATStringProc_TextBuffer;

const
  SecDefaultTokenTypeNames = 'Unknown' + #13#10 +
                             'Comment' + #13#10 +
                             'Id'      + #13#10 +
                             'Symbol'  + #13#10 +
                             'String'  + #13#10 +
                             'Number'  + #13#10 +
                             'Preprocessor';


// Local copy of ecUpCase. it is faster, uses AnsiChar UpCase
function ecUpCase(ch: WideChar): char; inline;
begin
  Result:= System.UpCase(char(ch));
end;


function IsPosSorted(const A, B: TPoint; AllowEq: boolean): boolean; inline;
begin
  if A.Y<>B.Y then
    Result:= A.Y<B.Y
  else
    Result:= (A.X<B.X) or (AllowEq and (A.X=B.X));
end;


{ TecSubLexerRange }

class operator TecSubLexerRange.Equal(constref a, b: TecSubLexerRange): boolean;
begin
  Result := false;
end;


{ TecTextRange }

constructor TecTextRange.Create(AStartIdx, AStartPos: integer);
begin
  inherited Create;
  StartIdx := AStartIdx;
  StartPos := AStartPos;
  EndIdx := -1;
  FEndCondIndex := -1;
  Index := -1;
end;

function TecTextRange.GetIsClosed: Boolean;
begin
  Result := EndIdx <> -1;
end;

function TecTextRange.GetKey: integer;
begin
  Result := StartPos;
end;

procedure TecTextRange._SetEndConditionIndex(iValue: integer);
begin
  FEndCondIndex:=iValue;
end;

function TecTextRange.GetLevel: integer;
var prn: TecTextRange;
begin
  prn := Parent;
  Result := 0;
  while prn <> nil do
   begin
     inc(Result);
     prn := prn.Parent;
   end;
end;



{ TecBlockRuleCollection }

function TecBlockRuleCollection.Add: TecTagBlockCondition;
begin
  Result := inherited Add as TecTagBlockCondition;
end;

constructor TecBlockRuleCollection.Create;
begin
  inherited Create(TecTagBlockCondition);
end;

function TecBlockRuleCollection.GetItem(Index: integer): TecTagBlockCondition;
begin
  Result := inherited Items[Index] as TecTagBlockCondition;
end;

{ TecTokenRuleCollection }

function TecTokenRuleCollection.Add: TecTokenRule;
begin
  Result := inherited Add as TecTokenRule;
end;

constructor TecTokenRuleCollection.Create;
begin
  inherited Create(TecTokenRule);
end;

function TecTokenRuleCollection.GetItem(Index: integer): TecTokenRule;
begin
  Result := inherited Items[Index] as TecTokenRule;
end;




{ TecSyntAnalyzer }

constructor TecSyntAnalyzer.Create(AOwner: TComponent);
begin
  inherited;
  FClientList := TList.Create;
  FMasters := TList.Create;
  FSampleText := TStringList.Create;
  FTokenTypeNames := TStringList.Create;
  FTokenTypeNames.Text := SecDefaultTokenTypeNames;
  TStringList(FTokenTypeNames).OnChange := TokenNamesChanged;

  FFormats := TecStylesCollection.Create;
  FFormats.OnChange := FormatsChanged;
  FFormats.SyntOwner := Self;

  FTokenRules := TecTokenRuleCollection.Create;
  FTokenRules.OnChange := TokenRuleChanged;
  FTokenRules.SyntOwner := Self;

  FBlockRules := TecBlockRuleCollection.Create;
  FBlockRules.OnChange := BlocksChanged;
  FBlockRules.SyntOwner := Self;

  FSubAnalyzers := TecSubAnalyzerRules.Create;
  FSubAnalyzers.SyntOwner := Self;
  FSubAnalyzers.OnChange := SubLexRuleChanged;

  FMarkedBlock := FFormats.Add as TecSyntaxFormat;
  FMarkedBlock.BgColor := clHighlight;
  FMarkedBlock.Font.Color := clHighlightText;
  FMarkedBlock.FormatType := ftColor;
  FMarkedBlock.DisplayName := 'Marked block';
  FMarkedBlock._SetIsBlock(True);

  FCodeTemplates := TecCodeTemplates.Create(Self);
  FSkipSpaces := True;

  FNotes := TStringList.Create;

  FGrammaParser := TGrammaAnalyzer.Create;
  FGrammaParser.OnChange := GrammaChanged;

  FIdleAppendDelayInit := 50;
  FIdleAppendDelay := 200;
end;

destructor TecSyntAnalyzer.Destroy;
begin
  FBlockRules.OnChange := nil;
  FTokenRules.OnChange := nil;
  FFormats.OnChange := nil;
  FSubAnalyzers.OnChange := nil;
  TStringList(FTokenTypeNames).OnChange:= nil;
  FGrammaParser.OnChange := nil;

  FreeAndNil(FFormats);
  FreeAndNil(FMasters);
  FreeAndNil(FSampleText);
  FreeAndNil(FBlockRules);
  FreeAndNil(FTokenRules);
  FreeAndNil(FCodeTemplates);
  FreeAndNil(FTokenTypeNames);
  FreeAndNil(FSubAnalyzers);
  FreeAndNil(FNotes);
  FreeAndNil(FGrammaParser);
  inherited;
  FreeAndNil(FClientList);
end;

procedure TecSyntAnalyzer.Assign(Source: TPersistent);
var Src: TecSyntAnalyzer;
    i: integer;
begin
  if not (Source is TecSyntAnalyzer) then Exit;
  Src := Source as TecSyntAnalyzer;
//  ClearClientContents;
  FCoping := True;
  try
    FAlwaysSyncBlockAnal := Src.FAlwaysSyncBlockAnal;
    Extentions := Src.Extentions;
    LexerName := Src.LexerName;
    SkipSpaces := Src.SkipSpaces;
    SampleText := Src.SampleText;
    FullRefreshSize := Src.FullRefreshSize;
    Formats := Src.Formats;
    MarkedBlockStyle := Src.MarkedBlockStyle;
    SearchMatchStyle := Src.SearchMatchStyle;
    CurrentLineStyle := Src.CurrentLineStyle;
    DefaultStyleName := Src.DefaultStyleName;
    CollapseStyleName := Src.CollapseStyleName;
    BlockRules := Src.BlockRules;
    TokenRules := Src.TokenRules;
    CodeTemplates := Src.CodeTemplates;
    SubAnalyzers := Src.SubAnalyzers;
//    DefaultStyle := Src.DefaultStyle;
    TokenTypeNames := Src.TokenTypeNames;
    Notes := Src.Notes;
    Internal := Src.Internal;
    Gramma := Src.Gramma;
    RestartFromLineStart := Src.RestartFromLineStart;
    ParseEndOfLine := Src.ParseEndOfLine;
    LineComment := Src.LineComment;
    FIdleAppendDelayInit := Src.FIdleAppendDelayInit;
    FIdleAppendDelay := Src.FIdleAppendDelay;
    for i := 0 to BlockRules.Count - 1 do
     begin
       BlockRules[i].BlockEnd := Src.BlockRules[i].BlockEnd;
       BlockRules[i].BlockName := Src.BlockRules[i].BlockName;
     end;
  finally
    FCoping := False;
    ClearClientContents;
  end;
  UpdateClients;
  for i := 0 to FClientList.Count - 1 do
   TecClientSyntAnalyzer(FClientList[i]).HandleAddWork();
end;

procedure TecSyntAnalyzer.HighlightKeywords(Client: TTokenHolder;
  const Source: ecString; OnlyGlobal: Boolean);
var i, N, ki, RefIdx: integer;
    Accept: Boolean;
    Tag: TecSyntToken;
begin
  N := Client.TagCount;
  for i := 0 to FBlockRules.Count - 1 do
   with FBlockRules[i] do
    if Enabled and (BlockType = btTagDetect) and
       (Block = nil) and (GrammaRule = nil) then
      begin
       if OnlyGlobal and not AlwaysEnabled then Continue;
       RefIdx := 0;
       Accept := Check(Source, TecClientSyntAnalyzer(Client), N, RefIdx);
       if Assigned(OnBlockCheck) then
         OnBlockCheck(FBlockRules[i], TecClientSyntAnalyzer(Client), Source, RefIdx, Accept);
       if Accept then
         begin
           if RefToCondEnd then ki := RefIdx - IdentIndex
             else ki := N - 1 - CheckOffset - IdentIndex;

           Tag := TecClientSyntAnalyzer(Client).Tags[ki];
           Tag.SetRule(FBlockRules[i]);
           if TokenType >= 0 then
              Tag.SetTokenType(TokenType);
           TecParserResults(Client).SetTags(ki, tag);// Tags[ki] := Tag;

           if CancelNextRules then Exit;   // 2.27
         end;
      end;
end;

procedure TecSyntAnalyzer.SelectTokenFormat(Client: TTokenHolder;
  const Source: ecString; OnlyGlobal: Boolean; N: integer);
var i, li, ki, strt, RefIdx: integer;
    Range: TecTextRange;
    Accept: Boolean;
    RClient: TecClientSyntAnalyzer;

  function CheckIndex(Idx: integer): Boolean; inline;
  begin
   Result := (Idx >= 0) and (Idx < N);
  end;

begin
  if N = -1 then
    N := Client.TagCount;
  if not (Client is TecClientSyntAnalyzer)  then Exit;
  RClient := TecClientSyntAnalyzer(Client);
  RClient._SetStartSeparateWork(N + 1);
  try
    for i := 0 to FBlockRules.Count - 1 do
      with FBlockRules[i] do
       if not SeparateBlockAnalysis or (BlockType <> btTagDetect) or
          (Block = nil) or (GrammaRule = nil) then
       if RClient.IsEnabled(FBlockRules[i], OnlyGlobal) then
        begin
          RefIdx := 0;
          if GrammaRule <> nil then
           begin
             RefIdx := FGrammaParser.TestRule(N - 1, GrammaRule, Client);
             Accept := RefIdx <> -1;
           end else
             Accept := Check(Source, RClient, N, RefIdx);

          if Assigned(OnBlockCheck) then
            OnBlockCheck(FBlockRules[i], RClient, Source, RefIdx, Accept);

          if Accept then begin
           TecClientSyntAnalyzer(Client).ApplyStates(FBlockRules[i]);
           if RefToCondEnd then strt := RefIdx
             else strt := N - 1 - CheckOffset;
      //    strt := N - 1 - CheckOffset;
           ki := strt - IdentIndex;
           if CheckIndex(ki) then
            case BlockType of
               btTagDetect: // Tag detection
                 if not RClient.DetectTag(FBlockRules[i], ki) then
                   Continue;
               btRangeStart: // Start of block
                begin
                  if FBlockRules[i].SelfClose then
                    RClient.CloseRange(FBlockRules[i], strt);
                  li := strt - BlockOffset;
                  if CheckIndex(li) then
                   begin
                    Range := TecTextRange.Create(li, RClient.Tags[li].Range.StartPos);
                    Range.IdentIdx := ki;
                    Range.Rule := FBlockRules[i];
                    Range.FCondIndex := N - 1;
                    if NoEndRule then
                     begin
                      Range.EndIdx := N - 1 - CheckOffset;
                      Range.FEndCondIndex := N - 1;
                      Range.StartIdx := RefIdx - BlockOffset;
                     end;
                    RClient.AddRange(Range);
                   end;
                end;
               btRangeEnd:  // End of block
                 if not RClient.CloseRange(FBlockRules[i], strt) then
                   Continue;
               btLineBreak:
                 begin
                   //AT: deleted support for line separators
                 end;
            end;
           if CancelNextRules then Break;
          end;
        end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TecSyntAnalyzer.SetSampleText(const Value: TStrings);
begin
  FSampleText.Assign(Value);
end;

function TecSyntAnalyzer.GetToken(Client: TTokenHolder; const Source: ecString;
  APos: integer; OnlyGlobal: Boolean): TecSyntToken;
var i, N, lp,trCount: integer;
    Rule: TecTokenRule;
    PntStart, PntEnd: TPoint;
    buf:TATStringBuffer;
    cli:TecClientSyntAnalyzer;
begin
  PntStart.X := -1;
  PntStart.Y := -1;
  Result := TecSyntToken.Create(nil, -1, -1, PntStart, PntStart);
  cli:=TecClientSyntAnalyzer(client);
  buf := cli.Buffer;
  if Assigned(FOnParseToken) then begin
      N := 0;
      Rule := nil;
      FOnParseToken(Client, Source, APos, N, Rule);
      if Assigned(Rule) then
        Result := TecSyntToken.Create(Rule,
               APos - 1,
               APos + N - 1,
               buf.StrToCaret(APos-1),
               buf.StrToCaret(APos+N-1)
               );
      Exit;
    end;

  lp := 0;
  trCount:=FTokenRules.Count - 1;
  for i := 0 to trCount do  begin
      Rule := FTokenRules[i];
      if cli.IsEnabled(Rule, OnlyGlobal) then
        with Rule do  begin
            if (ColumnFrom > 0) or (ColumnTo > 0) then
              begin
               if lp = 0 then
                 lp := buf.OffsetToDistanceFromLineStart(APos - 1)+1;

               if (ColumnFrom > 0) and (lp < ColumnFrom) or
                  (ColumnTo > 0) and (lp > ColumnTo) then
                  Continue;
              end;
            N := Match(Source, APos);
            if Assigned(OnMatchToken) then
              OnMatchToken(Rule, Client, Source, APos, N);
            if N > 0 then
              begin
                cli.ApplyStates(Rule);

                PntStart := buf.StrToCaret(APos-1);

                //optimization: if token is short, get PntEnd simpler
                if PntStart.X + N >= buf.LineLength(PntStart.Y) then
                  PntEnd := buf.StrToCaret(APos+N-1)
                else
                begin
                  PntEnd.Y := PntStart.Y;
                  PntEnd.X := PntStart.X + N;
                end;

                Result := TecSyntToken.Create(Rule,
                       APos - 1,
                       APos + N - 1,
                       PntStart,
                       PntEnd
                       );
                Exit;
              end;
          end;
    end;
end;

procedure TecSyntAnalyzer.FormatsChanged(Sender: TCollection; Item: TSyntCollectionItem);
var i: integer;
    blockRule:TecTagBlockCondition;
begin
  ClearClientContents;
  if Item = nil then
   begin
    if not FFormats.ValidItem(FMarkedBlock) then FMarkedBlock := nil;
    if not FFormats.ValidItem(FCurrentLine) then FCurrentLine := nil;
    if not FFormats.ValidItem(FDefStyle) then FDefStyle := nil;
    if not FFormats.ValidItem(FSearchMatch) then FSearchMatch := nil;
    for i := 0 to FBlockRules.Count - 1 do   begin
        blockRule:=FBlockRules[i];
        blockRule.RemoveInvalid(FFormats);
     end;

    for i := 0 to FTokenRules.Count - 1 do
     if not FFormats.ValidItem(FTokenRules[i].Style) then FTokenRules[i].Style := nil;

    for i := 0 to FSubAnalyzers.Count - 1 do
     if not FFormats.ValidItem(FSubAnalyzers[i].Style) then FSubAnalyzers[i].Style := nil;
   end;
//  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.BlocksChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
var i: integer;
begin
  ClearClientContents;
  if Item = nil then
   begin
    for i := 0 to FBlockRules.Count - 1 do
     begin
      if not FBlockRules.ValidItem(FBlockRules[i].Block) then FBlockRules[i].Block := nil;
      if not FBlockRules.ValidItem(FBlockRules[i].BlockEndCond) then FBlockRules[i].BlockEndCond := nil;
     end;
    for i := 0 to FTokenRules.Count - 1 do
     if not FBlockRules.ValidItem(FTokenRules[i].Block) then FTokenRules[i].Block := nil;
    for i := 0 to FSubAnalyzers.Count - 1 do
     if not FSubAnalyzers.ValidItem(FSubAnalyzers[i].Block) then FSubAnalyzers[i].Block := nil;
   end;
//  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.ClearClientContents;
var i:integer;
begin
  if FCoping then Exit;
  FCoping := True;
  try
    for i := 0 to FClientList.Count - 1 do
     with TecClientSyntAnalyzer(FClientList[i]) do
      begin
        Clear();
        //IdleAppend; //AlekXL Clear aready calls IdleAppend
      end;
    for i := 0 to FMasters.Count - 1 do
      TecSyntAnalyzer(FMasters[i]).ClearClientContents;
  finally
    FCoping := False;
  end;
  UpdateClients;
end;

procedure TecSyntAnalyzer.UpdateClients;
var i:integer;
begin
  if FCoping then Exit;
  FCoping := True;
  try
    for i := 0 to FClientList.Count - 1 do
     with TecClientSyntAnalyzer(FClientList[i]) do
         HandleFormatChanged();

    for i := 0 to FMasters.Count - 1 do
      TecSyntAnalyzer(FMasters[i]).UpdateClients;
  finally
    FCoping := False;
  end;
end;

function TecSyntAnalyzer.GetClients: TList;
begin
  result := FClientList;
end;

procedure TecSyntAnalyzer.Loaded;
var i: integer;
begin
  inherited;
  MarkedBlockStyle := FMarkedBlockName;
  SearchMatchStyle := FSearchMatchName;
  CurrentLineStyle := FCurrentLineName;
  CollapseStyleName := FCollapseStyleName;
  DefaultStyleName := FDefStyleName;

  FFormats.Loaded;
  FBlockRules.Loaded;
  FTokenRules.Loaded;
  FSubAnalyzers.Loaded;
  CompileGramma;
  DetectBlockSeparate;
  for i := 0 to FMasters.Count - 1 do
    TecSyntAnalyzer(FMasters[i]).DetectBlockSeparate;
end;

procedure TecSyntAnalyzer.SetBlockRules(const Value: TecBlockRuleCollection);
begin
  FBlockRules.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.SetCodeTemplates(const Value: TecCodeTemplates);
begin
  FCodeTemplates.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.SetTokenRules(const Value: TecTokenRuleCollection);
begin
  FTokenRules.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.SetFormats(const Value: TecStylesCollection);
begin
  FFormats.Assign(Value);
end;

function TecSyntAnalyzer.GetUniqueName(const Base: string): string;
var n: integer;
begin
  n := 1;
  if Owner = nil then Result := Base + '1' else
  repeat
   Result := Base + IntToStr(n);
   inc(n);
  until Owner.FindComponent(Result) = nil;
end;

procedure TecSyntAnalyzer.SetSkipSpaces(const Value: Boolean);
begin
  if FSkipSpaces <> Value then
   begin
     FSkipSpaces := Value;
     ClearClientContents;
   end;
end;

procedure TecSyntAnalyzer.SetSubAnalyzers(const Value: TecSubAnalyzerRules);
begin
  FSubAnalyzers.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.Notification(AComponent: TComponent;
  Operation: TOperation);
var i: integer;
begin
  inherited;
  if (Operation = opRemove)  and (AComponent <> Self) and (aComponent is TecSyntAnalyzer) and
     Assigned(FSubAnalyzers) and Assigned(FMasters) then
   begin
     for i := 0 to FSubAnalyzers.Count - 1 do
      if FSubAnalyzers[i].FSyntAnalyzer = AComponent then
       FSubAnalyzers[i].FSyntAnalyzer := nil;
     FMasters.Remove(AComponent);
   end;
end;

procedure TecSyntAnalyzer.SubLexRuleChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
begin
  DetectBlockSeparate;
  ClearClientContents;
  Change;
end;

procedure TecSyntAnalyzer.AddMasterLexer(SyntAnal: TecSyntAnalyzer);
begin
  if Assigned(SyntAnal) and (SyntAnal <> Self) and
     (FMasters.IndexOf(SyntAnal) = -1) then
   begin
     FMasters.Add(SyntAnal);
     SyntAnal.FreeNotification(Self);
   end;
end;

procedure TecSyntAnalyzer.RemoveMasterLexer(SyntAnal: TecSyntAnalyzer);
begin
  FMasters.Remove(SyntAnal);
end;

procedure TecSyntAnalyzer.TokenRuleChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
begin
  DetectBlockSeparate;
  ClearClientContents;
  Change;
end;

procedure TecSyntAnalyzer.SetTokenTypeNames(const Value: TStrings);
begin
  FTokenTypeNames.Assign(Value);
end;

procedure TecSyntAnalyzer.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TecSyntAnalyzer.SetSearchMatch(const Value: TecSyntaxFormat);
begin
  if FSearchMatch = Value then Exit;
  FSearchMatch := Value;
  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.SetMarkedBlock(const Value: TecSyntaxFormat);
begin
  if FMarkedBlock = Value then Exit;
  FMarkedBlock := Value;
  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.SetCurrentLine(const Value: TecSyntaxFormat);
begin
  if FCurrentLine = Value then Exit;
  FCurrentLine := Value;
  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.SetDefStyle(const Value: TecSyntaxFormat);
begin
  if FDefStyle = Value then Exit;
  FDefStyle := Value;
  UpdateClients;
  Change;
end;

function TecSyntAnalyzer.GetStyleName(const AName: string; const AStyle: TecSyntaxFormat): string;
begin
  if csLoading in ComponentState then
    Result := AName
  else
   if Assigned(AStyle) then
    Result := AStyle.DisplayName
   else
    Result := '';
end;

function TecSyntAnalyzer.GetMarkedBlockName: string;
begin
  Result := GetStyleName(FMarkedBlockName, FMarkedBlock);
end;

procedure TecSyntAnalyzer.SetMarkedBlockName(const Value: string);
begin
  if csLoading in ComponentState then
    FMarkedBlockName := Value
  else
    MarkedBlock := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

function TecSyntAnalyzer.GetSearchMatchStyle: string;
begin
  Result := GetStyleName(FSearchMatchName, FSearchMatch);
end;

procedure TecSyntAnalyzer.SetSearchMatchStyle(const Value: string);
begin
  if csLoading in ComponentState then
    FSearchMatchName := Value
  else
    FSearchMatch := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

function TecSyntAnalyzer.GetCurrentLineStyle: string;
begin
  Result := GetStyleName(FCurrentLineName, FCurrentLine);
end;

procedure TecSyntAnalyzer.SetCurrentLineStyle(const Value: string);
begin
  if csLoading in ComponentState then
    FCurrentLineName := Value
  else
    FCurrentLine := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

function TecSyntAnalyzer.GetDefaultStyleName: string;
begin
  Result := GetStyleName(FDefStyleName, FDefStyle);
end;

procedure TecSyntAnalyzer.SetDefaultStyleName(const Value: string);
begin
  if csLoading in ComponentState then
    FDefStyleName := Value
  else
    FDefStyle := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

procedure TecSyntAnalyzer.SetNotes(const Value: TStrings);
begin
  FNotes.Assign(Value);
end;

procedure TecSyntAnalyzer.SetInternal(const Value: boolean);
begin
  FInternal := Value;
end;

procedure TecSyntAnalyzer.SetRestartFromLineStart(const Value: Boolean);
begin
  FRestartFromLineStart := Value;
end;

procedure TecSyntAnalyzer.SetParseEndOfLine(const Value: Boolean);
begin
  if FParseEndOfLine <> Value then
    begin
      FParseEndOfLine := Value;
      ClearClientContents;
    end;
end;

procedure TecSyntAnalyzer.CompileGramma;
var i, brCount : integer;
    bRule:TecTagBlockCondition;
begin
  FGrammaParser.CompileGramma(FTokenTypeNames);
  brCount:=FBlockRules.Count;
  for i := 0 to brCount - 1 do begin
    bRule:=FBlockRules[i];
    bRule.SetRule(
     FGrammaParser.ParserRuleByName(bRule.GrammaRuleName) );
  end;
end;

procedure TecSyntAnalyzer.TokenNamesChanged(Sender: TObject);
begin
  if (csLoading in self.ComponentState ) then Exit;
  CompileGramma;
  Change;
end;

procedure TecSyntAnalyzer.SetGrammar(const Value: TGrammaAnalyzer);
begin
  FGrammaParser.Assign(Value);
  CompileGramma;
end;

procedure TecSyntAnalyzer.GrammaChanged(Sender: TObject);
begin
  if (csLoading in ComponentState ) then exit;
  CompileGramma;
end;

procedure TecSyntAnalyzer.SetLineComment(const Value: ecString);
begin
  FLineComment := Value;
end;

function TecSyntAnalyzer.GetSeparateBlocks: Boolean;
  function HasStateModif(List: TCollection): Boolean;
  var i: integer;
  begin
    for i := 0 to List.Count - 1 do
      with TRuleCollectionItem(List.Items[i]) do
        if (StatesAdd <> 0) or (StatesRemove <> 0) then
          begin
            Result := True;
            Exit;
          end;
    Result := False;
  end;
var i: integer;
begin
  if FSeparateBlocks = 0 then
    begin
      Result := not FAlwaysSyncBlockAnal and
                not HasStateModif(FBlockRules) and
                not HasStateModif(FSubAnalyzers);
      if Result then
        for i := 0 to TokenRules.Count - 1 do
          if TokenRules[i].Block <> nil then
            begin
              Result := False;
              Break;
            end;
      if Result then
        for i := 0 to SubAnalyzers.Count - 1 do
          if (SubAnalyzers[i].SyntAnalyzer <> nil) and
             not SubAnalyzers[i].SyntAnalyzer.SeparateBlockAnalysis then
            begin
              Result := False;
              Break;
            end;
      if Result then
        FSeparateBlocks := 1
      else
        FSeparateBlocks := 2;
    end
  else
    Result := FSeparateBlocks = 1;
end;

procedure TecSyntAnalyzer.DetectBlockSeparate;
begin
  FSeparateBlocks := 0;
end;

procedure TecSyntAnalyzer.SetAlwaysSyncBlockAnal(const Value: Boolean);
begin
  FAlwaysSyncBlockAnal := Value;
  if FAlwaysSyncBlockAnal and SeparateBlockAnalysis then
   begin
    DetectBlockSeparate;
    ClearClientContents;
   end;
end;

function TecSyntAnalyzer.GetCollapseStyleName: string;
begin
  Result := GetStyleName(FCollapseStyleName, FCollapseStyle);
end;

procedure TecSyntAnalyzer.SetCollapseStyleName(const Value: string);
begin
  if csLoading in ComponentState then
    FCollapseStyleName := Value
  else
    FCollapseStyle := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

procedure TecSyntAnalyzer.SetCollapseStyle(const Value: TecSyntaxFormat);
begin
  if FCollapseStyle <> Value then
    begin
      FCollapseStyle := Value;
      UpdateClients;
      Change;
    end;
end;

{ TecCodeTemplate }

constructor TecCodeTemplate.Create(Collection: TCollection);
begin
  inherited;
  FName:= '';
  FDescription:= '';
  FAdvanced:= false;
  FCode:= TStringList.Create;
end;

destructor TecCodeTemplate.Destroy;
begin
  FreeAndNil(FCode);
  inherited;
end;

function TecCodeTemplate.GetDisplayName: string;
begin
  Result := FName;
end;


{ TecCodeTemplates }

function TecCodeTemplates.Add: TecCodeTemplate;
begin
  Result := TecCodeTemplate(inherited Add);
end;

constructor TecCodeTemplates.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TecCodeTemplate);
end;

function TecCodeTemplates.GetItem(Index: integer): TecCodeTemplate;
begin
  Result := TecCodeTemplate(inherited Items[Index]);
end;

{ TecSyntaxManager }

function TecSyntaxManager.AddAnalyzer: TecSyntAnalyzer;
begin
  Result := TLibSyntAnalyzer.Create(Owner);
  Result.Name := Result.GetUniqueName('SyntAnal');
  Result.SetParentComponent(Self);
  FModified := True;
end;

procedure TecSyntaxManager.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList[0]).Free;
  end;

  Changed;
  FModified := True;
end;

constructor TecSyntaxManager.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
  FModified := False;
end;

destructor TecSyntaxManager.Destroy;
begin
  FOnChange := nil;
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TecSyntaxManager.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TecSyntaxManager.GeItem(Index: integer): TecSyntAnalyzer;
begin
  Result := TecSyntAnalyzer(FList[Index]);
end;

procedure TecSyntaxManager.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var i: integer;
begin
  if not (csDestroying in ComponentState) then
   for i := 0 to FList.Count - 1 do
    Proc(TComponent(FList[i]));
end;

function TecSyntaxManager.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TecSyntaxManager.LoadFromFile(const FileName: string);
begin
  Clear;
  inherited;
  Changed;
  FModified := False;
end;

procedure TecSyntaxManager.SaveToFile(const FileName: string);
begin
  inherited;
  FModified := False;
end;

procedure TecSyntaxManager.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  FModified := True;
end;

procedure TecSyntaxManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

procedure TecSyntaxManager.SetCurrentLexer(const Value: TecSyntAnalyzer);
begin
  if (FCurrentLexer <> Value) and ((Value = nil) or (FList.IndexOf(value) <> -1)) then
   begin
     FCurrentLexer := Value;
   end;
end;

function TecSyntaxManager.FindAnalyzer(
  const LexerName: string): TecSyntAnalyzer;
var i: integer;
begin
  for i := 0 to GetCount - 1 do
   if SameText(Analyzers[i].LexerName, LexerName) then
     begin
      Result := Analyzers[i];
      Exit;
     end;
  Result := nil;
end;

procedure TecSyntaxManager.OnReadError(Reader: TReader;
  const Message: string; var Handled: Boolean);
var S: string;
begin
  if not FIgnoreAll then
   begin
    if AnalyzerCount > 0 then
      S := 'Error in lexer: '+Analyzers[AnalyzerCount - 1].Name +'. '
    else
      S := '';
    S := S + Message;
    inherited OnReadError(Reader, S, Handled);
   end else
  inherited;
end;

{ TLibSyntAnalyzer }

constructor TLibSyntAnalyzer.Create(AOwner: TComponent);
begin
  if Assigned(AOwner) and (AOwner is TecSyntaxManager) then
   inherited Create((AOwner as TecSyntaxManager).Owner)
  else
   inherited Create(AOwner);
end;

destructor TLibSyntAnalyzer.Destroy;
begin
  if FParent <> nil then
   begin
     FParent.FList.Remove(Self);
     FParent := nil;
   end;
  inherited;
end;

function TLibSyntAnalyzer.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

function TLibSyntAnalyzer.HasParent: Boolean;
begin
  Result := True;
end;

procedure TLibSyntAnalyzer.LoadFromStream(const Stream: TStream);
begin
  inherited LoadFromStream(Stream);
end;

procedure TLibSyntAnalyzer.SetParentComponent(Value: TComponent);
begin
  if FParent = Value then Exit;
  if FSkipNewName and (Value = nil) then Exit;
  if FParent <> nil then FParent.FList.Remove(Self);
  if (Value <> nil) and (Value is TecSyntaxManager) then
   begin
     FParent := TecSyntaxManager(Value);
     FParent.FList.Add(Self);
   end else FParent := nil;
end;

{ TLoadableComponent }

var
  CheckExistingName: Boolean = False;

procedure TLoadableComponent.LoadFromFile(const FileName: string);
var
  Stream: TFileStreamUTF8;
begin
  FFileName := FileName; //AT
  Stream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TLoadableComponent.LoadFromResourceID(Instance: Cardinal;
  ResID: Integer; ResType: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID,
    PChar(ResType));
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TLoadableComponent.LoadFromResourceName(Instance: Cardinal;
  const ResName: string; ResType: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName,
    PChar(ResType));
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TLoadableComponent.LoadFromStream(const Stream: TStream);
begin
  FSkipNewName := True;
  CheckExistingName := True;
  try
    FIgnoreAll := False;
    LoadComponentFromStream(Self, Stream, OnReadError);
  finally
    FSkipNewName := False;
    CheckExistingName := False;
    FFileName := FileName;
 end;
end;

function TLoadableComponent.NotStored: Boolean;
begin
  Result := not FSaving;
end;

procedure TLoadableComponent.OnReadError(Reader: TReader;
  const Message: string; var Handled: Boolean);
begin
//  Handled := True;
  Handled := FIgnoreAll;
  if not Handled then
   case MessageDlg(Message + sLineBreak + 'Ignore this error?', mtError, [mbYes, mbNo, mbAll], 0) of
     mrYes: Handled := True;
     mrAll: begin
              Handled := True;
              FIgnoreAll := True;
            end;
   end;
end;

procedure TLoadableComponent.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
  FFileName := FileName;
end;

procedure TLoadableComponent.SaveToStream(Stream: TStream);
begin
  FSaving := True;
  try
    SaveComponentToStream(Self, Stream);
  finally
    FSaving := False;
  end;
end;

procedure TLoadableComponent.SetName(const NewName: TComponentName);
var Base: string;
    n:integer;
begin
  if not FSkipNewName then
   if CheckExistingName and (Owner.FindComponent(NewName) <> nil) then
    begin
     Base := ClassName;
     Delete(Base, 1, 1);
     n := 1;
     while Owner.FindComponent(Base + IntToStr(n)) <> nil do
      Inc(n);
     inherited SetName(Base + IntToStr(n));
    end
   else inherited;
end;

{ TecSubAnalyzerRule }

constructor TecSubAnalyzerRule.Create(Collection: TCollection);
begin
  inherited;
  FStartRegExpr := TecRegExpr.Create;
  FEndRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FStartRegExpr);
  SetDefaultModifiers(FEndRegExpr);
end;

destructor TecSubAnalyzerRule.Destroy;
begin
  FreeAndNil(FStartRegExpr);
  FreeAndNil(FEndRegExpr);
  inherited;
end;

procedure TecSubAnalyzerRule.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TecSubAnalyzerRule then
   with Dest as TecSubAnalyzerRule do
    begin
     StartExpression := Self.StartExpression;
     EndExpression := Self.EndExpression;
     SyntAnalyzer := Self.SyntAnalyzer;
     FromTextBegin := Self.FromTextBegin;
     ToTextEnd := Self.ToTextEnd;
     IncludeBounds := Self.IncludeBounds;
    end;
end;

function TecSubAnalyzerRule.GetEndExpression: string;
begin
  Result := FEndRegExpr.Expression;
end;

function TecSubAnalyzerRule.GetItemBaseName: string;
begin
  Result := 'Sub lexer rule';
end;

function TecSubAnalyzerRule.GetStartExpression: string;
begin
  Result := FStartRegExpr.Expression;
end;

function TecSubAnalyzerRule.MatchStart(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FStartRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

function TecSubAnalyzerRule.MatchEnd(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FEndRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

procedure TecSubAnalyzerRule.SetEndExpression(const Value: string);
begin
  FEndRegExpr.Expression := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetStartExpression(const Value: string);
begin
  FStartRegExpr.Expression := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetSyntAnalyzer(const Value: TecSyntAnalyzer);
var own: TecSyntAnalyzer;

  function IsLinked(SAnal: TecSyntAnalyzer): Boolean;
  var i: integer;
  begin
    for i := 0 to Collection.Count - 1 do
     if (Collection.Items[i] <> Self) and ((Collection.Items[i] as TecSubAnalyzerRule).SyntAnalyzer = SAnal) then
      begin
       Result := True;
       Exit;
      end;
    Result := False;
  end;

begin
  if FSyntAnalyzer <> Value then
   begin
     own := (Collection as TSyntCollection).SyntOwner as TecSyntAnalyzer;
     if Assigned(FSyntAnalyzer) and (FSyntAnalyzer <> own) and not IsLinked(FSyntAnalyzer) then
       FSyntAnalyzer.RemoveMasterLexer(own);
     FSyntAnalyzer := Value;
     if Assigned(FSyntAnalyzer) and (FSyntAnalyzer <> own) and not IsLinked(FSyntAnalyzer) then
      FSyntAnalyzer.AddMasterLexer(own);
     Changed(False);
   end;
end;

procedure TecSubAnalyzerRule.SetFromTextBegin(const Value: Boolean);
begin
  FFromTextBegin := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetToTextEnd(const Value: Boolean);
begin
  FToTextEnd := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetIncludeBounds(const Value: Boolean);
begin
  FIncludeBounds := Value;
  Changed(False);
end;

{ TecSubAnalyzerRules }

function TecSubAnalyzerRules.Add: TecSubAnalyzerRule;
begin
  Result := TecSubAnalyzerRule(inherited Add);
end;

constructor TecSubAnalyzerRules.Create;
begin
  inherited Create(TecSubAnalyzerRule);
end;

function TecSubAnalyzerRules.GetItem(Index: integer): TecSubAnalyzerRule;
begin
  Result := TecSubAnalyzerRule(inherited Items[Index]);
end;

{ TecSyntStyles }

constructor TecSyntStyles.Create(AOwner: TComponent);
begin
  inherited;
  FStyles := TecStylesCollection.Create;
end;

destructor TecSyntStyles.Destroy;
begin
  FreeAndNil(FStyles);
  inherited;
end;

procedure TecSyntStyles.SetStyles(const Value: TecStylesCollection);
begin
  FStyles.Assign(Value);
end;



initialization

  Classes.RegisterClass(TLibSyntAnalyzer);
end.

