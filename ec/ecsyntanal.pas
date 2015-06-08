{ *************************************************************************** }
{                                                                             }
{ EControl Syntax Editor SDK                                                  }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}
{$define EC_UNICODE}
//{$define ThreadEn}

unit ecSyntAnal;

interface

uses
  Classes, Graphics, Controls, ExtCtrls, Contnrs, SyncObjs,
  eczRegExpr, ecStrUtils, ecLists, ecSyntGramma, ATStringProc_TextBuffer,
  proc_StreamComponent;

const
  csCollapsed     = 1;
  csCollapsible   = 0;   // not collapsed, start line of collapsible range
  csCollapseEnd   = -1;  // end line of the collapsable range
  csInCollapse    = -2;  // in the collapsable lines range
  csOutCollapse   = -3;  // can not be hidden and is not start of the collapsible range (default)

type
  TStyleEntries = class;

  IecTextClient = interface
    ['{359632FE-CC0F-463F-B9CC-2F40F292DE40}']
    // Count < 0 => after delete, Pos = -1 => all text changed
    procedure TextChanged(Sender: TObject; Pos, Count, LineChange: integer);
  end;

  IecSyntClient = interface
    ['{045EAD6D-5584-4A60-849E-6B8994AA5B8F}']
    procedure FormatChanged; // Lexer properties changed (update without clear)
    procedure Finished;      // Compleat analysis
  end;

  IecSyntMemoClient = interface
    ['{577542E6-7DCE-44C1-A014-C2DA8335AF0F}']
    function CaretPosChanged: Boolean;
  end;

  IecSyntMemoPlugIn = interface
    ['{5C76D1AD-6D9D-469B-8380-6B448086B722}']
    function HandleKeyStroke(AShortCut: TShortCut): Boolean;
    function HandleMouse(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    procedure UpdateCursor(X, Y: Integer; Shift: TShiftState; var Cursor: TCursor);
  end;

  IecSyntMemoPlugIn2 = interface
    ['{C240E719-B016-49F0-9A4E-A76850BD854A}']
    procedure ScrollPosChanged;
  end;

  IecExternalFormatter = interface
    ['{C482008B-E5A0-4732-BA89-2230E1952221}']
    function GetStyleList(CurPos: integer; List: TStyleEntries): integer; // Formatting
    procedure AnalyzeToPos(APos: integer);                        // Analyzing
  end;

  IecCommandHook = interface
    ['{453306A6-8967-4C9E-A452-586EF3979B8C}']
    procedure BeforeCommand(Command: integer; Data: ecPointer; var Handled: Boolean);
    procedure AfterCommand(Command: integer; Data: ecPointer; var Handled: Boolean);
    procedure IsCommandEnabled(Command: integer; Data: ecPointer; var Enable: Boolean);
  end;

  TLineBreakPos = (lbTop, lbBottom);
  TLineBreakBound = set of TLineBreakPos; // for user blocks
  TVertAlignment = (vaTop, vaCenter, vaBottom);
  TFormatType = (ftCustomFont, // Any customizing
                 ftFontAttr,   // Except custom font
                 ftColor,      // Any color
                 ftBackGround);// Only background color

  TSyntAnalyzer       = class;
  TParserResults      = class;
  TClientSyntAnalyzer = class;
  TTagBlockCondition  = class;
  TSyntaxManager      = class;
  TSyntaxFormat       = class;
  TSubAnalyzerRule    = class;
  TTextRange          = class;

  TOnMatchToken = procedure(Sender: TObject; Client: TParserResults;
      const Text: ecString; APos: integer; var MatchLen: integer) of object;
  TOnBlockCheck = procedure(Sender: TObject; Client: TClientSyntAnalyzer;
      const Text: ecString; var RefIdx: integer; var Accept: Boolean) of object;

  TBoundDefEvent = procedure(Sender: TClientSyntAnalyzer; Range: TTextRange; var sIdx, eIdx: integer) of object;

  TSyntCollectionItem = class(TCollectionItem)
  private
    FName: string;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; virtual;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure Loaded; virtual;
    function GetIsInvalid: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    property IsInvalid: Boolean read GetIsInvalid;
  published
    property DisplayName;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TSyntItemChanged = procedure(Sender: TCollection; Item: TSyntCollectionItem) of object;

  TSyntCollection = class(TCollection)
  private
    FSyntOwner: TSyntAnalyzer;
    FOnChange: TSyntItemChanged;
    function GetItems(Index: integer): TSyntCollectionItem;
  protected
    procedure Update(Item: TCollectionItem); override;
    function  GetOwner: TPersistent; override;
    procedure Loaded;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    function ItemByName(const AName: string): TSyntCollectionItem;
    function ValidItem(Item: TSyntCollectionItem): Boolean;
    function GetUniqueName(const Base: string): string;

    property SyntOwner: TSyntAnalyzer read FSyntOwner write FSyntOwner;
    property Items[Index: integer]: TSyntCollectionItem read GetItems; default;
    property OnChange: TSyntItemChanged read FOnChange write FOnChange;
  end;

  TRuleCollectionItem = class(TSyntCollectionItem)
  private
    FStyleName: string;
    FBlockName: string;
    FFormat: TSyntaxFormat;
    FBlock: TTagBlockCondition;
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
    function GetSyntOwner: TSyntAnalyzer;
    procedure SetStatesAdd(const Value: integer);
    procedure SetStatesAbsent(const Value: integer);
    procedure SetStatesRemove(const Value: integer);
    procedure SetStatesPresent(const Value: integer);
  protected
    function ValidStyleName(const AStyleName: string; AStyle: TSyntaxFormat): string;
    function ValidSetStyle(const AStyleName: string; var AStyleField: string; var AStyle: TSyntaxFormat): string;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Loaded; override;
  public
    property Style: TSyntaxFormat read FFormat write FFormat;
    property Block: TTagBlockCondition read FBlock write FBlock;
    property SyntOwner: TSyntAnalyzer read GetSyntOwner;
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

// *******************************************************************
//  Format for syntax output
// *******************************************************************
  TBorderLineType = (blNone, blSolid, blDash, blDot, blDashDot, blDashDotDot,
                     blSolid2, blSolid3, blWavyLine, blDouble);
  TFormatFlag = (ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                 ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign);
  TFormatFlags = set of TFormatFlag;

  TSyntaxFormat = class(TSyntCollectionItem)
  private
    FIsBlock: Boolean;
    FFont: TFont;
    FBgColor: TColor;
    FVertAlign: TVertAlignment;
    FFormatType: TFormatType;
    FOnChange: TNotifyEvent;
    FHidden: Boolean;
    FBorderTypes: array[0..3] of TBorderLineType;
    FBorderColors: array[0..3] of TColor;
    FMultiLineBorder: Boolean;
    FReadOnly: Boolean;
    FChangeCase: TChangeCase;
    FFormatFlags: TFormatFlags;
    procedure SetFont(const Value: TFont);
    procedure SetBgColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    procedure SetVertAlign(const Value: TVertAlignment);
    procedure SetFormatType(const Value: TFormatType);
    procedure SetHidden(const Value: Boolean);
    function GetBorderColor(Index: Integer): TColor;
    function GetBorderType(Index: Integer): TBorderLineType;
    procedure SetBorderColor(Index: Integer; const Value: TColor);
    procedure SetBorderType(Index: Integer;
      const Value: TBorderLineType);
    procedure SetMultiLineBorder(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetChangeCase(const Value: TChangeCase);
    procedure SetFormatFlags(const Value: TFormatFlags);
    function GetHidden: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    procedure Change; dynamic;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function HasBorder: Boolean;

    procedure ApplyTo(Canvas: TCanvas; AllowChangeFont: Boolean = True);

    function IsEqual(Other: TSyntaxFormat): Boolean;
    // Merges style above this style
    procedure Merge(Over: TSyntaxFormat);
    // Save only common properties
    procedure Intersect(Over: TSyntaxFormat);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property BorderTypes[Index: integer]: TBorderLineType read GetBorderType write SetBorderType;
    property BorderColors[Index: integer]: TColor read GetBorderColor write SetBorderColor;
  published
    property Font: TFont read FFont write SetFont;
    property BgColor: TColor read FBgColor write SetBgColor default clNone;
    property VertAlignment: TVertAlignment read FVertAlign write SetVertAlign default vaCenter;
    property FormatType: TFormatType read FFormatType write SetFormatType default ftFontAttr;
    property Hidden: Boolean read GetHidden write SetHidden default False;
    property BorderTypeLeft: TBorderLineType index 0 read GetBorderType write SetBorderType default blNone;
    property BorderColorLeft: TColor index 0 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeTop: TBorderLineType index 1 read GetBorderType write SetBorderType default blNone;
    property BorderColorTop: TColor index 1 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeRight: TBorderLineType index 2 read GetBorderType write SetBorderType default blNone;
    property BorderColorRight: TColor index 2 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeBottom: TBorderLineType index 3 read GetBorderType write SetBorderType default blNone;
    property BorderColorBottom: TColor index 3 read GetBorderColor write SetBorderColor default clBlack;
    property MultiLineBorder: Boolean read FMultiLineBorder write SetMultiLineBorder default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ChangeCase: TChangeCase read FChangeCase write SetChangeCase default ccNone;
    property FormatFlags: TFormatFlags read FFormatFlags write SetFormatFlags
                 default [ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                          ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign];
  end;

  TStyleCache = class
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TSyntaxFormat;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddStyle(Style: TSyntaxFormat): integer;
    procedure AddNoCheck(Style: TSyntaxFormat);
    procedure Delete(Index: integer);

    property Count: integer read GetCount;
    property Items[Index: integer]: TSyntaxFormat read GetItem; default;
  end;

  TStylesCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TSyntaxFormat;
  public
    function Synchronize(Source: TStylesCollection): integer;
    constructor Create;
    function Add: TSyntaxFormat;
    property Items[Index: integer]: TSyntaxFormat read GetItem; default;
  end;

// *******************************************************************
// description classes of text contents
// *******************************************************************

  TSyntToken = class(TRange)
  private
    FTokenType: integer;
    FRule: TRuleCollectionItem;
    function GetStyle: TSyntaxFormat;
  public
    constructor Create(ARule: TRuleCollectionItem; AStartPos, AEndPos: integer);
    function GetStr(const Source: ecString): ecString;
    property TokenType: integer read FTokenType;
    property Rule: TRuleCollectionItem read FRule;
    property Style: TSyntaxFormat read GetStyle;
  end;

  TLineBreak = class
  private
    FRefTag: integer;
    FLine: integer;
    FRule: TTagBlockCondition;
  public
    property Rule: TTagBlockCondition read FRule;
    property Line: integer read FLine;
    property RefIdx: integer read FRefTag;
  end;

  TLineBreakRange = class(TRange)
  private
    FRule: TTagBlockCondition;
  public
    property Rule: TTagBlockCondition read FRule;
  end;

  TTextRange = class(TSortedItem)
  private
    FRule: TTagBlockCondition;
    FStart, FEnd, FIdent: integer;
    FStartPos: integer;
    FParent: TTextRange;
    FCondIndex: integer;
    FEndCondIndex: integer;
    FIndex: integer;
    function GetLevel: integer;
    function GetIsClosed: Boolean;
  protected
    function GetKey: integer; override;
  public
    constructor Create(AStartIdx, AStartPos: integer);
    function IsParent(Range: TTextRange): Boolean;

    property Rule: TTagBlockCondition read FRule;
    property StartIdx: integer read FStart;
    property EndIdx: integer read FEnd;
    property IdentIdx: integer read FIdent;
    property Parent: TTextRange read FParent;
    property Level: integer read GetLevel;
    property Index: integer read FIndex;
    property StartPos: Integer read FStartPos;

    property IsClosed: Boolean read GetIsClosed;
  end;

  TSubLexerRange = class(TRange)
  private
    FRule: TSubAnalyzerRule;   // Rule reference
    FCondEndPos: integer;      // Start pos of the start condition
    FCondStartPos: integer;    // End pos of the end condition
  public
    property Rule: TSubAnalyzerRule read FRule;
    property CondStartPos: integer read FCondStartPos;
    property CondEndPos: integer read FCondEndPos;
  end;

  TBlockStaple = class(TRange)
  private
    FXPos: integer;
    FRule: TTagBlockCondition;
  public
    property XPos: integer read FXPos write FXPos;
    property Rule: TTagBlockCondition read FRule;
  end;

  TStyledRange = class(TRange)
  private
    FRange: TObject;
    FStyle: TSyntaxFormat;
    FFlags: Byte;
  public
    constructor Create(AStartPos, AEndPos: integer; AStyle: TSyntaxFormat);
    constructor Create1(AStartPos: integer; AStyle: TSyntaxFormat);
    property Style: TSyntaxFormat read FStyle;
  end;

// *******************************************************************
//                Rules for syntax interpretation
// *******************************************************************

  TTagConditionType = (tcEqual, tcNotEqual, tcMask, tcSkip, tcStrictMask);

  TSingleTagCondition = class(TCollectionItem)
  private
    FTagList: TStrings;
    FCondType: TTagConditionType;
    FTokenTypes: DWORD;
    procedure SetTagList(const Value: TStrings);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetTokenTypes(const Value: DWORD);
    procedure SetCondType(const Value: TTagConditionType);
    procedure TagListChanged(Sender: TObject);
    function GetIgnoreCase: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CheckToken(const Source: ecString; Token: TSyntToken): Boolean;
  published
    property TagList: TStrings read FTagList write SetTagList;
    property CondType: TTagConditionType read FCondType write SetCondType default tcEqual;
    property TokenTypes: DWORD read FTokenTypes write SetTokenTypes default 0;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase default False;
  end;

  TConditionCollection = class(TCollection)
  private
    FOwner: TTagBlockCondition;
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TSingleTagCondition;
  protected
    procedure Update(Item: TCollectionItem); override;
    function  GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TTagBlockCondition);
    function Add: TSingleTagCondition;
    property Items[Index: integer]: TSingleTagCondition read GetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTagBlockType = (btTagDetect, btLineBreak, btRangeStart, btRangeEnd);
  THighlightPos = (cpAny, cpBound, cpBoundTag, cpRange, cpBoundTagBegin, cpOutOfRange);
  TDynamicHighlight = (dhNone, dhBound, dhRangeNoBound, dhRange);
  TAutoCloseMode = (acmDisabled, acmCloseNearest, acmCloseOpened);

  TTagBlockCondition = class(TRuleCollectionItem)
  private
    FConditions: TConditionCollection;
    FIdentIndex: integer;
    FLinePos: TLineBreakPos;
    FBlockOffset: integer;
    FBlockEndCond: TTagBlockCondition;
    FBlockType: TTagBlockType;
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
    FDynHighlight: TDynamicHighlight;
    FHighlightPos: THighlightPos;
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
    FTreeItemStyleObj: TSyntaxFormat;
    FTreeGroupStyle: string;
    FTreeGroupStyleObj: TSyntaxFormat;
    FTreeGroupImage: integer;
    FTreeItemImage: integer;
    FUseCustomPen: Boolean;
    FPen: TPen;
    FIgnoreAsParent: Boolean;
    FAutoCloseText: ecString;
    FAutoCloseMode: TAutoCloseMode;
    procedure ConditionsChanged(Sender: TObject);
    function GetBlockEndName: string;
    procedure SetBlockEndName(const Value: string);
    procedure SetBlockType(const Value: TTagBlockType);
    procedure SetConditions(const Value: TConditionCollection);
    procedure SetBlockEndCond(const Value: TTagBlockCondition);
    procedure SetLinePos(const Value: TLineBreakPos);
    procedure SetIdentIndex(const Value: integer);
    procedure SetBlockOffset(const Value: integer);
    procedure SetEndOfTextClose(const Value: Boolean);
    procedure SetNotCollapsed(const Value: Boolean);
    procedure SetSameIdent(const Value: Boolean);
    procedure SetHighlight(const Value: Boolean);
    procedure SetInvertColors(const Value: Boolean);
    procedure SetDisplayInTree(const Value: Boolean);
    procedure SetCancelNextRules(const Value: Boolean);
    procedure SetDynHighlight(const Value: TDynamicHighlight);
    procedure SetDynSelectMin(const Value: Boolean);
    procedure SetGroupFmt(const Value: ecString);
    procedure SetHighlightPos(const Value: THighlightPos);
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
    procedure SetAutoCloseMode(const Value: TAutoCloseMode);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    procedure Loaded; override;
    function CheckOffset: integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Check(const Source: ecString; Tags: TClientSyntAnalyzer;
                   N: integer;  var RefIdx: integer): Boolean;
    property BlockEndCond: TTagBlockCondition read FBlockEndCond write SetBlockEndCond;
    property TreeItemStyleObj: TSyntaxFormat read FTreeItemStyleObj;
    property TreeGroupStyleObj: TSyntaxFormat read FTreeGroupStyleObj;
  published
    property BlockType: TTagBlockType read FBlockType write SetBlockType default btRangeStart;
    property ConditionList: TConditionCollection read FConditions write SetConditions;
    property IdentIndex: integer read FIdentIndex write SetIdentIndex default 0;
    property LinePos: TLineBreakPos read FLinePos write SetLinePos default lbTop;
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
    property DynHighlight: TDynamicHighlight read FDynHighlight write SetDynHighlight default dhNone;
    property HighlightPos: THighlightPos read FHighlightPos write SetHighlightPos;
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
    property AutoCloseMode: TAutoCloseMode read FAutoCloseMode write SetAutoCloseMode default acmDisabled;
    property AutoCloseText: ecString read FAutoCloseText write SetAutoCloseText;
  end;

  TBlockRuleCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TTagBlockCondition;
  public
    constructor Create;
    function Add: TTagBlockCondition;
    property Items[Index: integer]: TTagBlockCondition read GetItem; default;
  end;

  TMouseClickEvent = procedure(Editor: TObject;{v2.25} TokenIndex: integer;
                               Button: TMouseButton; Shift: TShiftState) of object;

  // Token identification rule
  TTokenRule = class(TRuleCollectionItem)
  private
    FRegExpr: TecRegExpr;
    FTokenType: integer;
    FOnMatchToken: TOnMatchToken;
    FOnMouseClick: TMouseClickEvent;
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
    property OnMouseClick: TMouseClickEvent read FOnMouseClick write FOnMouseClick;
  end;

  TTokenRuleCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TTokenRule;
  public
    constructor Create;
    function Add: TTokenRule;
    property Items[Index: integer]: TTokenRule read GetItem; default;
  end;

  TSubAnalyzerRule = class(TRuleCollectionItem)
  private
    FStartRegExpr: TecRegExpr;
    FEndRegExpr: TecRegExpr;
    FSyntAnalyzer: TSyntAnalyzer;
    FFromTextBegin: Boolean;
    FToTextEnd: Boolean;
    FIncludeBounds: Boolean;
    function GetEndExpression: string;
    function GetStartExpression: string;
    procedure SetEndExpression(const Value: string);
    procedure SetStartExpression(const Value: string);
    procedure SetSyntAnalyzer(const Value: TSyntAnalyzer);
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
    property SyntAnalyzer: TSyntAnalyzer read FSyntAnalyzer write SetSyntAnalyzer;
    property FromTextBegin: Boolean read FFromTextBegin write SetFromTextBegin default False;
    property ToTextEnd: Boolean read FToTextEnd write SetToTextEnd default False;
    property IncludeBounds: Boolean read FIncludeBounds write SetIncludeBounds default False;
  end;

  TSubAnalyzerRules = class(TSyntCollection)
  private
    function GetItem(Index: integer): TSubAnalyzerRule;
  public
    constructor Create;
    function Add: TSubAnalyzerRule;
    property Items[Index: integer]: TSubAnalyzerRule read GetItem; default;
  end;

  TCodeTemplate = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FCode: TecStrings;
    FAdvanced: boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Code: TecStrings read FCode write FCode;
    property Advanced: Boolean read FAdvanced write FAdvanced default False;
  end;

  TCodeTemplates = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TCodeTemplate;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TCodeTemplate;
    function FindTemplate(AName: string): TCodeTemplate;
    property Items[Index: integer]: TCodeTemplate read GetItem; default;
  end;

  TChangeFixer = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Pos, Count: integer);
    function CurToOld(CurPos: integer): integer;
    function OldToCur(OldPos: integer): integer;
    function UpOldNoChange: integer;
  end;

// *******************************************************************
//  Parser classes
//
// *******************************************************************


// *******************************************************************
//  Syntax analizer for single client
//            container of description objects
// *******************************************************************
  TParserResults = class(TTokenHolder)
  private
    FSrcProc: TATStringBuffer;
    FClient: IecSyntClient;
    FOwner: TSyntAnalyzer;
    FFinished: Boolean;

    FSubLexerBlocks: TList;     // Sub Lexer Text Ranges
    FTagList: TRangeList;       // List of tokens
    FCurState: integer;
    FStateChanges: TList;
    function GetLastPos(const Source: ecString): integer;
    function ExtractTag(const Source: ecString; var FPos: integer; IsIdle: Boolean): Boolean;
    function GetTags(Index: integer): TSyntToken;
    function GetSubLexerRangeCount: integer;
    function GetSubLexerRange(Index: integer): TSubLexerRange;
  protected
    function GetTokenCount: integer; override;
    function GetTokenStr(Index: integer): ecString; override;
    function GetTokenType(Index: integer): integer; override;
    procedure CloseAtEnd(StartTagIdx: integer); virtual; abstract;
  protected
    FLastAnalPos: integer;
    procedure Finished; virtual;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean; virtual;
    procedure ApplyStates(Rule: TRuleCollectionItem);
    procedure SaveState;
    procedure RestoreState;
  public
    constructor Create(AOwner: TSyntAnalyzer; SrcProc: TATStringBuffer; const AClient: IecSyntClient); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    function AnalyzerAtPos(Pos: integer): TSyntAnalyzer;
    function ParserStateAtPos(TokenIndex: integer): integer;

    property Owner: TSyntAnalyzer read FOwner;
    property IsFinished: Boolean read FFinished;
    property TagStr[Index: integer]: ecString read GetTokenStr;
    property TagCount: integer read GetTokenCount;
    property Tags[Index: integer]: TSyntToken read GetTags; default;
    property SubLexerRangeCount: integer read GetSubLexerRangeCount;
    property SubLexerRanges[Index: integer]: TSubLexerRange read GetSubLexerRange;
    property ParserState: integer read FCurState write FCurState;
  end;

  TClientSyntAnalyzer = class(TParserResults)
  private
    FLineBreaks: TList;

    FRanges: TSortedList;
    FOpenedBlocks: TSortedList;    // Opened ranges (without end)

    FBreakIdle: Boolean;
    FIdleProc: Boolean;
    FIdleTimer: TTimer;

    FDynCurPos: integer;
    FDynOut: Boolean;

    FRangeStyles: TRangeCollection;    // all styled blocks (without tokens)
    FDynoConditions: TRangeCollection; // dynamic ranges hot-points
    FCollapsables: TRangeCollection;   // ranges that can be collapsed
    FStaples: TRangeCollection;        // ranges that can be collapsed
    FSavedTags: TRangeList;            // saved tokens
    FCurDynamic: TList;                // currently highlighted ranges
    FChanges: TChangeFixer;            // fixes all changes before objects will be updated
    FLineBreakRanges: TRangeCollection;// ranges maked with line breaks
    FFinishThread: TThread;
    FDataAccess: TCriticalSection;
    FNextTokIndex: integer;
    FStartSepRangeAnal: integer;
    FDisableIdleAppend: Boolean;
    FRepeateAnalysis: Boolean;
    function GetRangeCount: integer;
    function GetRanges(Index: integer): TTextRange;
    function GetTagPos(Index: integer): TPoint;
    procedure TerminateFinishThread;
    procedure FinishThreadEnd(Sender: TObject);
    function GetOpened(Index: integer): TTextRange;
    function GetOpenedCount: integer;
    procedure SetDisableIdleAppend(const Value: Boolean);
  protected
    procedure AddLineBreak(lb: TLineBreak);
    procedure AddRange(Range: TTextRange);
    function HasOpened(Rule: TRuleCollectionItem; Parent: TTagBlockCondition; Strict: Boolean): Boolean;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean; override;
    procedure Finished; override;
    procedure IntIdleAppend(Sender: TObject);
    procedure CloseAtEnd(StartTagIdx: integer); override;

    procedure UpdateDyno;
  public
    constructor Create(AOwner: TSyntAnalyzer; SrcProc: TATStringBuffer; const AClient: IecSyntClient); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ChangedAtPos(APos: integer);
    function GetLineBreak(Line: integer): TLineBreakRange;
    procedure GetLineHighlight(Line: integer; var InvSel: Boolean; var bgColor,
                               frColor: TColor; UseDyno: Boolean);
    function TokenAtPos(Pos: integer): integer;
    function PriorTokenAt(Pos: integer): integer;
    function NextTokenAt(Pos: integer): integer;
    function SetCurPos(Pos: integer; OutOfLine: Boolean): Boolean;

    function GetRangeBound(Range: TTextRange): TPoint;
    function GetColRangeBound(Range: TTextRange): TPoint;
    function RangeAtPos(APos: integer): TTextRange;
    function RangeIdxAtPos(APos: integer): integer;
    function NearestRangeAtPos(APos: integer): TTextRange;
    function NearestRangeIdxAtPos(APos: integer): integer;
    function GetRangeAtLine(Line: integer): TTextRange;
    function GetNearestColRange(Pos: integer): TTextRange;
    function GetLineState(Line: integer): integer;

    function RangeFormat(const FmtStr: ecString; Range: TTextRange): ecString;
    function GetRangeName(Range: TTextRange): ecString;
    function GetRangeGroup(Range: TTextRange): ecString;
    function GetCollapsedText(Range: TTextRange): ecString;
    function GetAutoCloseText(Range: TTextRange; const Indent: string): ecString;

    procedure TextChanged(Pos, Count, Line, LineChange: integer);
    procedure TryAppend(APos: integer);   // Tries to analyze to APos
    procedure AppendToPos(APos: integer); // Requires analyzed to APos
    procedure Analyze(ResetContent: Boolean = True); // Requires analyzed all text
    procedure IdleAppend;                 // Start idle analysis

    function GetTokenStyle(Pos: integer; StlList: TStyleEntries): integer;
    function GetRangeStyles(Pos: integer; StlList: TStyleEntries; UseDyno: Boolean): integer;
    procedure GetStaples(Line: integer; List: TList);
    procedure GetStapleLines(Staple: TBlockStaple; var sLine, eLine: integer);
    procedure ResetStaples;
    procedure CompleteAnalysis;

    procedure Lock;
    procedure Unlock;

    function CloseRange(Cond: TTagBlockCondition; RefTag: integer): Boolean;
    function CreateLineBreak(Rule: TTagBlockCondition; RefTag: integer): Boolean;
    function DetectTag(Rule: TTagBlockCondition; RefTag: integer): Boolean;

    property OpenCount: integer read GetOpenedCount;
    property Opened[Index: integer]: TTextRange read GetOpened;

    property RangeCount: integer read GetRangeCount;
    property Ranges[Index: integer]: TTextRange read GetRanges;
    property TagPos[Index: integer]: TPoint read GetTagPos;
    property DisableIdleAppend: Boolean read FDisableIdleAppend write SetDisableIdleAppend;
  end;

  // Internal class to store styles diapasons
  TStyleEntry = class(TRange)
    Style: TSyntaxFormat;
    IsDynoStyle: Boolean;   // Over line highlighting
  public
    constructor Create(AStyle: TSyntaxFormat; AStartPos, AEndPos: integer; AIsDynoStyle: Boolean = False);
  end;

  TStyleEntries = class(TObjectList)
  private
    function GetItems(Index: integer): TStyleEntry;
  public
    procedure Add(AStyle: TSyntaxFormat; AStartPos, AEndPos: integer; AIsDynoStyle: Boolean = False); overload;
    property Items[Index: integer]: TStyleEntry read GetItems; default;
  end;
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

  TParseTokenEvent = procedure(Client: TParserResults; const Text: ecString; Pos: integer;
      var TokenLength: integer; var Rule: TTokenRule) of object;

  TSyntAnalyzer = class(TLoadableComponent)
  private
    FClientList: TList;
    FMasters: TList;      // Master lexer, i.e. lexers that uses it
    FOnChange: TNotifyEvent;
    FSampleText: TStrings;
    FFormats: TStylesCollection;
    FTokenRules: TTokenRuleCollection;
    FBlockRules: TBlockRuleCollection;
    FCodeTemplates: TCodeTemplates;
    FExtentions: string;
    FLexerName: string;
    FCoping: Boolean;
    FSkipSpaces: Boolean;
    FSubAnalyzers: TSubAnalyzerRules;
    FTokenTypeNames: TStrings;
    FFullRefreshSize: integer;

    FMarkedBlock: TSyntaxFormat;
    FMarkedBlockName: string;
    FSearchMatch: TSyntaxFormat;
    FSearchMatchName: string;
    FCurrentLine: TSyntaxFormat;
    FCurrentLineName: string;
    FDefStyle: TSyntaxFormat;
    FDefStyleName: string;
    FCollapseStyle: TSyntaxFormat;
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
    FOnGetStapleRange: TBoundDefEvent;
    FOnCloseTextRange: TBoundDefEvent;
    FIdleAppendDelayInit: Cardinal;
    FIdleAppendDelay: Cardinal;
    FOnParseToken: TParseTokenEvent;
    procedure SetSampleText(const Value: TStrings);
    procedure FormatsChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure TokenRuleChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure BlocksChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure SubLexRuleChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure SetBlockRules(const Value: TBlockRuleCollection);
    procedure SetCodeTemplates(const Value: TCodeTemplates);
    procedure SetTokenRules(const Value: TTokenRuleCollection);
    procedure SetFormats(const Value: TStylesCollection);
    function GetUniqueName(const Base: string): string;
    procedure SetSkipSpaces(const Value: Boolean);
    procedure SetSubAnalyzers(const Value: TSubAnalyzerRules);
    procedure SetTokenTypeNames(const Value: TStrings);

    function GetStyleName(const AName: string; const AStyle: TSyntaxFormat): string;
    procedure SetMarkedBlock(const Value: TSyntaxFormat);
    function GetMarkedBlockName: string;
    procedure SetMarkedBlockName(const Value: string);
    procedure SetSearchMatch(const Value: TSyntaxFormat);
    function GetSearchMatchStyle: string;
    procedure SetSearchMatchStyle(const Value: string);
    procedure SetCurrentLine(const Value: TSyntaxFormat);
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
    procedure SetDefStyle(const Value: TSyntaxFormat);
    function GetDefaultStyleName: string;
    procedure SetDefaultStyleName(const Value: string);
    procedure SetLineComment(const Value: ecString);
    procedure DetectBlockSeparate;
    procedure SetAlwaysSyncBlockAnal(const Value: Boolean);
    function GetCollapseStyleName: string;
    procedure SetCollapseStyleName(const Value: string);
    procedure SetCollapseStyle(const Value: TSyntaxFormat);
    function GetSeparateBlocks: Boolean;
  protected
    function GetToken(Client: TParserResults; const Source: ecString;
                       APos: integer; OnlyGlobal: Boolean): TSyntToken; virtual;
    procedure HighlightKeywords(Client: TParserResults; const Source: ecString;
                       OnlyGlobal: Boolean); virtual;
    procedure SelectTokenFormat(Client: TParserResults; const Source: ecString;
                       OnlyGlobal: Boolean; N: integer = -1); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; dynamic;
    property SeparateBlockAnalysis: Boolean read GetSeparateBlocks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddClient(const Client: IecSyntClient; SrcProc: TATStringBuffer): TClientSyntAnalyzer;
    procedure ClearClientContents;
    procedure UpdateClients;

    procedure AddMasterLexer(SyntAnal: TSyntAnalyzer);
    procedure RemoveMasterLexer(SyntAnal: TSyntAnalyzer);

    property MarkedBlock: TSyntaxFormat read FMarkedBlock write SetMarkedBlock;
    property SearchMatch: TSyntaxFormat read FSearchMatch write SetSearchMatch;
    property CurrentLine: TSyntaxFormat read FCurrentLine write SetCurrentLine;
    property DefStyle: TSyntaxFormat read FDefStyle write SetDefStyle;
    property CollapseStyle: TSyntaxFormat read FCollapseStyle write SetCollapseStyle;
  published
    property Formats: TStylesCollection read FFormats write SetFormats;
    property TokenRules: TTokenRuleCollection read FTokenRules write SetTokenRules;
    property BlockRules: TBlockRuleCollection read FBlockRules write SetBlockRules;
    property CodeTemplates: TCodeTemplates read FCodeTemplates write SetCodeTemplates;
    property SubAnalyzers: TSubAnalyzerRules read FSubAnalyzers write SetSubAnalyzers;
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
    property OnGetStapleRange: TBoundDefEvent read FOnGetStapleRange write FOnGetStapleRange;
    property OnGetCollapseRange: TBoundDefEvent read FOnGetCollapseRange write FOnGetCollapseRange;
    property OnCloseTextRange: TBoundDefEvent read FOnCloseTextRange write FOnCloseTextRange;
    property OnParseToken: TParseTokenEvent read FOnParseToken write FOnParseToken;
  end;

  TLibSyntAnalyzer = class(TSyntAnalyzer)
  protected
    FParent: TSyntaxManager;

//    function GetChildParent: TComponent; override;
//    procedure SetName(const NewName: TComponentName); override;
{$IFDEF EC_DOTNET}
  public
{$ENDIF}
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(const Stream: TStream); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  end;

  TSyntaxManager = class(TLoadableComponent)
  private
    FOnChange: TNotifyEvent;
    FList: TList;
    FCurrentLexer: TSyntAnalyzer;
    FOnLexerChanged: TNotifyEvent;
    FModified: Boolean;
    function GeItem(Index: integer): TSyntAnalyzer;
    function GetCount: integer;
    procedure SetCurrentLexer(const Value: TSyntAnalyzer);
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
    function FindAnalyzer(const LexerName: string): TSyntAnalyzer;
    function AddAnalyzer: TSyntAnalyzer;
    procedure Clear;
    procedure Move(CurIndex, NewIndex: Integer);

    property AnalyzerCount: integer read GetCount;
    property Analyzers[Index: integer]: TSyntAnalyzer read GeItem;
    property FileName;
    property CurrentLexer: TSyntAnalyzer read FCurrentLexer write SetCurrentLexer;
    property Modified: Boolean read FModified write FModified;
  published
    property OnLexerChanged: TNotifyEvent read FOnLexerChanged write FOnLexerChanged stored NotStored;
    property OnChange: TNotifyEvent read FOnChange write FOnChange stored NotStored;
  end;

  TSyntTextSource = class(TComponent, IecSyntClient)
  private
    FLines: TATStringBuffer;
    FSyntRanges: TClientSyntAnalyzer;
    FReadOnly: Boolean;
    procedure SetLines(const Value: TATStringBuffer);
    function GetSyntRanges: TSyntAnalyzer;
    procedure SetSyntRanges(const Value: TSyntAnalyzer);
    function GetLines: TATStringBuffer;
  protected
    FClients: TList;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // IecSyntClient
    procedure FormatChanged; // Lexer properties changed (update without clear)
    procedure Finished;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddClient(Client: TObject); virtual;
    procedure RemoveClient(Client: TObject); virtual;

    function CaretPosToStrPos(const p: TPoint): integer; virtual;
    function StrPosToCaretPos(p: integer): TPoint; virtual;
    function LineLength(Index: integer): integer; virtual;
    procedure TextChanged(Sender: TObject; Pos, Count, LineChange: integer); virtual;

    property SyntObj: TClientSyntAnalyzer read FSyntRanges;
  published
    property Lines: TATStringBuffer read GetLines write SetLines;
    property SyntaxAnalyzer: TSyntAnalyzer read GetSyntRanges write SetSyntRanges;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;

  TSyntStyles = class(TLoadableComponent)
  private
    FStyles: TStylesCollection;
    procedure SetStyles(const Value: TStylesCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Styles: TStylesCollection read FStyles write SetStyles;
  end;

procedure DrawBorder(Canvas: TCanvas; bType: TBorderLineType; p1, p2: TPoint);
function GetBorderLineWidth(bType: TBorderLineType): integer;

type
  TInitRegularExpressionProc = procedure(RE: TecRegExpr);
var
  InitRegularExpressionProc: TInitRegularExpressionProc = nil;

const
  SecDefaultTokenTypeNames = 'Unknown' + #13#10 +
                             'Comment' + #13#10 +
                             'Id'      + #13#10 +
                             'Symbol'  + #13#10 +
                             'String'  + #13#10 +
                             'Number'  + #13#10 +
                             'Preprocessor';

implementation

uses
  SysUtils, Forms, Dialogs, ecSysUtils,
  Math;

// Flags const for TStyledRange (used only internally)
const
  srLineHighlight     = 1;  // line highlighting
  srInvertCondition   = 2;  // "Out of range" dynamic highlighting
  srInvertSelColor    = 4;  // invert colors when selected
  srSubLexerHighlight = 8;  // sub-lexer highlighting

procedure DrawBorder(Canvas: TCanvas; bType: TBorderLineType; p1, p2: TPoint);
var x, y, m: integer;
const dx = 2;
begin
  Canvas.Pen.Width := 1;
  case bType of
    blNone: Exit;
    blDouble,
    blSolid:     Canvas.Pen.Style := psSolid;
    blDash:      Canvas.Pen.Style := psDash;
    blDot:       Canvas.Pen.Style := psDot;
    blDashDot:   Canvas.Pen.Style := psDashDot;
    blDashDotDot:Canvas.Pen.Style := psDashDotDot;
    blSolid2: begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 2;
     end;
    blSolid3: begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 3;
     end;
    blWavyLine: begin
      Canvas.Pen.Style := psSolid;
     end;
  end;
  if bType = blDouble then
   begin
    if p1.Y = p2.Y then
     begin
      Canvas.MoveTo(p1.X, p1.Y -1);
      Canvas.LineTo(p2.X, p2.Y - 1);
      Canvas.MoveTo(p1.X, p1.Y + 1);
      Canvas.LineTo(p2.X, p2.Y + 1);
     end else
     begin
      Canvas.MoveTo(p1.X - 1, p1.Y);
      Canvas.LineTo(p2.X - 1, p2.Y);
      Canvas.MoveTo(p1.X + 1, p1.Y);
      Canvas.LineTo(p2.X + 1, p2.Y);
     end;
   end else
  if (bType = blWavyLine) and ((p1.Y = p2.Y) or (p1.X = p2.X)) then
   begin
     if p1.Y = p2.Y then
      begin
       x := p1.X;
       m := 1;
       Canvas.MoveTo(p1.X, p1.Y);
       while x <= p2.X - dx do
        begin
          Inc(x, dx);
          y := p1.Y - m * dx;
          m := 1 - m;
          Canvas.LineTo(x, y);
        end;
      end else
      begin
       y := p1.Y;
       m := 1;
       Canvas.MoveTo(p1.X, p1.Y);
       while y <= p2.Y - dx do
        begin
          Inc(y, dx);
          x := p1.X + m * dx - 1;
          m := 1 - m;
          Canvas.LineTo(x, y);
        end;
      end;
   end else
   begin
     Canvas.MoveTo(p1.X, p1.Y);
     Canvas.LineTo(p2.X, p2.Y);
   end;
end;

function GetBorderLineWidth(bType: TBorderLineType): integer;
begin
  Result := 1;
  case bType of
    blNone:    Result := 0;
    blSolid2:  Result := 2;
    blSolid3:  Result := 3;
    blWavyLine:Result := 2;
    blDouble:  Result := 3;
  end;
end;

procedure SetDefaultModifiers(RegExpr: TecRegExpr);
begin
  if Assigned(InitRegularExpressionProc) then
    InitRegularExpressionProc(RegExpr) else
  begin
    RegExpr.ModifierI := True;
    RegExpr.ModifierG := True;
    RegExpr.ModifierS := False;
    RegExpr.ModifierM := True;
    RegExpr.ModifierX := True;
    RegExpr.ModifierR := False;
  end;
end;

{ TSyntToken }

constructor TSyntToken.Create(ARule: TRuleCollectionItem; AStartPos,
  AEndPos: integer);
begin
  inherited Create(AStartPos, AEndPos);
  FRule := ARule;
  FTokenType := TTokenRule(ARule).TokenType;
end;

function TSyntToken.GetStr(const Source: ecString): ecString;
begin
  Result := Copy(Source, StartPos + 1, EndPos - StartPos);
end;

function TSyntToken.GetStyle: TSyntaxFormat;
begin
  if Rule = nil then Result := nil
   else Result := Rule.Style;
end;

{ TTextRange }

constructor TTextRange.Create(AStartIdx, AStartPos: integer);
begin
  inherited Create;
  FStart := AStartIdx;
  FStartPos := AStartPos;
  FEnd := -1;
  FEndCondIndex := -1;
  FIndex := -1;
end;

function TTextRange.GetIsClosed: Boolean;
begin
  Result := FEnd <> -1;
end;

function TTextRange.GetKey: integer;
begin
  Result := FStartPos;
end;

function TTextRange.GetLevel: integer;
var prn: TTextRange;
begin
  prn := Parent;
  Result := 0;
  while prn <> nil do
   begin
     inc(Result);
     prn := prn.Parent;
   end;
end;

function TTextRange.IsParent(Range: TTextRange): Boolean;
begin
  if Range = FParent then Result := True else
    if Assigned(FParent) then Result := FParent.IsParent(Range)
      else Result := False;
end;

{ TStyledRange }

constructor TStyledRange.Create(AStartPos, AEndPos: integer;
  AStyle: TSyntaxFormat);
begin
  inherited Create(AStartPos, AEndPos);
  FStyle := AStyle;
end;

constructor TStyledRange.Create1(AStartPos: integer;
  AStyle: TSyntaxFormat);
begin
  Create(AStartPos, AStartPos + 1, AStyle);
end;

{ TSyntCollectionItem }

procedure TSyntCollectionItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TSyntCollectionItem then
   begin
    (Dest as TSyntCollectionItem).DisplayName := DisplayName;
    (Dest as TSyntCollectionItem).Enabled := Enabled;
   end;
end;

constructor TSyntCollectionItem.Create(Collection: TCollection);
var NewName: string;
    n: integer;
begin
  inherited;
  FEnabled := True;
  if Collection = nil then Exit;
  n := 1;
  repeat
    NewName  := GetItemBaseName + ' ' + IntToStr(n);
    inc(n);
  until TSyntCollection(Collection).ItemByName(NewName) = nil;
  FName := NewName;
end;

function TSyntCollectionItem.GetDisplayName: string;
begin
  Result := FName;
end;

function TSyntCollectionItem.GetIsInvalid: Boolean;
begin
  Result := False;
end;

function TSyntCollectionItem.GetItemBaseName: string;
begin
  Result := 'Item';
end;

procedure TSyntCollectionItem.Loaded;
begin

end;

procedure TSyntCollectionItem.SetDisplayName(const Value: string);
var i: integer;
begin
  if Collection <> nil then
  for i := 0 to Collection.Count - 1 do
   if Collection.Items[i] <> Self then
    if Collection.Items[i].DisplayName = Value then
      Exit;
  FName := Value;
end;

procedure TSyntCollectionItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
   begin
     FEnabled := Value;
     Changed(False);
   end;
end;

{ TSyntCollection }

constructor TSyntCollection.Create(ItemClass: TCollectionItemClass);
begin
  if not ItemClass.InheritsFrom(TSyntCollectionItem) then
   raise Exception.Create('Allow only TSyntCollectionItem Class');
  inherited;
end;

function TSyntCollection.GetItems(Index: integer): TSyntCollectionItem;
begin
  Result := (inherited Items[Index]) as TSyntCollectionItem;
end;

function TSyntCollection.GetOwner: TPersistent;
begin
  Result := FSyntOwner;
end;

function TSyntCollection.GetUniqueName(const Base: string): string;
var n: integer;
begin
  Result := Base;
  n := 0;
  while ItemByName(Result) <> nil do
   begin
    Inc(n);
    Result := Base + IntToStr(n);
   end;
end;

function TSyntCollection.ItemByName(const AName: string): TSyntCollectionItem;
var i: integer;
begin
  for i := 0 to Count - 1 do
   if Items[i].DisplayName = AName then
    begin
      Result := Items[i] as TSyntCollectionItem;
      Exit;
    end;
  Result := nil;
end;

procedure TSyntCollection.Loaded;
var i: integer;
begin
  for i := 0 to Count - 1 do
   Items[i].Loaded;
end;

procedure TSyntCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Self, TSyntCollectionItem(Item));
end;

function TSyntCollection.ValidItem(Item: TSyntCollectionItem): Boolean;
var i: integer;
begin
  Result := True;
  if Item <> nil then
   for i := 0 to Count - 1 do
    if Items[i] = Item then Exit;
  Result := False;
end;

{ TSyntaxFormat }

constructor TSyntaxFormat.Create(Collection: TCollection);
var i: integer;
begin
  FIsBlock := False;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 10;
  FBgColor := clNone;
  FVertAlign := vaCenter;
  FFormatType := ftFontAttr;
  for i := 0 to 3 do
   begin
    FBorderTypes[i] := blNone;
    FBorderColors[i] := clBlack;
   end;
  FFormatFlags := [ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                   ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign];
  inherited;
  FFont.OnChange := FontChanged;
end;

destructor TSyntaxFormat.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TSyntaxFormat.AssignTo(Dest: TPersistent);
var i: integer;
begin
  inherited;
  if Dest is TSyntaxFormat then
   with Dest as TSyntaxFormat do
    begin
      FBgColor := Self.BgColor;
      FFont.Assign(Self.Font);
      FVertAlign := Self.FVertAlign;
      FIsBlock := Self.FIsBlock;
      FFormatType := Self.FFormatType;
      Hidden := Self.Hidden;
      ReadOnly := Self.ReadOnly;
      MultiLineBorder := Self.MultiLineBorder;
      FChangeCase := Self.ChangeCase;
      for i := 0 to 3 do
       begin
        FBorderTypes[i] := Self.FBorderTypes[i];
        FBorderColors[i] := Self.FBorderColors[i];
       end;
      FFormatFlags := Self.FFormatFlags;
    end;
end;

procedure TSyntaxFormat.SetBgColor(const Value: TColor);
begin
  FBgColor := Value;
  Change;
end;

procedure TSyntaxFormat.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Change;
end;

procedure TSyntaxFormat.FontChanged(Sender: TObject);
begin
  Change;
end;

procedure TSyntaxFormat.SetVertAlign(const Value: TVertAlignment);
begin
  FVertAlign := Value;
  Change;
end;

function TSyntaxFormat.GetItemBaseName: string;
begin
  Result := 'Style';
end;

procedure TSyntaxFormat.SetFormatType(const Value: TFormatType);
begin
  FFormatType := Value;
  Change;
end;

procedure TSyntaxFormat.Change;
begin
  Changed(False);
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSyntaxFormat.SetHidden(const Value: Boolean);
begin
  FHidden := Value;
  Change;
end;

function TSyntaxFormat.GetBorderColor(Index: Integer): TColor;
begin
  if (Index >= 0) and (Index <= 3) then
    Result := FBorderColors[Index]
  else
    Result := clBlack;
end;

function TSyntaxFormat.GetBorderType(Index: Integer): TBorderLineType;
begin
  if (Index >= 0) and (Index <= 3) then
    Result := FBorderTypes[Index]
  else
    Result := blNone;
end;

procedure TSyntaxFormat.SetBorderColor(Index: Integer;
  const Value: TColor);
begin
  if (Index >= 0) and (Index <= 3) then
   begin
    FBorderColors[Index] := Value;
    Change;
   end;
end;

procedure TSyntaxFormat.SetBorderType(Index: Integer;
  const Value: TBorderLineType);
begin
  if (Index >= 0) and (Index <= 3) then
   begin
    FBorderTypes[Index] := Value;
    Change;
   end;
end;

procedure TSyntaxFormat.SetMultiLineBorder(const Value: Boolean);
begin
  FMultiLineBorder := Value;
  Change;
end;

procedure TSyntaxFormat.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  Change;
end;

procedure TSyntaxFormat.SetChangeCase(const Value: TChangeCase);
begin
  FChangeCase := Value;
  Change;
end;

function TSyntaxFormat.HasBorder: Boolean;
var i: integer;
begin
  for i := 0 to 3 do
   if FBorderTypes[i] <> blNone then
     begin
      Result := True;
      Exit;
     end;
  Result := False;
end;

procedure TSyntaxFormat.SetFormatFlags(const Value: TFormatFlags);
begin
  if FFormatFlags <> Value then
    begin
      FFormatFlags := Value;
      Change;
    end;
end;

procedure TSyntaxFormat.ApplyTo(Canvas: TCanvas; AllowChangeFont: Boolean);
var fs: TFontStyles;
  procedure SwitchFontFlag(ff: TFormatFlag; f: TFontStyle);
  begin
    if ff in FormatFlags then
      if f in Font.Style then Include(fs, f)
       else Exclude(fs, f);
  end;
begin
  if not Enabled then Exit;

  if BgColor <> clNone then
    Canvas.Brush.Color := BgColor;

  if FormatType = ftBackGround then Exit else
   begin
     if Font.Color <> clNone then
       Canvas.Font.Color := Font.Color;
     if FormatType <> ftColor then
      begin
       fs := Canvas.Font.Style;
       SwitchFontFlag(ffBold, fsBold);
       SwitchFontFlag(ffItalic, fsItalic);
       SwitchFontFlag(ffUnderline, fsUnderline);
       SwitchFontFlag(ffStrikeOut, fsStrikeOut);
       if Canvas.Font.Style <> fs then
         Canvas.Font.Style := fs;
       if (FormatType = ftCustomFont) and AllowChangeFont then
        begin
          if ffFontName in FormatFlags then
            Canvas.Font.Name := Font.Name;
          if ffFontCharset in FormatFlags then
            Canvas.Font.Charset := Font.Charset;
          if ffFontSize in FormatFlags then
            Canvas.Font.Size := Font.Size;
        end;
      end;
   end;
end;

function TSyntaxFormat.IsEqual(Other: TSyntaxFormat): Boolean;
begin
  Result := (BgColor = Other.BgColor) and
            (FormatType = Other.FormatType) and
            (FormatFlags = Other.FormatFlags) and
            (Hidden = Other.Hidden) and
            (ReadOnly = Other.ReadOnly) and
            (ChangeCase = Other.ChangeCase) and
            (VertAlignment = Other.VertAlignment);
  if Result and (FormatType <> ftBackGround) then
    begin
      Result := Font.Color = Other.Font.Color;
      if Result and (FormatType <> ftColor) then
        begin
          Result := Font.Style = Other.Font.Style;
          if Result and (FormatType <> ftFontAttr) then
            begin
              Result := (not (ffFontName in FormatFlags) or
                         (Font.Name = Other.Font.Name))
                        and
                        (not (ffFontSize in FormatFlags) or
                         (Font.Size = Other.Font.Size))
                        and
                        (not (ffFontCharSet in FormatFlags) or
                         (Font.Charset = Other.Font.Charset));
            end;
        end;
    end;
end;

procedure TSyntaxFormat.Merge(Over: TSyntaxFormat);
var fs: TFontStyles;
  procedure SwitchFontFlag(ff: TFormatFlag; f: TFontStyle);
  begin
    if ff in Over.FormatFlags then
      begin
        Include(FFormatFlags, ff);
        if f in Over.Font.Style then Include(fs, f)
         else Exclude(fs, f);
      end;
  end;
begin
  if ffVertAlign in Over.FormatFlags then
    VertAlignment := Over.VertAlignment;
  if ffHidden in Over.FormatFlags then
    Hidden := Over.Hidden;
  if ffReadOnly in Over.FormatFlags then
    ReadOnly := Over.ReadOnly;
  if Over.BgColor <> clNone then
    BgColor := Over.BgColor;
  if Over.ChangeCase <> ccNone then
    ChangeCase := Over.ChangeCase;
  if Over.FormatType <> ftBackGround then
    begin
      if Over.Font.Color <> clNone then
        Font.Color := Over.Font.Color;
      if Over.FormatType <> ftColor then
        begin
          fs := Font.Style;
          SwitchFontFlag(ffBold, fsBold);
          SwitchFontFlag(ffItalic, fsItalic);
          SwitchFontFlag(ffUnderline, fsUnderline);
          SwitchFontFlag(ffStrikeOut, fsStrikeOut);
          Font.Style := fs;
          if Over.FormatType <> ftFontAttr then
            begin
              if ffFontName in Over.FormatFlags then
                Font.Name := Over.Font.Name;
              if ffFontCharset in Over.FormatFlags then
                Font.Charset := Over.Font.Charset;
              if ffFontSize in Over.FormatFlags then
                Font.Size := Over.Font.Size;
            end;
        end;
    end;
  FormatFlags := FormatFlags + Over.FormatFlags;
end;

function TSyntaxFormat.GetHidden: Boolean;
begin
  Result := FHidden and (ffHidden in FFormatFlags);
end;

procedure TSyntaxFormat.Intersect(Over: TSyntaxFormat);
begin
  FormatFlags := Over.FormatFlags * FormatFlags;
  if Over.FormatType < FormatType then
    FormatType := Over.FormatType;

  if (ffVertAlign in FormatFlags) and
     (VertAlignment <> Over.VertAlignment) then
    FormatFlags := FormatFlags - [ffVertAlign];

  if (ffReadOnly in FormatFlags) and
     (ReadOnly <> Over.ReadOnly) then
    FormatFlags := FormatFlags - [ffReadOnly];

  if (ffHidden in FormatFlags) and
     (Hidden <> Over.Hidden) then
    FormatFlags := FormatFlags - [ffHidden];

  if Over.ChangeCase <> ChangeCase then
    ChangeCase := ccNone;

  if BgColor <> Over.BgColor then
    BgColor := clNone;

  if FormatType = ftBackGround then Exit;

  if Font.Color <> Over.Font.Color then
    Font.Color := clNone;

  if FormatType = ftColor then Exit;

  if (ffBold in FormatFlags) and
     ((fsBold in Font.Style) <> (fsBold in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffBold];

  if (ffItalic in FormatFlags) and
     ((fsItalic in Font.Style) <> (fsItalic in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffItalic];

  if (ffUnderline in FormatFlags) and
     ((fsUnderline in Font.Style) <> (fsUnderline in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffUnderline];

  if (ffStrikeOut in FormatFlags) and
     ((fsStrikeOut in Font.Style) <> (fsStrikeOut in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffStrikeOut];

  if FormatType = ftFontAttr then Exit;

  if (ffFontName in FormatFlags) and
     (not SameText(Font.Name, Over.Font.Name)) then
    FormatFlags := FormatFlags - [ffFontName];

  if (ffFontSize in FormatFlags) and
     (Font.Size <> Over.Font.Size) then
    FormatFlags := FormatFlags - [ffFontSize];

  if (ffFontCharset in FormatFlags) and
     (Font.Charset <> Over.Font.Charset) then
    FormatFlags := FormatFlags - [ffFontCharset];
end;

{ TStylesCollection }

function TStylesCollection.Add: TSyntaxFormat;
begin
  Result := (inherited Add) as TSyntaxFormat;
end;

constructor TStylesCollection.Create;
begin
  inherited Create(TSyntaxFormat);
end;

function TStylesCollection.GetItem(Index: integer): TSyntaxFormat;
begin
  Result := (inherited Items[Index]) as TSyntaxFormat;
end;

function TStylesCollection.Synchronize(Source: TStylesCollection): integer;
var j: integer;
    f: TSyntaxFormat;
begin
  Result := 0;
  for j := 0 to Count - 1 do
   begin
     f := TSyntaxFormat(Source.ItemByName(Items[j].DisplayName));
     if f <> nil then
      begin
        Inc(Result);
        Items[j].Assign(f);
      end;
   end;
end;

{ TSingleTagCondition }

procedure TSingleTagCondition.AssignTo(Dest: TPersistent);
var dst: TSingleTagCondition;
begin
  if Dest is TSingleTagCondition then
   begin
     dst := Dest as TSingleTagCondition;
     dst.CondType := CondType;
     dst.TokenTypes := TokenTypes;
     dst.FTagList.Assign(FTagList);
     dst.IgnoreCase := IgnoreCase;
   end;
end;

function TSingleTagCondition.CheckToken(const Source: ecString;
  Token: TSyntToken): Boolean;
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
  if FTagList.Count > 0 then
   begin
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

constructor TSingleTagCondition.Create(Collection: TCollection);
begin
  inherited;
  FCondType := tcEqual;
  FTagList := TzStringList.Create;
  TzStringList(FTagList).Sorted := true;
  TzStringList(FTagList).Delimiter := ' ';
  TzStringList(FTagList).Duplicates := dupIgnore;
  TzStringList(FTagList).CaseSensitive := True;
  TzStringList(FTagList).OnChange := TagListChanged;
  {$IFDEF EC_VCL6_UP}
  TzStringList(FTagList).QuoteChar := ' ';
  {$ENDIF}
end;

destructor TSingleTagCondition.Destroy;
begin
  FreeAndNil(FTagList);
  inherited;
end;

procedure TSingleTagCondition.SetIgnoreCase(const Value: Boolean);
begin
  TzStringList(FTagList).CaseSensitive := not Value;
end;

function TSingleTagCondition.GetIgnoreCase: Boolean;
begin
  Result := not TzStringList(FTagList).CaseSensitive;
end;

procedure TSingleTagCondition.SetTagList(const Value: TStrings);
begin
  TzStringList(FTagList).DelimitedText := Value.Text;
  Changed(False);
end;

procedure TSingleTagCondition.SetTokenTypes(const Value: DWORD);
begin
  FTokenTypes := Value;
  Changed(False);
end;

procedure TSingleTagCondition.SetCondType(const Value: TTagConditionType);
begin
  FCondType := Value;
  Changed(False);
end;

procedure TSingleTagCondition.TagListChanged(Sender: TObject);
begin
  Changed(False);
end;

{ TConditionCollection }

function TConditionCollection.Add: TSingleTagCondition;
begin
  Result := (inherited Add) as TSingleTagCondition;
end;

constructor TConditionCollection.Create(AOwner: TTagBlockCondition);
begin
  inherited Create(TSingleTagCondition);
  FOwner := AOwner;
  PropName := 'Conditions';
end;

function TConditionCollection.GetItem(Index: integer): TSingleTagCondition;
begin
  Result := (inherited Items[Index]) as TSingleTagCondition;
end;

function TConditionCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TConditionCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TTagBlockCondition }

constructor TTagBlockCondition.Create(Collection: TCollection);
begin
  inherited;
  FConditions := TConditionCollection.Create(Self);
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

procedure TTagBlockCondition.AssignTo(Dest: TPersistent);
var dst: TTagBlockCondition;
begin
  inherited;
  if Dest is TTagBlockCondition then
   begin
     dst := Dest as TTagBlockCondition;
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

function TTagBlockCondition.Check(const Source: ecString;
  Tags: TClientSyntAnalyzer; N: integer; var RefIdx: integer): Boolean;
var i, offs, idx, skipped, skip_cond: integer;
begin
  Result := False;
  offs := CheckOffset;
  skipped := 0;
  skip_cond := 0;
  i := 0;
  while i < ConditionList.Count do
  begin
   idx := N - 1 - i - offs - skipped + skip_cond;
   if (ConditionList[i].CondType = tcSkip) and (i < ConditionList.Count - 1)
      and (ConditionList[i+1].CondType <> tcSkip) then
     begin
      inc(i);
      inc(skip_cond);
      while (idx >= 0) and not ConditionList[i].CheckToken(Source, Tags[idx]) do
       begin
         if not ConditionList[i - 1].CheckToken(Source, Tags[idx]) then
           Exit;
         dec(idx);
         inc(skipped);
       end;
      if idx < 0 then Exit;
     end;
   with ConditionList[i] do
    if (idx < 0) or not CheckToken(Source, Tags[idx]) then Exit;
   inc(i);
  end;

  Result := ConditionList.Count > 0;
//  if FRefToCondEnd then
  RefIdx := N - ConditionList.Count - offs - skipped + skip_cond;
//  else
//    RefIdx := N - 1 - offs;
end;

destructor TTagBlockCondition.Destroy;
begin
  FreeAndNil(FConditions);
  FreeAndNil(FPen);
  inherited;
end;

function TTagBlockCondition.GetItemBaseName: string;
begin
  Result := 'Tag block rule';
end;

function TTagBlockCondition.GetBlockEndName: string;
var FSynt: TSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := FBlockEndName
  else
   if Assigned(FBlockEndCond) then
    Result := FBlockEndCond.DisplayName
   else
    Result := '';
end;

procedure TTagBlockCondition.SetBlockEndName(const Value: string);
var FSynt: TSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    FBlockEndName := Value
  else
    FBlockEndCond := TTagBlockCondition(TBlockRuleCollection(Collection).ItemByName(Value));
  Changed(False);
end;

procedure TTagBlockCondition.Loaded;
var FSynt: TSyntAnalyzer;
begin
  inherited;
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if FBlockEndName <> '' then
    FBlockEndCond := TTagBlockCondition(FSynt.FBlockRules.ItemByName(FBlockEndName));
  if FTreeItemStyle <> '' then
    FTreeItemStyleObj := TSyntaxFormat(FSynt.Formats.ItemByName(FTreeItemStyle));
  if FTreeGroupStyle <> '' then
    FTreeGroupStyleObj := TSyntaxFormat(FSynt.Formats.ItemByName(FTreeGroupStyle));
end;

function TTagBlockCondition.CheckOffset: integer;
begin
  Result := 0;
  if FRefToCondEnd then Exit;
  if FIdentIndex < 0 then Result := -FIdentIndex;
  if (FBlockOffset < 0) and (FBlockOffset < FIdentIndex) then
    Result := -FBlockOffset;
end;

procedure TTagBlockCondition.SetBlockType(const Value: TTagBlockType);
begin
  FBlockType := Value;
  if FBlockType in [btTagDetect, btLineBreak] then
   begin
     FBlockOffset := 0;
     FBlockEndCond := nil;
   end;
  Changed(False);
end;

procedure TTagBlockCondition.SetConditions(
  const Value: TConditionCollection);
begin
  FConditions.Assign(Value);
end;

procedure TTagBlockCondition.ConditionsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TTagBlockCondition.SetBlockEndCond(
  const Value: TTagBlockCondition);
begin
  if FBlockEndCond <> Value then
    begin
      FBlockEndCond := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetLinePos(const Value: TLineBreakPos);
begin
  if FLinePos <> Value then
    begin
      FLinePos := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetIdentIndex(const Value: integer);
begin
  if FIdentIndex <> Value then
    begin
      FIdentIndex := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetBlockOffset(const Value: integer);
begin
  if FBlockOffset <> Value then
    begin
      FBlockOffset := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetEndOfTextClose(const Value: Boolean);
begin
  if FEndOfTextClose <> Value then
    begin
      FEndOfTextClose := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetNotCollapsed(const Value: Boolean);
begin
  if FNotCollapsed <> Value then
    begin
      FNotCollapsed := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetSameIdent(const Value: Boolean);
begin
  if FSameIdent <> Value then
    begin
      FSameIdent := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetHighlight(const Value: Boolean);
begin
  if FHighlight <> Value then
    begin
      FHighlight := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetInvertColors(const Value: Boolean);
begin
  if FInvertColors <> Value then
    begin
      FInvertColors := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetDisplayInTree(const Value: Boolean);
begin
  if FDisplayInTree <> Value then
    begin
      FDisplayInTree := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetCancelNextRules(const Value: Boolean);
begin
  if FCancelNextRules <> Value then
    begin
      FCancelNextRules := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetDynHighlight(
  const Value: TDynamicHighlight);
begin
  if FDynHighlight <> Value then
    begin
      FDynHighlight := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetDynSelectMin(const Value: Boolean);
begin
  if FDynSelectMin <> Value then
    begin
      FDynSelectMin := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetGroupFmt(const Value: ecString);
begin
  if FGroupFmt <> Value then
    begin
      FGroupFmt := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetHighlightPos(const Value: THighlightPos);
begin
  if FHighlightPos <> Value then
    begin
      FHighlightPos := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetNameFmt(const Value: ecString);
begin
  if FNameFmt <> Value then
    begin
      FNameFmt := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetRefToCondEnd(const Value: Boolean);
begin
  if FRefToCondEnd <> Value then
    begin
      FRefToCondEnd := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetDrawStaple(const Value: Boolean);
begin
  if FDrawStaple <> Value then
    begin
      FDrawStaple := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetCollapseFmt(const Value: ecString);
begin
  if FCollapseFmt <> Value then
    begin
      FCollapseFmt := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetSelfClose(const Value: Boolean);
begin
  if FSelfClose <> Value then
    begin
      FSelfClose := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetNoEndRule(const Value: Boolean);
begin
  if FNoEndRule <> Value then
    begin
      FNoEndRule := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetGrammaRuleName(const Value: string);
begin
  if FGrammaRuleName <> Value then
   begin
    FGrammaRuleName := Value;
    FGrammaRule :=
      TSyntCollection(Collection).SyntOwner.Gramma.ParserRuleByName(Value);
   end;
end;

procedure TTagBlockCondition.SetTokenType(const Value: integer);
begin
  if FTokenType <> Value then
    begin
      FTokenType := Value;
      Changed(False);
    end;
end;

function TTagBlockCondition.GetTreeItemStyle: string;
begin
  Result := ValidStyleName(FTreeItemStyle, FTreeItemStyleObj);
end;

procedure TTagBlockCondition.SetTreeItemStyle(const Value: string);
begin
  ValidSetStyle(Value, FTreeItemStyle, FTreeItemStyleObj);
end;

function TTagBlockCondition.GetTreeGroupStyle: string;
begin
  Result := ValidStyleName(FTreeGroupStyle, FTreeGroupStyleObj);
end;

procedure TTagBlockCondition.SetTreeGroupStyle(const Value: string);
begin
  ValidSetStyle(Value, FTreeGroupStyle, FTreeGroupStyleObj);
end;

procedure TTagBlockCondition.SetTreeGroupImage(const Value: integer);
begin
  if FTreeGroupImage <> Value then
    begin
      FTreeGroupImage := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetTreeItemImage(const Value: integer);
begin
  if FTreeItemImage <> Value then
    begin
      FTreeItemImage := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TTagBlockCondition.SetUseCustomPen(const Value: Boolean);
begin
  if FUseCustomPen <> Value then
    begin
      FUseCustomPen := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetIgnoreAsParent(const Value: Boolean);
begin
  if FIgnoreAsParent <> Value then
    begin
      FIgnoreAsParent := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetAutoCloseText(Value: ecString);
begin
  if Value = sLineBreak then
    Value := '';
  if FAutoCloseText <> Value then
    begin
      FAutoCloseText := Value;
      Changed(False);
    end;
end;

procedure TTagBlockCondition.SetAutoCloseMode(const Value: TAutoCloseMode);
begin
  if FAutoCloseMode <> Value then
    begin
      FAutoCloseMode := Value;
      Changed(False);
    end;
end;

{ TBlockRuleCollection }

function TBlockRuleCollection.Add: TTagBlockCondition;
begin
  Result := inherited Add as TTagBlockCondition;
end;

constructor TBlockRuleCollection.Create;
begin
  inherited Create(TTagBlockCondition);
end;

function TBlockRuleCollection.GetItem(Index: integer): TTagBlockCondition;
begin
  Result := inherited Items[Index] as TTagBlockCondition;
end;

{ TTokenRule }

procedure TTokenRule.AssignTo(Dest: TPersistent);
var dst: TTokenRule;
begin
  inherited;
  if Dest is TTokenRule then
   begin
    dst := Dest as TTokenRule;
    dst.FTokenType := FTokenType;
    dst.FRegExpr.Expression := Expression;
    dst.OnMatchToken := OnMatchToken;
    dst.OnMouseClick := OnMouseClick;
    dst.ColumnFrom := ColumnFrom;
    dst.ColumnTo := ColumnTo;
   end;
end;

constructor TTokenRule.Create(Collection: TCollection);
begin
  inherited;
  FBlock := nil;
  FFormat := nil;
  FRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FRegExpr);
end;

destructor TTokenRule.Destroy;
begin
  FreeAndNil(FRegExpr);
  inherited;
end;

function TTokenRule.GetExpression: ecString;
begin
  Result := FRegExpr.Expression;
end;

function TTokenRule.GetIsInvalid: Boolean;
begin
  Result := FRegExpr.IsInvalid;
end;

function TTokenRule.GetItemBaseName: string;
begin
  Result := 'Token rule';
end;

function TTokenRule.Match(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

procedure TTokenRule.SetColumnFrom(const Value: integer);
begin
  if FColumnFrom <> Value then
    begin
      FColumnFrom := Value;
      Changed(False);
    end;
end;

procedure TTokenRule.SetColumnTo(const Value: integer);
begin
  if FColumnTo <> Value then
    begin
      FColumnTo := Value;
      Changed(False);
    end;
end;

procedure TTokenRule.SetExpression(const Value: ecString);
begin
  try
    FRegExpr.Expression := Value;
  except
    Application.HandleException(Self);
  end;
  Changed(False);
end;

procedure TTokenRule.SetTokenType(const Value: integer);
begin
  if FTokenType <> Value then
    begin
      FTokenType := Value;
      Changed(False);
    end;
end;

{ TRuleCollectionItem }

function TRuleCollectionItem.ValidStyleName(const AStyleName: string;
  AStyle: TSyntaxFormat): string;
var FSynt: TSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  Result := '';
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := AStyleName
  else
   if Assigned(AStyle) then
    Result := AStyle.DisplayName;
end;

function TRuleCollectionItem.ValidSetStyle(const AStyleName: string;
  var AStyleField: string; var AStyle: TSyntaxFormat): string;
var FSynt: TSyntAnalyzer;
begin
  Result := '';
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    AStyleField := AStyleName
  else
    AStyle := TSyntaxFormat(FSynt.FFormats.ItemByName(AStyleName));
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
var FSynt: TSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if FStyleName <> '' then
    FFormat := TSyntaxFormat(FSynt.FFormats.ItemByName(FStyleName));
  if FBlockName <> '' then
    FBlock := TTagBlockCondition(FSynt.BlockRules.ItemByName(FBlockName));
end;

function TRuleCollectionItem.GetBlockName: string;
var FSynt: TSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
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
var FSynt: TSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    FBlockName := Value
  else
   begin
//    FBlock := TTagBlockCondition(FSynt.BlockRules.ItemByName(Value));
    FBlock := TTagBlockCondition(FSynt.BlockRules.ItemByName(Value));
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

function TRuleCollectionItem.GetSyntOwner: TSyntAnalyzer;
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

{ TTokenRuleCollection }

function TTokenRuleCollection.Add: TTokenRule;
begin
  Result := inherited Add as TTokenRule;
end;

constructor TTokenRuleCollection.Create;
begin
  inherited Create(TTokenRule);
end;

function TTokenRuleCollection.GetItem(Index: integer): TTokenRule;
begin
  Result := inherited Items[Index] as TTokenRule;
end;

{ TParserResults }

constructor TParserResults.Create(AOwner: TSyntAnalyzer;
  SrcProc: TATStringBuffer; const AClient: IecSyntClient);
begin
  inherited Create;
  if SrcProc = nil then
   raise Exception.Create('Source procedure not passed from syntax server');
  FOwner := AOwner;
  FSrcProc := SrcProc;
  FClient := AClient;
  FTagList := TRangeList.Create(False);
  FSubLexerBlocks := TObjectList.Create;
  FOwner.FClientList.Add(Self);
  FCurState := 0;
  FStateChanges := TObjectList.Create;
end;

destructor TParserResults.Destroy;
begin
  FOwner.FClientList.Remove(Self);
  FreeAndNil(FTagList);
  FreeAndNil(FSubLexerBlocks);
  FreeAndNil(FStateChanges);
  inherited;
end;

procedure TParserResults.Clear;
begin
  FTagList.Clear;
  FSubLexerBlocks.Clear;
  FStateChanges.Clear;
  FCurState := 0;
end;

procedure TParserResults.Finished;
//var i: integer;
begin
//  if FFinished then Exit;
  FFinished := True;
  // Performs Gramma parsing
//  AnalyzeGramma;
end;

function TParserResults.IsEnabled(Rule: TRuleCollectionItem;
  OnlyGlobal: Boolean): Boolean;
begin
  Result := Rule.Enabled and (not OnlyGlobal or Rule.AlwaysEnabled) and
            ((Rule.StatesPresent = 0) or ((FCurState and Rule.StatesPresent) = Rule.StatesPresent)) and
            ((Rule.StatesAbsent = 0) or ((FCurState and Rule.StatesAbsent) = 0));
end;

function TParserResults.GetTokenCount: integer;
begin
  Result := FTagList.Count;
end;

function TParserResults.GetTags(Index: integer): TSyntToken;
begin
  Result := TSyntToken(FTagList[Index]);
end;

function TParserResults.GetTokenStr(Index: integer): ecString;
begin
  with Tags[Index] do
    Result := FSrcProc.SubString(StartPos + 1, EndPos - StartPos);
end;

function TParserResults.GetLastPos(const Source: ecString): integer;
begin
  if FTagList.Count = 0 then Result := 1 else
    Result := TSyntToken(FTagList[FTagList.Count - 1]).EndPos + 1;
  if FLastAnalPos > Result then Result := FLastAnalPos;
end;

procedure TParserResults.SaveState;
var b: Boolean;
begin
 if FStateChanges.Count = 0 then
   b := FCurState <> 0
 else
   b := FCurState <> TRange(FStateChanges.Last).EndPos;
 if b then
   FStateChanges.Add(TRange.Create(FTagList.Count, FCurState));
end;

// True if end of the text
function TParserResults.ExtractTag(const Source: ecString; var FPos: integer; IsIdle: Boolean): Boolean;
var N: integer;
    p: TSyntToken;
    own: TSyntAnalyzer;

   // Select current lexer
   procedure GetOwner;
   var i, N: integer;
   begin
    own := FOwner;
    for i := FSubLexerBlocks.Count - 1 downto 0 do
     with TSubLexerRange(FSubLexerBlocks[i]) do
       if FPos > StartPos then
        if EndPos = -1 then
          begin
            // try close sub lexer
    //        if Rule.ToTextEnd then N := 0 else
            N := Rule.MatchEnd(Source, FPos);
            if N > 0 then
             begin
               if Rule.IncludeBounds then
                 begin // New mode in v2.35
                   FEndPos := FPos - 1 + N;
                   FCondEndPos := FEndPos;
                   own := Rule.SyntAnalyzer;
                 end else
                 begin
                   FEndPos := FPos - 1;
                   FCondEndPos := FEndPos + N;
                 end;
               // Close ranges which belongs to this sub-lexer range
               CloseAtEnd(FTagList.PriorAt(StartPos));
             end else
             begin
               own := Rule.SyntAnalyzer;
               Exit;
             end;
          end else
       if FPos < EndPos then
         begin
               own := Rule.SyntAnalyzer;
               Exit;
         end;
   end;

   procedure CheckIntersect;
   var i: integer;
   begin
    for i := FSubLexerBlocks.Count - 1 downto 0 do
     with TSubLexerRange(FSubLexerBlocks[i]) do
      if (p.EndPos > StartPos) and (p.StartPos < StartPos) then
       begin
        p.FEndPos := StartPos;
        Exit;
       end;
   end;

   function CanOpen(Rule: TSubAnalyzerRule): Boolean;
   var N: integer;
       sub: TSubLexerRange;
   begin
     Result := IsEnabled(Rule, False) and (Rule.SyntAnalyzer <> nil);
     if not Result then Exit;
     Result := Rule.FromTextBegin and (FPos = 1);
     if Result then N := 0 else
                    N := Rule.MatchStart(Source, FPos);
     Result := Result or (N > 0);
     if not Result then Exit;
     // To prevent repeated opening
     if FSubLexerBlocks.Count > 0 then
       if (TSubLexerRange(FSubLexerBlocks.Last).EndPos = FPos - 1) and
          (TSubLexerRange(FSubLexerBlocks.Last).Rule = Rule) then Exit;

     ApplyStates(Rule);
     sub := TSubLexerRange.Create(0,0);
     sub.FRule := Rule;
     sub.FCondStartPos := FPos - 1;
     if Rule.IncludeBounds then
       sub.FStartPos := FPos - 1
     else
       sub.FStartPos := FPos + N - 1;
     sub.FEndPos := -1;
     sub.FCondEndPos := -1;
     FSubLexerBlocks.Add(sub);
   end;


   procedure TryOpenSubLexer;
   var i: integer;
   begin
     for i := 0 to own.SubAnalyzers.Count - 1 do
      if CanOpen(own.SubAnalyzers[i]) then Exit;
     if own <> FOwner then
      for i := 0 to FOwner.SubAnalyzers.Count - 1 do
       if FOwner.SubAnalyzers[i].AlwaysEnabled and CanOpen(FOwner.SubAnalyzers[i]) then Exit;
   end;

begin
  GetOwner;
  TryOpenSubLexer;
  if own.SkipSpaces then
    begin
     if own.ParseEndOfLine then N := SkipSpacesNoLineBreak(Source, FPos)
      else N := SkipSpaces(Source, FPos);
    end
   else if FPos > Length(Source) then N := -1 else N := 0;
  TryOpenSubLexer;
  GetOwner;

  Result := N = -1;
  if Result then Exit;

  p := FOwner.GetToken(Self, Source, FPos, own <> FOwner);
  if (own <> FOwner) and (p = nil) then
    p := own.GetToken(Self, Source, FPos, False);
  if p = nil then  // no token
   begin
     Inc(FPos);
   end else
   begin
    CheckIntersect;
    SaveState;
    FTagList.Add(p);
    if not FOwner.SeparateBlockAnalysis then
     begin
      FOwner.SelectTokenFormat(Self, Source, own <> FOwner);
      if own <> FOwner then
        own.SelectTokenFormat(Self, Source, False);
     end else
//    if not IsIdle then
     begin  // Only for first iteration of analysis
      FOwner.HighlightKeywords(Self, Source, own <> FOwner);
      if own <> FOwner then
        own.HighlightKeywords(Self, Source, False);
     end;
    FPos := p.EndPos + 1;
   end;
   FLastAnalPos := FPos;
end;

function TParserResults.AnalyzerAtPos(Pos: integer): TSyntAnalyzer;
var i: integer;
begin
 Result := FOwner;
 if Pos >= 0 then
 for i := 0 to FSubLexerBlocks.Count - 1 do
  with TSubLexerRange(FSubLexerBlocks[i]) do
   if Pos < StartPos then Break else
    if (EndPos = -1) or (Pos < EndPos) then
      Result := Rule.SyntAnalyzer;
end;

function TParserResults.GetSubLexerRangeCount: integer;
begin
  Result := FSubLexerBlocks.Count;
end;

function TParserResults.GetSubLexerRange(Index: integer): TSubLexerRange;
begin
  Result := TSubLexerRange(FSubLexerBlocks[Index]);
end;

function TParserResults.GetTokenType(Index: integer): integer;
begin
  Result := Tags[Index].TokenType;
end;

type
  // Special object to handle dyno ranges
  TDynoRange = class(TRange)
    FRange: TObject; // Reference to range (only for link with TStyledRange)
    FRule: TObject;  // Is not nil for selecting nearest range of the rule
  public
    constructor Create(AStart, AEnd: integer; ARange: TTextRange);
  end;

  TTextChangeInfo = class
    FPos: integer;
    FCount: integer;
  public
    constructor Create(Pos, Count: integer);
  end;

  TCollapsibleRange = class(TRange)
    FSource: integer;
  public
    constructor Create(ASource, AStart, AEnd: integer);
  end;

constructor TDynoRange.Create(AStart, AEnd: integer; ARange: TTextRange);
begin
 inherited Create(AStart, AEnd);
 FRange := ARange;
 if ARange.Rule.DynSelectMin then
  FRule := ARange.Rule;
end;

constructor TTextChangeInfo.Create(Pos, Count: integer);
begin
  inherited Create;
  FPos := Pos;
  FCount := Count;
end;

constructor TCollapsibleRange.Create(ASource, AStart, AEnd: integer);
begin
  inherited Create(AStart, AEnd);
  FSource := ASource;
end;

procedure TParserResults.ApplyStates(Rule: TRuleCollectionItem);
begin
  if Rule.StatesRemove <> 0 then
    FCurState := FCurState and not Rule.StatesRemove;
  if Rule.StatesAdd <> 0 then
    FCurState := FCurState or Rule.StatesAdd;
end;

procedure TParserResults.RestoreState;
var i: integer;
begin
  for i := FStateChanges.Count - 1 downto 0 do
    if TRange(FStateChanges.Last).StartPos >= TagCount then
      FStateChanges.Delete(FStateChanges.Count - 1)
    else
      Break;
  if FStateChanges.Count > 0 then
    FCurState := TRange(FStateChanges.Last).EndPos
  else
    FCurState := 0;
end;

function TParserResults.ParserStateAtPos(TokenIndex: integer): integer;
var i: integer;
begin
   for i := FStateChanges.Count - 1 downto 0 do
     if TRange(FStateChanges[i]).StartPos <= TokenIndex then
       begin
         Result := TRange(FStateChanges[i]).EndPos;
         Exit;
       end;
   Result := 0;
end;

{ TChangeFixer }

constructor TChangeFixer.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TChangeFixer.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TChangeFixer.Clear;
begin
  FList.Clear;
end;

procedure TChangeFixer.Add(Pos, Count: integer);
begin
  FList.Add(TTextChangeInfo.Create(Pos, Count));
end;

function TChangeFixer.CurToOld(CurPos: integer): integer;
var i: integer;
begin
  for i := FList.Count - 1 downto 0 do
   with TTextChangeInfo(FList[i]) do
    if CurPos > FPos then
     if (FCount > 0) and (FPos + FCount > CurPos) then CurPos := FPos
      else Dec(CurPos, FCount);
  Result := CurPos;
end;

function TChangeFixer.OldToCur(OldPos: integer): integer;
var i: integer;
begin
  for i := 0 to FList.Count - 1 do
   with TTextChangeInfo(FList[i]) do
    if OldPos >= FPos then
     if (FCount < 0) and (FPos - FCount > OldPos) then OldPos := FPos
      else Inc(OldPos, FCount);
  Result := OldPos;
end;

function TChangeFixer.UpOldNoChange: integer;
var i, t: integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
   with TTextChangeInfo(FList[i]) do
    begin
      if FPos < Result then
       if (FCount < 0) and (FPos - FCount > Result) then Result := FPos
        else Inc(Result, FCount);
      if FCount > 0 then t := FPos + FCount
       else t := FPos;
      if Result < t then Result := t;
    end;
end;

type
  TSyntFinishThread = class(TThread)
  private
    FOwner: TClientSyntAnalyzer;
    FRangeStyles: TRangeCollection;    // all styled blocks (without tokens)
    FDynoConditions: TRangeCollection; // dynamic ranges hot-points
    FCollapsables: TRangeCollection;   // ranges that can be collapsed
    FStaples: TRangeCollection;        // ranges that can be collapsed
    FLineBreakRanges: TRangeCollection;// ranges maked with line breaks
  protected
    procedure Execute; override;
    property Owner: TClientSyntAnalyzer read FOwner;
  public
    constructor Create(AOwner: TClientSyntAnalyzer);
    destructor Destroy; override;
  end;

constructor TSyntFinishThread.Create(AOwner: TClientSyntAnalyzer);
begin
  FOwner := AOwner;
  FRangeStyles := TRangeCollection.Create;
  FDynoConditions := TRangeCollection.Create;
  FCollapsables := TRangeCollection.Create;
  FStaples := TRangeCollection.Create;
  FLineBreakRanges := TRangeCollection.Create;

  inherited Create(False);
  OnTerminate := AOwner.FinishThreadEnd;
  FreeOnTerminate := True;
end;

destructor TSyntFinishThread.Destroy;
begin
  {//AT
  if Terminated then
   begin
    FreeAndNil(FRangeStyles);
    FreeAndNil(FDynoConditions);
    FreeAndNil(FCollapsables);
    FreeAndNil(FStaples);
    FreeAndNil(FLineBreakRanges);
   end;
  }
  inherited;
end;

procedure TSyntFinishThread.Execute;
var i, sIdx, eIdx: integer;
    Flags: Byte;
    Range: TTextRange;
    Staple: TBlockStaple;
    StlRange1, StlRange2: TStyledRange;
    Lb: TLineBreakRange;
begin
  if Terminated then Exit;
  Owner.Lock;
  try
    for i := Owner.FSubLexerBlocks.Count - 1 downto 0 do
     if Terminated then Exit else
     with TSubLexerRange(Owner.FSubLexerBlocks[i]) do
      if Rule.Style <> nil then
       begin
         StlRange1 := TStyledRange.Create(StartPos, EndPos, Rule.Style);
         StlRange1.FFlags := srSubLexerHighlight;
         FRangeStyles.Add(StlRange1);
       end;

    if Terminated then Exit else
    for i := 0 to Owner.RangeCount - 1 do
     if Terminated then Exit else
     with Owner.Ranges[i] do
      begin
        if Rule.DrawStaple and  (EndIdx <> -1) then
          begin
            sIdx := StartIdx;
            eIdx := EndIdx;
            if Assigned(FOwner.Owner.OnGetStapleRange) then
              FOwner.Owner.OnGetStapleRange(FOwner, Owner.Ranges[i], sIdx, eIdx);
            if eIdx <> -1 then
              begin
                Staple := TBlockStaple.Create(Owner.Tags[sIdx].StartPos, Owner.Tags[eIdx].EndPos);
                Staple.FRule := Owner.Ranges[i].Rule;
                Staple.XPos := -1;
                FStaples.Add(Staple);
              end;
          end;

        // Collapsable ranges
        if (EndIdx <> -1) and not Rule.NotCollapsed then
          begin
            sIdx := StartIdx;
            eIdx := EndIdx;
            if Assigned(FOwner.Owner.OnGetCollapseRange) then
              FOwner.Owner.OnGetCollapseRange(FOwner, Owner.Ranges[i], sIdx, eIdx);
            if eIdx <> -1 then
              FCollapsables.Add(TCollapsibleRange.Create(i, Owner.Tags[sIdx].StartPos, Owner.Tags[eIdx].EndPos));
          end;

        // Ranges with highlighting
        if (Rule.Style <> nil) and (Rule.DynHighlight <> dhNone) then
         begin
           // Creating styled ranges
           StlRange1 := nil;
           StlRange2 := nil;
           case Rule.DynHighlight of
             dhBound: begin
                         StlRange1 := TStyledRange.Create(
                                   Owner.Tags[StartIdx].StartPos,
                                   Owner.Tags[StartIdx].EndPos,
                                   Rule.Style);
                         if EndIdx <> -1 then
                           StlRange2 := TStyledRange.Create(
                                   Owner.Tags[EndIdx].StartPos,
                                   Owner.Tags[EndIdx].EndPos,
                                   Rule.Style);
                      end;
             dhRangeNoBound:
               if EndIdx <> -1 then
                StlRange1 := TStyledRange.Create(Owner.Tags[StartIdx].EndPos, Owner.Tags[EndIdx].StartPos, Rule.Style);
             dhRange:
               if EndIdx <> -1 then
                StlRange1 := TStyledRange.Create(Owner.Tags[StartIdx].StartPos, Owner.Tags[EndIdx].EndPos, Rule.Style);
           end;
           if StlRange1 = nil then Continue;
           // Creaing dynamic conditions
           if Rule.HighlightPos <> cpAny then
            begin
              Range := Owner.Ranges[i];
              case Rule.HighlightPos of
                cpBound:
                 begin
                  FDynoConditions.Add(TDynoRange.Create(Owner.Tags[StartIdx].StartPos, Owner.Tags[StartIdx].StartPos + 1, Range));
                  if EndIdx <> -1 then
                    FDynoConditions.Add(TDynoRange.Create(Owner.Tags[EndIdx].EndPos, Owner.Tags[EndIdx].EndPos + 1, Range));
                 end;
                cpBoundTag:
                 begin
                  FDynoConditions.Add(TDynoRange.Create(Owner.Tags[StartIdx].StartPos, Owner.Tags[StartIdx].EndPos, Range));
                  if EndIdx <> -1 then
                    FDynoConditions.Add(TDynoRange.Create(Owner.Tags[EndIdx].StartPos, Owner.Tags[EndIdx].EndPos, Range));
                 end;
                cpBoundTagBegin:
                 begin
                  FDynoConditions.Add(TDynoRange.Create(Owner.Tags[StartIdx].StartPos, Owner.Tags[StartIdx].StartPos + 1, Range));
                  if EndIdx <> -1 then
                    FDynoConditions.Add(TDynoRange.Create(Owner.Tags[EndIdx].StartPos, Owner.Tags[EndIdx].StartPos + 1, Range));
                 end;
                cpRange, cpOutOfRange:
                  if EndIdx <> -1 then
                    FDynoConditions.Add(TDynoRange.Create(Owner.Tags[StartIdx].StartPos, Owner.Tags[EndIdx].EndPos, Range));
              end;
              StlRange1.FRange := Range;
              if StlRange2 <> nil then StlRange2.FRange := Range;
            end;
           // Adding to lists
           Flags := 0;
           if Rule.Highlight then Inc(Flags, srLineHighlight);
           if Rule.InvertColors then Inc(Flags, srInvertSelColor);
           if Rule.HighlightPos = cpOutOfRange then Inc(Flags, srInvertCondition);
           StlRange1.FFlags := Flags;
           FRangeStyles.Add(StlRange1);
           if StlRange2 <> nil then
             begin
               StlRange2.FFlags := Flags;
               FRangeStyles.Add(StlRange2);
             end;
         end;
      end;

    if Terminated then Exit else
    for i := 0 to Owner.FLineBreaks.Count - 1 do
     if Terminated then Exit else
     with TLineBreak(Owner.FLineBreaks[i]) do
      begin
        with Owner.Tags[FRefTag] do
         Lb := TLineBreakRange.Create(StartPos, EndPos);
        Lb.FRule :=  TLineBreak(Owner.FLineBreaks[i]).Rule;
//        Lb.FPos := FRule.FLinePos;
//        if (Rule <> nil) and (Rule.Style <> nil) then
//          Lb.FColor := Rule.Style.BgColor
//        else
//          Lb.FColor := clBlack;
        FLineBreakRanges.Add(Lb);
      end;
  finally
    Owner.Unlock;
  end;
end;
{ TClientSyntAnalyzer }

constructor TClientSyntAnalyzer.Create(AOwner: TSyntAnalyzer; SrcProc: TATStringBuffer;
  const AClient: IecSyntClient);
begin
//  FFinishEvent := TEvent.Create(nil, True, False, '');
  FDataAccess := TCriticalSection.Create;

  inherited Create( AOwner, SrcProc, AClient);
  FRanges := TSortedList.Create(True);
  FLineBreaks := TObjectList.Create;
  FOpenedBlocks := TSortedList.Create(False);
  FCurDynamic := TList.Create;

  FRangeStyles := TRangeCollection.Create;
  FDynoConditions := TRangeCollection.Create;
  FCollapsables := TRangeCollection.Create;
  FChanges := TChangeFixer.Create;
  FSavedTags := TRangeList.Create;
  FStaples := TRangeCollection.Create;
  FLineBreakRanges := TRangeCollection.Create;

  FIdleTimer := TTimer.Create(nil);
  FIdleTimer.OnTimer := IntIdleAppend;
  FIdleTimer.Enabled := False;
  FIdleTimer.Interval := 100;

  IdleAppend;
end;

destructor TClientSyntAnalyzer.Destroy;
begin
  TerminateFinishThread;
  SafeDestroying(Self);

  if Assigned(FIdleTimer) then
    FIdleTimer.Enabled := False;
  FreeAndNil(FIdleTimer);

  FreeAndNil(FRangeStyles);
  FreeAndNil(FDynoConditions);
  FreeAndNil(FStaples);
  FreeAndNil(FChanges);
  FreeAndNil(FSavedTags);
  FreeAndNil(FLineBreakRanges);
  FreeAndNil(FRanges);
  FreeAndNil(FLineBreaks);
  FreeAndNil(FOpenedBlocks);
  FreeAndNil(FCurDynamic);
  FreeAndNil(FCollapsables);
  FreeAndNil(FDataAccess);
  inherited;
end;

procedure TClientSyntAnalyzer.Clear;
begin
  TerminateFinishThread;
  Lock;
  try
    inherited;
    FRepeateAnalysis := False;
    FTagList.Clear;
    FRanges.Clear;
    FLineBreaks.Clear;
    FOpenedBlocks.Clear;
    FCurDynamic.Clear;
    FRangeStyles.Clear;
    FDynoConditions.Clear;
    FCollapsables.Clear;
    FChanges.Clear;
    FSavedTags.Clear;
    FStaples.Clear;
    FLineBreakRanges.Clear;

    FFinished := False;
    FBreakIdle := True;
    FLastAnalPos := 0;
    FStartSepRangeAnal := 0;
  finally
    Unlock;
  end;
  IdleAppend;
end;

procedure TClientSyntAnalyzer.TerminateFinishThread;
begin
  if FFinishThread <> nil then
   begin
    FFinishThread.OnTerminate := nil;
    FFinishThread.Terminate;
    //FFinishThread.WaitFor; //no need
    FFinishThread := nil;
   end;
end;

procedure TClientSyntAnalyzer.FinishThreadEnd(Sender: TObject);
begin
  Lock;
  try
    FChanges.Clear;
    FSavedTags.Clear;

    FreeAndNil(FRangeStyles);
    FreeAndNil(FCollapsables);
    FreeAndNil(FDynoConditions);
    FreeAndNil(FStaples);
    FreeAndNil(FLineBreakRanges);

    if FFinishThread<>nil then
      with TSyntFinishThread(FFinishThread) do
       begin
        Self.FRangeStyles := FRangeStyles;
        Self.FCollapsables := FCollapsables;
        Self.FDynoConditions := FDynoConditions;
        Self.FStaples := FStaples;
        Self.FLineBreakRanges := FLineBreakRanges;
       end;

    FFinishThread := nil;
  finally
    Unlock;
  end;
  UpdateDyno;
  if FClient <> nil then  FClient.Finished;
end;

procedure TClientSyntAnalyzer.AddRange(Range: TTextRange);
begin
  Range.FIndex := FRanges.Count;
  FRanges.Add(Range);
  if FOpenedBlocks.Count > 0 then
    Range.FParent := TTextRange(FOpenedBlocks[FOpenedBlocks.Count - 1]);
  if Range.FEnd = -1 then
    FOpenedBlocks.Add(Range);
end;

function IndentOf(const S: ecString): Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    case S[i] of
      ' ': Inc(Result);
      #9: Inc(Result, 4);
      else Break;
   end;
end;

function TClientSyntAnalyzer.CloseRange(Cond: TTagBlockCondition; RefTag: integer): Boolean;
var j: integer;
    b: boolean;
begin
  for j := FOpenedBlocks.Count - 1 downto 0 do
   with TTextRange(FOpenedBlocks[j]) do
     if Assigned(FRule) then
       begin
         if Cond.BlockType = btRangeStart then
           b := Cond.SelfClose and (FRule = Cond)
         else
           b := (FRule.FBlockEndCond = Cond) or (FRule = Cond.FBlockEndCond);
         if b then
           begin
             if Cond.SameIdent and not SameText(TagStr[RefTag - Cond.IdentIndex] , TagStr[FIdent]) then Continue;
             FEnd := RefTag - Cond.BlockOffset;
             if (FRule = Cond) and (FEnd > 0) then Dec(FEnd); // for self closing
             FEndCondIndex := RefTag;
             if Assigned(Owner.OnCloseTextRange) then
               Owner.OnCloseTextRange(Self, TTextRange(FOpenedBlocks[j]), FStart, FEnd);
             FOpenedBlocks.Delete(j);
             Result := True;
             Exit;
           end;
       end;
  Result := False;
end;

function TClientSyntAnalyzer.HasOpened(Rule: TRuleCollectionItem; Parent: TTagBlockCondition; Strict: Boolean): Boolean;
var i: integer;
    prn: TTagBlockCondition;
begin
  if Strict then
    begin
      if FOpenedBlocks.Count > 0 then
        begin
          i := FOpenedBlocks.Count - 1;
          prn := TTextRange(FOpenedBlocks[i]).Rule;
          if (Rule is TTagBlockCondition) and TTagBlockCondition(Rule).SelfClose and (prn = Rule) then
            Dec(i);
          repeat
            if i < 0 then
              begin
                Result := False;
                Exit;
              end;
            prn := TTextRange(FOpenedBlocks[i]).Rule;
            Dec(i);
          until not prn.IgnoreAsParent;
          Result := prn = Parent;
        end else Result := Parent = nil;
    end
  else
    begin
      Result := True;
      if Parent = nil then Exit;
      for i := FOpenedBlocks.Count - 1 downto 0 do
        if TTextRange(FOpenedBlocks[i]).Rule = Parent then
          Exit;
      Result := False;
    end;
end;

procedure TClientSyntAnalyzer.Finished;
var i: integer;
begin
  if FFinished then Exit;
  inherited Finished;

  // Close SubLexers at the End of Text
  for i := FSubLexerBlocks.Count - 1 downto 0 do
   with TSubLexerRange(FSubLexerBlocks[i]) do
    if (EndPos = -1) and Rule.ToTextEnd then
     begin
       FEndPos := FSrcProc.TextLength{ - 1};
       FCondEndPos := FEndPos;
     end;

  // Close blocks at the end of text
  CloseAtEnd(0);

  {$ifdef ThreadEn}
  FFinishThread := TSyntFinishThread.Create(Self);
  {$endif}

  FRepeateAnalysis := True;
end;

procedure TClientSyntAnalyzer.IntIdleAppend(Sender: TObject);
var FPos, tmp, i: integer;
    own: TSyntAnalyzer;
begin
  if FIdleProc or FDisableIdleAppend then Exit;
  FIdleTimer.Enabled := False;
  FBreakIdle := False;
  FIdleProc := True;
  FPos := 0;
  try
    while not FBreakIdle and not FFinished do
    begin
     tmp := GetLastPos(FSrcProc.FText);
     if tmp > FPos then FPos := tmp;
     if ExtractTag(FSrcProc.FText, FPos, True) then
      begin
       if FOwner.SeparateBlockAnalysis then
        for i := FStartSepRangeAnal + 1 to TagCount do
         begin
          own := Tags[i - 1].Rule.SyntOwner;
          FOwner.SelectTokenFormat(Self, FSrcProc.FText, own <> FOwner, i);
          if own <> FOwner then
            own.SelectTokenFormat(Self, FSrcProc.FText, False, i);
          if SafeProcessMessages(Self) <> 0 then
            Exit; // Exit if analyzer is destroyed after processing messages
          if FBreakIdle then
            begin
              FIdleProc := False;
              Exit; // Exit when breaking
            end;
         end;
       Finished;
//       Exit;
      end else
      begin
        if SafeProcessMessages(Self) <> 0 then
          Exit;   // Exit if analyzer is destroyed after processing messages
      end;
    end;
  except
    //MessageBox(0, 'Error while idle', 'Error', MB_OK);
  end;
  FIdleProc := False;
end;

procedure TClientSyntAnalyzer.IdleAppend;
begin
  if not FFinished then
   begin
     FIdleTimer.Enabled := False;
     if FRepeateAnalysis then
       FIdleTimer.Interval := Owner.IdleAppendDelay
     else
       FIdleTimer.Interval := Owner.IdleAppendDelayInit;
     FIdleTimer.Enabled := True;
   end;
end;

procedure TClientSyntAnalyzer.AppendToPos(APos: integer);
var FPos: integer;
begin
  if Length(FSrcProc.FText) = 0 then Exit;
  if FFinished then Exit;
  FPos := GetLastPos(FSrcProc.FText);
  while FPos - 1 <= APos + 1 do
   begin
     if ExtractTag(FSrcProc.FText, FPos, False) then
      begin
       if not FOwner.SeparateBlockAnalysis then
         Finished else
       if not FIdleProc then
          IdleAppend; //IntIdleAppend(nil)
       Break;
      end;
   end;
end;

procedure TClientSyntAnalyzer.TryAppend(APos: integer);
var sPos, sPos1: integer;
begin
  if FSavedTags.Count = 0 then AppendToPos(APos) else
   begin
    sPos1 := FChanges.OldToCur(TSyntToken(FSavedTags[0]).StartPos);
    sPos := FChanges.UpOldNoChange;// + 1;
    if sPos1 > sPos then
       sPos := sPos1;
    if (sPos <> 0) and (sPos < APos) then AppendToPos(sPos)
     else AppendToPos(APos);
   end;
end;

procedure TClientSyntAnalyzer.ChangedAtPos(APos: integer);
var i, N: integer;

 procedure CleanRangeList(List: TSortedList; IsClosed: Boolean);
 var i: integer;
 begin
   for i := List.Count - 1 downto 0 do
    with TTextRange(List[i]) do
     if (FCondIndex >= N) or (FStart >= N) or IsClosed and
        ((FEndCondIndex >= N) or (FEnd >= N)) then
      List.Delete(i);
 end;

begin
{ if FSrcProc.TextLength <= Owner.FullRefreshSize then
  begin
   Clear;
   Exit;
  end;}

 TerminateFinishThread;
 Lock;
 try
   FFinished := False;
   Dec(APos);
   FBreakIdle := True;
   FIdleTimer.Enabled := False;
   if FSrcProc.TextLength <= Owner.FullRefreshSize then
    begin
      APos := 0;
    end else
   if Owner.RestartFromLineStart then
    begin
      N := FSrcProc.StrToCaret(APos + 1).Y;
      APos := min(APos, FSrcProc.CaretToStr(Classes.Point(0, N)));
    end;

   // Check sub lexer ranges
   for i := FSubLexerBlocks.Count - 1 downto 0 do
    with TSubLexerRange(FSubLexerBlocks[i]) do
     if APos < StartPos then
      begin
        if APos > CondStartPos then APos := CondStartPos;
        FSubLexerBlocks.Delete(i);  // remove sub lexer
      end else
     if APos < CondEndPos then
      begin
        if APos > EndPos then APos := EndPos;
        FEndPos := -1;       // open sub lexer
        FCondEndPos := -1;
      end;
   // Remove tokens
   if FSavedTags.Count = 0 then
     FTagList.ClearFromPos(APos, FSavedTags)
   else
     FTagList.ClearFromPos(APos);
   FLastAnalPos := 0;   // Reset current position
   N := FTagList.Count;
   FStartSepRangeAnal := N;
   // Remove text ranges from service containers
   CleanRangeList(FOpenedBlocks, False);
   // Remove text ranges from main storage
   for i := FRanges.Count - 1 downto 0 do
    with TTextRange(FRanges[i]) do
     if (FCondIndex >= N) or (FStart >= N) then FRanges.Delete(i)  else
      if (FEndCondIndex >= N) or (FEnd >= N) then
       begin
         FEnd := -1;
         FEndCondIndex := -1;
         FOpenedBlocks.Add(FRanges[i]);
       end;

   // Remove separators
   for i := FLineBreaks.Count - 1 downto 0 do
    if TLineBreak(FLineBreaks[i]).RefIdx >= N then
      FLineBreaks.Delete(i);

   // Restore parser state
   RestoreState;
 finally
   Unlock;
 end;
 IdleAppend;
end;

function TClientSyntAnalyzer.TokenAtPos(Pos: integer): integer;
begin
  AppendToPos(Pos);
  Result := FTagList.RangeAt(Pos);
end;

function TClientSyntAnalyzer.PriorTokenAt(Pos: integer): integer;
begin
  Result := FTagList.PriorAt(Pos);
end;

procedure TClientSyntAnalyzer.AddLineBreak(lb: TLineBreak);
begin
  FLineBreaks.Add(lb);
end;

function TClientSyntAnalyzer.GetRangeBound(Range: TTextRange): TPoint;
begin
  Result.X := TSyntToken(FTagList[Range.FStart]).StartPos;
  if Range.FEnd = - 1 then Result.Y := Result.X
   else Result.Y := TSyntToken(FTagList[Range.FEnd]).EndPos;
end;

function TClientSyntAnalyzer.GetColRangeBound(Range: TTextRange): TPoint;
var sIdx, eIdx: integer;
begin
  sIdx := Range.StartIdx;
  eIdx := Range.EndIdx;
  if Assigned(Owner.OnGetCollapseRange) then
    Owner.OnGetCollapseRange(Self, Range, sIdx, eIdx);

  Result.X := Tags[sIdx].StartPos;
  if eIdx = -1 then Result.Y := Result.X
   else Result.Y := Tags[eIdx].EndPos;
end;

function TClientSyntAnalyzer.RangeAtPos(APos: integer): TTextRange;
begin
  Result := TTextRange(FRanges.GetAt(APos));
end;

function TClientSyntAnalyzer.RangeIdxAtPos(APos: integer): integer;
begin
  Result := FRanges.GetIndexAt(APos);
end;

function TClientSyntAnalyzer.NearestRangeAtPos(APos: integer): TTextRange;
var idx: integer;
begin
  idx := NearestRangeIdxAtPos(APos);
  if idx <> -1 then Result := Ranges[idx]
    else Result := nil;
{  idx := FRanges.PriorAt(APos);
  if idx <> -1 then
  for i := idx downto 0 do
   with TTextRange(FRanges[i]) do
    if (EndIdx <> -1) and (Tags[EndIdx].EndPos >= APos) then
     begin
       Result := TTextRange(FRanges[i]);
       Exit;
     end;
  Result := nil;}
end;

function TClientSyntAnalyzer.NearestRangeIdxAtPos(APos: integer): integer;
begin
  Result := FRanges.PriorAt(APos);
  if Result <> -1 then
    while Result >= 0 do
     with TTextRange(FRanges[Result]) do
      if (EndIdx <> -1) and (Tags[EndIdx].EndPos >= APos) then
        Exit
      else
        Dec(Result);
end;

function TClientSyntAnalyzer.GetRangeCount: integer;
begin
  Result := FRanges.Count;
end;

function TClientSyntAnalyzer.GetRanges(Index: integer): TTextRange;
begin
  Result := TTextRange(FRanges[Index]);
end;

procedure TClientSyntAnalyzer.Analyze(ResetContent: Boolean);
var OldSep: integer;
begin
  if IsFinished then Exit;
  if ResetContent then
    begin
      OldSep := FOwner.FSeparateBlocks;
      FOwner.FSeparateBlocks := 2; // disanle separation analysis
      Clear;
      AppendToPos(Length(FSrcProc.FText));
      FOwner.FSeparateBlocks := OldSep;
    end else
    begin
      AppendToPos(Length(FSrcProc.FText));
    end;
end;

procedure TClientSyntAnalyzer.CompleteAnalysis;
var own: TSyntAnalyzer;
    i: integer;
begin
  AppendToPos(Length(FSrcProc.FText));
  if FOwner.SeparateBlockAnalysis then
    for i := FStartSepRangeAnal + 1 to TagCount do
     begin
      own := Tags[i - 1].Rule.SyntOwner;
      FOwner.SelectTokenFormat(Self, FSrcProc.FText, own <> FOwner, i);
      if own <> FOwner then
        own.SelectTokenFormat(Self, FSrcProc.FText, False, i);
      FBreakIdle := True;
      Finished;
     end;
end;

function TClientSyntAnalyzer.RangeFormat(const FmtStr: ecString;
  Range: TTextRange): ecString;
var i, j, idx, N, to_idx: integer;
    rng: TTextRange;
    LineMode: integer;

{ HAW: hans@werschner.de [Oct'07] ......... additions to formatting token parts ......

     I have added syntax to each single ".... %xyz ...." clause processing

     a) %(S|E)P*(L|Z)?[0-9]+  may be expanded to

        %(S|E)P*([\[]<token>[\]]<offset>?)?

          where <token> is a specific token that is "searched from the specified
          starting point (S for first token in the range , or E for the last token)
          towards the respective range end (up- or downwards). The search-direction
          is kept in the variable "rngdir" which is set in the "S" , "E" decision.

          example:  line(s) is/are ".... for x = 1 to 12 do ...... end ;  ..."
                                         0   1 2 3 4  5  6  ...    27  28

          range-start = "for", range-end = "end"

          then "...%s[to] ..." will skip forward to the token "to" (with index 4).

          The token values are searched on a "asis" basis, there is no case-insensitivity
          option yet.

          A "numeric number following the token value will define an <offset> relative
          to the found token.

          For this clause, the variable "idx" is not set by taking the static
          numeric value as in "...%s2 ..." , instead the "found token index" is kept.

          For "%S..." the search starts at idx=0 up to max 28.     ---> rngdir = +1;
          For "%E..." the search starts at idx=28 downto min 0.    ---> rngdir = -1;

          The options L or Z introduced in V2.35 will not combine with the new (range)
          specifying options --> somebody else may find a use for such extended ranges.

          Notes:  Avoid to search for tokens that can occur at multiple places (for
                  example a ";" between statements).

                  The above syntax is simple as it allows to identify the

                    block-start-tokens      "for x = 1 to 12 do"

                    block-body                anything after block-start tokens up to

                    block-end-tokens        "end ;"

                  but many syntax formats do not trivially support this separation.

                  The current implementation does not provide the information where
                  "block-start", "block-body" and "block-end" are beginning/ending.
                  A "%B0..." for the "block-body" portion and a "ignore block-body
                  tokens" option may be nice !?

     b) any such clause (either absolute or given by token value) can "start a token
        range" by additionally specifying:

          %(S|E)...~(S|E)P*[0-9]+

        or

          %(S|E)...~(S|E)P*([\[]<token>[\]]<offset>?)?

        or

          %(S|E)...~[\[]<token>[\]]

        The first form uses the static index specification to define the end-range:

          "%s0~s3"        results in "for x = 1"            (tokens 0, 1, ... 3)

        The 2nd form uses the new syntax to "search for an end-token beginning at the
        starting range index (idx) up- or down-wards.

          "%s0~s[do]"     results in "for x = 1 to 12 do"   (tokens 0, 1, ... 6)

          if a search is not satisfied, the complete range up to "e0" is taken.

          Because of the same "S", the search starts with "TagStr[idx]" ...

          "s0~e[do]"      results in the same string, but starts at the final "end"
                          of the block and scanning downwards.

          Caution: This may produce WRONG results if nested loops are scanned !

                   I could not find a valid representation of "range-start" token-
                   streams, the range-body alone and/or the range-end token-stream.

                   Such information may be helpful to better display blocks and/or
                   collapse display of the "block-body" alone.

        The 3rd form is an abbreviation where the S/E indicators are taken to be
        identical as the starting point

          "S1~[do]1"      results in "x = 1 to 12"          (tokens 1, 2, ... 5)

                          The <offset> "1" will here skip back by 1 from the found
                          token "do".

        The range-end is kept in the variable "to_idx".

        The "token-value" to search for can not be whitespace #00..#20. Leading and
        trailing whitespace withing the "...[vvvvvvv] ..." enclosed by [ and ]
        characters sequence is removed before searching. the "vvvvvv" can contain
        escaped characters like "... [\]] ..." to allow "[" and/or "]" to be part
        of the value. The \r, \n, \f ...escapes are not supported here.

        The token accumulation simply (?) takes all tokens from "idx" ... "to_idx"
        and builds a string by appending all tokens with ONE " " (blank) as separating
        delimiter. There is no process to keep the original token positions within
        the source line(s) and any whitepace including cr/lf's there. This may be an
        addition but I currently do not see a need for it.

     c) "ranges as specified above may accumulate many tokens and it may be desirable
        to "limit" the result string.

        This can be done by using another operand syntax

          %(S|E)...~[0-9]+

        or

          %(S|E)...~(S|E)([\[]<token>[\]]<offset>?)?~[0-9]+

        or

          %(S|E)...~[\[]<token>[\]]~[0-9]+

        In all three forms the "~" is immediately followed by a numeric value which
        is interpreted as

          "maximum number of tokens in the substituted string", if the range takes
          MORE than this maximum

        The value is internally kept in the variable "rngmax" below.

        When the result string is accumulated (taking all tokens between "idx" up-
        resp. down-to "to_idx") the number of appended tokens can not go beyond "rngmax".

        If this happens the result will be created in the form "t-1 t-2 -- t-max ..." with
        the ellipsis string " ..."  appended.

     d) There is another addition to the token clause syntax which is NOT YET operational:

        I am not too happy that the collapsed string displayed is completely in "grey"
        colour. As I want to have some control over "highlighting" in this string also,
        I tried to add some style-reference information to the token clause.

        *** I currently do not yet understand HOW to activate such a style within
            the results of this formatting, but I will TRY !!

        OK, the added syntax for styles for future use ;-)

          .... %:<style>:(S|E) ....

        where the <style> is the alphanumeric name of a style as defined in the lexer
        definition and this style-name is "enclosed" with the ":" characters.

        The code keeps the found style-name in the variable "rngstyle" for any later
        use.

        This addition only assigns styles to the token substitution parts, but not to any
        text outside and/or the total "collapsed" text formatting. This may be some
        enhancement to define in the lexer GUI.

    Hans L. Werschner, Oct '07
}
var rngstyle: string;                      // HAW: add style identifier to range expression
    rngtoken, rngResult: string;           //      a few more vars
    swp_idx, rngdir, rngoffset, rngmax: integer;
    to_rng: TTextRange;

function RangeNumber( const FmtStrNumber: string; var gotnbr: integer ): boolean;
begin
    N := 0; Result := false;
    while (j + N) <= length( FmtStrNumber ) do
      if (FmtStrNumber[j + N] >= '0') and (FmtStrNumber[j + N] <= '9') or (N = 0) and
         ((FmtStrNumber[j + N] = '+') or (FmtStrNumber[j + N] = '-'))
         then inc(N) else Break;
    if  N > 0  then  begin
      gotnbr := StrToInt( copy( FmtStrNumber, j, N ) );
      inc( j, N );
      Result := true;
    end;
end;

//var S_: string;
begin
  idx := 0;
  Result := FmtStr;
  try

   // HAW: obsolete -> to_idx := Length(Result);
   //      the variable "j" is now always pointing to the next character to process.
   //      Only during numeric sub-operand scan, the "N" will keep the found digits
   //      count. After such number, the var "j" is immediately adjusted again.

   for i := Length(Result) - 1 downto 1 do
    if Result[i] = '%' then
    begin
     j := i + 1;

     rngstyle := '';                  // HAW: keep style name
     if  Result[j] = ':' then  begin  // HAW: begin of embedded style name
       inc( j );
       while  (j <= length( Result ))  and  (Result[j] <> ':') do begin
         if  Result[j] > ' '  then
           rngstyle := rngstyle+Result[j];
         inc( j );
       end;
       if  (j > length( Result ))  or  (Result[j] <> ':')  then
         continue;
       inc( j );
       // now we have a style name, and can resume after "%:ssssss:"
     end;

     rng    := Range;
     rngdir := 1;                     // HAW: positive increment (for "%..e..")
                                      //      negative for "..e.." clauses
     rngmax := 1000000000;            // HAW: allow a great amount of tokens

     while ecUpCase(Result[j]) = 'P' do
      begin
       rng := rng.Parent;
       if (rng = nil) or (j = Length(Result)) then Continue;
       inc(j);
      end;

     case ecUpCase(Result[j]) of
       'S': idx := rng.StartIdx + rng.Rule.BlockOffset;
       'E': begin  rngdir := -1;      // HAW: mark downwards direction
                   if (rng.EndIdx <> -1) and Assigned(rng.Rule.BlockEndCond) then
                     idx := rng.EndIdx + rng.Rule.BlockEndCond.BlockOffset
                   else
                     idx := 1000000000;
            end;
       else continue;
     end;
     inc(j);

     case ecUpCase(Result[j]) of // <== v2.35
       'L': LineMode := 1; // from start of line
       'Z': LineMode := 2; // to end of line
       else LineMode := 0;
     end;
     if LineMode <> 0 then Inc(j);

     // HAW: check for "...s[token]..." instead of numeric index
     if  LineMode = 0  then
       if  (j < length( Result ))  and  (Result[j] = '[')  then  begin
         inc( j );  rngtoken := '';
         while  (j < length( Result ))  and  (Result[j] <> ']')  do  begin
           if  Result[j] = '\' then  inc( j );
           rngtoken := rngtoken + Result[j];  inc( j );
         end;
         if  j > length( Result ) then
           continue;
         while  (rngtoken <> '')  and  (rngtoken[length( rngtoken )] < ' ')  do
           rngtoken := copy( rngtoken, 1, length( rngtoken )-1 );
         while  (rngtoken <> '')  and  (rngtoken[1] < ' ')  do
           rngtoken := copy( rngtoken, 2, length( rngtoken )-1 );
         if  rngtoken = ''  then
           continue;
         inc( j );
         if  rngdir > 0 then  begin  // upwards search
           while  idx <= (rng.EndIdx + rng.Rule.BlockEndCond.BlockOffset) do  begin
             if  rngtoken = TagStr[idx]  then  break;
             inc( idx );
           end;
         end  else
         if  rngdir < 0 then         // downwards search
           while  idx >= (rng.StartIdx + rng.Rule.BlockOffset) do  begin
             if  rngtoken = TagStr[idx]  then  break;
             dec( idx );
           end;
         rngdir := 0;    // allow for missing <offset>
       end;
     if  not RangeNumber( Result, rngoffset )  then  begin
       if  rngdir <> 0 then
         Continue;
     end  else
       idx := idx - rngoffset;

     to_idx := idx;
     to_rng := rng;

     // HAW: now allow an explicit "to_idx" range by using "%from-idx~to-idx"
     if  (j < length( Result ))  and  (Result[j] = '~')  then
       // a numeric value alone sets just the maximum tokens to use
       if (Result[j+1] >= '0') and (Result[j+1] <= '9')  then  begin  // only positive values !
         to_idx := to_rng.EndIdx + to_rng.Rule.BlockEndCond.BlockOffset;
         LineMode := 3;
       end else
       begin

         if  LineMode <> 0  then  // not a good combination
           continue;
         // ... otherwise we have a real end-token clause
         inc( j );  // skip over the [

         rngdir := 1;
         if  Result[j] <> '['   then  begin
           // to_rng := Range;  // be sure that we start with the range itself
           while ecUpCase(Result[j]) = 'P' do
            begin
             to_rng := rng.Parent;
             if (to_rng = nil) or (j = Length(Result)) then Continue;
             inc(j);
            end;

           case ecUpCase(Result[j]) of
             'S': to_idx := to_rng.StartIdx + to_rng.Rule.BlockOffset;
             'E': begin
                    rngdir := -1;       // HAW: mark downwards direction
                    if (to_rng.EndIdx <> -1) and Assigned(to_rng.Rule.BlockEndCond) then
                     to_idx := to_rng.EndIdx + to_rng.Rule.BlockEndCond.BlockOffset
                    else
                     to_idx := 1000000000;
                  end;
             else continue;
           end;
           inc(j);
         end;
         if  (j < length( Result ))  and  (Result[j] = '[')  then  begin
           inc( j );  rngtoken := '';
           while  (j < length( Result ))  and  (Result[j] <> ']')  do  begin
             if  Result[j] = '\' then  inc( j );
             rngtoken := rngtoken + Result[j];  inc( j );
           end;
           if  j > length( Result ) then
             continue;
           while  (rngtoken <> '')  and  (rngtoken[length( rngtoken )] < ' ')  do
             rngtoken := copy( rngtoken, 1, length( rngtoken )-1 );
           while  (rngtoken <> '')  and  (rngtoken[1] < ' ')  do
             rngtoken := copy( rngtoken, 2, length( rngtoken )-1 );
           if  rngtoken = ''  then
             continue;
           inc( j );
           if  rngdir > 0 then  begin  // upwards search
             while  to_idx <= (rng.EndIdx + rng.Rule.BlockEndCond.BlockOffset) do  begin
               if  rngtoken = TagStr[to_idx]  then  break;
               inc( to_idx );
             end;
           end  else
           if  rngdir < 0 then         // downwards search
             while  to_idx >= (rng.StartIdx + rng.Rule.BlockOffset) do  begin
               if  rngtoken = TagStr[to_idx]  then  break;
               dec( to_idx );
             end;
           rngdir := 0;  // allow for missing <offset>
         end;
         if  not RangeNumber( Result, rngoffset )  then  begin
           if  rngdir <> 0 then
             Continue;
         end  else
           to_idx := to_idx - rngoffset;

         LineMode := 3;  // enforce new mode as we have an explicit range
       end;

     if  (j < length( Result ))  and
         (Result[j] = '~')         and
         (Result[j+1] >= '0') and (Result[j+1] <= '9')  // only positive values !
     then  begin  // we hav a "maximum" range value attached
       inc( j );
       if  not RangeNumber( Result, rngmax ) then
         Continue;
     end;
     // HAW: ... end of added range checking ,
     //      variable "j" points to first character AFTER all clauses
     Delete(Result, i, j - i);

     if (idx >= 0) and (idx < FTagList.Count) then
       case LineMode of
         0: Insert(TagStr[idx], Result, i);
         1: begin
              N := FSrcProc.StrToCaret(Tags[idx].StartPos).Y;
              N := FSrcProc.LineIndex(N);
              to_idx := Tags[idx].EndPos;
              Insert(FSrcProc.SubString(N, to_idx - N + 1), Result, i);
            end;
         2: begin
              N := FSrcProc.StrToCaret(Tags[idx].EndPos).Y;
              to_idx := FSrcProc.LineIndex(N) + FSrcProc.LineLength(N);
              N:= Tags[idx].StartPos + 1;
              Insert(FSrcProc.SubString(N, to_idx - N), Result, i);
            end;
         // HAW: new mode = 3 --- explicit range  idx...to_idx
         3: if  (to_idx >= 0)  and  (to_idx < FTagList.Count)  then  begin
              if  to_idx < idx  then  begin
                swp_idx := idx;  idx := to_idx;  to_idx := swp_idx;
              end;
              rngResult := '';
              while  idx <= to_idx  do  begin
                if  rngmax <= 0   then  begin
                  rngResult := rngResult+' ...';
                  break;
                end;
                if  (rngResult <> '') and (idx > 0) and (Tags[idx-1].EndPos <> Tags[idx].StartPos) then //MZ fix
                  rngResult := rngResult + ' ';
                rngResult := rngResult + TagStr[idx];
                inc( idx );  dec( rngmax );
              end;
              Insert(rngResult, Result, i);
            end;
         // HAW: ... end of token range accumulation mode
      end;

      // HAW: I am currently not sure how to handle the "stylename" property here
      //      ... will be added, when available
    end;
   Exit;
  except
    Result := '';
  end;
end;

function TClientSyntAnalyzer.GetRangeName(Range: TTextRange): ecString;
begin
  Result := '';
  if Assigned(Range.Rule) and (Range.Rule.NameFmt <> '') then
     Result := RangeFormat(Range.Rule.NameFmt, Range);
  if Result = '' then
   Result := TagStr[Range.IdentIdx];
end;

function TClientSyntAnalyzer.GetRangeGroup(Range: TTextRange): ecString;
begin
  Result := RangeFormat(Range.Rule.GroupFmt, Range);
end;

function TClientSyntAnalyzer.GetCollapsedText(Range: TTextRange): ecString;
begin
  Result := RangeFormat(Range.Rule.CollapseFmt, Range);
end;

// Returns True if Dynamic Highlight changed
function TClientSyntAnalyzer.SetCurPos(Pos: integer; OutOfLine: Boolean): Boolean;
var i: integer;
    Lst: TList;

  function LookForRule(ARule: TObject): Boolean;
  var j: integer;
  begin
    for j := i + 1 to Lst.Count - 1 do
     if TDynoRange(Lst[j]).FRule = ARule then
      begin
       Result := True;
       Exit;
      end;
    Result := False;
  end;

  function IsDynoChanged: Boolean;
  var j: integer;
  begin
    Result := Lst.Count <> FCurDynamic.Count;
    if not Result then
     for j := 0 to Lst.Count - 1 do
      if TDynoRange(Lst[j]).FRange <> FCurDynamic[j] then
       begin
        Result := True;
        Exit;
       end;
  end;

begin
  FDynCurPos := Pos;
  FDynOut := OutOfLine;
  Lst := TList.Create;
  try
    Pos := FChanges.CurToOld(Pos);
    FDynoConditions.GetRangesAtPos(Lst, Pos);
    // Checking out of line condition
    if FDynOut then
     for i := Lst.Count - 1 downto 0 do
      with TDynoRange(Lst[i]) do
       if EndPos <= Pos then
        Lst.Delete(i);

    // Removing with the same rule (DynSelectMin)
    i := 0;
    while i < Lst.Count do
     with TDynoRange(Lst[i]) do
      if (FRule <> nil) and LookForRule(FRule) then
       Lst.Delete(i)
      else
       Inc(i);

    // Is dynamic changed ?
    Result := IsDynoChanged;
    if Result then
     begin
      FCurDynamic.Clear;
      for i := 0 to Lst.Count - 1 do
       FCurDynamic.Add(TDynoRange(Lst[i]).FRange);
     end;
  finally
    FreeAndNil(Lst);
  end;
end;

function TClientSyntAnalyzer.IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean;
begin
  Result := inherited IsEnabled(Rule, OnlyGlobal) and
      (HasOpened(Rule, Rule.Block, Rule.StrictParent) xor Rule.NotParent);
end;

procedure TClientSyntAnalyzer.UpdateDyno;
begin
  SetCurPos(FDynCurPos, FDynOut);
end;

function TClientSyntAnalyzer.GetTokenStyle(Pos: integer;
  StlList: TStyleEntries): integer;
var i, ps, sp, ep: integer;
    stl: TSyntaxFormat;
begin
//  TryAppend(Pos);
  if (FNextTokIndex < FTagList.Count) and (FNextTokIndex >= 0) then
   begin
     with TSyntToken(FTagList[FNextTokIndex]) do
       if StartPos = Pos then
             i := FNextTokIndex else
        if EndPos = Pos then
         begin
          if FNextTokIndex < FTagList.Count - 1 then
             i := FNextTokIndex + 1
          else
             i := -1;
         end else
             i := FTagList.NextAt(Pos);
   end else
  i := FTagList.NextAt(Pos);

  FNextTokIndex := i;
  if i = -1 then Result := -1 else
   with TSyntToken(FTagList[i]) do
    if StartPos > Pos then Result := StartPos else
     begin
      Result := EndPos;
      stl := Style;
{      Inc(i);
      while (i < FTagList.Count) and
            (Tags[i].Style = stl) and
            ((Tags[i].StartPos = Tags[i -1].EndPos) or
             (stl = nil) or (stl.BgColor = clNone)) do
        begin
          Result := Tags[i].EndPos;
          FNextTokIndex := i;
          Inc(i);
        end;}
      if stl <> nil then
        StlList.Add(stl, StartPos, Result);
     end;

  if (Result = -1) and (FSavedTags.Count > 0) then
   begin
    ps := FChanges.CurToOld(Pos);
    i := FSavedTags.NextAt(ps);
    if i = -1 then Exit;
    ep := FChanges.OldToCur(TSyntToken(FSavedTags[i]).EndPos);
    while ep <= Pos do
     begin
      Inc(i);
      if i = FSavedTags.Count then Exit;
      ep := FChanges.OldToCur(TSyntToken(FSavedTags[i]).EndPos);
     end;
    with TSyntToken(FSavedTags[i]) do
     begin
       sp := FChanges.OldToCur(StartPos);
       if StartPos > ps then Result := sp else
         begin
          Result := ep;
          if (Rule <> nil) and (Rule.Style <> nil) then
            StlList.Add(Rule.Style, sp, Result);
         end;
     end;
   end;
end;

function TClientSyntAnalyzer.GetRangeStyles(Pos: integer; StlList: TStyleEntries;
   UseDyno: Boolean): integer;
var Lst: TList;
    i, sp, ep: integer;
begin
  Lst := TList.Create;
  try
    Result := FRangeStyles.GetRangesAtPos(Lst, FChanges.CurToOld(Pos));
    Result := FChanges.OldToCur(Result);
    for i := 0 to Lst.Count - 1 do
     with TStyledRange(Lst[i]) do
      if (FFlags and srLineHighlight) = 0 then
      begin
        sp := FChanges.OldToCur(StartPos);
        ep := FChanges.OldToCur(EndPos);
        if (ep <= Pos) or (sp > Pos) then Continue;
        if FRange = nil then
         StlList.Add(Style, sp, ep, False) else
        if UseDyno and
          (((FFlags and srInvertCondition) <> 0) =
           (FCurDynamic.IndexOf(FRange) = -1)) then
         StlList.Add(Style, sp, ep, True);
      end;
  finally
    FreeAndNil(Lst);
  end;
end;

procedure TClientSyntAnalyzer.GetLineHighlight(Line: integer; var InvSel: Boolean;
  var bgColor, frColor: TColor; UseDyno: Boolean);
var i, sp, ep: integer;
    List: TList;
begin
  List := TList.Create;
  try
    sp := FSrcProc.LineIndex(Line) - 1;
    ep := FChanges.CurToOld(sp + FSrcProc.LineSpace(Line));
    sp := FChanges.CurToOld(sp);
    FRangeStyles.GetRangesAtRange(List, sp, ep);
    for i := 0 to List.Count - 1 do
     with TStyledRange(List[i]) do
      if ((FFlags and srLineHighlight) <> 0) and Style.Enabled then
      if (FRange = nil) or
         UseDyno and
         (((FFlags and srInvertCondition) <> 0) =
          (FCurDynamic.IndexOf(FRange) = -1)) then
       begin
        InvSel := (FFlags and srInvertSelColor) <> 0;
        if Style.BgColor <> clNone then
          bgColor := Style.BgColor;
        if (Style.FormatType <> ftBackGround) and (Style.Font.Color <> clNone) then
          frColor := Style.Font.Color;
       end;
  finally
    FreeAndNil(List);
  end;
end;

function TClientSyntAnalyzer.GetLineState(Line: integer): integer;
var i, sp, ep: integer;
    List: TList;
begin
  List := TList.Create;
  try
    sp := FSrcProc.LineIndex(Line) - 1;
    ep := FChanges.CurToOld(sp + FSrcProc.LineSpace(Line));
    sp := FChanges.CurToOld(sp);
    FCollapsables.GetRangesAtRange(List, sp, ep);
    Result := csOutCollapse;
    for i := 0 to List.Count - 1 do
     with TRange(List[i]) do
      if StartPos >= sp then
       begin
        if EndPos >= ep then
         begin
           Result := csCollapsible;
           Exit;
         end;
       end else
       if EndPos < ep then Result := csCollapseEnd else
        if Result = csOutCollapse then Result := csInCollapse;
  finally
    FreeAndNil(List);
  end;
end;

function TClientSyntAnalyzer.GetRangeAtLine(Line: integer): TTextRange;
var i, sp, ep: integer;
    List: TList;
begin
  List := TList.Create;
  try
    sp := FSrcProc.LineIndex(Line) - 1;
    ep := FChanges.CurToOld(sp + FSrcProc.LineSpace(Line));
    sp := FChanges.CurToOld(sp);
    FCollapsables.GetRangesAtRange(List, sp, ep);
    for i := 0 to List.Count - 1 do
     with TCollapsibleRange(List[i]) do
      if (StartPos >= sp) and (EndPos >= ep) and (FSource < FRanges.Count) and
         (Tags[TTextRange(FRanges[FSource]).StartIdx].StartPos = StartPos) then
       begin
         Result := TTextRange(FRanges[FSource]);
         Exit;
       end;
    Result := nil;
  finally
    FreeAndNil(List);
  end;
end;

function TClientSyntAnalyzer.GetLineBreak(Line: integer): TLineBreakRange;
var List: TList;

  function DoLine(Line: integer; Top: Boolean): TLineBreakRange;
  var i, sp, ep: integer;
  begin
    Result := nil;
    if (Line >= FSrcProc.Count) or (Line < 0) then Exit;
    sp := FSrcProc.LineIndex(Line) - 1;
    ep := FChanges.CurToOld(sp + FSrcProc.LineSpace(Line));
    sp := FChanges.CurToOld(sp);
    FLineBreakRanges.GetRangesAtRange(List, sp, ep);
    for i := List.Count - 1 downto 0 do
     with TLineBreakRange(List[i]) do
      if Top and (StartPos >= sp) and (Rule.LinePos = lbTop) or
         not Top and (EndPos < ep) and (Rule.LinePos = lbBottom) then
       begin
         Result := TLineBreakRange(List[i]);
         Exit;
       end;
  end;

begin
  List := TList.Create;
  try
    Result := DoLine(Line, True);
    if not Assigned(Result) then
      Result := DoLine(Line - 1, False);
  finally
    FreeAndNil(List);
  end;
end;

procedure TClientSyntAnalyzer.TextChanged(Pos, Count, Line, LineChange: integer);
begin
  if Pos = -1 then Clear else
    begin
      ChangedAtPos(Pos);
//      CorrectStaples(Line, LineChange);
      FChanges.Add(Pos, Count);
    end;
end;

function TClientSyntAnalyzer.GetNearestColRange(Pos: integer): TTextRange;
var i: integer;
    List: TList;
begin
  List := TList.Create;
  try
    FCollapsables.GetRangesAtPos(List, FChanges.CurToOld(Pos));
    for i := List.Count - 1 downto 0 do
     with TCollapsibleRange(List[i]) do
       if  FSource < FRanges.Count then
         begin
           Result := TTextRange(FRanges[FSource]);
           Exit;
         end;
    Result := nil;
  finally
    FreeAndNil(List);
  end;
end;

function TClientSyntAnalyzer.GetTagPos(Index: integer): TPoint;
var ln_pos, i: integer;
begin
  Result := FSrcProc.StrToCaret(Tags[Index].StartPos);
  ln_pos := FSrcProc.LineIndex(Result.y) - 1;
  Result.X := 0;
  for i := Index - 1 downto 0 do
   if Tags[i].EndPos > ln_pos then Result.X := Result.X + 1
    else Exit;
end;

procedure TClientSyntAnalyzer.GetStaples(Line: integer; List: TList);
var sp, ep: integer;
begin
  sp := FSrcProc.LineIndex(Line) - 1;
  ep := FChanges.CurToOld(sp + FSrcProc.LineSpace(Line));
  sp := FChanges.CurToOld(sp);
  FStaples.GetRangesAtRange(List, sp, ep);
end;

procedure TClientSyntAnalyzer.GetStapleLines(Staple: TBlockStaple;
  var sLine, eLine: integer);
begin
  sLine := FSrcProc.StrToCaret(FChanges.OldToCur(Staple.StartPos)).Y;
  eLine := FSrcProc.StrToCaret(FChanges.OldToCur(Staple.EndPos)).Y;
end;

function TClientSyntAnalyzer.NextTokenAt(Pos: integer): integer;
begin
  Result := FTagList.NextAt(Pos);
end;

procedure TClientSyntAnalyzer.ResetStaples;
var i, j: integer;
begin
  for i := 0 to FStaples.LevelCount - 1 do
    for j := 0 to FStaples[i].Count - 1 do
      TBlockStaple(FStaples[i][j]).XPos := -1;
end;

procedure TClientSyntAnalyzer.Lock;
begin
  FDataAccess.Enter;
end;

procedure TClientSyntAnalyzer.Unlock;
begin
  FDataAccess.Leave;
end;

function TClientSyntAnalyzer.GetOpened(Index: integer): TTextRange;
begin
  Result := TTextRange(FOpenedBlocks[Index]);
end;

function TClientSyntAnalyzer.GetOpenedCount: integer;
begin
  Result := FOpenedBlocks.Count;
end;

function TClientSyntAnalyzer.GetAutoCloseText(Range: TTextRange;
  const Indent: string): ecString;
var St: TecStringList;
    i: integer;
begin
  St := TecStringList.Create;
  try
    St.Text := RangeFormat(Range.Rule.AutoCloseText, Range);
    for i := 0 to St.Count - 1 do
      St[i] := Indent + St[i];
    Result := St.Text;
    if Length(Result) > 2 then
      Delete(Result, Length(Result) - 1, 2);
  finally
    FreeAndNil(St);
  end;
end;

procedure TClientSyntAnalyzer.SetDisableIdleAppend(const Value: Boolean);
begin
  if FDisableIdleAppend <> Value then
    begin
      FDisableIdleAppend := Value;
      if not IsFinished then
        IntIdleAppend(nil);
    end;
end;

function TClientSyntAnalyzer.CreateLineBreak(Rule: TTagBlockCondition;
  RefTag: integer): Boolean;
var lb: TLineBreak;
begin
  lb := TLineBreak.Create;
  lb.FRefTag := RefTag;
  lb.FRule := Rule;
  if Rule.LinePos = lbBottom then
    lb.FLine := FSrcProc.StrToCaret(Tags[RefTag].EndPos).Y + 1
  else
    lb.FLine := FSrcProc.StrToCaret(Tags[RefTag].StartPos).Y;
  AddLineBreak(lb);
  Result := True;
end;

function TClientSyntAnalyzer.DetectTag(Rule: TTagBlockCondition;
  RefTag: integer): Boolean;
begin
  Tags[RefTag].FRule := Rule;
  if Rule.TokenType >= 0 then
    Tags[RefTag].FTokenType := Rule.TokenType;
  Result := True;
end;

procedure TClientSyntAnalyzer.CloseAtEnd(StartTagIdx: integer);
const
  cSpecIndentID = 20;
    //special number for "Group index" lexer property, which activates indent-based folding for a rule
  cSpecTokenStart: char = '1';
    //special char - must be first of token's type name (e.g. "1keyword");
    //Also such tokens must contain spaces+tabs at the beginning (use parser regex like "^[\x20\x09]*\w+")
var i, j, Ind: integer;
    Range: TTextRange;
    s: string;
begin
  for i := FOpenedBlocks.Count - 1 downto 0 do
   begin
    Range := TTextRange(FOpenedBlocks[i]);
    if Range.FRule.EndOfTextClose and
       ((StartTagIdx = 0) or (Range.StartIdx >= StartTagIdx)) then
     begin
       Range.FEnd := TagCount - 1;
       if Range.FRule.GroupIndex = cSpecIndentID then
       begin
         Ind := IndentOf(TagStr[Range.StartIdx]);
         for j := Range.StartIdx+1 to TagCount-1 do
         begin
           s := Owner.TokenTypeNames[Tags[j].FTokenType];
           if (s[1] = cSpecTokenStart) and (IndentOf(TagStr[j]) <= Ind) then
           begin
             Range.FEnd := j-1;
             Break
           end;
         end;
       end;
       FOpenedBlocks.Delete(i);
     end;
   end;
end;

{ TSyntAnalyzer }

constructor TSyntAnalyzer.Create(AOwner: TComponent);
begin
  inherited;
  FClientList := TList.Create;
  FMasters := TList.Create;
  FSampleText := TStringList.Create;
  FTokenTypeNames := TStringList.Create;
  FTokenTypeNames.Text := SecDefaultTokenTypeNames;
  TStringList(FTokenTypeNames).OnChange := TokenNamesChanged;

  FFormats := TStylesCollection.Create;
  FFormats.OnChange := FormatsChanged;
  FFormats.SyntOwner := Self;

  FTokenRules := TTokenRuleCollection.Create;
  FTokenRules.OnChange := TokenRuleChanged;
  FTokenRules.SyntOwner := Self;

  FBlockRules := TBlockRuleCollection.Create;
  FBlockRules.OnChange := BlocksChanged;
  FBlockRules.SyntOwner := Self;

  FSubAnalyzers := TSubAnalyzerRules.Create;
  FSubAnalyzers.SyntOwner := Self;
  FSubAnalyzers.OnChange := SubLexRuleChanged;

  FMarkedBlock := FFormats.Add as TSyntaxFormat;
  FMarkedBlock.BgColor := clHighlight;
  FMarkedBlock.Font.Color := clHighlightText;
  FMarkedBlock.FormatType := ftColor;
  FMarkedBlock.DisplayName := 'Marked block';
  FMarkedBlock.FIsBlock := True;

  FCodeTemplates := TCodeTemplates.Create(Self);
  FSkipSpaces := True;

  FNotes := TStringList.Create;

  FGrammaParser := TGrammaAnalyzer.Create;
  FGrammaParser.OnChange := GrammaChanged;

  FIdleAppendDelayInit := 50;
  FIdleAppendDelay := 200;
end;

destructor TSyntAnalyzer.Destroy;
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

procedure TSyntAnalyzer.Assign(Source: TPersistent);
var Src: TSyntAnalyzer;
    i: integer;
begin
  if not (Source is TSyntAnalyzer) then Exit;
  Src := Source as TSyntAnalyzer;
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
   TClientSyntAnalyzer(FClientList[i]).IdleAppend;
end;

procedure TSyntAnalyzer.HighlightKeywords(Client: TParserResults;
  const Source: ecString; OnlyGlobal: Boolean);
var i, N, ki, RefIdx: integer;
    Accept: Boolean;
begin
  N := Client.TagCount;
  for i := 0 to FBlockRules.Count - 1 do
   with FBlockRules[i] do
    if Enabled and (BlockType = btTagDetect) and
       (Block = nil) and (FGrammaRule = nil) then
      begin
       if OnlyGlobal and not AlwaysEnabled then Continue;
       RefIdx := 0;
       Accept := Check(Source, TClientSyntAnalyzer(Client), N, RefIdx);
       if Assigned(OnBlockCheck) then
         OnBlockCheck(FBlockRules[i], TClientSyntAnalyzer(Client), Source, RefIdx, Accept);
       if Accept then
         begin
           if FRefToCondEnd then ki := RefIdx - IdentIndex
             else ki := N - 1 - CheckOffset - IdentIndex;
           TClientSyntAnalyzer(Client).Tags[ki].FRule := FBlockRules[i];
           if TokenType >= 0 then
              TClientSyntAnalyzer(Client).Tags[ki].FTokenType := TokenType;
           if CancelNextRules then Exit;   // 2.27
         end;
      end;
end;

procedure TSyntAnalyzer.SelectTokenFormat(Client: TParserResults;
            const Source: ecString; OnlyGlobal: Boolean; N: integer);
var i, li, ki, strt, RefIdx: integer;
    Range: TTextRange;
    Accept: Boolean;
    RClient: TClientSyntAnalyzer;
  function CheckIndex(Idx: integer): Boolean;
  begin
   Result := (Idx >= 0) and (Idx < N);
  end;
begin
  if N = -1 then
    N := Client.TagCount;
  if not (Client is TClientSyntAnalyzer)  then Exit;
  RClient := TClientSyntAnalyzer(Client);
  RClient.FStartSepRangeAnal := N + 1;
  try
    for i := 0 to FBlockRules.Count - 1 do
      with FBlockRules[i] do
       if not SeparateBlockAnalysis or (BlockType <> btTagDetect) or
          (Block = nil) or (FGrammaRule = nil) then
       if Client.IsEnabled(FBlockRules[i], OnlyGlobal) then
        begin
          RefIdx := 0;
          if FGrammaRule <> nil then
           begin
             RefIdx := FGrammaParser.TestRule(N - 1, FGrammaRule, Client);
             Accept := RefIdx <> -1;
           end else
             Accept := Check(Source, RClient, N, RefIdx);

          if Assigned(OnBlockCheck) then
            OnBlockCheck(FBlockRules[i], RClient, Source, RefIdx, Accept);

          if Accept then
          begin
           Client.ApplyStates(FBlockRules[i]);
           if FRefToCondEnd then strt := RefIdx
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
                    Range := TTextRange.Create(li, RClient.Tags[li].StartPos);
                    Range.FIdent := ki;
                    Range.FRule := FBlockRules[i];
                    Range.FCondIndex := N - 1;
                    if NoEndRule then
                     begin
                      Range.FEnd := N - 1 - CheckOffset;
                      Range.FEndCondIndex := N - 1;
                      Range.FStart := RefIdx - BlockOffset;
                     end;
                    RClient.AddRange(Range);
                   end;
                end;
               btRangeEnd:  // End of block
                 if not RClient.CloseRange(FBlockRules[i], strt) then
                   Continue;
               btLineBreak:
                 if not RClient.CreateLineBreak(FBlockRules[i], ki) then
                   Continue;
            end;
           if CancelNextRules then Break;
          end;
        end;
  except
    Application.HandleException(Self);
  end;
end;

function TSyntAnalyzer.AddClient(const Client: IecSyntClient;
                         SrcProc: TATStringBuffer): TClientSyntAnalyzer;
begin
  Result := TClientSyntAnalyzer.Create(Self, SrcProc, Client);
end;

procedure TSyntAnalyzer.SetSampleText(const Value: TStrings);
begin
  FSampleText.Assign(Value);
end;

function TSyntAnalyzer.GetToken(Client: TParserResults; const Source: ecString;
                              APos: integer; OnlyGlobal: Boolean): TSyntToken;
var i, N, lp: integer;
    Rule: TTokenRule;
begin
  if Assigned(FOnParseToken) then
    begin
      N := 0;
      Rule := nil;
      FOnParseToken(Client, Source, APos, N, Rule);
      if Assigned(Rule) then
        Result := TSyntToken.Create(Rule, APos - 1, APos + N - 1)
      else
        Result := nil;
      Exit;
    end;

  lp := 0;
  for i := 0 to FTokenRules.Count - 1 do
    begin
      Rule := FTokenRules[i];
      if Client.IsEnabled(Rule, OnlyGlobal) then
        with Rule do
          begin
            if (ColumnFrom > 0) or (ColumnTo > 0) then
              begin
               if lp = 0 then lp := Client.FSrcProc.StrToCaret(APos - 1).X + 1;
               if (ColumnFrom > 0) and (lp < ColumnFrom) or
                  (ColumnTo > 0) and (lp > ColumnTo) then
                  Continue;
              end;
            N := Match(Source, APos);
            if Assigned(OnMatchToken) then
              OnMatchToken(Rule, Client, Source, APos, N);
            if N > 0 then
              begin
                Client.ApplyStates(Rule);
                Result := TSyntToken.Create(Rule, APos - 1, APos + N - 1);
                Exit;
              end;
          end;
    end;
  Result := nil;
end;

procedure TSyntAnalyzer.FormatsChanged(Sender: TCollection; Item: TSyntCollectionItem);
var i: integer;
begin
  ClearClientContents;
  if Item = nil then
   begin
    if not FFormats.ValidItem(FMarkedBlock) then FMarkedBlock := nil;
    if not FFormats.ValidItem(FCurrentLine) then FCurrentLine := nil;
    if not FFormats.ValidItem(FDefStyle) then FDefStyle := nil;
    if not FFormats.ValidItem(FSearchMatch) then FSearchMatch := nil;
    for i := 0 to FBlockRules.Count - 1 do
     begin
      if not FFormats.ValidItem(FBlockRules[i].Style) then FBlockRules[i].Style := nil;
      if not FFormats.ValidItem(FBlockRules[i].TreeItemStyleObj) then FBlockRules[i].FTreeItemStyleObj := nil;
      if not FFormats.ValidItem(FBlockRules[i].TreeGroupStyleObj) then FBlockRules[i].FTreeGroupStyleObj := nil;
     end;
    for i := 0 to FTokenRules.Count - 1 do
     if not FFormats.ValidItem(FTokenRules[i].Style) then FTokenRules[i].Style := nil;
    for i := 0 to FSubAnalyzers.Count - 1 do
     if not FFormats.ValidItem(FSubAnalyzers[i].Style) then FSubAnalyzers[i].Style := nil;
   end;
//  UpdateClients;
  Change;
end;

procedure TSyntAnalyzer.BlocksChanged(Sender: TCollection;
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

procedure TSyntAnalyzer.ClearClientContents;
var i:integer;
begin
  if FCoping then Exit;
  FCoping := True;
  try
    for i := 0 to FClientList.Count - 1 do
     with TClientSyntAnalyzer(FClientList[i]) do
      begin
        Clear;
        IdleAppend;
      end;
    for i := 0 to FMasters.Count - 1 do
      TSyntAnalyzer(FMasters[i]).ClearClientContents;
  finally
    FCoping := False;
  end;
  UpdateClients;
end;

procedure TSyntAnalyzer.UpdateClients;
var i:integer;
begin
  if FCoping then Exit;
  FCoping := True;
  try
    for i := 0 to FClientList.Count - 1 do
     with TClientSyntAnalyzer(FClientList[i]) do
       if FClient <> nil then
         FClient.FormatChanged;
    for i := 0 to FMasters.Count - 1 do
      TSyntAnalyzer(FMasters[i]).UpdateClients;
  finally
    FCoping := False;
  end;
end;

procedure TSyntAnalyzer.Loaded;
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
    TSyntAnalyzer(FMasters[i]).DetectBlockSeparate;
end;

procedure TSyntAnalyzer.SetBlockRules(const Value: TBlockRuleCollection);
begin
  FBlockRules.Assign(Value);
  ClearClientContents;
end;

procedure TSyntAnalyzer.SetCodeTemplates(const Value: TCodeTemplates);
begin
  FCodeTemplates.Assign(Value);
  ClearClientContents;
end;

procedure TSyntAnalyzer.SetTokenRules(const Value: TTokenRuleCollection);
begin
  FTokenRules.Assign(Value);
  ClearClientContents;
end;

procedure TSyntAnalyzer.SetFormats(const Value: TStylesCollection);
begin
  FFormats.Assign(Value);
end;

function TSyntAnalyzer.GetUniqueName(const Base: string): string;
var n: integer;
begin
  n := 1;
  if Owner = nil then Result := Base + '1' else
  repeat
   Result := Base + IntToStr(n);
   inc(n);
  until Owner.FindComponent(Result) = nil;
end;

procedure TSyntAnalyzer.SetSkipSpaces(const Value: Boolean);
begin
  if FSkipSpaces <> Value then
   begin
     FSkipSpaces := Value;
     ClearClientContents;
   end;
end;

procedure TSyntAnalyzer.SetSubAnalyzers(const Value: TSubAnalyzerRules);
begin
  FSubAnalyzers.Assign(Value);
  ClearClientContents;
end;

procedure TSyntAnalyzer.Notification(AComponent: TComponent;
  Operation: TOperation);
var i: integer;
begin
  inherited;
  if (Operation = opRemove)  and (AComponent <> Self) and (aComponent is TSyntAnalyzer) and
     Assigned(FSubAnalyzers) and Assigned(FMasters) then
   begin
     for i := 0 to FSubAnalyzers.Count - 1 do
      if FSubAnalyzers[i].FSyntAnalyzer = AComponent then
       FSubAnalyzers[i].FSyntAnalyzer := nil;
     FMasters.Remove(AComponent);
   end;
end;

procedure TSyntAnalyzer.SubLexRuleChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
begin
  DetectBlockSeparate;
  ClearClientContents;
  Change;
end;

procedure TSyntAnalyzer.AddMasterLexer(SyntAnal: TSyntAnalyzer);
begin
  if Assigned(SyntAnal) and (SyntAnal <> Self) and
     (FMasters.IndexOf(SyntAnal) = -1) then
   begin
     FMasters.Add(SyntAnal);
     SyntAnal.FreeNotification(Self);
   end;
end;

procedure TSyntAnalyzer.RemoveMasterLexer(SyntAnal: TSyntAnalyzer);
begin
  FMasters.Remove(SyntAnal);
end;

procedure TSyntAnalyzer.TokenRuleChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
begin
  DetectBlockSeparate;
  ClearClientContents;
  Change;
end;

procedure TSyntAnalyzer.SetTokenTypeNames(const Value: TStrings);
begin
  FTokenTypeNames.Assign(Value);
end;

procedure TSyntAnalyzer.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSyntAnalyzer.SetSearchMatch(const Value: TSyntaxFormat);
begin
  if FSearchMatch = Value then Exit;
  FSearchMatch := Value;
  UpdateClients;
  Change;
end;

procedure TSyntAnalyzer.SetMarkedBlock(const Value: TSyntaxFormat);
begin
  if FMarkedBlock = Value then Exit;
  FMarkedBlock := Value;
  UpdateClients;
  Change;
end;

procedure TSyntAnalyzer.SetCurrentLine(const Value: TSyntaxFormat);
begin
  if FCurrentLine = Value then Exit;
  FCurrentLine := Value;
  UpdateClients;
  Change;
end;

procedure TSyntAnalyzer.SetDefStyle(const Value: TSyntaxFormat);
begin
  if FDefStyle = Value then Exit;
  FDefStyle := Value;
  UpdateClients;
  Change;
end;

function TSyntAnalyzer.GetStyleName(const AName: string; const AStyle: TSyntaxFormat): string;
begin
  if csLoading in ComponentState then
    Result := AName
  else
   if Assigned(AStyle) then
    Result := AStyle.DisplayName
   else
    Result := '';
end;

function TSyntAnalyzer.GetMarkedBlockName: string;
begin
  Result := GetStyleName(FMarkedBlockName, FMarkedBlock);
end;

procedure TSyntAnalyzer.SetMarkedBlockName(const Value: string);
begin
  if csLoading in ComponentState then
    FMarkedBlockName := Value
  else
    MarkedBlock := TSyntaxFormat(FFormats.ItemByName(Value));
end;

function TSyntAnalyzer.GetSearchMatchStyle: string;
begin
  Result := GetStyleName(FSearchMatchName, FSearchMatch);
end;

procedure TSyntAnalyzer.SetSearchMatchStyle(const Value: string);
begin
  if csLoading in ComponentState then
    FSearchMatchName := Value
  else
    FSearchMatch := TSyntaxFormat(FFormats.ItemByName(Value));
end;

function TSyntAnalyzer.GetCurrentLineStyle: string;
begin
  Result := GetStyleName(FCurrentLineName, FCurrentLine);
end;

procedure TSyntAnalyzer.SetCurrentLineStyle(const Value: string);
begin
  if csLoading in ComponentState then
    FCurrentLineName := Value
  else
    FCurrentLine := TSyntaxFormat(FFormats.ItemByName(Value));
end;

function TSyntAnalyzer.GetDefaultStyleName: string;
begin
  Result := GetStyleName(FDefStyleName, FDefStyle);
end;

procedure TSyntAnalyzer.SetDefaultStyleName(const Value: string);
begin
  if csLoading in ComponentState then
    FDefStyleName := Value
  else
    FDefStyle := TSyntaxFormat(FFormats.ItemByName(Value));
end;

procedure TSyntAnalyzer.SetNotes(const Value: TStrings);
begin
  FNotes.Assign(Value);
end;

procedure TSyntAnalyzer.SetInternal(const Value: boolean);
begin
  FInternal := Value;
end;

procedure TSyntAnalyzer.SetRestartFromLineStart(const Value: Boolean);
begin
  FRestartFromLineStart := Value;
end;

procedure TSyntAnalyzer.SetParseEndOfLine(const Value: Boolean);
begin
  if FParseEndOfLine <> Value then
    begin
      FParseEndOfLine := Value;
      ClearClientContents;
    end;
end;

procedure TSyntAnalyzer.CompileGramma;
var i: integer;
begin
  FGrammaParser.CompileGramma(FTokenTypeNames);
  for i := 0 to FBlockRules.Count - 1 do
    FBlockRules[i].FGrammaRule :=
     FGrammaParser.ParserRuleByName(FBlockRules[i].FGrammaRuleName);
end;

procedure TSyntAnalyzer.TokenNamesChanged(Sender: TObject);
begin
  CompileGramma;
  Change;
end;

procedure TSyntAnalyzer.SetGrammar(const Value: TGrammaAnalyzer);
begin
  FGrammaParser.Assign(Value);
  CompileGramma;
end;

procedure TSyntAnalyzer.GrammaChanged(Sender: TObject);
begin
  CompileGramma;
end;

procedure TSyntAnalyzer.SetLineComment(const Value: ecString);
begin
  FLineComment := Value;
end;

function TSyntAnalyzer.GetSeparateBlocks: Boolean;
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

procedure TSyntAnalyzer.DetectBlockSeparate;
begin
  FSeparateBlocks := 0;
end;

procedure TSyntAnalyzer.SetAlwaysSyncBlockAnal(const Value: Boolean);
begin
  FAlwaysSyncBlockAnal := Value;
  if FAlwaysSyncBlockAnal and SeparateBlockAnalysis then
   begin
    DetectBlockSeparate;
    ClearClientContents;
   end;
end;

function TSyntAnalyzer.GetCollapseStyleName: string;
begin
  Result := GetStyleName(FCollapseStyleName, FCollapseStyle);
end;

procedure TSyntAnalyzer.SetCollapseStyleName(const Value: string);
begin
  if csLoading in ComponentState then
    FCollapseStyleName := Value
  else
    FCollapseStyle := TSyntaxFormat(FFormats.ItemByName(Value));
end;

procedure TSyntAnalyzer.SetCollapseStyle(const Value: TSyntaxFormat);
begin
  if FCollapseStyle <> Value then
    begin
      FCollapseStyle := Value;
      UpdateClients;
      Change;
    end;
end;

{ TCodeTemplate }

constructor TCodeTemplate.Create(Collection: TCollection);
begin
  inherited;
  FName:= '';
  FDescription:= '';
  FCode:= nil;
  FAdvanced:= false;
end;

function TCodeTemplate.GetDisplayName: string;
begin
  Result := FName;
end;


{ TCodeTemplates }

function TCodeTemplates.Add: TCodeTemplate;
begin
  Result := TCodeTemplate(inherited Add);
end;

constructor TCodeTemplates.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCodeTemplate);
end;

function TCodeTemplates.FindTemplate(AName: string): TCodeTemplate;
var i: integer;
begin
  for i := 0 to Count - 1 do
   if SameText(AName, Items[i].Name) then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

function TCodeTemplates.GetItem(Index: integer): TCodeTemplate;
begin
  Result := TCodeTemplate(inherited Items[Index]);
end;

{ TSyntaxManager }

function TSyntaxManager.AddAnalyzer: TSyntAnalyzer;
begin
  Result := TLibSyntAnalyzer.Create(Owner);
  Result.Name := Result.GetUniqueName('SyntAnal');
  Result.SetParentComponent(Self);
  FModified := True;
end;

procedure TSyntaxManager.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList[0]).Free;
  end;

  Changed;
  FModified := True;
end;

constructor TSyntaxManager.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
  FModified := False;
end;

destructor TSyntaxManager.Destroy;
begin
  FOnChange := nil;
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TSyntaxManager.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TSyntaxManager.GeItem(Index: integer): TSyntAnalyzer;
begin
  Result := TSyntAnalyzer(FList[Index]);
end;

procedure TSyntaxManager.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var i: integer;
begin
  if not (csDestroying in ComponentState) then
   for i := 0 to FList.Count - 1 do
    Proc(TComponent(FList[i]));
end;

function TSyntaxManager.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TSyntaxManager.LoadFromFile(const FileName: string);
begin
  Clear;
  inherited;
  Changed;
  FModified := False;
end;

procedure TSyntaxManager.SaveToFile(const FileName: string);
begin
  inherited;
  FModified := False;
end;

procedure TSyntaxManager.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  FModified := True;
end;

procedure TSyntaxManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

procedure TSyntaxManager.SetCurrentLexer(const Value: TSyntAnalyzer);
begin
  if (FCurrentLexer <> Value) and ((Value = nil) or (FList.IndexOf(value) <> -1)) then
   begin
     FCurrentLexer := Value;
   end;
end;

function TSyntaxManager.FindAnalyzer(
  const LexerName: string): TSyntAnalyzer;
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

procedure TSyntaxManager.OnReadError(Reader: TReader;
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
  if Assigned(AOwner) and (AOwner is TSyntaxManager) then
   inherited Create((AOwner as TSyntaxManager).Owner)
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
  if (Value <> nil) and (Value is TSyntaxManager) then
   begin
     FParent := TSyntaxManager(Value);
     FParent.FList.Add(Self);
   end else FParent := nil;
end;

{ TLoadableComponent }

var
  CheckExistingName: Boolean = False;

procedure TLoadableComponent.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  FFileName := FileName; //AT
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
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
    {$IFNDEF EC_DOTNET}Pchar(ResType){$ELSE}ResType{$ENDIF});
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
    {$IFNDEF EC_DOTNET}Pchar(ResType){$ELSE}ResType{$ENDIF});
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
  Stream := TFileStream.Create(FileName, fmCreate);
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

{ TSyntTextSource }

procedure TSyntTextSource.AddClient(Client: TObject);
begin
  if (FClients.IndexOf(TObject(Client)) = -1)  then
   begin
     FClients.Add(Client);
     if Client is TComponent then
       (Client as TComponent).FreeNotification(Self);
   end;
end;

constructor TSyntTextSource.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TATStringBuffer.Create;
  FLines.OnChange := TextChanged;
  FClients := TList.Create;
end;

destructor TSyntTextSource.Destroy;
begin
  inherited;
  FreeAndNil(FSyntRanges);
  FreeAndNil(FLines);
  FreeAndNil(FClients);
end;

procedure TSyntTextSource.TextChanged(Sender: TObject; Pos, Count,
  LineChange: integer);
var i: integer;
    iObj: IecTextClient;
begin
  if FSyntRanges <> nil then
    FSyntRanges.TextChanged(Pos, Count, FLines.StrToCaret(Pos).Y, LineChange);
  for i := FClients.Count - 1 downto 0 do
   if Supports(TObject(FClients[i]), IecTextClient, iObj) then
    iObj.TextChanged(Sender, Pos, Count, LineChange);
end;

procedure TSyntTextSource.Finished;
var i: integer;
    iObj: IecSyntClient;
begin
  for i := 0 to FClients.Count - 1 do
   if Supports(TObject(FClients[i]), IecSyntClient, iObj) then
    iObj.Finished;
end;

procedure TSyntTextSource.FormatChanged;
var i: integer;
    iObj: IecSyntClient;
begin
  for i := 0 to FClients.Count - 1 do
   if Supports(TObject(FClients[i]), IecSyntClient, iObj) then
    iObj.FormatChanged;
end;

function TSyntTextSource.GetSyntRanges: TSyntAnalyzer;
begin
  if Assigned(FSyntRanges) then
   Result := FSyntRanges.Owner
  else
   Result := nil;
end;

procedure TSyntTextSource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
   begin
     FClients.Remove(AComponent);
     if (AComponent is TSyntAnalyzer) and Assigned(FSyntRanges) and
        (AComponent = GetSyntRanges) then
       begin
          FreeAndNil(FSyntRanges);
          if not (csDestroying in ComponentState) then
            TextChanged(FLines, 0, 0, 0);
       end;
   end;
end;

procedure TSyntTextSource.RemoveClient(Client: TObject);
begin
  FClients.Remove(Client);
end;

procedure TSyntTextSource.SetLines(const Value: TATStringBuffer);
begin
  /////FLines.Assign(Value);
  showmessage('not done TSyntTextSource.SetLines');
end;

procedure TSyntTextSource.SetSyntRanges(const Value: TSyntAnalyzer);
begin
  if GetSyntRanges = Value then Exit;
  if Assigned(FSyntRanges) then
    FreeAndNil(FSyntRanges);
  if Assigned(Value) then
    FSyntRanges := Value.AddClient(Self, GetLines)
  else
    FSyntRanges := nil;

  if Assigned(FSyntRanges) then
    FSyntRanges.Owner.FreeNotification(Self);
  TextChanged(FLines, -1, 0, 0);
  if Assigned(FSyntRanges) then FSyntRanges.IdleAppend
   else Finished;
end;

function TSyntTextSource.GetLines: TATStringBuffer;
begin
  Result := FLines;
end;

function TSyntTextSource.CaretPosToStrPos(const p: TPoint): integer;
begin
  Result := Lines.CaretToStr(p);
end;

function TSyntTextSource.StrPosToCaretPos(p: integer): TPoint;
begin
  Result := Lines.StrToCaret(p);
end;

function TSyntTextSource.LineLength(Index: integer): integer;
begin
  Result := Lines.LineLength(Index);
end;

{ TSubAnalyzerRule }

constructor TSubAnalyzerRule.Create(Collection: TCollection);
begin
  inherited;
  FStartRegExpr := TecRegExpr.Create;
  FEndRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FStartRegExpr);
  SetDefaultModifiers(FEndRegExpr);
end;

destructor TSubAnalyzerRule.Destroy;
begin
  FreeAndNil(FStartRegExpr);
  FreeAndNil(FEndRegExpr);
  inherited;
end;

procedure TSubAnalyzerRule.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSubAnalyzerRule then
   with Dest as TSubAnalyzerRule do
    begin
     StartExpression := Self.StartExpression;
     EndExpression := Self.EndExpression;
     SyntAnalyzer := Self.SyntAnalyzer;
     FromTextBegin := Self.FromTextBegin;
     ToTextEnd := Self.ToTextEnd;
     IncludeBounds := Self.IncludeBounds;
    end;
end;

function TSubAnalyzerRule.GetEndExpression: string;
begin
  Result := FEndRegExpr.Expression;
end;

function TSubAnalyzerRule.GetItemBaseName: string;
begin
  Result := 'Sub lexer rule';
end;

function TSubAnalyzerRule.GetStartExpression: string;
begin
  Result := FStartRegExpr.Expression;
end;

function TSubAnalyzerRule.MatchStart(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FStartRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

function TSubAnalyzerRule.MatchEnd(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FEndRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

procedure TSubAnalyzerRule.SetEndExpression(const Value: string);
begin
  FEndRegExpr.Expression := Value;
  Changed(False);
end;

procedure TSubAnalyzerRule.SetStartExpression(const Value: string);
begin
  FStartRegExpr.Expression := Value;
  Changed(False);
end;

procedure TSubAnalyzerRule.SetSyntAnalyzer(const Value: TSyntAnalyzer);
var own: TSyntAnalyzer;

  function IsLinked(SAnal: TSyntAnalyzer): Boolean;
  var i: integer;
  begin
    for i := 0 to Collection.Count - 1 do
     if (Collection.Items[i] <> Self) and ((Collection.Items[i] as TSubAnalyzerRule).SyntAnalyzer = SAnal) then
      begin
       Result := True;
       Exit;
      end;
    Result := False;
  end;

begin
  if FSyntAnalyzer <> Value then
   begin
     own := (Collection as TSyntCollection).SyntOwner;
     if Assigned(FSyntAnalyzer) and (FSyntAnalyzer <> own) and not IsLinked(FSyntAnalyzer) then
       FSyntAnalyzer.RemoveMasterLexer(own);
     FSyntAnalyzer := Value;
     if Assigned(FSyntAnalyzer) and (FSyntAnalyzer <> own) and not IsLinked(FSyntAnalyzer) then
      FSyntAnalyzer.AddMasterLexer(own);
     Changed(False);
   end;
end;

procedure TSubAnalyzerRule.SetFromTextBegin(const Value: Boolean);
begin
  FFromTextBegin := Value;
  Changed(False);
end;

procedure TSubAnalyzerRule.SetToTextEnd(const Value: Boolean);
begin
  FToTextEnd := Value;
  Changed(False);
end;

procedure TSubAnalyzerRule.SetIncludeBounds(const Value: Boolean);
begin
  FIncludeBounds := Value;
  Changed(False);
end;

{ TSubAnalyzerRules }

function TSubAnalyzerRules.Add: TSubAnalyzerRule;
begin
  Result := TSubAnalyzerRule(inherited Add);
end;

constructor TSubAnalyzerRules.Create;
begin
  inherited Create(TSubAnalyzerRule);
end;

function TSubAnalyzerRules.GetItem(Index: integer): TSubAnalyzerRule;
begin
  Result := TSubAnalyzerRule(inherited Items[Index]);
end;

{ TSyntStyles }

constructor TSyntStyles.Create(AOwner: TComponent);
begin
  inherited;
  FStyles := TStylesCollection.Create;
end;

destructor TSyntStyles.Destroy;
begin
  FreeAndNil(FStyles);
  inherited;
end;

procedure TSyntStyles.SetStyles(const Value: TStylesCollection);
begin
  FStyles.Assign(Value);
end;

{ TStyleCache }

constructor TStyleCache.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TStyleCache.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TStyleCache.Clear;
begin
  FList.Clear;
end;

function TStyleCache.GetCount: integer;
begin
  Result := FList.Count;
end;

function TStyleCache.GetItem(Index: integer): TSyntaxFormat;
begin
  Result := TSyntaxFormat(FList[Index]);
end;

function TStyleCache.AddStyle(Style: TSyntaxFormat): integer;
var i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if Items[i].IsEqual(Style) then
      begin
        Result := i;
        FreeAndNil(Style);
        Exit;
      end;
  Result := FList.Add(Style);
end;

procedure TStyleCache.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

procedure TStyleCache.AddNoCheck(Style: TSyntaxFormat);
begin
  FList.Add(Style);
end;

{ TStyleEntry }

constructor TStyleEntry.Create(AStyle: TSyntaxFormat; AStartPos,
  AEndPos: integer; AIsDynoStyle: Boolean);
begin
  inherited Create(AStartPos, AEndPos);
  Style := AStyle;
  IsDynoStyle := AIsDynoStyle;
end;

{ TStyleEntries }

procedure TStyleEntries.Add(AStyle: TSyntaxFormat; AStartPos,
  AEndPos: integer; AIsDynoStyle: Boolean);
begin
  Add(TStyleEntry.Create(AStyle, AStartPos, AEndPos, AIsDynoStyle));
end;

function TStyleEntries.GetItems(Index: integer): TStyleEntry;
begin
  Result := TStyleEntry(inherited Items[Index]);
end;

initialization
  Classes.RegisterClass(TLibSyntAnalyzer);

end.
