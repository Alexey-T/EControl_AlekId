unit ec_SyntaxClient;

{$mode Delphi}{$H+}
{$modeswitch ADVANCEDRECORDS}
{$IFDEF DEBUG}
{$INLINE OFF}
{$ENDIF}
interface

uses
  Classes, SysUtils,
  ec_Lists,
  ec_StrUtils,
  ec_rules,
  ec_token_holder,
  ec_SyntAnal,
  ec_Async,
  {$IFDEF DEBUGLOG} SynCommons,SynLog,mORMotHttpClient,  {$ENDIF}
 ATStringProc_TextBuffer;




type
  TecParserResults=class;
   { TecAsyncSelector }

  TecAsyncSelector<TEntity:class>=record
   strict private
   FParent:TecParserResults;
   FEntity, FWorkerEntity:TEntity;
   private
   procedure Release();
   procedure Swap();
   public
   procedure Init(const parent:TecParserResults;
     const aEntity, workEntitiy: TEntity);
   property Parent :TecParserResults read FParent;
   property WorkerEntity:TEntity read FWorkerEntity;
   property Entity:TEntity read FEntity;
   function Get:TEntity;inline;
  end;

  TecTokenList = GRangeList<TecSyntToken>;


  TecTagListSelector=TecAsyncSelector<TecTokenList>;

  { TecTagListSelectorHelp }

  TecTagListSelectorHelp=record helper for TecTagListSelector
   procedure SyncWorkerList(toWorker:boolean);
  end;

  TecSubLexerBlocksSelector=TecAsyncSelector<TecSubLexerRanges>;


  { TecParserResults }
  ISynEditAdapter=interface
      procedure SyntaxDoneHandler();
      procedure AppendToPosDone();
   end;


  { TParseStatus }

  TParseStatus= packed record
   public
   type _TParseStatus=(psNone, psInterrupt, psPostpone, {anything FINAL goes after psAborted,
                anything in-progess goes before} psAborted, psComplete);
   strict private
   var FStatus:_TParseStatus;
   procedure SetValue(stat:_TParseStatus);
   public
   procedure ResetNonComplete();
   procedure Reset();
   class operator LessThan(const a:TParseStatus; b:_TParseStatus):boolean;inline;
   class operator GreaterThanOrEqual(const a:TParseStatus; b:_TParseStatus):boolean;inline;
   class operator Equal(const a:TParseStatus;b:_TParseStatus):boolean;inline;
//   class operator Implicit(val:_TParseStatus):TParseStatus;inline;
   class operator Implicit(val:TParseStatus):_TParseStatus;
   property Value:_TParseStatus write SetValue;
  end;

  TecParserResults = class(TTokenHolder, IAsyncSyntaxClient)
  strict private
    FCurState: integer;
    FStateChanges: TecRangeList;
    function GetTags(Index: integer): TecSyntToken;override;
    function GetSubLexerRangeCount: integer;
    function GetSubLexerRange(Index: integer): TecSubLexerRange;
    strict protected
    FOwner: TecSyntAnalyzer;

    FParserStatus: TParseStatus;

    FLastAnalPos: integer;

    FSubLexerBlocks : TecSubLexerBlocksSelector;
    FTagList: TecTagListSelector;
    FBuffer: TATStringBuffer;
    FWorkerRequested:boolean;
    FDelayWorker:smallInt;
    procedure Finished; virtual;
    procedure SaveState;
    procedure RestoreState;
    function GetLastPos(const Source: ecString): integer;
    function ExtractTag(const Source: ecString; var FPos: integer): Boolean;
    function GetTokenCount: integer; override;
    procedure CloseAtEnd(wrk:PSyntaxWork; StartTagIdx: integer); virtual; abstract;
    // whether current thread is Syntaxer
    function GetIsCoherentThread:boolean;

    procedure AssertNonWorker();
    procedure AssertCoherent();inline;

    procedure SwitchContext(toWorker:boolean);
    procedure WaitCoherentHarder(const roSync: boolean);
    procedure WorkerDetached();
    procedure BeforeDestruction();override;
    procedure EnterTagsSync();
    procedure LeaveTagsSync();
    function GetParserStatus():TParseStatus;
  protected
    FClient: IecSyntClient;
    FWorkerThread:TecSyntaxerThread;
    function AcquireWorker():TecSyntaxerThread;
    function GetIsSyntaxThread():boolean;inline;

    function GetTokenStr(index: integer): ecString; override;
    function GetTokenType(Index: integer): integer; override;

    procedure CancelCurrentSyntax;
    function _GetRuleEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean;inline;
  public
    constructor Create(AOwner: TecSyntAnalyzer; ABuffer: TATStringBuffer;
                   const AClient: IecSyntClient;const SynEditAdapter:ISynEditAdapter; useWorkerThread:boolean); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function __UnsafeGetTagPtr(index:integer):PecSyntToken;inline;

    procedure SetTags(Index: integer; constref AValue: TecSyntToken);
    procedure ApplyStates(Rule: TRuleCollectionItem);
    procedure HandleFormatChanged();
    function AnalyzerAtPos(APos: integer): TecSyntAnalyzer;
    function ParserStateAtPos(TokenIndex: integer): integer;
    function WaitTillCoherent(roSync:boolean=false; timeOut:Cardinal=High(Cardinal) ):boolean;

    procedure ReleaseBackgroundLock();inline;


    property Owner: TecSyntAnalyzer read FOwner;
    property Buffer: TATStringBuffer read FBuffer;
    property ParserStatus: TParseStatus read GetParserStatus;
    property TagStr[Index: integer]: ecString read GetTokenStr;
//    property Tags[Index: integer]: TecSyntToken read GetTags write SetTags; default;
    property SubLexerRangeCount: integer read GetSubLexerRangeCount;
    property SubLexerRanges[Index: integer]: TecSubLexerRange read GetSubLexerRange;
    property ParserState: integer read FCurState write FCurState;
    property LastPos:integer read FLastAnalPos;

    //property TagIndexes[Index: integer]: TRangeListIndex read GetTagIndexes;
  end;


  { TecClientSyntAnalyzer }

  TecClientSyntAnalyzer = class(TecParserResults)
  strict private
    FRanges: TSortedList;
    FOpenedBlocks: TSortedList;    // Opened ranges (without end)


    FTaskAppendDisabled: Boolean;
    FPrevProgress: integer;

    FDestroying,FRepeateAnalysis: Boolean;
    FAppendAtPosArg:integer;
    FParseOffsetTargetP:TPoint;
    FParseOffsetTarget:integer;

    function GetRangeCount: integer;
    function GetRanges(Index: integer): TecTextRange;
    function GetOpened(Index: integer): TecTextRange;
    function GetOpenedCount: integer;
    function CheckSyncRequest(wrk:PSyntaxWork;tokenCounter:integer;timeWait:Cardinal=500):boolean;inline;
    function SyncDataToMainThread(const wrk:PSyntaxWork;timeWait:Cardinal):boolean;
    procedure HandlePostpone;

//    procedure SetDisableIdleAppend(const Value: Boolean);

  private
    FStartSepRangeAnal: integer;
  strict protected
    FEditAdapter:ISynEditAdapter;
    function DoAnalizeBlocks(const wrk: PSyntaxWork):boolean;
    procedure HandleAppendToPosDone(thread:TecSyntaxerThread; wrk:PSyntaxWork);
    function HasOpened(aRule: TRuleCollectionItem; Parent: TecTagBlockCondition; Strict: Boolean): Boolean;
    procedure Finished; override;
    procedure FireUpdateEditor();
    function  DoSyntaxWork(wrk:PSyntaxWork):boolean;
    function  DoAsyncPause(wrk:PSyntaxWork):boolean;
    procedure DelayedWork();
    procedure CloseAtEnd(wrk:PSyntaxWork; StartTagIdx: integer); override;
    function DoChangeAtPos(wrk:PSyntaxWork):boolean;
    procedure SetParseOffsetTarget(ofs:integer);
    procedure CheckProgress(curPos:integer);
    procedure QueueSyntaxWork(delayed:boolean);
    function GetStopRequested(wrk:PSyntaxWork):boolean; inline;
  public

    constructor Create(AOwner: TecSyntAnalyzer; SrcProc: TATStringBuffer;
                const AClient: IecSyntClient; const synEditAdapter:ISynEditAdapter; useWorkerThread:boolean); override;

    destructor Destroy; override;
    procedure FireAdapterDetached;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean;inline;
    function StopSyntax(AndWait: boolean): boolean;

    procedure _SetStartSeparateWork(tokenIx:integer);inline;
    procedure AddRange(Range: TecTextRange);

    procedure Clear; override;
    procedure HandleAddWork();
    procedure ChangedAtPos(APos: integer);

    function PriorTokenAt(Pos: integer): integer;

    function RangeFormat(const FmtStr: ecString; Range: TecTextRange): ecString;
    function GetRangeName(Range: TecTextRange): ecString;
    function GetRangeGroup(Range: TecTextRange): ecString;
    function GetCollapsedText(Range: TecTextRange): ecString;

    procedure TextChanged(APos: integer);
    // Requires analyzed to APos
    procedure AppendToPos(APos: integer; force: boolean= false);
    // Requires analyzed to APos
    procedure AppendToPosAsync(APos: integer; force:boolean);
    function DoAppendToPos(wrk:PSyntaxWork):boolean;
    procedure SyntaxDoneHandler(thread:TecSyntaxerThread;wrk:PSyntaxWork);
    procedure Analyze(ResetContent: Boolean = True); // Requires analyzed all text


    procedure CompleteAnalysis;

    function CloseRange(Cond: TecTagBlockCondition; RefTag: integer): Boolean;
    function DetectTag(Rule: TecTagBlockCondition; RefTag: integer): Boolean;
    function GetIsLineParsed(lineIndex:integer):boolean;

    property ParseOffsetTarget:TPoint read FParseOffsetTargetP;
    property OpenCount: integer read GetOpenedCount;
    property Opened[Index: integer]: TecTextRange read GetOpened;

    property RangeCount: integer read GetRangeCount;
    property Ranges[Index: integer]: TecTextRange read GetRanges;
//    property DisableIdleAppend: Boolean read FTaskAppendDisabled write SetDisableIdleAppend;
    property SynEditAdapter:ISynEditAdapter read FEditAdapter;
  end;

implementation
uses math, forms;

const minTokenStep=0;

{ TParseStatus }

class operator TParseStatus.LessThan(const a:TParseStatus; b:_TParseStatus): boolean;
begin
  result:= a.FStatus<b;
end;

class operator TParseStatus.GreaterThanOrEqual(const a:TParseStatus; b:_TParseStatus): boolean;
begin
  result := not (a<b);
end;

class operator TParseStatus.Equal(const a:TParseStatus; b:_TParseStatus): boolean;
begin
  result := a.FStatus = b;
end;

procedure TParseStatus.SetValue(stat: _TParseStatus);
begin
 if (FStatus>=psComplete) and (stat<psComplete) then
  FStatus:=stat
  else
   FStatus:=stat
end;

procedure TParseStatus.ResetNonComplete();
begin
 if FStatus<psComplete then
    FStatus:=psNone;
end;

procedure TParseStatus.Reset();
begin
FStatus:=psNone;
end;




class operator TParseStatus.Implicit(val: TParseStatus): _TParseStatus;
begin
 result:=val.FStatus;
end;

{ TecParserResults }

constructor TecParserResults.Create(AOwner: TecSyntAnalyzer;
  ABuffer: TATStringBuffer; const AClient: IecSyntClient;const SynEditAdapter:ISynEditAdapter; useWorkerThread:boolean);
begin
  self._AddRef();
 FLastAnalPos:=0;
  inherited Create;
  if ABuffer = nil then
    raise Exception.Create('TextBuffer not passed to parser');
  FOwner := AOwner;
  FBuffer := ABuffer;
  FClient := AClient;
  FWorkerThread:=nil;

 //FSubLexerBlocks := TecSubLexerRanges.Create;
  FSubLexerBlocks.Init(self, TecSubLexerRanges.Create, nil);
  FCurState := 0;
  FStateChanges := TecRangeList.Create;
  FWorkerRequested:=useWorkerThread;
  if useWorkerThread then begin
     FTagList.Init(self, TecTokenList.Create(False),nil );
  end
  else begin
    FTagList.Init(self,TecTokenList.Create(False), nil);
  end;

  FOwner.GetClients.Add(Self);
end;

destructor TecParserResults.Destroy;
begin

 inherited;
  if assigned(FWorkerThread) then begin
   FWorkerThread.Terminate2;
   FWorkerThread:=nil;
  end;

  FOwner.GetClients.Remove(Self);
  FTagList.Release();

//  FreeAndNil(FTagList);
//  FreeAndNil(FSubLexerBlocks);
  FSubLexerBlocks.Release();
  FreeAndNil(FStateChanges);
end;

procedure TecParserResults.Clear;
begin
  WaitTillCoherent();
  try
    FTagList.Get.Clear;
    FSubLexerBlocks.Get.Clear;
    FStateChanges.Clear;
    FCurState := 0;
  finally ReleaseBackgroundLock();end;
end;



function TecParserResults.__UnsafeGetTagPtr(index: integer): PecSyntToken;
begin
 result:=FTagList.Get._GetItemPtr(index);
end;

procedure TecParserResults.Finished;
begin
  AssertCoherent();
  FParserStatus.Value := psComplete;
  // Performs Gramma parsing
  //AnalyzeGramma;
  //FTagList.UpdateIndexer; //AT
end;



function TecParserResults._GetRuleEnabled(Rule: TRuleCollectionItem;
  OnlyGlobal: Boolean): Boolean;
  var statesPresent, statesAbsent, curState:integer;
begin
  statesPresent:=Rule.StatesPresent;
  statesAbsent :=Rule.StatesAbsent;
  curState:= FCurState;
  Result :=
         Rule.Enabled and
         (not OnlyGlobal or Rule.AlwaysEnabled) and
         ({(statesPresent = 0) or} ((curState and statesPresent) = statesPresent))
         and  ({(StatesAbsent = 0) or} ((curState and StatesAbsent) = 0));
end;

function TecParserResults.GetTokenCount: integer;
begin
  WaitTillCoherent(false);
  try
    Result := FTagList.Get.Count;
  finally ReleaseBackgroundLock();  end;
end;

function TecParserResults.GetIsCoherentThread: boolean;
begin
  result := not assigned(FWorkerThread) or GetIsSyntaxThread();
end;

function TecParserResults.GetIsSyntaxThread(): boolean;
begin
 result:= assigned(FWorkerThread) and
  FWorkerThread.GetIsWorkerThread();

end;

function TecParserResults.GetTags(Index: integer): TecSyntToken;
var tl:TecTokenList;
begin
  WaitTillCoherent(false);
  try
   tl :=FTagList.Get;
   if tl=nil then
   tl:=nil;
   Result := tl.Items[Index];

  finally ReleaseBackgroundLock();  end;
end;



function TecParserResults.GetTokenStr(index: integer): ecString;
var pTok:PecSyntToken;
begin
  WaitTillCoherent();
  try
    if index >= 0 then begin
      pTok:=__UnsafeGetTagPtr(index);
      if assigned(pTok) then begin
        Result:= pTok.GetStr(FBuffer.FText);
      end
      else
         Result:='';
      //with Tags[index] do
      //  Result := FBuffer.SubString(Range.StartPos + 1, Range.EndPos - Range.StartPos)
    end//index>=0
    else
      Result := '';
  finally ReleaseBackgroundLock();  end;
end;

function TecParserResults.GetLastPos(const Source: ecString): integer;
var tagCount:integer;
begin
   AssertCoherent();
   tagCount:=FTagList.Get.Count;
  if tagCount = 0 then Result := 1 else
    Result := FTagList.Get[tagCount - 1].Range.EndPos + 1;
  if FLastAnalPos > Result then Result := FLastAnalPos;
end;

procedure TecParserResults.SaveState;
var b: Boolean;
begin
 AssertCoherent();
 if FStateChanges.Count = 0 then
   b := FCurState <> 0
 else
   b := FCurState <> FStateChanges.Last.EndPos;
 if b then
   FStateChanges.Add(TRange.Create(FTagList.Get.Count, FCurState));
end;

// True if end of the text
function TecParserResults.ExtractTag(const Source: ecString; var FPos: integer): Boolean;
var N: integer;
    token: TecSyntToken;
    own: TecSyntAnalyzer;
    subLexerBlocks:TecSubLexerRanges;

   // Select current lexer
   procedure GetOwner;
   var i, N: integer;
     Sub: TecSubLexerRange;
   begin
    own := FOwner;
    for i := subLexerBlocks.Count - 1 downto 0 do
     begin
       Sub:= subLexerBlocks[i];
       if FPos > Sub.Range.StartPos then
        if Sub.Range.EndPos = -1 then
          begin
            // try close sub lexer
    //        if Rule.ToTextEnd then N := 0 else
            N := Sub.Rule.MatchEnd(Source, FPos);
            if N > 0 then
             begin
               if Sub.Rule.IncludeBounds then
                 begin // New mode in v2.35
                   Sub.Range.EndPos := FPos - 1 + N;
                   Sub.Range.PointEnd := FBuffer.StrToCaret(Sub.Range.EndPos);
                   Sub.CondEndPos := Sub.Range.EndPos;
                   own := Sub.Rule.SyntAnalyzer;
                 end else
                 begin
                   Sub.Range.EndPos := FPos - 1;
                   Sub.Range.PointEnd := FBuffer.StrToCaret(Sub.Range.EndPos);
                   Sub.CondEndPos := Sub.Range.EndPos + N;
                 end;
               // Close ranges which belongs to this sub-lexer range
               CloseAtEnd(nil, FTagList.Get.PriorAt(Sub.Range.StartPos));
               subLexerBlocks[i] := Sub; // Write back to list
             end else
             begin
               own := Sub.Rule.SyntAnalyzer;
               Exit;
             end;
          end else
       if FPos < Sub.Range.EndPos then
         begin
               own := Sub.Rule.SyntAnalyzer;
               Exit;
         end;
    end;
   end;

   procedure CheckIntersect;
   var i: integer;
     Sub: TecSubLexerRange;
   begin
    for i := subLexerBlocks.Count - 1 downto 0 do
    begin
      Sub := subLexerBlocks[i];
      if (token.Range.EndPos > Sub.Range.StartPos) and (token.Range.StartPos < Sub.Range.StartPos) then
       begin
        token.CorrectEndRange(Sub.Range.StartPos, FBuffer.StrToCaret(token.Range.EndPos));
        Exit;
       end;
    end;
   end;

   function CanOpen(Rule: TecSubAnalyzerRule): Boolean;
   var N: integer;
       sub: TecSubLexerRange;
   begin
     Result := _GetRuleEnabled(Rule, False) and (Rule.SyntAnalyzer <> nil);
     if not Result then Exit;
     Result := Rule.FromTextBegin and (FPos = 1);
     if Result then N := 0 else
                    N := Rule.MatchStart(Source, FPos);
     Result := Result or (N > 0);
     if not Result then Exit;
     // To prevent repeated opening
     if subLexerBlocks.Count > 0 then
     begin
       sub := subLexerBlocks.Last;
       if (sub.Range.EndPos = FPos - 1) and
          (sub.Rule = Rule) then Exit;
     end;

     ApplyStates(Rule);

     FillChar(sub, SizeOf(sub), 0);
     sub.Rule := Rule;
     sub.CondStartPos := FPos - 1;
     if Rule.IncludeBounds then
       sub.Range.StartPos := FPos - 1
     else
       sub.Range.StartPos := FPos + N - 1;
     sub.Range.EndPos := -1;
     sub.Range.PointStart := FBuffer.StrToCaret(sub.Range.StartPos);
     sub.CondEndPos := -1;
     subLexerBlocks.Add(sub);
   end;


   procedure TryOpenSubLexer;
   var i,sCnt: integer;
   begin
     sCnt :=own.SubAnalyzers.Count;
     for i := 0 to sCnt - 1 do
      if CanOpen(own.SubAnalyzers[i]) then Exit;
     if own <> FOwner then begin
      sCnt:=FOwner.SubAnalyzers.Count;
      for i := 0 to sCnt - 1 do
       if FOwner.SubAnalyzers[i].AlwaysEnabled and CanOpen(FOwner.SubAnalyzers[i]) then Exit;
     end;
   end;

var
  NNextPos: integer;
begin
   AssertCoherent();
  subLexerBlocks :=FSubLexerBlocks.Get;
  GetOwner;
  TryOpenSubLexer;
  if own.SkipSpaces then
    begin
     if own.ParseEndOfLine then
      N := SkipSpacesNoLineBreak(Source, FPos)
      else N := SkipSpaces(Source, FPos);
    end
   else if FPos > Length(Source) then N := -1 else N := 0;
  TryOpenSubLexer;
  GetOwner;

  Result := N = -1;
  if Result then Exit;

  FOwner.GetToken(token, Self, Source, FPos, own <> FOwner);
  if (own <> FOwner) and (token.Range.StartPos < 0) then begin
     own.GetToken(token, Self, Source, FPos, False)
  end;
  if token.Range.StartPos < 0 then  // no token
   begin
     NNextPos := FPos;
     SkipSpaces(Source, NNextPos); // needed for huge space-only lines, where Inc(FPos) is very slow
     if NNextPos > FPos then
       FPos := NNextPos
     else
       Inc(FPos);
   end else
   begin
    CheckIntersect;
    SaveState;
    FTagList.Get.Add(token);
    if not FOwner.SeparateBlockAnalysis then  begin
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
    FPos := token.Range.EndPos + 1;
   end;
   FLastAnalPos := FPos;
end;

function TecParserResults.AnalyzerAtPos(APos: integer): TecSyntAnalyzer;
var
  N: integer;
  Rng: TecSubLexerRange;
  subLexerBlocks:TecSubLexerRanges;
begin
  Result := FOwner;
  if APos < 0 then Exit;
  WaitTillCoherent();
  subLexerBlocks :=FSubLexerBlocks.Get;
  try
    N := subLexerBlocks.PriorAt(APos);
    if N < 0 then Exit;
    Rng :=   subLexerBlocks.Items[N];
    if (Rng.Range.StartPos<=APos) and (APos<Rng.Range.EndPos) then
      Result := Rng.Rule.SyntAnalyzer;
   {
 for i := 0 to FSubLexerBlocks.Count - 1 do
  with FSubLexerBlocks[i] do
   if APos < Range.StartPos then Break else
    if (Range.EndPos = -1) or (APos < Range.EndPos) then
      Result := Rule.SyntAnalyzer;
  }
  finally ReleaseBackgroundLock();end;
  end;

function TecParserResults.GetSubLexerRangeCount: integer;
begin
  WaitTillCoherent();
  try
    Result := FSubLexerBlocks.Get.Count;
  finally ReleaseBackgroundLock();end;
end;

function TecParserResults.GetSubLexerRange(Index: integer): TecSubLexerRange;
begin
  WaitTillCoherent();
  try
    Result := FSubLexerBlocks.Get[Index];
  finally ReleaseBackgroundLock();end;
end;

procedure TecParserResults.SetTags(Index: integer; constref AValue: TecSyntToken);
begin
  WaitTillCoherent();
  try
    FTagList.Get[Index] := AValue
  finally ReleaseBackgroundLock();end;
end;

function TecParserResults.GetTokenType(Index: integer): integer;
begin
  WaitTillCoherent();
try
  Result := Tags[Index].TokenType;
finally ReleaseBackgroundLock();end;
end;

procedure TecParserResults.CancelCurrentSyntax;
begin
  if not assigned(FWorkerThread) then exit;
  FWorkerThread.CancelExpendableTasks();
  FWorkerThread.StopCurrentTask();
end;



procedure TecParserResults.SwitchContext(toWorker:boolean);
begin
 //if not toWorker then
 //  Dec(FWorkerTaskMustStop);
  //FWorkerTaskMustStop := false;
 EnterTagsSync();
 FSubLexerBlocks.Swap();
 FTagList.Swap();
 LeaveTagsSync();

end;

procedure TecParserResults.WaitCoherentHarder(const roSync: boolean);
var
  isMain: boolean;
  tm: cardinal;
begin
    isMain := not GetIsSyntaxThread();
    if isMain and not (roSync) then begin
    {$IFDEF DEBUGLOG}
     tm:=GetTickCount;
    {$ENDIF}
     if FParserStatus<psAborted then
          FParserStatus.Value:=psPostpone;
     CancelCurrentSyntax;
     FWorkerThread.AcquireSync(false);
      {$IFDEF DEBUGLOG}
      tm:=GetTickCount- tm;
      if tm > 50 then
         TSynLog.Add.Log(sllWarning, 'WaitTillCoherent(false) waited: % ms', [tm
           ]);
      {$ENDIF}
  end
  else
     FWorkerThread.AcquireSync(roSync);
end;

procedure TecParserResults.WorkerDetached();
begin
 if assigned(FWorkerThread) then
    FWorkerThread.RequestRelease(FWorkerThread);
end;

procedure TecParserResults.BeforeDestruction();
begin
  frefcount:=0;
end;


function TecParserResults.WaitTillCoherent(roSync:boolean; timeOut:Cardinal): boolean;
var timeWait:cardinal;
begin
  if not assigned(FWorkerThread)
      or FWorkerThread.TryAcquireSync then exit(true);

  WaitCoherentHarder(roSync);
  result:=true;
end;



procedure TecParserResults.AssertNonWorker();
var isWorker:boolean;
begin
 isWorker :=  GetIsSyntaxThread();
 if isWorker then
    Assert(false);
end;


procedure TecParserResults.AssertCoherent();
{$IFDEF DEBUG}
var coherent, isWorker, noTask, notWorking, synAccessible:boolean;
begin

  coherent :=(FWorkerThread= nil);
  if coherent then exit;
  isWorker := GetIsSyntaxThread();
  if isWorker then exit;
  noTask:=true;//not FSyntaxerThread.GetIsTaskAssigned();
  notWorking:=not FWorkerThread.BusyWorking;
  synAccessible:=FWorkerThread.GetSyncNowAccessible();
  coherent := noTask and notWorking and synAccessible ;
  if not coherent then
  Assert(coherent);
{$ELSE}
begin
{$ENDIF}

end;


 procedure TecParserResults.ReleaseBackgroundLock();
begin
 if not assigned(FWorkerThread) then exit;

 FWorkerThread.ReleaseSync();
// if (FParserStatus=psPostpone) and not GetIsSyntaxThread() then
//       QueueSyntaxWork;
end;



procedure TecParserResults.ApplyStates(Rule: TRuleCollectionItem);
begin
  WaitTillCoherent();
try
  if Rule.StatesRemove <> 0 then
    FCurState := FCurState and not Rule.StatesRemove;
  if Rule.StatesAdd <> 0 then
    FCurState := FCurState or Rule.StatesAdd;
finally ReleaseBackgroundLock();end;
end;

procedure TecParserResults.HandleFormatChanged();
begin
 if FClient <> nil then
    FClient.FormatChanged;
end;

procedure TecParserResults.RestoreState;
var i: integer;
begin
  WaitTillCoherent();
  try
    for i := FStateChanges.Count - 1 downto 0 do
      if FStateChanges.Last.StartPos >= TagCount then
        FStateChanges.Delete(FStateChanges.Count - 1)
      else
        Break;

    if FStateChanges.Count > 0 then
      FCurState := FStateChanges.Last.EndPos
    else
      FCurState := 0;
  finally ReleaseBackgroundLock();end;
end;

function TecParserResults.ParserStateAtPos(TokenIndex: integer): integer;
var i: integer;
begin
 WaitTillCoherent();
try
   for i := FStateChanges.Count - 1 downto 0 do
     if FStateChanges[i].StartPos <= TokenIndex then
       begin
         Result := FStateChanges[i].EndPos;
         Exit;
       end;
   Result := 0;
finally ReleaseBackgroundLock();end;
end;

procedure TecParserResults.EnterTagsSync();
begin
  if not assigned(FWorkerThread) then exit;
  FWorkerThread.DoTagSync(true);
end;

procedure TecParserResults.LeaveTagsSync();
begin
  if not assigned(FWorkerThread) then exit;
  FWorkerThread.DoTagSync(false);
end;

function TecParserResults.GetParserStatus(): TParseStatus;
begin
  result :=FParserStatus;
end;

function TecParserResults.AcquireWorker(): TecSyntaxerThread;
begin
   result := FWorkerThread;

   if assigned(result) then begin
    result.ResetIdleTimer();
    exit;
   end;
   result := TecSyntaxerThread.Create(self, nil);
   FWorkerThread:=result;
end;




{ TecClientSyntAnalyzer }

constructor TecClientSyntAnalyzer.Create(AOwner: TecSyntAnalyzer; SrcProc: TATStringBuffer;
  const AClient: IecSyntClient;const synEditAdapter:ISynEditAdapter; useWorkerThread:boolean);
begin
  FEditAdapter:=nil;
  FDestroying:= false;
  FTaskAppendDisabled:=false;
  FRepeateAnalysis:=false;
  FEditAdapter:=SynEditAdapter;
  inherited Create( AOwner, SrcProc, AClient,synEditAdapter, useWorkerThread);
  FRanges := TSortedList.Create(True);
  FOpenedBlocks := TSortedList.Create(False);
  FPrevProgress := -1;

end;

destructor TecClientSyntAnalyzer.Destroy;
begin
  FDestroying:= true;
  if assigned(FWorkerThread) then
     FWorkerThread.Terminate();
  StopSyntax(true);

  Application.ProcessMessages;
  FreeAndNil(FRanges);
  FreeAndNil(FOpenedBlocks);
  inherited;
end;

procedure TecClientSyntAnalyzer._SetStartSeparateWork(tokenIx: integer);
begin
  FStartSepRangeAnal:=tokenIx;
end;


procedure TecClientSyntAnalyzer.Clear;
begin
  WaitTillCoherent();
  try
    inherited;
    FRepeateAnalysis := False;
    FTagList.Get.Clear;
    FRanges.Clear;
    FOpenedBlocks.Clear;
    StopSyntax(true);
    FParserStatus.Reset();
    FLastAnalPos := 0;
    FStartSepRangeAnal := 0;
  finally ReleaseBackgroundLock();end;
  //HandleAddWork();
end;

procedure TecClientSyntAnalyzer.AddRange(Range: TecTextRange);
begin
  WaitTillCoherent();
  try
    Range.Index := FRanges.Count;
    FRanges.Add(Range);
    if FOpenedBlocks.Count > 0 then
      Range.Parent := TecTextRange(FOpenedBlocks[FOpenedBlocks.Count - 1]);
    if Range.EndIdx = -1 then
      FOpenedBlocks.Add(Range);
  finally ReleaseBackgroundLock();end;
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

function TecClientSyntAnalyzer.CloseRange(Cond: TecTagBlockCondition; RefTag: integer): Boolean;
var j: integer;
    b: boolean;
begin
  WaitTillCoherent();
  try
    for j := FOpenedBlocks.Count - 1 downto 0 do
     with TecTextRange(FOpenedBlocks[j]) do
       if Assigned(Rule) then  begin
           if Cond.BlockType = btRangeStart then
             b := Cond.SelfClose and (Rule = Cond)
           else
             b := (Rule.BlockEndCond  = Cond) or (Rule = Cond.BlockEndCond);
           if b then  begin
               if Cond.SameIdent and not SameText(TagStr[RefTag - Cond.IdentIndex] , TagStr[IdentIdx]) then Continue;
               EndIdx := RefTag - Cond.BlockOffset;
               if (Rule = Cond) and (EndIdx > 0) then Dec(EndIdx); // for self closing
               _SetEndConditionIndex(RefTag);
               if Assigned(Owner.OnCloseTextRange) then
                 Owner.OnCloseTextRange(Self, TecTextRange(FOpenedBlocks[j]), StartIdx, EndIdx);
               FOpenedBlocks.Delete(j);
               Result := True;
               Exit;
            end;
         end;//if rule
  finally  ReleaseBackgroundLock(); end;
  Result := False;
end;

function TecClientSyntAnalyzer.HasOpened(aRule: TRuleCollectionItem; Parent: TecTagBlockCondition; Strict: Boolean): Boolean;
var openBlkIx, openBlkCnt: integer;
    prnRule: TecTagBlockCondition;
    curBlock:TecTextRange;
begin
 AssertCoherent();
  openBlkCnt:=FOpenedBlocks.Count-1;
  if Strict then  begin
      if openBlkCnt >= 0 then    begin
          openBlkIx := openBlkCnt;//FOpenedBlocks.Count - 1;
          curBlock:=TecTextRange(FOpenedBlocks[openBlkIx]);
          prnRule := curBlock.Rule;
          if (aRule is TecTagBlockCondition) and
              TecTagBlockCondition(aRule).SelfClose and (prnRule = aRule) then
            Dec(openBlkIx);

          repeat
            if openBlkIx < 0 then
                Exit(false);

            prnRule := TecTextRange(FOpenedBlocks[openBlkIx]).Rule;
            Dec(openBlkIx);
          until not prnRule.IgnoreAsParent;

          Result := prnRule = Parent;
        end
        else Result := Parent = nil;
    end
  else  begin
      Result := True;
      if Parent = nil then Exit;
      for openBlkIx := openBlkCnt downto 0 do
        if TecTextRange(FOpenedBlocks[openBlkIx]).Rule = Parent then
          Exit(true);

      Result := False;
  end;
end;

procedure TecClientSyntAnalyzer.HandleAppendToPosDone(thread:TecSyntaxerThread;wrk:PSyntaxWork);
begin

  SyntaxDoneHandler(thread, wrk);

  if assigned(thread) and assigned(FEditAdapter) then begin
     {$IFDEF DEBUGLOG}
     TSynLog.Add.Log(sllServiceCall, 'AppendToPos Done, request repaint' );
     {$ENDIF}

     TThread.ForceQueue(nil,
     FEditAdapter.AppendToPosDone );
     QueueSyntaxWork(true);
  end;
end;

function TecClientSyntAnalyzer.DoAnalizeBlocks(const wrk: PSyntaxWork):boolean;
label _Abort;
var
  syntaxer: TecSyntAnalyzer;
  tagIx, tagCount: integer;
  time:Cardinal;

begin
  time:=GetTickCount;
  tagCount:=self.TagCount;
  for tagIx := FStartSepRangeAnal + 1 to tagCount do begin

       if  (FParserStatus>=psPostpone) or
           GetStopRequested(wrk) or
           CheckSyncRequest(wrk, tagIx) then
                                        goto _Abort;

       syntaxer := TecSyntAnalyzer( Tags[tagIx - 1].Rule.SyntOwner);
       FOwner.SelectTokenFormat(Self, FBuffer.FText, syntaxer <> FOwner,
         tagIx);
       if syntaxer <> FOwner then
         syntaxer.SelectTokenFormat(Self, FBuffer.FText, False, tagIx);
   end;//for
   time:=GetTickCount-time;
   {$IFDEF DEBUGLOG}
   TSynLog.Add.Log(sllResult, 'Block Analize took % ms', [time]);
   {$ENDIF}
   Exit(true);
_Abort:
     result:=false;
end;

procedure TecClientSyntAnalyzer.Finished;
var i: integer;
  Sub: TecSubLexerRange;
  subLexerBlocks:TecSubLexerRanges;
begin
  if FParserStatus>=psAborted then Exit;

  AssertCoherent();
  subLexerBlocks:=FSubLexerBlocks.Get;
  // Close SubLexers at the End of Text
  for i := subLexerBlocks.Count - 1 downto 0 do  begin
    Sub := subLexerBlocks[i];
    if (Sub.Range.EndPos = -1) and Sub.Rule.ToTextEnd then
     begin
       Sub.Range.EndPos := FBuffer.TextLength{ - 1};
       Sub.Range.PointEnd := Point(
                      FBuffer.LineLength(FBuffer.Count-1),
                      FBuffer.Count-1); //at end
       Sub.CondEndPos := Sub.Range.EndPos;
       subLexerBlocks[i] := Sub;
     end;
  end;

  // Close blocks at the end of text
  CloseAtEnd(nil,0);

  FRepeateAnalysis := True;
  inherited Finished;
  if  GetIsSyntaxThread() and assigned(FEditAdapter) then
   TThread.Queue(nil, FireUpdateEditor);
end;

procedure TecClientSyntAnalyzer.FireUpdateEditor();
begin
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllServiceCall, 'FireUpdateEditor - parser Finished');
  {$ENDIF}
  if assigned(FEditAdapter) then
      FEditAdapter.SyntaxDoneHandler();
end;

procedure TecClientSyntAnalyzer.FireAdapterDetached;
begin
  FEditAdapter:=nil;
end;

function TecClientSyntAnalyzer.DoSyntaxWork(wrk:PSyntaxWork):boolean;
var curPos, tmp, bufVersion: integer;
    Progress: integer;
    isAsync,bufferChanged: boolean;
    tokenCounter:integer;
   time:Cardinal;
   label _Exit;

begin
 curPos := 0;
 result := true; // assume work is done
 tokenCounter:=0;
 isAsync := GetIsSyntaxThread();
 if isAsync then
   bufVersion:=FBuffer.Version;

 if FParserStatus>=psAborted then
                              goto _Exit;

 FParserStatus.ResetNonComplete();
 time:=GetTickCount;

  if  CheckSyncRequest(wrk, minTokenStep) then goto _Exit;


   while (FParserStatus<psPostpone) and not GetStopRequested(wrk)  do  begin
    Inc(tokenCounter);
     if  CheckSyncRequest(wrk, tokenCounter) then goto _Exit;
     tmp := GetLastPos(FBuffer.FText);

     if tmp > curPos then curPos := tmp;
     if tokenCounter and 255 = 255 then CheckProgress(curPos);

     result:=ExtractTag(FBuffer.FText, curPos{, True}) ;

     if result then  begin
       if  FOwner.SeparateBlockAnalysis then begin
        result:=DoAnalizeBlocks(wrk);
       end;
       if result then
              Finished;
     end//if done
   end;  //while

   _Exit:
   if FLastAnalPos>FParseOffsetTarget then
    SetParseOffsetTarget(FLastAnalPos);
   //FParseOffsetTarget:=FBuffer.StrToCaret(curPos);
   if (FParserStatus=psPostpone) then begin
    HandlePostpone;
   end;
 result:= true;
end;

function TecClientSyntAnalyzer.DoAsyncPause(wrk: PSyntaxWork): boolean;
var thr:TecSyntaxerThread;
     time:cardinal;
     bufChanged:boolean;
begin
 thr:= TecSyntaxerThread(TThread.CurrentThread);
 time:=GetTickCount;
 repeat
  thr.IssueSyncRequest();
  bufChanged:=CheckSyncRequest(wrk, minTokenStep, 1000);
 until (GetTickCount-time>1000) or bufChanged or (GetStopRequested(wrk));
 FBuffer.Unlock;
end;

procedure TecClientSyntAnalyzer.DelayedWork();
var worker:TecSyntaxerThread;
begin
  if FDestroying then exit;
  worker :=AcquireWorker();
  worker.ScheduleWork(DoAsyncPause, 1000, FBuffer.Version, nil, true
  {$IFDEF DEBUGLOG},'DoAsyncPause'{$ENDIF});
  HandleAddWork();
end;

procedure TecClientSyntAnalyzer.AppendToPos(APos: integer; force: boolean);
var FPos, len: integer;

begin
  if FDestroying then exit;
 FParserStatus.ResetNonComplete();
 len :=FBuffer.TextLength;
  if len <= 0 then Exit;
  if FParserStatus>=psAborted then Exit;
  FAppendAtPosArg:=APos;
  if FWorkerRequested then begin
      AppendToPosAsync(APos, force);
  end
  else  begin
   DoAppendToPos(nil);
  end;
end;

procedure TecClientSyntAnalyzer.AppendToPosAsync(APos: integer; force:boolean);
var newPos:TPoint;
begin
  FPrevProgress:=-1;
  if FBuffer.TextLength = 0 then Exit;
  if FParserStatus>=psAborted then Exit;
  if (apos<=FParseOffsetTarget) and (not force) and (FParseOffsetTarget-apos<5000)  then
    exit;


  CancelCurrentSyntax;
  SetParseOffsetTarget(apos);
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllNewRun, 'AppendToPosAsync to line %', [FParseOffsetTargetP.Y]);
  {$ENDIF}
  FParserStatus.ResetNonComplete();
  FBuffer.Lock;//SyntaxDoneHandler would release the lock
  AcquireWorker().ScheduleWork(DoAppendToPos,APos,FBuffer.Version,
               HandleAppendToPosDone, true
  {$IFDEF DEBUGLOG},'DoAppendToPosAsync'{$ENDIF} );
end;

function TecClientSyntAnalyzer.DoAppendToPos(wrk:PSyntaxWork):boolean;
var aPos, currentPos, tokenCount, bufVersion, save_sep:integer;
    callRemainingSyntax,tokensDone, isAsync, bufferChanged:boolean;


begin
 result:=true;
 isAsync:=GetIsSyntaxThread();

  if isAsync then begin
   aPos:= wrk.Arg;
   save_sep:=Self.Owner._SeparateBlocks;
   //Self.Owner._SeparateBlocks:=;
 end
 else
    aPos:=FAppendAtPosArg;

 callRemainingSyntax:= true;
 tokenCount:=0;
 bufferChanged:=false;


    currentPos := GetLastPos(FBuffer.FText);
    while (currentPos - 1 <= APos + 1) and
          (FParserStatus<psPostpone) and
          not GetStopRequested(wrk)  do   begin

       if apos-currentPos>3000 then begin
          bufferChanged:= CheckSyncRequest(wrk, tokenCount);
          if bufferChanged then break;
       end;

       inc(tokenCount);
       if tokenCount and 255 = 255 then CheckProgress(currentPos);
       tokensDone := ExtractTag(FBuffer.FText, currentPos{, False});
       if tokensDone then begin
         CheckProgress(currentPos);
        callRemainingSyntax:=FOwner.SeparateBlockAnalysis;

        FParserStatus.ResetNonComplete();
        if not callRemainingSyntax then
             Finished
        else begin
           if isAsync then
             QueueSyntaxWork(true)
           else
             DoSyntaxWork(wrk);
        end;

         Break;
       end;//if done
    end;// while
    if isAsync then begin
       //  Self.Owner._SeparateBlocks:=save_sep;
    end;

    if FParserStatus=psPostpone then
     HandlePostpone();
end;

procedure TecClientSyntAnalyzer.SyntaxDoneHandler(thread: TecSyntaxerThread;wrk:PSyntaxWork);
begin
  FBuffer.Unlock;
end;

procedure TecClientSyntAnalyzer.ChangedAtPos(APos: integer);
begin
 if FDestroying then exit;
 Dec(APos);
 if APos<0 then APos := 0;
 SetParseOffsetTarget(-1);
 if FBuffer.TextLength <= Owner.FullRefreshSize then
   APos := 0
 else
 if Owner.RestartFromLineStart then
   APos := Min(APos, FBuffer.OffsetToOffsetOfLineStart(APos + 1));


   StopSyntax(false);
   FDelayWorker:=1000;
   FParserStatus.Reset();

   if (FWorkerRequested) then  begin
     AcquireWorker().ScheduleWork(DoChangeAtPos, aPos,FBuffer.Version, nil, false
     {$IFDEF DEBUGLOG},'ChangedAtPos'{$ENDIF} );
   end
   else begin
      FAppendAtPosArg:=APos;
      DoChangeAtPos(nil);
   end;
end;

 function TecClientSyntAnalyzer.PriorTokenAt(Pos: integer): integer;
begin
  WaitTillCoherent(false);
  try
  Result := FTagList.Get.PriorAt(Pos);
  finally ReleaseBackgroundLock();end;
end;

function TecClientSyntAnalyzer.GetRangeCount: integer;
begin
WaitTillCoherent();
try
  Result := FRanges.Count;
finally ReleaseBackgroundLock(); end;
end;

function TecClientSyntAnalyzer.GetRanges(Index: integer): TecTextRange;
begin
WaitTillCoherent();
try
  Result := TecTextRange(FRanges[Index]);
finally ReleaseBackgroundLock(); end;
end;

procedure TecClientSyntAnalyzer.Analyze(ResetContent: Boolean);
var OldSep: integer;
begin
  if ParserStatus>=psAborted then Exit;
  if ResetContent then  begin
      WaitTillCoherent();
      try
        OldSep := FOwner._SeparateBlocks;
        FOwner._SeparateBlocks := 2; // disable separation analysis
        Clear;
        AppendToPos(FBuffer.TextLength);
        FOwner._SeparateBlocks := OldSep;
      finally ReleaseBackgroundLock();  end;
    end else
    begin
      AppendToPos(FBuffer.TextLength);
    end;
end;

procedure TecClientSyntAnalyzer.CompleteAnalysis;
var own: TecSyntAnalyzer;
    i: integer;
begin
  WaitTillCoherent();
  try
    AppendToPos(FBuffer.TextLength);
    if FOwner.SeparateBlockAnalysis then
      for i := FStartSepRangeAnal + 1 to TagCount do  begin
        own := TecSyntAnalyzer( Tags[i - 1].Rule.SyntOwner );
        FOwner.SelectTokenFormat(Self, FBuffer.FText, own <> FOwner, i);
        if own <> FOwner then
          own.SelectTokenFormat(Self, FBuffer.FText, False, i);
        StopSyntax(true);
        Finished;
      end;//for
  finally  ReleaseBackgroundLock();  end;
end;

function TecClientSyntAnalyzer.RangeFormat(const FmtStr: ecString;
  Range: TecTextRange): ecString;
var i, j, idx, N, to_idx: integer;
    rng: TecTextRange;
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
var rngstyle: ecString;                      // HAW: add style identifier to range expression
    rngtoken, rngResult: ecString;           //      a few more vars
    swp_idx, rngdir, rngoffset, rngmax: integer;
    to_rng: TecTextRange;

function RangeNumber( const FmtStrNumber: ecString; var gotnbr: integer ): boolean;
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
  WaitTillCoherent();
  try try
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

     if (idx >= 0) and (idx < FTagList.Get.Count) then
       case LineMode of
         0: Insert(TagStr[idx], Result, i);
         1: begin
              N := FBuffer.OffsetToOffsetOfLineStart(Tags[idx].Range.StartPos);
              to_idx := Tags[idx].Range.EndPos;
              Insert(FBuffer.SubString(N, to_idx - N + 1), Result, i);
            end;
         2: begin
              to_idx := FBuffer.OffsetToOffsetOfLineEnd(Tags[idx].Range.EndPos);
              N := Tags[idx].Range.StartPos;
              Insert(FBuffer.SubString(N+1, to_idx - N + 1), Result, i); //AT: fixed substring offset/len (2 patches)
            end;
         // HAW: new mode = 3 --- explicit range  idx...to_idx
         3: if  (to_idx >= 0)  and  (to_idx < FTagList.Get.Count)  then  begin
              if  to_idx < idx  then  begin
                swp_idx := idx;  idx := to_idx;  to_idx := swp_idx;
              end;
              rngResult := '';
              while  idx <= to_idx  do  begin
                if  rngmax <= 0   then  begin
                  rngResult := rngResult+' ...';
                  break;
                end;
                if  (rngResult <> '') and (idx > 0) and (Tags[idx-1].Range.EndPos <> Tags[idx].Range.StartPos) then //MZ fix
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
  finally ReleaseBackgroundLock();end;
  except Result := ''; end;
end;

function TecClientSyntAnalyzer.GetRangeName(Range: TecTextRange): ecString;
begin
 Result := '';
 WaitTillCoherent();
 try
  if Assigned(Range.Rule) and (Range.Rule.NameFmt <> '') then
     Result := RangeFormat(Range.Rule.NameFmt, Range);
  if Result = '' then
   Result := TagStr[Range.IdentIdx];
 finally ReleaseBackgroundLock(); end;
end;

function TecClientSyntAnalyzer.GetRangeGroup(Range: TecTextRange): ecString;
begin
  WaitTillCoherent();
  try
  Result := RangeFormat(Range.Rule.GroupFmt, Range);
  finally ReleaseBackgroundLock(); end;
end;

function TecClientSyntAnalyzer.GetCollapsedText(Range: TecTextRange): ecString;
begin
  WaitTillCoherent();
  try
  Result := RangeFormat(Range.Rule.CollapseFmt, Range);
  finally ReleaseBackgroundLock(); end;
end;

function TecClientSyntAnalyzer.IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean;
begin
  AssertCoherent();
  WaitTillCoherent();
  try
  Result := _GetRuleEnabled(Rule, OnlyGlobal) and
      (HasOpened(Rule, Rule.Block, Rule.StrictParent) xor Rule.NotParent);

  finally ReleaseBackgroundLock();  end;
end;

procedure TecClientSyntAnalyzer.TextChanged(APos: integer);
begin
  if APos = -1 then
    Clear
  else
    ChangedAtPos(APos);
end;

function TecClientSyntAnalyzer.GetOpened(Index: integer): TecTextRange;
begin
  WaitTillCoherent();
  try
  Result := TecTextRange(FOpenedBlocks[Index]);
  finally ReleaseBackgroundLock(); end;
end;

function TecClientSyntAnalyzer.GetOpenedCount: integer;
begin
  WaitTillCoherent();
  try
  Result := FOpenedBlocks.Count;
  finally ReleaseBackgroundLock(); end;
end;

function TecClientSyntAnalyzer.CheckSyncRequest(wrk: PSyntaxWork;tokenCounter:integer;
  timeWait:Cardinal): boolean;

begin
  if ( not assigned(wrk) ) or (tokenCounter and minTokenStep <>minTokenStep)
      or ( not FWorkerThread.GetHasSyncRequests )    then
           exit(false);

  result:=SyncDataToMainThread(wrk, timeWait);

end;


procedure TecClientSyntAnalyzer.HandlePostpone;
begin
 {$IFDEF DEBUGLOG }
  TSynLog.Add.Log(sllLeave, 'HandleStopRequest');
 {$ENDIF}
 FParserStatus.ResetNonComplete();
 QueueSyntaxWork(true);
end;



function TecClientSyntAnalyzer.SyncDataToMainThread(const wrk: PSyntaxWork;timeWait:Cardinal):boolean;
var bufferWasLocked:boolean;
begin
  bufferWasLocked:=FBuffer.IsLocked;
  FBuffer.Unlock;
  FWorkerThread.YieldData(wrk, timeWait);
  result := wrk.BufferVersion<>FBuffer.Version;
  if result  then  begin
   wrk.DoneHandler:=nil; exit(true);
  end;
  if bufferWasLocked then
       FBuffer.Lock;
  Result:=false;
end;

//procedure TecClientSyntAnalyzer.SetDisableIdleAppend(const Value: Boolean);
//begin
//
//  if FTaskAppendDisabled <> Value then
//    begin
//      FTaskAppendDisabled := Value;
//      if  not (ParserStatus<psAborted ) then
//        DoSyntaxWork(nil);// TimerIdleTick(nil);
//    end;
//end;

function TecClientSyntAnalyzer.StopSyntax(AndWait: boolean): boolean;
const cPauseForTaskDone=10*1000;
var isAsync: boolean;
begin
  isAsync:= assigned(FWorkerThread);
  FParserStatus.Value:=psAborted;
  if not isAsync then exit(true);

  CancelCurrentSyntax;
  CheckProgress(-1);

  if AndWait and assigned(FWorkerThread) then  begin
    result:=FWorkerThread.WaitTaskDone(cPauseForTaskDone);
    assert(result);
  end
  else
     result := not FWorkerThread.GetIsTaskAssigned();
  //dont call App.ProgressMessages, it causes CudaText bug #1927
end;



function TecClientSyntAnalyzer.DetectTag(Rule: TecTagBlockCondition;
  RefTag: integer): Boolean;
var
  Tag: TecSyntToken;
begin
  WaitTillCoherent();
  try
    Tag := Tags[RefTag];
    Tag.SetRule(Rule);
    if Rule.TokenType >= 0 then
      Tag.SetTokenType(Rule.TokenType);

    SetTags(RefTag, Tag);
    Result := True;
  finally ReleaseBackgroundLock();  end;
end;

function TecClientSyntAnalyzer.GetIsLineParsed(lineIndex: integer): boolean;
var ofs:integer;
begin
  //assume is multithreaded
  ofs:=FBuffer.LineIndex(lineIndex+1);
  result := FLastAnalPos >= ofs;
end;

procedure TecClientSyntAnalyzer.CloseAtEnd(wrk:PSyntaxWork; StartTagIdx: integer);
const
  cSpecIndentID = 20;
    //special number for "Group index" lexer property, which activates indent-based folding for a rule
  cSpecTokenStart: char = '1';
    //special char - must be first of token's type name (e.g. "1keyword");
    //Also such tokens must contain spaces+tabs at the beginning (use parser regex like "^[\x20\x09]*\w+")
var i, j, IndentSize: integer;
    Range: TecTextRange;
    Token: TecSyntToken;
    isAsync:boolean;
    S: string;

begin
  AssertCoherent();
  isAsync := GetIsSyntaxThread();

  for i := FOpenedBlocks.Count - 1 downto 0 do  begin
    if  CheckSyncRequest(wrk, i) then exit;
    Range := TecTextRange(FOpenedBlocks[i]);
    if Range.Rule.EndOfTextClose and
       ((StartTagIdx = 0) or (Range.StartIdx >= StartTagIdx)) then
     begin
       Range.EndIdx := TagCount - 1;
       if Range.Rule.SyntOwner = Owner then
       if Range.Rule.GroupIndex = cSpecIndentID then
       begin
         IndentSize := IndentOf(TagStr[Range.StartIdx]);
         for j := Range.StartIdx+1 to TagCount-1 do  begin
           if  CheckSyncRequest(wrk, j) then exit;
           Token := Tags[j];
           if Token.Rule.SyntOwner <> Owner then Continue; // Check that token is not from sublexer
           S := Owner.TokenTypeNames[Token.TokenType];
           if (S <> '') and (S[1] = cSpecTokenStart) and (IndentOf(TagStr[j]) <= IndentSize) then
           begin
             Range.EndIdx := j-1;
             Break
           end;
         end;
       end;
       FOpenedBlocks.Delete(i);
     end;
   end;
end;

function TecClientSyntAnalyzer.DoChangeAtPos(wrk:PSyntaxWork):boolean;
var aPos:integer;
   i, N,tokenCount: integer;
   time:Cardinal;
  Sub: TecSubLexerRange;
  sublexerBlocks:TecSubLexerRanges;
  txtRange:  TecTextRange;
  tagList:TecTokenList;
  isAsync:boolean;

  procedure CleanRangeList(List: TSortedList; IsClosed: Boolean);
 var i: integer;
 begin
   for i := List.Count - 1 downto 0 do
    with TecTextRange(List[i]) do
     if (CondIndex >= N) or (StartIdx >= N) or IsClosed and
        ((EndCondIndex >= N) or (EndIdx >= N)) then
      List.Delete(i);
 end;

begin

   isAsync:= GetIsSyntaxThread();
   if isAsync then
      aPos:= wrk.Arg
   else
      aPos:= FAppendAtPosArg;
   FParserStatus.Reset();
   FLastAnalPos := aPos-1;   // Reset current position
   result:= true;
   WaitTillCoherent();
   time :=GetTickCount;
   sublexerBlocks := FSubLexerBlocks.Get;
   try
     // Check sub lexer ranges
     for i := sublexerBlocks.Count - 1 downto 0 do begin
      tokenCount:=i;
      //CheckSyncRequest();
       Sub:= sublexerBlocks[i];
       if APos < Sub.Range.StartPos then begin
          if APos > Sub.CondStartPos then APos := Sub.CondStartPos;
          sublexerBlocks.Delete(i);  // remove sub lexer
        end else
       if APos < Sub.CondEndPos then begin
          if APos > Sub.Range.EndPos then APos := Sub.Range.EndPos;
          Sub.Range.EndPos := -1;       // open sub lexer
          Sub.CondEndPos := -1;
          sublexerBlocks[i] := Sub;
        end;
     end; //for
     // Remove tokens
     tagList := FTagList.Get;
     tagList.ClearFromPos(APos);


     N := tagList.Count;
     FStartSepRangeAnal := N;
     // Remove text ranges from service containers
     CleanRangeList(FOpenedBlocks, False);
     // Remove text ranges from main storage
     for i := FRanges.Count - 1 downto 0 do begin
      tokenCount:=i;
      //CheckSyncRequest();
      txtRange := TecTextRange(FRanges[i]);
      with txtRange  do
       if (CondIndex >= N) or (StartIdx >= N) then FRanges.Delete(i)  else
        if (EndCondIndex >= N) or (EndIdx >= N) then
         begin
           EndIdx := -1;
           _SetEndConditionIndex(-1);
           FOpenedBlocks.Add(FRanges[i]);
         end;
     end;

     // Restore parser state
     RestoreState;
     time:= GetTickCount-time;
     //SetParseOffsetTarget(0);
  finally ReleaseBackgroundLock();end;
end;

procedure TecClientSyntAnalyzer.SetParseOffsetTarget(ofs: integer);
begin
  if ofs>=0 then
    FParseOffsetTargetP:=FBuffer.StrToCaret(ofs)
  else
    FParseOffsetTargetP.Y:=-1;
  FParseOffsetTarget:=ofs;
end;

procedure TecClientSyntAnalyzer.CheckProgress(curPos: integer);
const
  cProgressStep = 2; //update progressbar when increased by N %
  cProgressMinPos = 2000; //don't update progressbar for small files
var progress: integer;
begin
   if curPos < 0 then
   begin
     if Assigned(OnLexerParseProgress) then
       OnLexerParseProgress(Owner, -1);
     exit;
   end;

   if curPos < cProgressMinPos then
     progress := 0
   else
     progress := curPos * 100 div FBuffer.TextLength;

   if Abs(progress-FPrevProgress)>cProgressStep then
   begin
     FPrevProgress := progress;
     if Assigned(OnLexerParseProgress) then
       OnLexerParseProgress(Owner, progress);
   end;
end;

procedure TecClientSyntAnalyzer.QueueSyntaxWork(delayed: boolean);
var mtd:TThreadMethod;
begin
  FParserStatus.ResetNonComplete();
  if FParserStatus>=psComplete then exit;
  if delayed then mtd:=DelayedWork
  else mtd:=HandleAddWork;
  TThread.ForceQueue(nil,
     mtd);
end;

function TecClientSyntAnalyzer.GetStopRequested(wrk: PSyntaxWork): boolean;
begin
  result:= assigned(wrk) and (wrk.StopRequested);
  {$IFDEF DEBUGLOG}
  if result then
    TSynLog.Add.Log(sllServiceReturn, 'Stop Requested: '+wrk.Name );
  {$ENDIF}
end;


procedure TecClientSyntAnalyzer.HandleAddWork();
var worker:TecSyntaxerThread;
begin
  if FDestroying then exit;
  if FParserStatus=psComplete then exit;

  FParserStatus.ResetNonComplete();
  if not FWorkerRequested then begin
    DoSyntaxWork(nil);
    exit;
  end;
  FPrevProgress:=-1;
 if FLastAnalPos<FParseOffsetTarget then begin

    AppendToPos(FParseOffsetTarget, true);
 end
 else begin
  worker:=AcquireWorker();
  FBuffer.Lock;
  worker.ScheduleWork(DoSyntaxWork, -100,FBuffer.Version, SyntaxDoneHandler, true
  {$IFDEF DEBUGLOG},'DoSyntaxWork'{$ENDIF});
 end;


end;


{ TecAsyncSelector }

procedure TecAsyncSelector<TEntity>.Init(const parent: TecParserResults;
  const aEntity, workEntitiy: TEntity);
begin
 FParent := parent;
 FEntity :=aEntity;
 FWorkerEntity:= workEntitiy;
end;

procedure TecAsyncSelector<TEntity>.Release();
begin
 FreeAndNil(FEntity);
 FreeAndNil(FWorkerEntity);
end;



procedure TecAsyncSelector<TEntity>.Swap();
var glass:TEntity;
begin
 with FParent.FWorkerThread do begin
    DoTagSync(true);
    try
      glass:=FWorkerEntity;
      FWorkerEntity:=FEntity;
      FEntity:=glass;
    finally DoTagSync(false);  end;
  end;
end;

function TecAsyncSelector<TEntity>.Get: TEntity;
begin
   if FParent.GetIsSyntaxThread() then
     Result:=FWorkerEntity
   else
     Result:= FEntity;
   if not Assigned(Result) then
    Result:=nil;
end;

{ TecTagListSelectorHelp }

procedure TecTagListSelectorHelp.SyncWorkerList(toWorker:boolean);
begin
 with  self.Parent.FWorkerThread do begin
   DoTagSync(true);
   try
   if toWorker then
    WorkerEntity.Assign(Entity)
   else
     Entity.Assign(WorkerEntity);
   finally DoTagSync(false);  end;
 end;
end;




end.
