unit ec_Async;

{$mode Delphi}{$H+}
{$IFDEF DEBUG}
{$INLINE OFF}
{$ENDIF}
interface

uses
  Classes, SysUtils,  syncobjs,
  Generics.Collections
   {$IFDEF DEBUGLOG},Forms , SynCommons, SynLog,mORMotHttpClient  {$ENDIF};
  {$IFDEF TRICK}
  {$MESSAGE WARN 'ПЕДАЛЬ В ПОЛ!!!!!!!!!!!111'}
  {$ENDIF}
type
    IAsyncSyntaxClient=interface
     procedure SwitchContext(toWorker:boolean);
     procedure WorkerDetached();
    end;
    TExceptionHandler = procedure (const e:Exception) of object;

     PSyntaxWork=^TSyntaxWork;
     TSyntaxWorkDelegate = function(pTask:PSyntaxWork):boolean of object;

     TecSyntaxerThread=class;
     TTaskDoneHandler= procedure (thread:TecSyntaxerThread; wrk:PSyntaxWork) of object;

     { TSyntaxWork }
     TSyntaxWork=record
      strict private
      FExpendable,FStop:boolean;
      FTask:TSyntaxWorkDelegate;
      FBufferVersion:Integer;
      public

      //happens when task is done(sender is thred) or cancelled(sender is nil)
      DoneHandler:TTaskDoneHandler;

      Arg: Integer;
      {$IFDEF DEBUGLOG}Name:ansistring;{$ENDIF}
      procedure Setup(const task:TSyntaxWorkDelegate;aArg:integer;
      aBufferVersion:Integer; const doneEvent:TTaskDoneHandler; aExpendable:boolean
      {$IFDEF DEBUGLOG}; aName:string{$ENDIF}
      );overload;
      procedure Setup(constref other:TSyntaxWork);overload;
      procedure Clear();
      procedure Stop;inline;
      function TaskAssigned():boolean;inline;
      function IsSame(const other:TSyntaxWork):boolean;
      property StopRequested:boolean read FStop;
      property Task :TSyntaxWorkDelegate read FTask;
      property Expendable:boolean read FExpendable;
      property BufferVersion:integer read FBufferVersion;
     end;

    { TecSyntaxerThread }
    TecSyntaxerThread=class(TThread)
    private
      FSyncRequestCount: integer;

    protected

    type
    TWorkQueue= TQueue<TSyntaxWork>;
    TIdleDestroyTime= 1..300;
    ///<summary>Request and wait for access to the syntaxer data</summary>
    procedure AcquireSyncHarder(const roSync: boolean);
    strict protected
    FClientSyntaxer:IAsyncSyntaxClient;
    //FStopCurrentTask,
    FSchedulingDisabled : boolean;
    FCurrentWork  : PSyntaxWork;
    FSyntaxQueue:TWorkQueue;
    FWakeUpEvent, FTaskDoneEvent : TEvent;
    FIdleTimeStart:Cardinal;
    FSyncWall, _FSchedulerWall, FTagSync: TCriticalSection;
    FOnError : TExceptionHandler;
    FStartTime: Cardinal;
    FIdleDestroyTime: TIdleDestroyTime;
    class var FThreadCount:integer;
    const FPauseTimeOnSyncRequest:integer=60;//in ms

    procedure HandleTaskDone(var task:TSyntaxWork);
    procedure HandleTaskStart(constref task:TSyntaxWork);
    procedure HandleError(const e:Exception);

    function GetScheduled(out task:TSyntaxWork): boolean;
    function WaitForNewTask(out task:TSyntaxWork): boolean;
    procedure Execute;override;
    function ExecuteTask(var wrk:TSyntaxWork):boolean;
    procedure EnsureTerminated;
    procedure SetSchedulingEnabled(doEnable:boolean);
    procedure BeginSchedule;
    procedure EndSchedule;
    procedure SleepAtWork(const wrk:PSyntaxWork; timeToSleep:cardinal);
    public
    constructor Create(const syntaxer:IAsyncSyntaxClient;
                   const errorHandler:TExceptionHandler);
    destructor Destroy();override;
    procedure ResetIdleTimer();
    function GetIsTaskAssigned():boolean;inline;
    function GetIsWorkerThread():boolean;inline;
    function RequestRelease(var me:TecSyntaxerThread):boolean;
    function GetSyncNowAccessible:boolean;
    procedure AcquireSync(roSync:boolean);inline;
    function TryAcquireSync():boolean;inline;
    function GetHasSyncRequests():boolean;inline;
    procedure ReleaseSync();inline;
//    function WaitTaskAndSync(const timeOut:Cardinal):boolean;
    function GetIsWorking():boolean; inline;
    procedure YieldData(wrk:PSyntaxWork; timeWait:Cardinal);
    procedure StopCurrentTask();
    procedure CancelExpendableTasks();
    function WaitTaskDone(time:Cardinal):boolean;
    function ScheduleWork(syntaxProc : TSyntaxWorkDelegate;
                   aArg, aBufferVersion:integer; aDoneHandler:TTaskDoneHandler;
                   expendable:boolean
                   {$IFDEF DEBUGLOG}; const name:ansistring{$ENDIF}):boolean;
    procedure IssueSyncRequest();
    procedure DoTagSync(enter:boolean);
    procedure Terminate2;
    property SyncRequestCount:integer read FSyncRequestCount;
    property BusyWorking:boolean read GetIsWorking;
    property IdleDestroyTime:TIdleDestroyTime read FIdleDestroyTime write FIdleDestroyTime;
  end;



function InterlockedCompareExchangeLb(var target: LongBool;
        NewValue, Comperand: LongBool): LongBool;inline;


var _ThreadMgr :TThreadManager;// workaround FPC/RTL bug
implementation

{ TecSyntaxerThread.TSyntaxWork }

procedure TSyntaxWork.Setup(const task: TSyntaxWorkDelegate;
  aArg:integer; aBufferVersion:Integer; const doneEvent: TTaskDoneHandler;
  aExpendable:boolean {$IFDEF DEBUGLOG}; aName:string{$ENDIF});
begin
    self.FTask:=task; DoneHandler:=doneEvent;
    self.Arg:=aArg; self.FExpendable:=aExpendable;
    self.FBufferVersion:=aBufferVersion;
    self.FStop:=false;
    {$IFDEF DEBUGLOG}
    self.name:= aName;
    {$ENDIF}
end;

procedure TSyntaxWork.Setup(constref other: TSyntaxWork);
begin
  FTask := other.FTask; DoneHandler:=other.DoneHandler;
  Arg:=other.Arg; FExpendable:=other.FExpendable;
  FBufferVersion:= other.FBufferVersion;
  FStop:= other.FStop;
  {$IFDEF DEBUGLOG}
  Name:= other.Name;
  {$ENDIF}
end;

procedure TSyntaxWork.Clear();
begin
  FTask:=nil;DoneHandler:=nil;
end;

procedure TSyntaxWork.Stop;
begin
  FStop:=true;
end;

function TSyntaxWork.TaskAssigned(): boolean;
begin
 result :=assigned(FTask);
end;

function TSyntaxWork.IsSame(const other: TSyntaxWork
  ): boolean;
begin
  result := @FTask = @other.FTask;
end;



{ TecSyntaxerThread }

function TecSyntaxerThread.GetIsWorking(): boolean;
begin
  result := assigned(FCurrentWork);
end;



constructor TecSyntaxerThread.Create(
  const syntaxer:IAsyncSyntaxClient;
  const errorHandler:TExceptionHandler);
begin
  FIdleDestroyTime:=10;
  FClientSyntaxer:=syntaxer;
  Inc(FThreadCount);
  FSyntaxQueue := TWorkQueue.Create();
  FSyncRequestCount:= 0;
  FWakeUpEvent:=TEvent.Create(nil, false, false,'');
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllInfo, 'Worker thread created');
  {$ENDIF}

  FTaskDoneEvent:=TEvent.Create(nil, true, true,'');
  //  Format('TecSyntaxerThreadWake%d',[FThreadCount]));
  FTagSync :=TCriticalSection.Create();
  FSyncWall := TCriticalSection.Create();
  _FSchedulerWall := TCriticalSection.Create();
  inherited Create(false);
  FreeOnTerminate:=true;
end;

procedure TecSyntaxerThread.AcquireSync(roSync:boolean);
var  done:boolean;
begin
 done:=FSyncWall.TryEnter;
 if done then exit;
 AcquireSyncHarder(roSync);
end;

function TecSyntaxerThread.TryAcquireSync(): boolean;
begin
 result:=FSyncWall.TryEnter;
end;

function TecSyntaxerThread.GetHasSyncRequests(): boolean;
begin
 result :=volatile(FSyncRequestCount)<>0;
end;

procedure TecSyntaxerThread.ReleaseSync();
begin
//if not GetIsWorkerThread() then
//     FSynRequestCount:=0;
  {$IFDEF DEBUG}
  //if FSynRequestCount <0  then
  //  assert(false, 'ReleaseSync');
  {$ENDIF}
  FSyncWall.Release();
end;

procedure TecSyntaxerThread.StopCurrentTask();
begin
BeginSchedule;
 if assigned(FCurrentWork) then FCurrentWork.Stop();
EndSchedule;
end;

procedure TecSyntaxerThread.CancelExpendableTasks();
label Tail;
var task:TSyntaxWork;
    q:TWorkQueue;
    queueNonEmpty:boolean;
begin

BeginSchedule;
queueNonEmpty:= FSyntaxQueue.Count>0;
try
if queueNonEmpty  then begin
  if (FSyntaxQueue.Count=1) and (not FSyntaxQueue.Peek.Expendable)
    then goto Tail;
  {$IFDEF DEBUGLOG}
    TSynLog.Add.Log(sllCache, 'CancelExpendableTasks');
  {$ENDIF}
  q:=TWorkQueue.Create;
  for task in FSyntaxQueue do begin
        if not task.Expendable then
             q.Enqueue(task)
        else if Assigned(task.DoneHandler) then
           task.DoneHandler(nil, @task);
  end;
  FSyntaxQueue.Clear;
   for task in q do
     FSyntaxQueue.Enqueue(task);
   q.Free();
end;
 Tail:
finally EndSchedule; end;
end;

function TecSyntaxerThread.WaitTaskDone(time: Cardinal): boolean;
var timeWaited:integer;
    wr:TWaitResult;
begin

 timeWaited:= GetTickCount();
 wr :=FTaskDoneEvent.WaitFor(time);
 result :=wr=wrSignaled;
 if not result then
  exit(false);

 timeWaited:= GetTickCount()- timeWaited;
 {$IFDEF DEBUGLOG}
   if timeWaited>50 then begin
      TSynLog.Add.Log(sllWarning, 'Waited for task done % s', [timeWaited] );
   end;
 {$ENDIF}
end;

function TecSyntaxerThread.ScheduleWork(syntaxProc: TSyntaxWorkDelegate;
  aArg, aBufferVersion:integer; aDoneHandler: TTaskDoneHandler;
  expendable:boolean {$IFDEF DEBUGLOG}; const name:ansistring{$ENDIF}): boolean;
var isBusy:boolean;
    newTask:TSyntaxWork;
    label Tail;

begin
  assert( not Terminated, 'Attempt to schedule work on terminated thread');

  if FSchedulingDisabled then
    Assert(false, 'Schedule, when it''s disabled');

  BeginSchedule;
  ResetIdleTimer();
   try
      newTask.Setup(syntaxProc,aArg,aBufferVersion, aDoneHandler, expendable
      {$IFDEF DEBUGLOG},name{$ENDIF} );

      {$IFDEF DEBUGLOG}
         TSynLog.Add.Log(sllSQL, 'Task %s added [%d]', [name, FSyntaxQueue.Count]);
      {$ENDIF}
      FSyntaxQueue.Enqueue(newTask);
  finally
    EndSchedule;
  end;
  FWakeUpEvent.SetEvent;
end;

procedure TecSyntaxerThread.IssueSyncRequest();
begin
self.FSyncRequestCount:=1;
end;

procedure TecSyntaxerThread.DoTagSync(enter: boolean);
begin
  if enter then
    FTagSync.Enter
  else
    FTagSync.Leave;
end;

procedure TecSyntaxerThread.Terminate2;
begin
 inherited Terminate;

 FClientSyntaxer:=nil;
end;

function TecSyntaxerThread.GetIsWorkerThread(): boolean;
begin
 result := self.ThreadID = _ThreadMgr.GetCurrentThreadId ;
end;

function TecSyntaxerThread.RequestRelease(var me:TecSyntaxerThread): boolean;
begin
BeginSchedule;
try
result:= not GetIsWorking() ;
if result then begin
  result:= FSyntaxQueue.Count<=0;
  if result then  begin
     me:=nil;
end;
end;
finally EndSchedule; end;
if result then  Terminate;


end;










procedure TecSyntaxerThread.AcquireSyncHarder(const roSync: boolean);
var time, spinCnt:Cardinal;
    acquired:boolean;
begin
  {$IFDEF DEBUGLOG}
   time:=GetTickCount();
  {$ENDIF}
   if roSync then begin
     spinCnt:=0;
      repeat
         FSyncRequestCount:=1;
         Inc(spinCnt);
         acquired:=FSyncWall.TryEnter;
      until  acquired or (spinCnt>1000*1000) ;

      if not acquired then //gave up spinning, wait properly
         FSyncWall.Enter;

      FSyncRequestCount:=0;
   end
   else
      FSyncWall.Enter();
   {$IFDEF DEBUGLOG}
   time:=GetTickCount-time;
   if (time > 50) or (roSync and (time>30)) then
      TSynLog.Add.Log(sllWarning, 'AcquireSync waited: % ms', [time]);
   {$ENDIF}
end;

procedure TecSyntaxerThread.HandleTaskDone(var task:TSyntaxWork);
begin
  if assigned(task.DoneHandler) then
           task.DoneHandler(self, @task);
 {$IFDEF DEBUGLOG}
  FStartTime:=GetTickCount()-FStartTime;
  TSynLog.Add.Log(sllTrace, 'Task %s done in % ms',[task.Name, FStartTime]);
 {$ENDIF}

 FTaskDoneEvent.SetEvent();

 BeginSchedule;
 try
 FCurrentWork:=nil;
 task.Clear();
 finally  EndSchedule; end;
end;

procedure TecSyntaxerThread.HandleTaskStart(constref task: TSyntaxWork);
begin

  FStartTime:=GetTickCount();
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllInfo, 'Task %s started', [task.Name]);
  {$ENDIF}
  FTaskDoneEvent.ResetEvent();
  FCurrentWork :=@task;
end;

function TecSyntaxerThread.GetScheduled(out task:TSyntaxWork): boolean;
begin
 result:= false;
 if FSchedulingDisabled then exit;
  BeginSchedule;
  try
  result:= FSyntaxQueue.Count >0;
  if result then begin
   task.Setup(FSyntaxQueue.Dequeue());
  end;
  finally EndSchedule; end;
end;

function TecSyntaxerThread.WaitForNewTask(out task:TSyntaxWork): boolean;
var waitRslt:TWaitResult;
    timeWait:Cardinal;
begin
   result := GetScheduled(task);
   if result then exit;

   waitRslt:= FWakeUpEvent.WaitFor(1000);

   Assert(waitRslt <> wrError, 'FWakeUpEvent released? Must never!');
   BeginSchedule;
   try
   result := GetScheduled(task);
   if not result then begin
     timeWait:=GetTickCount -FIdleTimeStart;
     if (timeWait>FIdleDestroyTime*1000) and not (Terminated) then begin
       if assigned(FClientSyntaxer) then begin
       TThread.Queue(nil,
       FClientSyntaxer.WorkerDetached);
       end;

     end;
   end;
   finally
     EndSchedule;
   end;
end;

procedure TecSyntaxerThread.Execute;
    var
    taskDelegate:TSyntaxWork;
    taskComplete, gotTask: boolean;
    timeElapsed :Cardinal;
    const task_max_ms = 10*1000;//10 seconds, or goto hell
begin
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllInfo, 'Worker thread started %', [ThreadID]);
  {$ENDIF}
  FSyncRequestCount:= 0;
  taskDelegate.Clear;
  FIdleTimeStart:=GetTickCount();
  repeat
   if not taskDelegate.TaskAssigned() then begin // fetch new task

     gotTask:=WaitForNewTask(taskDelegate);
     if  not gotTask then  continue;
   end;
   if Terminated then break;
   AcquireSync(false);

   FClientSyntaxer.SwitchContext(true);
   try
     HandleTaskStart(taskDelegate);
     timeElapsed:=self.GetTickCount();
     taskComplete:=   ExecuteTask(taskDelegate) ;
   finally
     HandleTaskDone(taskDelegate);
     FClientSyntaxer.SwitchContext(false);
     ReleaseSync();
   end;
   FIdleTimeStart:=GetTickCount();
  until Terminated;
  FCurrentWork:=nil;
  FClientSyntaxer:=nil;
end;

function TecSyntaxerThread.ExecuteTask(var wrk: TSyntaxWork):boolean;
begin
  result := true; // in case of error
  assert(GetIsWorkerThread(),
             'ExecuteTask must only be invoked asynchronously');
  try
   result := wrk.Task(@wrk);
  except
   on e:Exception do  HandleError(e);
  end;
end;

procedure TecSyntaxerThread.EnsureTerminated;
var wr:integer;
begin
 StopCurrentTask();
 Terminate;
 //wr :=self.WaitFor();
//  assert(wr=0);
end;

procedure TecSyntaxerThread.SetSchedulingEnabled(doEnable: boolean);
begin
 BeginSchedule;
 FSchedulingDisabled:= not doEnable;
 EndSchedule;
end;

procedure TecSyntaxerThread.BeginSchedule;
begin
 {$IFDEF DEBUGLOG}
// TSynLog.Add.Log(sllEnter, 'schedule BEGIN' );
 {$ENDIF}
 _FSchedulerWall.Enter;
end;

procedure TecSyntaxerThread.EndSchedule;
begin
 {$IFDEF DEBUGLOG}
// TSynLog.Add.Log(sllLeave, 'schedule End' );
 {$ENDIF}
 _FSchedulerWall.Leave;
end;

procedure TecSyntaxerThread.SleepAtWork(const wrk: PSyntaxWork; timeToSleep:cardinal);
label prematureStop;
var startTime:Cardinal;

begin
  startTime:=GetTickCount;
  repeat
    if wrk.StopRequested then goto prematureStop;
    Sleep(30);
  until GetTickCount()-startTime >= timeToSleep;
  exit;
  prematureStop:
  {$IFDEF DEBUGLOG}
     TSynLog.Add.Log(sllMonitoring, 'SleepAtWork:Premature Stop');
  {$ENDIF}
end;



procedure TecSyntaxerThread.YieldData(wrk:PSyntaxWork; timeWait:Cardinal);
var doWait:boolean;
begin
//  if FSynRequestCount<=0 then exit;
  FClientSyntaxer.SwitchContext(false);
  FSyncWall.Release;
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log( sllEnter, 'Yield Data %ms!', [timeWait]);
  {$ENDIF}
  Yield;
  doWait :=timeWait>0;
  if (FSyncRequestCount>0) or doWait then begin
     if doWait then
        SleepAtWork(wrk, timeWait)
     else
       Sleep(30);
  end;
  FSyncRequestCount:=0;
  FSyncWall.Acquire;
  FClientSyntaxer.SwitchContext(true);
end;

procedure TecSyntaxerThread.HandleError(const e: Exception);
begin
  if assigned(FOnError) then
     FOnError(e);
end;



destructor TecSyntaxerThread.Destroy();
begin
  EnsureTerminated;
  {$IFDEF DEBUGLOG}
  if not Application.Terminated then
  TSynLog.Add.Log(sllInfo,'Worker destroyed  %', [ThreadID]);
  {$ENDIF}
  FreeAndNil(FWakeUpEvent);
  FreeAndNil(FSyncWall);
  FreeAndNil(_FSchedulerWall);
  FreeAndNil(FTagSync);
  FreeAndNil(FSyntaxQueue);
  inherited Destroy();
end;

procedure TecSyntaxerThread.ResetIdleTimer();
begin
 BeginSchedule;
 FIdleTimeStart:=GetTickCount;
 EndSchedule;
end;

function TecSyntaxerThread.GetIsTaskAssigned(): boolean;
begin
  result := assigned(FCurrentWork);
end;

function TecSyntaxerThread.GetSyncNowAccessible: boolean;
begin
 result:= FSyncWall.TryEnter;
 if result then FSyncWall.Leave;
end;

//function TecSyntaxerThread.WaitTaskAndSync(const timeOut: Cardinal): boolean;
//var timeStart, elapsed:Cardinal;
//begin
// assert(self<> TThread.CurrentThread,
//    'WaitTaskAsync is to called outside worker thread');
//
// timeStart := GetTickCount();
// SetSchedulingEnabled(false);
// repeat
//  FTaskDoneEvent.WaitFor(timeout);
//  elapsed := GetTickCount() - timeStart;
//  result :=not GetIsWorking();
// until result or (elapsed > timeOut) ;
// {$IFDEF DEBUGLOG}
// if elapsed> 350 then
// TSynLog.Add.Log(sllWarning, 'Waited task %d ms', [elapsed]);
// {$ENDIF}
// if result then
//      AcquireSync(false);
// SetSchedulingEnabled(true);
//end;





function InterlockedCompareExchangeLb(var target: LongBool;
        NewValue, Comperand: LongBool): LongBool;inline;
var _target : LongWord absolute Target;
begin
 result:=LongBool( InterlockedCompareExchange(_target, LongWord(NewValue),
          LongWord(Comperand) ) );
end;


{$IFDEF DEBUGLOG}
var hlog:TSQLHttpClient;
procedure MakeLogger();
begin
 TSynLog.Family.LevelStackTrace:=[sllWarning,sllCustom1, sllCustom2
 {$IFDEF DEBUG}
 ,sllCache
 //, sllSQL
 {$ENDIF}
 ] ;

 TSynLog.Family.Level:=LOG_VERBOSE;
 TSynLog.Family.PerThreadLog:=ptIdentifiedInOnFile;
 try
 hlog:=TSQLHttpClient.CreateForRemoteLogging('127.0.0.1', TSynLog);
 except end;

 TSynLog.Add.LogThreadName('Main');
end;

procedure StopLogger;
begin

 TSynLog.Family.EchoRemoteStop();

end;

{$ENDIF}

initialization
GetThreadManager(_ThreadMgr);
{$IFDEF DEBUGLOG}
 MakeLogger();
{$ENDIF}
finalization
  {$IFDEF DEBUGLOG}
    StopLogger;
//    Halt(0);
  {$ENDIF}
end.

