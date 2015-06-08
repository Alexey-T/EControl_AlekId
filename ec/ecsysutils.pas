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

unit ecSysUtils;

interface

uses
  Classes, Controls, Forms;

procedure StartWait;
procedure StopWait;

// 0 - all is ok
// 1 - application terminated
// 2 - object destroyed
function SafeProcessMessages(Sender: TObject): integer;
procedure SafeDestroying(Sender: TObject);

//======================
// Dialog routines
// Shows dialog made from frame
function ShowDialogFromFrame(Frame: TFrame; const Caption: string; HelpIdx: THelpContext = 0; KeepFrame: Boolean = False): TModalResult;
procedure EnableControl(Control: TControl; bEnable: Boolean);

function GetCollectionOwner(Collection: TCollection): TPersistent;

implementation

uses SysUtils, StdCtrls;

{$J+}

const
  WaitCount: Integer = 0;
  WaitCursor: TCursor = crHourGlass;
  SaveCursor: TCursor = crDefault;

procedure StartWait;
begin
  if WaitCount = 0 then
   begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := WaitCursor;
   end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then begin
    Dec(WaitCount);
    if WaitCount = 0 then Screen.Cursor := SaveCursor;
  end;
end;

// 0 - all is ok
// 1 - application terminated
// 2 - object destroyed
var
  ProcessCounter: Cardinal;
  RefList: TList = nil;

function SafeProcessMessages(Sender: TObject): integer;
begin
  if RefList = nil then
    RefList := TList.Create;
  RefList.Add(Sender);
  Inc(ProcessCounter);
  try
    Application.ProcessMessages;
    if ProcessCounter mod 10000 = 0 then
     Application.HandleMessage;
  finally
    if RefList.IndexOf(Sender) = -1 then
      Result := 2
    else if Application.Terminated then
      Result := 1
    else
      Result := 0;
    RefList.Remove(Sender);
  end;
end;

procedure SafeDestroying(Sender: TObject);
begin
  if RefList <> nil then
    RefList.Remove(Sender);
end;

// ============================================================================
// Dialog routines
// ============================================================================
function ShowDialogFromFrame(Frame: TFrame; const Caption: string; HelpIdx: THelpContext; KeepFrame: Boolean): TModalResult;
var Dlg: TForm;
    X, Y: integer;
  function CreateBtn: TButton;
  begin
    Result := TButton.Create(Dlg);
    Result.Parent := Dlg;
    Result.Left := X - Result.Width - 8;
    Result.Top := Y;
    X := Result.Left;
  end;
begin
  Dlg := TForm.Create(Application);
  try
    Dlg.HelpContext := HelpIdx;
    Dlg.ClientWidth := Frame.Width;
    Dlg.ClientHeight := Frame.Height + 40;
    Dlg.Position := poScreenCenter;
    Dlg.Caption := Caption;
    Dlg.BorderStyle := bsDialog;
    Frame.Parent := Dlg;
    Frame.Align := alTop;
    Frame.Visible := True;
    Y := Dlg.ClientHeight - 33;
    X := Dlg.ClientWidth;

    with CreateBtn do
      begin
        ModalResult := mrCancel;
        Cancel := True;
        Caption := 'Cancel';
      end;
    with CreateBtn do
      begin
        ModalResult := mrOK;
        Default := True;
        Caption := 'OK';
      end;
    Result := Dlg.ShowModal;
    if KeepFrame then
      Frame.Parent := nil;
  finally
    Dlg.Free;
  end;
end;

type
  TControlAccess = class(TControl);
  TPersistentAccess = class(TPersistent);

procedure EnableControl(Control: TControl; bEnable: Boolean);
begin
  Control.Enabled := bEnable;
end;

function GetCollectionOwner(Collection: TCollection): TPersistent;
begin
  Result := Collection.Owner;
end;

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

var
  MenuKeyCaps: array[TMenuKeyCap] of string = (
    'BkSp', 'Tab', 'Esc', 'Enter', 'Space', 'PgUp',
    'PgDn', 'End', 'Home', 'Left', 'Up', 'Right',
    'Down', 'Ins', 'Del', 'Shift', 'Ctrl', 'Alt');


initialization
  RefList := TList.Create;

finalization
  FreeAndNil(RefList);


end.
