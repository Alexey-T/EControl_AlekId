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

unit ecSysUtils;

interface

uses
  Classes, Controls, Forms;

// 0 - all is ok
// 1 - application terminated
// 2 - object destroyed
function SafeProcessMessages(Sender: TObject): integer;
procedure SafeDestroying(Sender: TObject);

implementation

uses SysUtils;

function SafeProcessMessages(Sender: TObject): integer;
begin
  Application.ProcessMessages;
  if Application.Terminated then Result:= 1 else Result:= 0; 
end;

procedure SafeDestroying(Sender: TObject);
begin
end;

(*
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
*)


initialization
  //RefList := TList.Create;

finalization
  //FreeAndNil(RefList);


end.
