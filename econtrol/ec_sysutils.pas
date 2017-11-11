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

unit ec_SysUtils;

interface

uses
  Classes, Forms;

// 0 - all is ok
// 1 - application terminated
function SafeProcessMessages(Sender: TObject): integer;

implementation

uses SysUtils;

function SafeProcessMessages(Sender: TObject): integer;
begin
  Application.ProcessMessages;
  if Application.Terminated then Result:= 1 else Result:= 0; 
end;


end.
