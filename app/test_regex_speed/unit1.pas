unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    SText: string;
    procedure Test_EC(const Subj: Unicodestring);
    procedure Test_FPC(const Subj: Unicodestring);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  RegExpr,
  ec_RegExpr;

{$R *.lfm}

const
  Rules: array[0..5] of string = (
    '//.*',
    '\{.*?\}',
    '\d+(\.\d+)?',
    '\w+',
    '[\-\+/\*\[\]\(\)\.,:=]+',
    '''.*?'''
    );

procedure TForm1.Test_EC(const Subj: Unicodestring);
var
  Obj: array[0..Length(Rules)-1] of TecRegExpr;
  NPos, NLen: integer;
  bRuleFound, bLastFound: boolean;
  IndexRule, i: integer;
  ch: Widechar;
begin
  for i:= 0 to Length(Rules)-1 do
  begin
    Obj[i]:= TecRegExpr.Create;
    Obj[i].Expression:= UTF8Decode(Rules[i]);
    Obj[i].ModifierI:= false;
    Obj[i].ModifierS:= false; //don't catch all text by .*
    Obj[i].ModifierM:= true; //allow to work with ^$
    Obj[i].ModifierX:= false; //don't ingore spaces
  end;

  NPos:= 1;
  NLen:= 1;
  bLastFound:= false;

  repeat
    if NPos>Length(Subj) then Break;
    bRuleFound:= false;

    ch:= Subj[NPos];
    if ((ch<>' ') and (ch<>#9)) then
      for IndexRule:= 0 to Length(Rules)-1 do
      begin
        NLen:= Obj[IndexRule].MatchLength(Subj, NPos);
        if NLen>0 then
        begin
          bRuleFound:= true;

          //Listbox1.Items.Add('> '+Copy(Subj, NPos, NLen));

          Break;
        end;
      end;

    if not bRuleFound then
    begin
      Inc(NPos);
    end
    else
    begin
      Inc(NPos, NLen);
    end;

    bLastFound:= bRuleFound;
  until false;


  for i:= 0 to Length(Rules)-1 do
    Obj[i].Free;
end;

procedure TForm1.Test_FPC(const Subj: Unicodestring);
begin

end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  t, tt: qword;
begin
  t:= GetTickCount64;
  Test_EC(SText);
  t:= GetTickCount64-t;

  tt:= GetTickCount64;
  Test_FPC(SText);
  tt:= GetTickCount64-tt;

  Listbox1.Items.Add(Format('EControl ec_RegExpr: %d ms', [t]));
  Listbox1.Items.Add(Format('FPC RegExpr: %d ms', [tt]));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  L: TStringList;
  fn: string;
begin
  fn:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+
    DirectorySeparator+'econtrol'+DirectorySeparator+'ec_syntanal.pas';
  if not FileExists(fn) then
  begin
    Listbox1.Items.Add('Cannot find sample file: '+fn);
    exit;
  end;

  L:= TStringList.Create;
  L.LoadFromFile(fn);
  SText:= L.Text;
  L.Free;

  Listbox1.Items.Add('Test string length (ec_syntanal.pas): '+IntToStr(Length(SText)));
end;

end.

