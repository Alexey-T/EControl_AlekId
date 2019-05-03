unit ec_token_holder;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils,
  ec_StrUtils,
  ec_rules;

type
    TTokenHolder = class abstract(TInterfacedObject)
  strict protected
    function GetTokenCount: integer; virtual; abstract;
    function GetTags(Index: integer): TecSyntToken;virtual; abstract;
    public
    function GetTokenType(Index: integer): integer; virtual; abstract;
    function GetTokenStr(Index: integer): ecString; virtual; abstract;
    property Tags[Index: integer]: TecSyntToken read GetTags;default;
    property TagCount: integer read GetTokenCount;
  end;

implementation

end.

