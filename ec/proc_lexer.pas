unit proc_lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ecSyntAnal,
  ecStrUtils,
  ATStringProc;

function DoFindLexerForFilename(LexLib: TecSyntaxManager; const FileName: string): TecSyntAnalyzer;
function DoGetLexerFileFilter(an: TecSyntAnalyzer): string;
function DoGetLexerDefaultExt(an: TecSyntAnalyzer): string;

implementation

function DoFindLexerForFilename(LexLib: TecSyntaxManager; const FileName: string): TecSyntAnalyzer;
var
  fname, ext1, ext2: string;
  i: integer;
  st: TzStringList;
begin
  Result:= nil;

  fname:= '/' + LowerCase(ExtractFileName(FileName));

  ext1:= LowerCase(ExtractFileExt(FileName));
  if SBegin(ext1, '.') then Delete(ext1, 1, 1);

  ext2:= '';
  if ext1<>'' then
  begin
    ext2:= LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
    if SBegin(ext2, '.') then Delete(ext2, 1, 1);
    if ext2<>'' then
      ext2:= ext2+'.'+ext1;
  end;

  st:= TzStringList.Create;
  try
    st.Delimiter:= ' ';

    //find by double extension
    if ext2<>'' then
      for i:= 0 to LexLib.AnalyzerCount-1 do
        with LexLib.Analyzers[i] do
          if not Internal then
          begin
            st.DelimitedText:= Extentions;
            if (ext2<>'') and (st.IndexOf(ext2)>=0) then
            begin
              Result:= LexLib.Analyzers[i];
              Exit;
            end;
          end;

    //find by usual extension + filename
    for i:= 0 to LexLib.AnalyzerCount-1 do
      with LexLib.Analyzers[i] do
        if not Internal then
        begin
          st.DelimitedText:= Extentions;
          if ((ext1<>'') and (st.IndexOf(ext1)>=0)) or
                            (st.IndexOf(fname)>=0) then
          begin
            Result:= LexLib.Analyzers[i];
            Exit;
          end;
        end;
  finally
    st.Free;
  end;
end;

function DoGetLexerFileFilter(an: TecSyntAnalyzer): string;
var
  s: string;
  st: TzStringList;
  i: integer;
begin
  Result:= '';
  st:= TzStringList.Create;
  try
    st.Delimiter:= ' ';
    st.DelimitedText:= an.Extentions;
    if st.Count=0 then Exit;
    Result:= an.LexerName+'|';
    for i:= 0 to st.Count-1 do
      Result:= Result+'*.'+st[i]+';';
    Result:= Result+'|';
  finally
    st.Free;
  end;

  Result:= Result+'All files|*|';
end;

function DoGetLexerDefaultExt(an: TecSyntAnalyzer): string;
var
  n: integer;
begin
  Result:= an.Extentions;
  n:= Pos(' ', Result);
  if n>0 then Delete(Result, n, Maxint);
end;


end.

