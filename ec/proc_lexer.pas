(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ecSyntAnal,
  ecStrUtils,
  ATStringProc;

function DoFindLexerForFilename(LexLib: TecSyntaxManager; const FileName: string): TecSyntAnalyzer;
function DoGetLexerFileFilter(an: TecSyntAnalyzer; const AllFilesText: string): string;
function DoGetLexerDefaultExt(an: TecSyntAnalyzer): string;

implementation

{
This finds lexer by Extensions-property of lexer.
It is space-separated items.
Items are
- usual extension: "pas" finds "dir/filename.pas"
- double extension (higher priority): "some.html" finds "dir/myfile.some.html"
  (before lexer HTML finds it)
- filename: "/name.ext" finds "any/dir/name.ext"
}
function DoFindLexerForFilename(LexLib: TecSyntaxManager; const FileName: string): TecSyntAnalyzer;
var
  fname, ext1, ext2: string;
  i: integer;
  st: TzStringList;
begin
  Result:= nil;

  fname:= '/' + LowerCase(ExtractFileName(FileName));

  //final extension
  ext1:= LowerCase(ExtractFileExt(FileName));
  if SBeginsWith(ext1, '.') then Delete(ext1, 1, 1);

  //2nd+final extension
  ext2:= '';
  if ext1<>'' then
  begin
    ext2:= LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
    if SBeginsWith(ext2, '.') then Delete(ext2, 1, 1);
    if ext2<>'' then
      ext2:= ext2+'.'+ext1;
  end;

  st:= TzStringList.Create;
  try
    //space separated
    st.Delimiter:= ' ';

    //find by double extension
    if ext2<>'' then
      for i:= 0 to LexLib.AnalyzerCount-1 do
        with LexLib.Analyzers[i] do
          if not Internal then
          begin
            st.DelimitedText:= Extentions;
            if (st.IndexOf(ext2)>=0) then
              Exit(LexLib.Analyzers[i]);
          end;

    //find by usual extension + filename
    for i:= 0 to LexLib.AnalyzerCount-1 do
      with LexLib.Analyzers[i] do
        if not Internal then
        begin
          st.DelimitedText:= Extentions;
          if ((ext1<>'') and (st.IndexOf(ext1)>=0)) or
                            (st.IndexOf(fname)>=0) then
            Exit(LexLib.Analyzers[i]);
        end;
  finally
    st.Free;
  end;
end;

function DoGetLexerFileFilter(an: TecSyntAnalyzer; const AllFilesText: string): string;
var
  st: TzStringList;
  i: integer;
begin
  Result:= '';
  st:= TzStringList.Create;
  try
    st.Delimiter:= ' ';
    st.DelimitedText:= an.Extentions;
    if st.Count=0 then Exit;
    Result:= an.LexerName+' ('+an.Extentions+')|';
    for i:= 0 to st.Count-1 do
      Result:= Result+'*.'+st[i]+';';
    Result:= Result+'|';
  finally
    st.Free;
  end;

  if AllFilesText<>'' then
    Result:= Result+AllFilesText+'|'+AllFilesMask+'|';
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

