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

function DoFindLexerForFilename(LexLib: TecSyntaxManager; FileName: string): TecSyntAnalyzer;
function DoGetLexerFileFilter(an: TecSyntAnalyzer; const AllFilesText: string): string;
function DoGetLexerDefaultExt(an: TecSyntAnalyzer): string;

implementation

function SItemListed(const AItem, AList: string): boolean;
const
  sep=' ';
begin
  if (AItem='') or (AList='') then
    Result:= false
  else
    Result:= Pos(sep+AItem+sep, sep+AList+sep)>0;
end;

{
This finds lexer by Extensions-property of lexer.
It is space-separated items. In lower case.
Items are
- usual extension: "pas" finds "dir/filename.pas"
- double extension (higher priority): "some.html" finds "dir/myfile.some.html"
  (before lexer HTML finds it)
- filename: "/name.ext" finds "any/dir/name.ext"
}
function DoFindLexerForFilename(LexLib: TecSyntaxManager; FileName: string): TecSyntAnalyzer;
var
  fname, ext1, ext2: string;
  i: integer;
begin
  Result:= nil;

  //strip path, lower case
  FileName:= LowerCase(ExtractFileName(FileName));
  fname:= '/'+FileName;

  //usual extension
  ext1:= ExtractFileExt(FileName);
  if SBeginsWith(ext1, '.') then Delete(ext1, 1, 1);

  //double extension
  ext2:= '';
  if ext1<>'' then
  begin
    ext2:= ExtractFileExt(ChangeFileExt(FileName, ''));
    if SBeginsWith(ext2, '.') then Delete(ext2, 1, 1);
    if ext2<>'' then
      ext2:= ext2+'.'+ext1;
  end;

  //find by double extension
  if ext2<>'' then
    for i:= 0 to LexLib.AnalyzerCount-1 do
      with LexLib.Analyzers[i] do
        if not Internal then
          if SItemListed(ext2, Extentions) then
            Exit(LexLib.Analyzers[i]);

  //find by extension/filename
  for i:= 0 to LexLib.AnalyzerCount-1 do
    with LexLib.Analyzers[i] do
      if not Internal then
        if SItemListed(ext1, Extentions) or
           SItemListed(fname, Extentions) then
          Exit(LexLib.Analyzers[i]);
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

