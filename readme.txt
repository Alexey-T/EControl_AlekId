
This is base code of EControl Syntax Editor SDK http://econtrol.ru
For Lazarus (1.4+), with ATSynEdit

Code contains: 
- lexer parser 
- lexer manager (linked list of lexers)
- helper grammar parser
- helper lists
- regex engine (lexer needs it, cannot use Lazarus regex)
Code does not contain: 
- SyntaxMemo control 
- ecMemoStrings class
- popup listboxes classes 
- other visual controls

Code is modified, to work with ATSynEdit
(ecMemoStrings class deleted, replaced with ATStringBuffer class [same methods, much less code]).

LICENSE
EControl author [Michael Zakharov] gave permission to use this code (modified for ATSynEdit) only inside **open source** projects. It's NOT ALLOWED to use this code in closed source. For usage in closed source, you must buy license from EControl (for full code).
Copyright (c) 2004-2015, EControl
Copyright for added parts (c) 2015 Alexey Torgashin, UVviewsoft.com
