
This is base code of EControl Syntax Editor SDK http://econtrol.ru
For Lazarus (1.4+), with ATSynEdit

Code contains: 
- syntax parser (lexer engine), 
- syntax manager (linked list of lexers),
- helper lists, 
- regex engine (lexer needs it, cannot use Lazarus regex).
Code does not contain: 
- SyntaxMemo control, 
- popup lists, 
- more visual controls.

Code modified! can use it with ATSynEdit only. 
(ecmemostrings object deleted, replaced with ATStringbuffer object [same methods], less code).

LICENSE
EControl author [Michael Zakharov] gave permission to use this code (modified for ATSynEdit) only inside **open source** projects. It's NOT ALLOWED to use this code in closed source. For usage in closed source, you must buy license from EControl (for full code).
Copyright (c) 2004-2015, EControl
Ported by A. Torgashin, UVviewsoft.com
