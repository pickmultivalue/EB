# Edit Basic

The EB utility is primarily designed for editing, compiling jBC code. It can be used to edit other languages (e.g. C/C++ and supports ctags for zooming into #include files).

EB was originally written outside jBASE and so the command interface doesn't (currently) use JBASECommandNext for it's commands.

The legend to EB's commands is in EB.PARAMS*<termtype>. 

e.g. if you're term type is vt220 then it will read that file for it's keystroke mapping.

The include EB.EQUS/EB.CHARS holds the mapping of EB.PARAMS.

## Compilation

EB uses BASIC and CATALOG for compiling jBC code. You can however, customise the CATALOG options for
executables/subroutines:

Create an entry in EB.PARAMS called *filename*_lib and enter the options to use for subroutines
on line 1 and executables on line 2.

e.g.

<1> -L/home/jbaseadm/lib
<2> -o/home/jbaseadm/bin

You can also create an entry for a specific source item: EB.PARAMS *filename*_*sourcename*_lib

When EB is operating on a selection of jBC code the CATALOG is performed when the last item is processed.

## Features

 - History of EB edits (e.g. simply entering "EB" will popup a list)
 - Zoom into an INCLUDE/CALL/GOSUB/function
 - Mouse click (reposition cursor on screen)
 - Search/replace for (W)hole words (i.e. variables)
 - History of search/replaces
 - Wild card search/replace (using @1, @2, ... as place markers)
 - Special @x{n} replacement used for auto sequencing an array or other list
 - jsh shell
 - copy/cut/paste from starting to ending position (i.e. not just whole lines)
 - paste items can be named
 - Move forward/backward through item list
 - Auto indent/reformat
 - sWap command to flip code (e.g. PROGRAM->SUBROUTINE, READ -> WRITE, X = Y -> Y = X)
 - edit (V)alue for editing a multi-value attribute like a record
 - (R)otate attributes/multi-values (useful for associated multi-values)
 - Edit list item (EBL)


## To Do

Integrate "git" (currently source control assumes svn).
