## Key bindings
### Basic
Key binding | Description |    | Key binding | Description
----------- | ----------- | -- | ----------- | -----------
C-g     | abort binding || C-x C-s  | save file
C-/     | undo          || C-x C-f  | find / create file
C-z     | suspend Emacs || C-x s    | save some files
C-x C-c | exit Emacs    || C-r      | minibuffer previous
C-x b   | switch buffer || C-s      | minibuffer next
C-x k   | kill buffer   || C-x C- - | zoom out
C-x C-b | list buffers  || C-x C- + | zoom in
### Window
Key binding | Description |    | Key binding | Description
----------- | ----------- | -- | ----------- | -----------
q     | quit help     || C-x 1 | close all but current
C-x o | jump to next  || C-x 2 | split north-south
C-x 0 | close current || C-x 3 | split west-east
### Move
Key binding | Description |    | Key binding | Description
----------- | ----------- | -- | ----------- | -----------
C-f   | next char            || C-b   | previous char
M-f   | next word            || M-b   | previous word
C-n   | next line            || C-p   | previous line
C-a   | line start           || C-e   | line end
M-e   | next sentence        || M-a   | previous sentence
C-v   | next screen          || M-v   | previous screen
M-<   | buffer start         || M->   | buffer end
C-s   | search forward       || C-r   | search backward
C-M-s | search forward regex || C-M-r | search backward regex
M-g g | jump to line         || C-l   | center / top / bottom
C-M-v | scroll other window  || C-M-l | scroll heuristically
### Text
Key binding | Description |    | Key binding | Description
----------- | ----------- | -- | ----------- | -----------
C-space | set mark         || C-M-h     | select frm
M-w     | copy selected    || C-x h     | select all
C-y     | paste latest cut || M-y       | replace C-y with previous cut
C-d     | delete char      || C-j       | new line + indent
M-d     | cut word         || M-;       | (un)comment selected
C-k     | cut line         || M-/       | autocomplete
M-k     | cut sentence     || M-\       | clear spaces around point
C-w     | cut selected     || M-%       | replace
M-u     | uppercase word   || M-l       | lowercase word
C-x C-u | uppercase region || C-x C-l   | lowercase region
C-x r t | string rectangle || C-x space | rectangle region
### Paredit
Key binding | Description |    | Key binding | Description
----------- | ----------- | -- | ----------- | -----------
(      | open and close delimiter     || )     | go through closing delimiters
M-([{" | wrap with delimiters         || M-s   | remove delimiters
C-(    | move opening delimiter left  || C-)   | move closing delimiter right
C-{    | move opening delimiter right || C-}   | move closing delimiter left
M-down | remove next and unwrap       || M-up  | remove previous and unwrap
M-S    | split delimiters             || M-J   | join delimiters
C-M-f  | jump to closing delimiter    || C-M-b | jump to opening delimiter
- delimiters: `() [] {} ""`
### Company
Key binding | Description
----------- | -----------
M-n       | next choice
M-p       | previous choice
M-(digit) | nth choice
f1        | see docs for selected
C-w       | see source for selected
### Cider
Key binding | Description |    | Key binding | Description
----------- | ----------- | -- | ----------- | -----------
C-c C-k     | eval buffer                     || C-c M-j     | run and open
C-c M-z     | eval buffer, go to repl         || C-c C-c     | kill process
C-c C-c     | eval top-level frm, show result || C-u C-c C-o | clear buffer
C-x C-e     | eval, show result               || C-c M-n M-n | switch repl ns
C-u C-x C-e | eval, write result              || C-c M-s     | cider selector
C-c C-v w   | eval, replace with result       || C-c C-z     | go to repl or back
C-c M-e     | eval, show result in repl       || M-.         | go to symbol definition
C-c C-p     | eval, popup pprint result       || C-c C-.     | go to ns on classpath
C-c C-m     | popup macroexpand-1             || M-,         | return to previous location
C-c M-m     | popup macroexpand-all           || C-u C-c C-c | debug frm
C-c M-p     | copy frm to repl                || C-c C-t C-n | run ns tests
C-c C-d C-d | display doc string              || C-c C-t C-p | run project tests
## Installation (Windows)
1. [Download the latest version](http://ftp.gnu.org/gnu/emacs/windows/) (`i686` for 32-bit, `x86_64` for 64-bit) and extract it to `C:\emacs-version`
2. Computer -> Properties -> Advanced system settings -> Environment Variables  
   -> System variables -> Path -> Edit -> add `C:\emacs-version\bin`
3. Create folder `C:\home`
4. Computer -> Properties -> Advanced system settings -> Environment Variables  
   -> System variables -> New -> Variable name: `HOME` Variable value: `C:\home`
5. Clone or download this repository to `C:\home\.emacs.d`
6. `C:\emacs-version\bin\runemacs.exe` -> Send to -> Desktop (create shortcut)