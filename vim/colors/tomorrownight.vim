" Tomorrow Night Modified

let g:colors_name = "tomorrow-night"

let s:bright_red = "196"
let s:red = "167"
let s:orange = "130"
let s:yellow = "179"
let s:green = "107"
let s:cyan = "66"
let s:blue = "33"
let s:light_blue = "75"
let s:purple = "96"
let s:magenta = "127"

let s:window = "238"
let s:black = "16"
let s:bright_white = "255"
let s:white = "252"
let s:gray0 = "234"
let s:gray1 = "238"
let s:gray2 = "240"
let s:gray3 = "244"
let s:line = "235"

hi clear
syntax reset

" Sets the highlighting for the given group
fun <SID>X(group, fg, bg, attr)
  if a:fg != ""
    exec "hi " . a:group . " ctermfg=" . a:fg
  endif
  if a:bg != ""
    exec "hi " . a:group . " ctermbg=" . a:bg
  endif
  if a:attr != ""
    exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
  endif
endfun

" Vim Highlighting
call <SID>X("Normal", "", "", "")
call <SID>X("LineNr", s:gray1, s:gray0, "")
call <SID>X("NonText", s:gray1, "", "")
call <SID>X("SpecialKey", s:gray1, "", "")
call <SID>X("Search", "", s:yellow, "")
call <SID>X("TabLine", s:window, "", "reverse")
call <SID>X("TabLineFill", s:window, "", "reverse")
call <SID>X("StatusLine", s:window, s:yellow, "reverse")
call <SID>X("StatusLineNC", s:window, "", "reverse")
call <SID>X("VertSplit", s:window, s:window, "none")
call <SID>X("Visual", "", s:gray1, "")
call <SID>X("Directory", s:cyan, "", "")
call <SID>X("ModeMsg", s:green, "", "")
call <SID>X("MoreMsg", s:green, "", "")
call <SID>X("Question", s:green, "", "")
call <SID>X("WarningMsg", s:red, "", "")
call <SID>X("MatchParen", "", s:gray1, "")
call <SID>X("Folded", s:black, "", "")
call <SID>X("FoldColumn", "", "", "")
if version >= 700
  call <SID>X("CursorLine", "", s:line, "none")
  call <SID>X("CursorColumn", "", s:line, "none")
  call <SID>X("PMenu", "", s:gray1, "none")
  call <SID>X("PMenuSel", "", s:gray1, "reverse")
  call <SID>X("SignColumn", "", "", "none")
end
if version >= 703
  call <SID>X("ColorColumn", "", s:line, "none")
end

" Standard Highlighting
call <SID>X("Todo", s:red, "", "")
call <SID>X("Title", s:magenta, "", "")
call <SID>X("Identifier", s:red, "", "none")
call <SID>X("Statement", s:red, "", "")
call <SID>X("Conditional", "", "", "")
call <SID>X("Repeat", "", "", "")
call <SID>X("Structure", s:purple, "", "")
call <SID>X("Function", s:red, "", "")
call <SID>X("Constant", s:yellow, "", "")
call <SID>X("Number", s:blue, "", "")
call <SID>X("String", s:cyan, "", "")
call <SID>X("Special", s:gray3, "", "")
call <SID>X("Comment", s:gray2, "", "")
call <SID>X("PreProc", s:orange, "", "")
call <SID>X("Operator", s:white, "", "none")
call <SID>X("Type", s:cyan, "", "none")
call <SID>X("Define", s:gray1, "", "none")
call <SID>X("Include", s:magenta, "", "")

" Coffeescript
call <SID>X("coffeeSpecialIdent", s:blue, "", "")

" Vim Highlighting
call <SID>X("vimCommand", s:red, "", "none")

" Haskell Highlighting
call <SID>X("ConId", s:red, "", "")
call <SID>X("hsNumber", s:blue, "", "")
call <SID>X("hsStatement", s:purple, "", "")
call <SID>X("hsStructure", s:gray3, "", "")
call <SID>X("hsConditional", s:orange, "", "")
call <SID>X("hsTypedef", s:orange, "", "")
call <SID>X("hsModule", s:magenta, "", "")
call <SID>X("hsDelimiter", s:yellow, "", "")
call <SID>X("hsVarSym", s:gray3, "", "")
call <SID>X("hsConSym", s:gray3, "", "")
call <SID>X("hsSpecialChar", s:magenta, "", "bold")
call <SID>X("hsCharacter", s:blue, "", "")

" Ruby Highlighting
call <SID>X("rubySymbol", s:light_blue, "", "")
call <SID>X("rubyConstant", s:yellow, "", "")
call <SID>X("rubyAccess", s:orange, "", "")
call <SID>X("rubyAttribute", s:cyan, "", "")
call <SID>X("rubyInclude", s:orange, "", "")
call <SID>X("rubyLocalVariableOrMethod", s:orange, "", "")
call <SID>X("rubyStringDelimiter", s:cyan, "", "")
call <SID>X("rubyInterpolationDelimiter", s:orange, "", "")
call <SID>X("rubyConditional", s:purple, "", "")
call <SID>X("rubyRepeat", s:purple, "", "")
call <SID>X("rubyControl", s:purple, "", "")
call <SID>X("rubyException", s:purple, "", "")
call <SID>X("rubyPseudoVariable", s:green, "", "")
call <SID>X("rubyBoolean", s:green, "", "")
call <SID>X("rubyInstanceVariable", s:light_blue, "", "")
call <SID>X("rubyBlockParameter", s:green, "", "")

" Clojure Highlighting
call <SID>X("clojureKeyword", s:yellow, "", "")
call <SID>X("clojureParen", s:orange, "", "")
call <SID>X("clojureSpecial", s:red, "", "")
call <SID>X("clojureBoolean", s:green, "", "")
call <SID>X("clojureConstant", s:green, "", "")
call <SID>X("clojureQuote", s:bright_white, "", "")
call <SID>X("clojureUnquote", s:bright_white, "", "")
call <SID>X("clojureMacro", s:red, "", "")
call <SID>X("clojureCharacter", s:green, "", "")

" JavaScript Highlighting
call <SID>X("javaScriptBraces", "", "", "")
call <SID>X("javaScriptFunction", s:purple, "", "")
call <SID>X("javaScriptConditional", s:purple, "", "")
call <SID>X("javaScriptRepeat", s:purple, "", "")
call <SID>X("javaScriptNumber", s:orange, "", "")
call <SID>X("javaScriptMember", s:orange, "", "")

" HTML Highlighting
call <SID>X("htmlTag", s:gray2, "", "")
call <SID>X("htmlEndTag", s:gray2, "", "")
call <SID>X("htmlTagName", s:gray2, "", "")
call <SID>X("htmlArg", s:gray2, "", "")
call <SID>X("htmlScriptTag", s:gray2, "", "")

" ERB Highlighting
call <SID>X("erubyDelimiter", s:magenta, "", "")

" Markdown Highlighting
call <SID>X("markdownHeadingDelimiter", "235", "", "")
call <SID>X("markdownH1", s:green, "", "")
call <SID>X("markdownH2", s:yellow, "", "")
call <SID>X("markdownH3", s:red, "", "")
call <SID>X("markdownItalic", s:blue, "", "")

" Python Highlighting
call <SID>X("pythonInclude", s:purple, "", "")
call <SID>X("pythonStatement", s:purple, "", "")
call <SID>X("pythonConditional", s:purple, "", "")
call <SID>X("pythonRepeat", s:purple, "", "")
call <SID>X("pythonException", s:purple, "", "")
call <SID>X("pythonFunction", s:cyan, "", "")
call <SID>X("pythonPreCondit", s:purple, "", "")
call <SID>X("pythonRepeat", s:cyan, "", "")
call <SID>X("pythonExClass", s:orange, "", "")

" C Highlighting
call <SID>X("cType", s:yellow, "", "")
call <SID>X("cStorageClass", s:purple, "", "")
call <SID>X("cConditional", s:purple, "", "")
call <SID>X("cRepeat", s:purple, "", "")

" Diff Highlighting
call <SID>X("diffAdded", s:green, "", "")
call <SID>X("diffRemoved", s:red, "", "")

" Go Highlighting
call <SID>X("goDirective", s:purple, "", "")
call <SID>X("goDeclaration", s:purple, "", "")
call <SID>X("goStatement", s:purple, "", "")
call <SID>X("goConditional", s:purple, "", "")
call <SID>X("goConstants", s:orange, "", "")
call <SID>X("goTodo", s:yellow, "", "")
call <SID>X("goDeclType", s:cyan, "", "")
call <SID>X("goBuiltins", s:purple, "", "")

" Nerd tree
call <SID>X("NERDTreeDirSlash", s:red, "", "")

" Delete Functions
delf <SID>X

set background=dark
