" Vim syntax file
" Language: Spark
" Maintainer: Ben L.

if exists("b:current_syntax") 
    finish
endif

syn keyword sprkControlFlow if while ret break switch

syn match sprkTypeName '\(\(i\|u\)\(8\|16\|32\|64\|size\)\)\|bool' display
syn match sprkCustomName '\w\(\(\w\|:\|_\|\d\)*\)' contained display
syn keyword sprkTypeStart struct union let type nextgroup=sprkTypeName,sprkCustomName skipwhite

syn keyword sprkStorageModifier const ext asm 
syn match sprkLineComment '//.*' contained display

syn keyword sprkDecl fun 
syn keyword sprkNameSpacing ns use 

syn region sprkString start='"' end='"' display
syn match sprkConstant '\(\d\+\)\|true\|false' display

" syn region sprkBlock start="{" end="}" fold transparent contained contains=sprkString,sprkConstant,sprkTypeName,sprkLineComment,sprkTypeStart,sprkControlFlow

let b:current_syntax="spark"
hi def link sprkLineComment Comment
hi def link sprkTypeStart Statement
hi def link sprkTypeName Type
hi def link sprkCustomName Type
hi def link sprkNameSpacing Statement
hi def link sprkConstant Constant
hi def link sprkStorageModifier PreProc
hi def link sprkDecl Statement
hi def link sprkControlFlow Statement 
hi def link sprkString Constant
