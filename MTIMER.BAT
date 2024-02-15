@echo off
if not exist Release\NUL mkdir Release
jwasm -mz -nologo -Fl=Release\ -Fo=Release\ -Sg APICTmr.asm
rem ml -c -nologo -FlRelease\APICTmr.lst -FoRelease\APICTmr.obj -Sg APICTmr.asm
