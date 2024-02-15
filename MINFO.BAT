@echo off
if not exist Release\NUL mkdir Release
jwasm -mz -nologo -Fl=Release\ -Fo=Release\ -Sg APICinfo.asm
