
;--- set the APIC timer to a periodic interval.
;--- this program runs as DPMI client.

	.286
	.model tiny
	.dosseg
	.stack 2048
	option casemap:none
	.386

TIMERINT equ 80h
NUMCHARS equ 50

lf	equ 10

CStr macro text:vararg
local sym
	.const
sym  db text,0
	.code
	exitm <offset sym>
endm

	include apic.inc

	.data

abar dd 0		; MSR 1Bh; local APIC base in bits 12-63
oldvec dd 0		; saved real-mode int vector for APIC timer irq
wFlat dw 0		; flat zero-based descriptor
wCSSeg dw 0		; real-mode CS
wScrPos dw 0	; screen start position
bCnt db 0		; irq counter
bIrq db 0		; 1 if APIC timer interrupt has occured

	.code

;--- APIC timer interrupt proc ( real-mode )

timerint proc
	pusha
	push ds
	mov bx, cs:[wScrPos]
	movzx ax, cs:[bCnt]
	mov cl, NUMCHARS
	div cl
	movzx ax, ah
	add bx, ax
	add bx, ax
	inc ax
	mov cs:[bCnt], al
	push 0B800h
	pop ds
	xor byte ptr [bx+1], 1000b	; just switch char attribute
	mov cs:[bIrq], 1
	pop ds
	popa
	iret
timerint endp

	include printf.inc

;--- init screen variable wScrPos
;--- and write "pattern" chars
;--- ES=flat

PrepareScreen proc
	movzx ax,byte ptr es:[450h+1] ; cur line
	mov cx, word ptr es:[44Ah]	; cols/line
	shl cx,1
	mul cx
	movzx cx, byte ptr es:[450h+0]	; cur col
	shl cx, 1
	add ax, cx
	mov wScrPos, ax
	movzx ebx, ax
	add ebx, 0b8000h
	mov cx, NUMCHARS
@@:
	mov byte ptr es:[ebx], 0B0h
	add ebx, 2
	loop @B
	ret
PrepareScreen endp

;--- set the APIC timer if it's unused (=still masked);
;--- then wait until ESC is pressed.

SetAPICTimer proc stdcall uses si di dwAPIC:dword

;--- map APIC mmio to linear address space

	mov cx, word ptr dwAPIC+0
	mov bx, word ptr dwAPIC+2
	mov si, 0
	mov di, sizeof APICREGS
	mov ax,800h
	int 31h
	jc failed
	push bx
	push cx
	pop edi
	mov es, wFlat

;--- es:edi -> APIC

	mov eax, es:[edi].APICREGS.SIVR
	.if !( ah & 1)
		invoke printf, CStr("APIC disabled",lf)
		jmp exit
	.endif

	mov eax, es:[edi].APICREGS.TiLVTE
	bt eax, 16
	.if !CARRY?
		invoke printf, CStr("APIC timer already in use",lf)
		jmp exit
	.endif
	invoke printf, CStr("Timer LVTE=%lX",lf), eax

;--- set timer interrupt proc ( real-mode! )
	mov bl, TIMERINT
	mov ax, 200h
	int 31h
	mov word ptr oldvec+0, dx
	mov word ptr oldvec+2, cx
	mov cx, wCSSeg
	mov dx, offset timerint
	mov ax, 201h
	int 31h

	invoke printf, CStr("press ESC to exit...",lf)

	call PrepareScreen

;--- set timer divisor to 32

	mov eax, es:[edi].APICREGS.TDCR
	and al, 0f4h		; clear bits 0-1 and 3
	or al, 8			; set divisor to 100b (=32)   
	mov es:[edi].APICREGS.TDCR, eax

;--- set timer initial count to 62500
;--- count * divisor = 62500 * 32 = 2.000.000
;--- assuming "bus frequency" of 100 MHz the result is
;--- 100.000.000 / 2.000.000 = 50 Hz

	mov eax, 62500
	mov es:[edi].APICREGS.TICR, eax

;--- set timer mode, unmask timer
	mov eax, es:[edi].APICREGS.TiLVTE
	mov al, TIMERINT	; load interrupt vector for fixed destination
	and ah, 0F8h		; set destination mode (bits 8-10) to 0
	bts eax, 17			; set timer mode to periodic
	btr eax, 16			; unmask timer
	mov es:[edi].APICREGS.TiLVTE, eax

;--- wait for timer interrupts to appear.
;--- since the interrupt is handled in real-mode, none will get lost.
@@:
	.if bIrq
		mov bIrq, 0
		xor eax, eax
		mov es:[edi].APICREGS.EOI, eax
	.endif
	mov ah,1
	int 16h
	jz @B
	mov ah,0
	int 16h
	cmp al,1Bh
	jnz @B

	invoke printf, CStr(lf)

;--- mask timer
	mov eax, es:[edi].APICREGS.TiLVTE
	bts eax, 16
	mov es:[edi].APICREGS.TiLVTE, eax

;--- restore real-mode int
	mov dx, word ptr oldvec+0
	mov cx, word ptr oldvec+2
	mov bl, TIMERINT
	mov ax, 201h
	int 31h

;--- send EOI if another irq has occured
	.if bIrq
		mov bIrq, 0
		xor eax, eax
		mov es:[edi].APICREGS.EOI, eax
	.endif

exit:
	push edi
	pop cx
	pop bx
	mov ax,801h	; unmap APIC mmio
	int 31h
	ret
failed:
	invoke printf, CStr("cannot map APIC %lX into linear address space",lf), dwAPIC
	stc
	ret

SetAPICTimer endp

;--- enter protected-mode and alloc a flat 4GB descriptor.

GoProtected proc stdcall uses si di

local dwHost:far16

	mov ax, 1687h
	int 2Fh
	cmp ax, 0
	jnz failed1
	mov word ptr dwHost+0, di
	mov word ptr dwHost+2, es
	and si, si
	jz @F
	mov bx, si
	mov ah, 48h
	int 21h
	jc failed2
	mov es, ax
@@:
	mov ax, 1
	call [dwHost]
	jc failed3

	movzx ebp, bp		; ensure hiword EBP is clear to avoid exc 0C for LEAVE

;--- alloc a flat descriptor
	mov cx, 1
	xor ax, ax
	int 31h
	jc failed4
	mov bx, ax
	mov wFlat, ax
	or cx, -1
	or dx, -1
	mov ax, 8	; set limit
	int 31h
	ret
failed1:
	mov dx, CStr("APICTmr requires an installed 32-bit DPMI host")
errexit:
	invoke printf, CStr("%s",lf), dx
	stc
	ret
failed2:
	mov dx, CStr("Out of memory")
	jmp errexit
failed3:
	mov dx, CStr("Switch to protected-mode failed")
	jmp errexit
failed4:
	mov dx, CStr("Allocating descriptor failed")
	jmp errexit

GoProtected endp

;--- check if CPUID is supported

checkcpuid proc stdcall
	pushfd
	push 200000h		;push ID flag
	popfd
	pushfd
	pop  eax
	popfd
	test eax,200000h	;is it set now?
	setnz al
	sub al, 1
	ret
checkcpuid endp

;--- check if local APIC exists and, if enabled, try to setup APIC timer

main proc

	call checkcpuid
	.if CARRY?
		invoke printf, CStr("CPUID instruction not supported",lf)
		ret
	.endif
	mov eax, 1
	.586
	cpuid
	.386
	bt edx, 9			; local APIC exists?
	.if !CARRY?
		invoke printf, CStr("no onchip APIC found",lf)
		ret
	.endif

;--- RDMSR is a priviledged instruction and may fail if running in v86 mode

	mov ecx, 1Bh		; read local APIC base (IA32_APIC_BASE MSR)
	.586p
	rdmsr
	.386
	mov abar, eax

	invoke printf, CStr("APIC base address register (MSR 1Bh): %lX%08lX",lf), edx, eax

	mov wCSSeg, cs		; real-mode CS is needed for IVT hooking

	call GoProtected	; enter protected-mode   
	jc exit

	bt abar, 11
	.if CARRY?
		invoke printf, CStr("APIC enabled",lf)
		mov eax, abar
		and ax, 0f000h
		invoke SetAPICTimer, eax
	.else
		invoke printf, CStr("APIC disabled",lf)
	.endif

	push ds
	pop es
	mov bx, wFlat
	mov ax, 1
	int 31h
exit:
	ret

main endp

;--- startup code. Init a tiny model.

start:
	push cs
	pop ds
	mov ax,ss
	mov dx,cs
	sub ax,dx
	shl ax,4
	push cs
	pop ss
	add sp,ax
	mov bx, sp
	shr bx, 4
	add bx, 10h
	mov ah, 4Ah	; free unused memory
	int 21h
	call main
	mov ax,4c00h
	int 21h

	end start

