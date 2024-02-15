
;--- display local APIC, MADT and IOAPI info.
;--- this program runs as DPMI client.

	.286
	.model tiny
	.dosseg
	.stack 2048
	option casemap:none
	.386

lf	equ 10

CStr macro text:vararg
local sym
	.const
sym  db text,0
	.code
	exitm <offset sym>
endm

	include acpi.inc
	include apic.inc

	.data

;--- abar: if src was MSR 1Bh, it's the physical address of the lAPIC
;--- in bits 12-31. If src was MADT, it's just the physical address.
abar dd 0
dwMADT dd 0		; physical address MADT
wCS dw 0		; CS segment value
wFlat dw 0		; flat descriptor

Sig db "    ",0
bCntIOAPIC db 0	; # of IOAPICs found

	.data?

apic	APICREGS <>
sdthdr	ACPISDTHeader <>
madt	MADT <>
madtent	MADTTypeX <>
qwPtr dq ?
dwIOAPIC dd 8 dup (?)	; max 8 IOAPICs

	.code

;--- read MSR - real-mode proc

readmsr proc far
	mov ecx, 1Bh		; read local APIC base (IA32_APIC_BASE MSR)
	.586p
	rdmsr
	.386
	ret
readmsr endp

	include printf.inc

;--- copy contents at physical address dwPhysAddr to pDst

readextmem proc stdcall uses esi edi pDst:ptr, dwPhysAddr:dword, wSize:word

	mov cx, word ptr dwPhysAddr+0
	mov bx, word ptr dwPhysAddr+2
	mov si,0
	mov di, wSize
	mov ax,800h
	int 31h
	jc failed

	push ds
	pop es
	mov ds, [wFlat]
	push bx
	push cx
	pop esi
	movzx ecx, wSize
	movzx edi, pDst
	rep movsb [esi], [edi]
	push es
	pop ds

	push esi
	pop cx
	pop bx
	mov ax, 801h
	int 31h
	jc failed2
	ret
failed:
	invoke printf, CStr("cannot map physical address %lX",lf), bx::cx
	stc
	ret
failed2:
	invoke printf, CStr("cannot unmap physical address %lX",lf), dwPhysAddr
	stc
	ret
readextmem endp

;--- print a vector table entry

PrintVTE proc stdcall pName:ptr, dwValue:dword
	invoke printf, CStr("%s=%lX "), pName, dwValue
	bt dwValue, 16
	.if CARRY?
		invoke printf, CStr("(masked) ")
	.else
		mov eax, dwValue
		and ah,111b
		.if ah==0
			invoke printf, CStr("fixed ")
		.elseif ah == 2
			invoke printf, CStr("SMI ")
		.elseif ah == 4
			invoke printf, CStr("NMI ")
		.elseif ah == 7
			invoke printf, CStr("extINT ")
		.endif
	.endif
	invoke printf, CStr(lf)
	ret
PrintVTE endp

;--- read and print local APIC fields 

PrintAPIC proc stdcall dwAPIC:dword

	invoke readextmem, addr apic, dwAPIC, sizeof APICREGS
	jc exit
	invoke printf, CStr("APICID=%lX",lf), apic.APICID
	invoke printf, CStr("APICVersion=%lX",lf), apic.APICVers
	invoke printf, CStr("Task Priority Register=%lX",lf), apic.TPR
	invoke printf, CStr("Destination Format Register=%lX",lf), apic.DFR
	mov eax, apic.SIVR
	.if eax & 100h
		mov ecx, CStr("enabled")
	.else
		mov ecx, CStr("disabled")
	.endif
	invoke printf, CStr("Spurious Interrupt Vector Register=%lX (APIC %s)",lf), apic.SIVR, ecx

	invoke printf, CStr("Interrupt Command Register Low=%lX",lf), apic.ICRLo
	invoke printf, CStr("Interrupt Command Register High=%lX",lf), apic.ICRHi

	invoke PrintVTE, CStr("Timer Local Vector Table Entry"), apic.TiLVTE
	invoke PrintVTE, CStr("Thermal Local Vector Table Entry"), apic.ThLVTE
	invoke PrintVTE, CStr("Performance Counter Local Vector Table Entry"), apic.PCLVTE
	invoke PrintVTE, CStr("Local Interrupt 0 Vector Table Entry"), apic.LI0VTE
	invoke PrintVTE, CStr("Local Interrupt 1 Vector Table Entry"), apic.LI1VTE
	invoke PrintVTE, CStr("Error Vector Table Entry"), apic.EVTE

	invoke printf, CStr("Timer Initial Count Register=%lX",lf), apic.TICR
	invoke printf, CStr("Timer Current Count Register=%lX",lf), apic.TCCR
	invoke printf, CStr("Timer Divide Config Register=%lX",lf), apic.TDCR
	invoke printf, CStr("Extended APIC Feature Register=%lX",lf), apic.EAFR
	invoke printf, CStr("Extended APIC Control Register=%lX",lf), apic.EACR
exit:
	ret

PrintAPIC endp

;--- display entries in MADT

PrintMADT proc stdcall uses edi esi dwAddr:dword

	mov edi, dwAddr
	invoke readextmem, addr madt, edi, sizeof madt	; read the MADT header
	jc failed
	invoke printf, CStr(lf,"MADT (length=%lu, LAPIC=%lX, flags=%lX):",lf), madt.dwLength, madt.LocalAPICAddr, madt.dwFlags
	.if madt.dwFlags & 1
		invoke printf, CStr("legacy PICs installed",lf)
	.endif
	mov esi, madt.dwLength
	add esi, edi
	add edi, sizeof madt
	.while edi < esi
		invoke readextmem, addr madtent, edi, sizeof madtent	; read MADT entry
		jc failed
		invoke printf, CStr("Type %u: "), madtent.bType
		mov al, madtent.bType
		.if al == 0
			invoke printf, CStr("Processor LAPIC, ACPI Processor ID=%X APIC ID=%X flags=%lX"), madtent.t0.bACPIProcessorID, madtent.t0.bAPICID, madtent.t0.dwFlags
		.elseif al == 1
			invoke printf, CStr("IOAPIC, ID=%u address=%lX GblSysIntBase=%lX"), madtent.t1.bIOAPICID, madtent.t1.dwIOAPICAddr, madtent.t1.dwGblSysIntBase
			.if bCntIOAPIC < 8	; save addresses of IOAPICs found
				movzx bx, bCntIOAPIC
				shl bx, 2
				add bx, offset dwIOAPIC
				mov eax, madtent.t1.dwIOAPICAddr
				mov [bx], eax
				inc bCntIOAPIC
			.endif
		.elseif al == 2
			invoke printf, CStr("IOAPIC Interrupt Source Override, Bus Src=%X Irq Src=%X GblSysInt=%lX flags=%X"), madtent.t2.bBusSrc, madtent.t2.bIrqSrc, madtent.t2.dwGblSysInt, madtent.t2.wFlags
		.elseif al == 3
			invoke printf, CStr("IOAPIC NMI, Src=%X flags=%X GblSysInt=%lX"), madtent.t3.bNMISrc, madtent.t3.wFlags, madtent.t3.dwGblSysInt
		.elseif al == 4
			invoke printf, CStr("LAPIC NMI, ACPI Processor ID=%X flags=%X LINT=%u"), madtent.t4.bACPIProcessorID, madtent.t4.wFlags, madtent.t4.bLINT
		.elseif al == 5
			invoke printf, CStr("LAPIC Address Override, addr=%lX%08lX"), dword ptr madtent.t5.qwPhysAddr+4, dword ptr madtent.t5.qwPhysAddr+0
		.elseif al == 9
			invoke printf, CStr("Processor Local x2APIC, ID=%lX flags=%lX ACPIID=%lX"), madtent.t9.dwX2APICID, madtent.t9.dwFlags, madtent.t9.dwACPIID
		.endif
		invoke printf, CStr(lf)
		movzx eax, madtent.bLength
		add edi, eax
	.endw
failed:
	ret

PrintMADT endp

;--- scan ACPI tables to find an MADT ( table signature "APIC" )

SearchMADT proc uses esi di

local dwXSDT:dword	;RSDT/XSDT physical address

	invoke printf, CStr(lf,"Scanning SDTs...",lf)

;--- first scan F000:0000

	mov es, wFlat
	mov edi, 0F0000h
	mov cx, 10000h shr 4
nextblock:
	mov eax, " DSR"
nextline:
	cmp eax, es:[edi]
	jnz contline
	cmp dword ptr es:[edi+4]," RTP"
	jz found
contline:
	add edi, 10h
	loop nextline
	cmp edi, 10000h
	jnz @F
	mov ax, word ptr es:[40eh]	; after BIOS ROM, scan XBDA
	and ax, ax					; if it exists ...
	jz @F
	movzx edi, ax
	shl edi, 4
	mov cx, 400h shr 4
	jmp nextblock
@@:
	invoke printf, CStr("No RSDP/XSDP found",lf)
	stc
	ret
found:

;--- todo: test checksum

	movzx ax, es:[edi].RSDP.bRev
	cmp al, 0
	jz isv1
	cmp al, 2
	jz isv2
	invoke printf, CStr("ACPI version %u unknown",lf), ax
	stc
	ret
isv1:
	mov eax, es:[edi].RSDP.dwRSDT
	jmp commoncode
isv2:
	cmp dword ptr es:[edi].XSDP.qwXSDT+4,0
	jz @F
	invoke printf, CStr("XSDT beyond 4GB",lf)
	stc
	ret
@@:
	mov eax, dword ptr es:[edi].XSDP.qwXSDT
commoncode:
	mov dwXSDT, eax
	invoke readextmem, addr sdthdr, eax, sizeof sdthdr
	jc failed
	mov eax, dword ptr sdthdr.sig
	mov dword ptr Sig, eax
	invoke printf, CStr("RSDT/XSDT header at %lX",lf), dwXSDT
	invoke printf, CStr("Table Signature: %s",lf), addr Sig
	invoke printf, CStr("Table length: %lX",lf), sdthdr.dwLength
	mov eax, sdthdr.dwLength
	sub eax, sizeof ACPISDTHeader
	shr eax, 3		; each pointer is 8 bytes
	mov esi, eax
	invoke printf, CStr("Entries: %lu",lf), eax
	mov edi, dwXSDT
	add edi, sizeof ACPISDTHeader
	.while esi
		invoke readextmem, addr qwPtr, edi, sizeof QWORD
		jc failed
;		invoke printf, CStr("SDT at %lX, "), dword ptr qwPtr
		invoke readextmem, addr sdthdr, dword ptr qwPtr+0, sizeof sdthdr
		jc failed
		mov eax, dword ptr sdthdr.sig
		mov dword ptr Sig, eax
		.if eax == "CIPA"	; MADT found?
			invoke printf, CStr("MADT at %lX, "), dword ptr qwPtr
			invoke printf, CStr("sig=%s, length=%lu",lf), addr Sig, sdthdr.dwLength
			mov eax, dword ptr qwPtr+0
			mov dwMADT, eax
		.endif
;		invoke printf, CStr("sig=%s, length=%lu",lf), addr Sig, sdthdr.dwLength
		add edi, sizeof QWORD
		dec esi
	.endw
failed:
	ret

SearchMADT endp

;--- print registers of IOAPICs found in MADT.
;--- these registers must be accessed as DWORDs;

PrintIOAPIC proc uses si di

local bMaxEntry:byte
local dwBase:dword

	invoke printf, CStr(lf)
	mov si, offset dwIOAPIC
	.while bCntIOAPIC
		mov cx, [si+0]
		mov bx, [si+2]
		push si
		mov di, 1000h
		xor si, si
		mov ax, 800h
		int 31h
		pop si
		jc failed
		mov word ptr dwBase+0, cx
		mov word ptr dwBase+2, bx

		mov edi, [si]
		mov es, wFlat
		mov ebx, dwBase
		mov eax, 0
		mov es:[ebx], eax
		mov eax, es:[ebx+10h]
		invoke printf, CStr("IOAPIC %lX",lf," 0 IOAPICID=%lX",lf), edi, eax
		mov eax, 1
		mov es:[ebx], eax
		mov eax, es:[ebx+10h]
		mov edx, eax
		shr edx, 16
		mov bMaxEntry, dl
		invoke printf, CStr(" 1 IOAPICVER=%lX",lf), eax
		mov eax, 2
		mov es:[ebx], eax
		mov eax, es:[ebx+10h]
		invoke printf, CStr(" 2 IOAPICARB=%lX",lf), eax

;--- display IOAPIC table entries
;--- 0-7: vector
;--- 8-10: delivery mode
;--- 11: destination
;--- 12: delivery status
;--- 13: pin polarity
;--- 14: remote IRR
;--- 15: trigger mode
;--- 16: mask

		mov cl, 0
		.while cl <= bMaxEntry
			push cx
			movzx ecx, cl
			shl cx, 1
			add ecx, 10h
			mov es:[ebx], ecx
			mov edx, es:[ebx+10h]
			inc cx
			mov es:[ebx], ecx
			mov eax, es:[ebx+10h]
			dec cx
			invoke printf, CStr("%2u IOREDTBL=%lX.%lX",lf), cx, eax, edx
			pop cx
			inc cl
		.endw

;--- free mapping; linear address in BX:CX

		mov cx, word ptr dwBase+0
		mov bx, word ptr dwBase+2
		mov ax, 801h
		int 31h
		add si, sizeof DWORD
		dec bCntIOAPIC
		invoke printf, CStr(lf)
	.endw
	ret
failed:
	invoke printf, CStr("cannot map physical address %lX",lf), bx::cx
	ret

PrintIOAPIC endp

;--- enter protected-mode and alloc a flat 4GB descriptor.

GoProtected proc stdcall

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
	jc failed3a
	mov bx, ax
	mov wFlat, bx
	or cx, -1
	or dx, -1
	mov ax, 8	; set limit
	int 31h
	ret
failed1:
	invoke printf, CStr("APICinfo requires an installed 32-bit DPMI host",lf)
	stc
	ret
failed2:
	invoke printf, CStr("Out of memory",lf)
	stc
	ret
failed3:
	invoke printf, CStr("Switch to protected-mode failed",lf)
	stc
	ret
failed3a:
	invoke printf, CStr("Allocating descriptor failed",lf)
	stc
	ret

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

;--- call real-mode to execute RDMSR

ReadMSRHlp proc stdcall uses es edi msr:dword

local dpmiregs[25]:word

	mov eax, msr
	mov dword ptr dpmiregs+24, eax	; ECX
	mov dpmiregs+42, offset readmsr
	mov ax, wCS
	mov dpmiregs+44, ax
	xor eax, eax
	mov dword ptr dpmiregs+46, eax	; SS:SP
	mov dpmiregs+32, ax				; Flags
	push ds
	pop es
	lea edi, dpmiregs
	xor cx, cx
	xor bx, bx
	mov ax, 301h
	int 31h
	jc @F
	mov eax, dword ptr dpmiregs+28	; EAX
@@:
	ret
ReadMSRHlp endp

;--- 1. check if local APIC exists and, if enabled, display current status.
;--- 2. Scan ACPI tables for MADT; display IOAPICs.

main proc

;--- store value of SP register, in case we have to
;--- recover from an exception 0Dh.

	call checkcpuid
	.if CARRY?
		invoke printf, CStr("CPUID instruction not supported",lf)
		ret
	.endif

	mov eax, 1
	.586
	cpuid
	.386
	bt edx, 9			; local APIC exists and enabled?
	.if !CARRY?
		invoke printf, CStr("no onchip APIC found",lf)
		ret
	.endif

	mov wCS, cs
	call GoProtected
	jc exit

	call SearchMADT

	.if dwMADT
		invoke readextmem, addr madt, dwMADT, sizeof madt	; read the MADT header
		jc exit
		mov eax, madt.LocalAPICAddr
		mov abar, eax
		invoke printf, CStr(lf,"APIC base address: %lX",lf), eax
	.else
		invoke ReadMSRHlp, 1Bh
		mov abar, eax
		invoke printf, CStr(lf,"APIC base address register (MSR 1Bh): %lX%08lX",lf), edx, eax
		bt abar, 8
		.if CARRY?
			invoke printf, CStr("bootstrap CPU",lf)
		.else
			invoke printf, CStr("application CPU",lf)
		.endif
		bt abar, 10
		.if CARRY?
			invoke printf, CStr("x2APIC enabled",lf)
		.else
			invoke printf, CStr("x2APIC disabled or not supported",lf)
		.endif
		bt abar, 11
		.if CARRY?
			invoke printf, CStr("APIC enabled",lf)
		.else
			invoke printf, CStr("APIC disabled",lf)
		.endif
	.endif

	mov eax, abar
	and ax, 0f000h
	invoke PrintAPIC, eax

	.if dwMADT
		invoke PrintMADT, dwMADT
		.if bCntIOAPIC
			call PrintIOAPIC
		.endif
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

