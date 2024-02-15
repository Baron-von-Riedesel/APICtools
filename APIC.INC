
;--- APIC Base Addr Register:
;--- bit 8: BSC: 1=boot strap CPU core, 0 = application processor
;--- bit 10: EXTD: 1=x2APIC enabled
;--- bit 11: AE: 1=xAPIC enable
;--- bits 12-51: base physical address for APIC register set (4 kB).
;--- all APIC registers are 16-byte aligned

;--- TPR:
;--- must be 0 to receive interrupts of all priorities

;--- EOI:
;--- NMI, SMI, INIT, extINT, SIPI, INIT need NO EOI

;--- SIVR - Spurious Interrupt Vector Register:
;--- 0-7: vector receiving spurious interrupts
;--- 8, ASE: 1=APIC software enabled, 0=disabled
;--- 9, FCC: Focus Cpu Core checking, 1=enabled 
;--- 12: EOI Broadcast Suppression, 1=enabled

;--- LVTE format: 
;--- 0-7 vector (if MT=fixed, 0-15 are illegal)
;--- 8-10: message type/delivery mode (not for EVTE):
;---   000=fixed
;---   010=SMI
;---   100=NMI
;---   111=external interrupt (for compatibility) 
;---   101=Init
;--- 12: delivery status (0=idle, 1=send pending)
;--- 13-15 are for LINT0 & LINT1 only
;--- 13: interrupt Input Pin Polarity (Intel only?)
;--- 14: remote IRR
;--- 15: trigger mode
;--- 16: mask
;--- 17-18: timer mode (0=one shot, 1=periodic,10=TSC Deadline)
;--- rest reserved
;--- TDCR: cpu core clock divisor for timer
;--- 0-1: divide value 1:0
;--- 3: divide value 2
;---     000=2, 001=4, 010=8, 011=16, 100=32, 101=64
;---     110=128, 111=1
;--- rest reserved.

;--- in DOS, LVINT0 usually is in "virtual wire mode", that is,
;--- delivery mode is 111 (extINT).

APICREGS struct
		dd ?,?,?,?,?,?,?,?
APICID	dd ?,?,?,?	;+20h
APICVers dd ?,?,?,?	;+30h
		dd ?,?,?,?	;+40h
		dd ?,?,?,?	;+50h
		dd ?,?,?,?	;+60h
		dd ?,?,?,?	;+70h
TPR		dd ?,?,?,?	;+80h task priority register
APR		dd ?,?,?,?	;+90h arbitration priority register
PPR		dd ?,?,?,?	;+A0h processor priority register
EOI		dd ?,?,?,?	;+B0h end of interrupt register
RRR		dd ?,?,?,?	;+C0h remote read register
LDR		dd ?,?,?,?	;+D0h logical destination register
DFR		dd ?,?,?,?	;+E0h destination format register
SIVR	dd ?,?,?,?	;+F0h spurious interrupt vector register
ISR		dd 4*8 dup (?)	;+100h-17Fh in-service register (8*32 bits)
TMR		dd 4*8 dup (?)	;+180h-1FFh trigger mode register (8*32 bits)
IRR		dd 4*8 dup (?)	;+200h-27Fh interrupt request register (8*32 bits)
ESR		dd ?,?,?,?	;+280h error status register
		dd 4*6 dup (?)	; 290h-2EFh
CMCI	dd ?,?,?,?	;+2F0h obsolete?
ICRLo	dd ?,?,?,?	;+300h interrupt command register low
ICRHi	dd ?,?,?,?	;+310h interrupt command register high
TiLVTE	dd ?,?,?,?	;+320h timer local vector table entry
ThLVTE	dd ?,?,?,?	;+330h thermal local vector table entry
PCLVTE	dd ?,?,?,?	;+340h performance counter local vector table entry
LI0VTE	dd ?,?,?,?	;+350h local interrupt 0 vector table entry
LI1VTE	dd ?,?,?,?	;+360h local interrupt 1 vector table entry
EVTE	dd ?,?,?,?	;+370h error vector table entry
TICR	dd ?,?,?,?	;+380h timer initial count register
TCCR	dd ?,?,?,?	;+390h timer current count register
		dd ?,?,?,?	;+3A0
		dd ?,?,?,?	;+3B0
		dd ?,?,?,?	;+3C0
		dd ?,?,?,?	;+3D0
TDCR	dd ?,?,?,?	;+3E0h timer divide configuration register
		dd ?,?,?,?	;+3F0
EAFR	dd ?,?,?,?	;+400h extended APIC feature register
EACR	dd ?,?,?,?	;+410h extended APIC control register
SEOI	dd ?,?,?,?	;+420h specific end of interrupt register
		dd 4*5 dup (?)	;+430h-47Fh
IER		dd 4*8 dup (?)	;+480h-4FFh interrupt enable register (8*32 bits)
EILVTR	dd 4*4 dup (?)	;+500h-53Fh extended interrupt local vector table reg
APICREGS ends

LVTE_MASKED_BIT equ 16	; if LVTE is masked

;--- IMCR: "interrupt mode control register"
;--- mentioned in Intel MP Specification.
;--- modes:
;--- + PIC mode: PIC interrupt line connected to INTR ( APIC circumvented )
;--- + virtual wire mode: local APIC active, IOAPIC not used;
;---   (local APIC LINT0 set to extINT for DOS compatibility).
;--- + symetric IO mode. 
;--- accessible thru port 22h/23h: writing 70h to port 22h selects register
;--- bit 0:
;---  0=PIC mode, no APICs
;---  1=virtual wire mode / symetric IO mode

