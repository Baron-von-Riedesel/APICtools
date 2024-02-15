
;--- ACPI definitions

;--- ACPI v1

RSDP struct
sig		db 8 dup (?)
bChksum	db ?
OEMID	db 6 dup (?)
bRev	db ?
dwRSDT	dd ?
RSDP ends

;--- since ACPI v2.0

XSDP struct
	RSDP <>
dwLength	dd ?
qwXSDT		dq ?
bExChksum	db ?
			db ?,?,?
XSDP ends

ACPISDTHeader struct
sig		db 4 dup (?)
dwLength dd ?
bRev	db ?
bChksum	db ?
OEMID	db 6 dup (?)
OEMTableID	db 8 dup (?)
dwOEMRev	dd ?
dwCreatorID	dd ?
dwCreatorRev dd ?
ACPISDTHeader ends

XSDT struct
	ACPISDTHeader <>
;--- here comes an array of QWORD pointers to other SDTs
XSDT ends

;--- MADT structs
;--- APIC structure types

MADTTypeHdr struct
bType	db ?
bLength	db ?
MADTTypeHdr ends

;--- type 0: Processor Local APIC
MADTType0 struct
bACPIProcessorID db ?
bAPICID db ?
dwFlags dd ?
MADTType0 ends

;--- type 1: IO APIC
MADTType1 struct
bIOAPICID db ?
	db ?
dwIOAPICAddr dd ?		; I/O APIC Address
dwGblSysIntBase dd ?	; Global System Interrupt Base
MADTType1 ends

;--- type 2: IO APIC Interrupt Source Override
MADTType2 struct
bBusSrc db ?
bIrqSrc db ?
dwGblSysInt dd ?	; Global System Interrupt
wFlags	dw ?
MADTType2 ends

;--- type 3: IO APIC NMI source
MADTType3 struct
bNMISrc db ?
	db ?
wFlags	dw ?
dwGblSysInt dd ?	; Global System Interrupt
MADTType3 ends

;--- type 4: Local APIC NMI
MADTType4 struct
bACPIProcessorID db ?
wFlags	dw ?
bLINT db ? ;(0 or 1)
MADTType4 ends

;--- type 5: Local APIC Address Override
MADTType5 struct
	dw ?
qwPhysAddr dq ?
MADTType5 ends

;--- type 6: IOSAPIC
;--- type 7: local SAPIC
;--- type 8: platform interrupt sources

;--- type 9: Processor Local x2APIC
MADTType9 struct
	dw ?
dwX2APICID dd ?
dwFlags dd ?
dwACPIID dd ?
MADTType9 ends

;--- type 10: local x2APIC NMI
;--- type 11: GIC
;--- type 12: GICD
;--- type 13-255: reserved 

MADTTypeX struct
	MADTTypeHdr <>
union
t0	MADTType0 <>
t1	MADTType1 <>
t2	MADTType2 <>
t3	MADTType3 <>
t4	MADTType4 <>
t5	MADTType5 <>
t9	MADTType9 <>
ends
MADTTypeX ends

MADT struct
	ACPISDTHeader <>
LocalAPICAddr dd ?	;local APIC address
dwFlags dd ?		;flags (0: 1=legacy PICs installed)
;--- here comes an array of variable length records, type MADTType0|1|2|3|4|5|9
MADT ends

