;                     /\         /\__
;                   // \       (  0 )_____/\            __
;                  // \ \     (vv          o|          /^v\
;                //    \ \   (vvvv  ___-----^        /^^/\vv\
;              //  /     \ \ |vvvvv/               /^^/    \v\
;             //  /       (\\/vvvv/              /^^/       \v\
;            //  /  /  \ (  /vvvv/              /^^/---(     \v\
;           //  /  /    \( /vvvv/----(O        /^^/           \v\
;          //  /  /  \  (/vvvv/               /^^/             \v|
;        //  /  /    \( vvvv/                /^^/               ||
;       //  /  /    (  vvvv/                 |^^|              //
;      //  / /    (  |vvvv|                  /^^/            //
;     //  / /   (    \vvvvv\          )-----/^^/           //
;    // / / (          \vvvvv\            /^^^/          //
;   /// /(               \vvvvv\        /^^^^/          //
;  ///(              )-----\vvvvv\    /^^^^/-----(      \\
; //(                        \vvvvv\/^^^^/               \\
;/(                            \vvvv^^^/                 //
;                                \vv^/         /        //
;                                             /<______//
;                                            <<<------/
;                                             \<
;                                              \
;**************************************************
;* NEONFOX_TEST.ASM           SOURCE FILE         *
;* Copyright (C) 2022 Esteban Looser-Rojas.       *
;* Contains test program for the NeonFox          *
;* processor validation platform.                 *
;**************************************************

INCLUDE "NEONFOX.INC"

MACRO	SDADDR ADDRESS	;SHORT DATA ADDRESS
	LIM H, AUX1, `LH ADDRESS
	LIM L, AUX1, `LL ADDRESS
	MOVE W, AUX1, DAL
ENDMACRO SDADDR

MACRO LDADDR ADDRESS	;LONG DATA ADDRESS
	LIM H, AUX1, `HH ADDRESS
	LIM L, AUX1, `HL ADDRESS
	LIM H, AUX2, `LH ADDRESS
	LIM L, AUX2, `LL ADDRESS
	MOVE W, AUX1, DAH
	MOVE W, AUX2, DAL
ENDMACRO LDADDR

MACRO IADDR ADDRESS	;IO ADDRESS
	LIM H, AUX1, `LH ADDRESS
	LIM L, AUX1, `LL ADDRESS
	MOVE W, AUX1, IAL
ENDMACRO IADDR

MACRO LPLOAD ADDRESS, TARGET	;GET DATA FROM LONG POINTER
	LIM H, AUX1, `HH ADDRESS
	LIM L, AUX1, `HL ADDRESS
	LIM H, AUX2, `LH ADDRESS
	LIM L, AUX2, `LL ADDRESS
	MOVE W, AUX1, DAH
	MOVE W, AUX2, DAL
	MOVE W, DD, TARGET
ENDMACRO LPLOAD

MACRO SPLOAD ADDRESS, TARGET	;GET DATA FROM SHORT POINTER
	LIM H, AUX1, `LH ADDRESS
	LIM L, AUX1, `LL ADDRESS
	MOVE W, AUX1, DAL
	MOVE W, DD, TARGET
ENDMACRO SPLOAD

MACRO IPLOAD ADDRESS, TARGET	;GET DATA FROM IO POINTER
	LIM H, AUX1, `LH ADDRESS
	LIM L, AUX1, `LL ADDRESS
	MOVE W, AUX1, IAL
	MOVE W, ID, TERGET
ENDMACRO IPLOAD

;VARIABLES
CHR_PATTERN EQU $1000
CHR_PATTERN_END EQU $1FFF
CHR_COLOR EQU $2000
CHR_BG_COLOR EQU $2001
DRAW_TEXT_OFFSET EQU $2002
DRAW_TEXT_PALETTE EQU $2003
TEXT_BUFFER EQU $2004
TEXT_BUFFER_END EQU $2083

	ORG 0
	BRA INT_RESET
	NOP
	RET	;INT1
	NOP
	RET ;INT2
	NOP
	RET	;INT3
	NOP
	RET	;INT4
	NOP
	RET	;INT5
	NOP
	RET	;INT6
	NOP
	RET	;INT7
	NOP
	RET	;INT8
	NOP
	RET ;INT9
	NOP
	RET	;INT10
	NOP
	RET	;INT11
	NOP
	RET	;INT12
	NOP
	RET	;INT13
	NOP
	RET	;INT14
	NOP
	RET ;INT15
	NOP

INT_RESET
;SET ALL REGISTERS TO ZERO
	LIM W, AUX0, $00
	LIM W, AUX1, $00
	MOVE W, AUX0, AUX2
	MOVE W, AUX1, AUX3
	MOVE W, AUX0, R0
	MOVE W, AUX0, R1
	MOVE W, AUX0, R2
	MOVE W, AUX0, R3
	MOVE W, AUX0, R4
	MOVE W, AUX0, R5
	MOVE W, AUX0, R6
	MOVE W, AUX0, R7
	MOVE W, AUX0, R8
	MOVE W, AUX0, R9
	MOVE W, AUX0, R10
	MOVE W, AUX0, R11
	MOVE W, AUX0, R12
	MOVE W, AUX0, R13
	MOVE W, AUX0, R14
	MOVE W, AUX0, R15
	MOVE W, AUX0, DAH
	MOVE W, AUX0, DAL
	MOVE W, AUX0, IAL
	MOVE W, AUX0, CAH
	MOVE W, AUX0, CAL
	MOVE W, AUX0, DD
	MOVE W, AUX0, ID
	MOVE W, AUX0, STATUS
	MOVE W, AUX0, CONTROL

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $01
	MOVE W, AUX0, ID

	LIM H, AUX1, `HH CHR_COLOR
	LIM L, AUX1, `HL CHR_COLOR
	LIM H, AUX2, `LH CHR_COLOR
	LIM L, AUX2, `LL CHR_COLOR
	LIM W, AUX0, $0C
	MOVE W, AUX1, DAH
	MOVE W, AUX2, DAL
	MOVE W, AUX0, DD

	LIM H, AUX2, `LH CHR_BG_COLOR
	LIM L, AUX2, `LL CHR_BG_COLOR
	LIM W, AUX0, $03
	MOVE W, AUX2, DAL
	MOVE W, AUX0, DD

	LIM H, AUX1, `HH BUILD_CHR_PATTERN
	LIM L, AUX1, `HL BUILD_CHR_PATTERN
	LIM H, AUX0, `LH BUILD_CHR_PATTERN
	LIM L, AUX0, `LL BUILD_CHR_PATTERN
	MOVE W, AUX1, CAH
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $02
	MOVE W, AUX0, ID

	LIM H, AUX1, `HH COPY_PATTERN_TO_MEM
	LIM L, AUX1, `HL COPY_PATTERN_TO_MEM
	LIM H, AUX0, `LH COPY_PATTERN_TO_MEM
	LIM L, AUX0, `LL COPY_PATTERN_TO_MEM
	MOVE W, AUX1, CAH
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $03
	MOVE W, AUX0, ID

	LIM H, AUX1, `HH SET_ATTRIBUTE
	LIM L, AUX1, `HL SET_ATTRIBUTE
	LIM H, AUX0, `LH SET_ATTRIBUTE
	LIM L, AUX0, `LL SET_ATTRIBUTE
	MOVE W, AUX1, CAH
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $04
	MOVE W, AUX0, ID

	LIM H, AUX1, `HH XGRI_TEST
	LIM L, AUX1, `HL XGRI_TEST
	LIM H, AUX0, `LH XGRI_TEST
	LIM L, AUX0, `LL XGRI_TEST
	MOVE W, AUX1, CAH
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $05
	MOVE W, AUX0, ID

	LIM H, AUX1, `HH LOAD_TEST_STRING
	LIM L, AUX1, `HL LOAD_TEST_STRING
	LIM H, AUX0, `LH LOAD_TEST_STRING
	LIM L, AUX0, `LL LOAD_TEST_STRING
	MOVE W, AUX1, CAH
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $06
	MOVE W, AUX0, ID

	LDADDR DRAW_TEXT_OFFSET
	LIM H, AUX0, `LH 3212
	LIM L, AUX0, `LL 3212
	MOVE W, AUX0, DD

	LDADDR DRAW_TEXT_PALETTE
	LIM W, AUX0, $07
	MOVE W, AUX0, DD

	LIM H, AUX1, `HH DRAW_TEXT
	LIM L, AUX1, `HL DRAW_TEXT
	LIM H, AUX0, `LH DRAW_TEXT
	LIM L, AUX0, `LL DRAW_TEXT
	MOVE W, AUX1, CAH
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $07
	MOVE W, AUX0, ID

	

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $08
	MOVE W, AUX0, ID

	

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $09
	MOVE W, AUX0, ID

	

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM W, AUX0, $0A
	MOVE W, AUX0, ID

	BRA HALT
	NOP

	NOP
	NOP
	NOP
	NOP



BUILD_CHR_PATTERN
;CREATE CHR PATTERN FROM CHR ROW WITH SPECIFIED COLOR
	LIM H, AUX0, `LH CHR_ROM
	LIM L, AUX0, `LL CHR_ROM
	LIM H, AUX1, `LH CHR_PATTERN
	LIM L, AUX1, `LL CHR_PATTERN
	LIM H, AUX2, `LH CHR_BG_COLOR
	LIM L, AUX2, `LL CHR_BG_COLOR
	LIM H, AUX3, `LH CHR_COLOR			; Pointer to chr color pallete offset
	LIM L, AUX3, `LL CHR_COLOR

	MOVE W, AUX0, R0	;WE WILL KEEP POINTER TO CHR_ROM IN R0 FOR NOW
	MOVE W, AUX1, R1	;POINTER TO CHR_PATTERN IN R1
	MOVE W, AUX2, R5	;POINTER TO CHR_BG_COLOR IN R5
	MOVE W, AUX3, R3	;POINTER TO CHR_COLOR IN R3

	LIM H, AUX1, `LH CHR_ROM_END
	LIM L, AUX1, `LL CHR_ROM_END
	LIM W, AUX3, $00

	MOVE W, AUX1, R4	;POINTER TO CHR_ROM END IN R4
	MOVE W, AUX3, R6	;KEEP A ZERO HANDY IN R6

	;GET CHR COLOR IN R3
	MOVE W, R6, DAH
	MOVE W, R3, DAL
	MOVE W, DD, R3

	;GET BG COLOR IN R5
	MOVE W, R5, DAL
	MOVE W, DD, R5

BCP_LOOP_WORD
	;READ CHR WORD
	MOVE W, R6, DAH	;ZERO DAH
	MOVE W, R0, DAL	;POINT DAL TO CHR_ROM
	MOVE W, DD, R7	;PUT CHR WORD IN R7

	LIM W, AUX0, $03
	MOVE W, AUX0, R2	;START NIBBLE LOOP COUNTER AT 3

BCP_LOOP_NIBBLE
	MOVE W, R6, R9	;CLEAR R9 (USED TO HOLD RESULT WORD)

	;GET BIT 3
	LIM W, AUX0, $01
	ROL W, R7, R7
	AND W, R7, R8	;PUT THE BIT IN R8

	;DECIDE WHICH COLOR TO USE
	BRNZ BCP_FG_3
	MOVE W, R3, AUX0	;DUE TO DELAYED BRANCHING THIS INSTRUCTION IS NEVER SKIPPED
	MOVE W, R5, AUX0	;GET THE SELECTED COLOR IN LOWER NIBBLE OF AUX0
BCP_FG_3
	;COPY NIBBLE TO R9, THEN ROTATE LEFT
	OR W, R9, R9	;OR AUX0 WITH R9
	ROL W, R9, R9
	ROL W, R9, R9
	ROL W, R9, R9
	ROL W, R9, R9

	;GET BIT 2
	LIM W, AUX0, $01
	ROL W, R7, R7
	AND W, R7, R8	;PUT THE BIT IN R8

	;DECIDE WHICH COLOR TO USE
	BRNZ BCP_FG_2
	MOVE W, R3, AUX0	;DUE TO DELAYED BRANCHING THIS INSTRUCTION IS NEVER SKIPPED
	MOVE W, R5, AUX0	;GET THE SELECTED COLOR IN LOWER NIBBLE OF AUX0
BCP_FG_2
	;COPY NIBBLE TO R9, THEN ROTATE LEFT
	OR W, R9, R9	;OR AUX0 WITH R9
	ROL W, R9, R9
	ROL W, R9, R9
	ROL W, R9, R9
	ROL W, R9, R9

	;GET BIT 1
	LIM W, AUX0, $01
	ROL W, R7, R7
	AND W, R7, R8	;PUT THE BIT IN R8

	;DECIDE WHICH COLOR TO USE
	BRNZ BCP_FG_1
	MOVE W, R3, AUX0	;DUE TO DELAYED BRANCHING THIS INSTRUCTION IS NEVER SKIPPED
	MOVE W, R5, AUX0	;GET THE SELECTED COLOR IN LOWER NIBBLE OF AUX0
BCP_FG_1
	;COPY NIBBLE TO R9, THEN ROTATE LEFT
	OR W, R9, R9	;OR AUX0 WITH R9
	ROL W, R9, R9
	ROL W, R9, R9
	ROL W, R9, R9
	ROL W, R9, R9

	;GET BIT 0
	LIM W, AUX0, $01
	ROL W, R7, R7
	AND W, R7, R8	;PUT THE BIT IN R8

	;DECIDE WHICH COLOR TO USE
	BRNZ BCP_FG_0
	MOVE W, R3, AUX0	;DUE TO DELAYED BRANCHING THIS INSTRUCTION IS NEVER SKIPPED
	MOVE W, R5, AUX0	;GET THE SELECTED COLOR IN LOWER NIBBLE OF AUX0
BCP_FG_0
	;COPY NIBBLE TO R9, THIS IS THE LAST NIBBLE SO WE DON'T ROTATE
	OR W, R9, R9	;OR AUX0 WITH R9

	;WRITE THE PATTERN WORD TO MEMORY
	MOVE W, R6, DAH
	MOVE W, R1, DAL
	LIM W, AUX0, $01
	MOVE W, R9, DD
	ADD W, R1, R1	;INCREMENT POINTER TO CHR_PATTERN

	TEST W, R2
	LIM W, AUX0, $01
	BRNZ BCP_LOOP_NIBBLE
	SUB W, R2, R2	;DECREMENT NIBBLE LOOP COUNTER

	;CHECK IF WE ARE DONE AND INCREMENT CHR_ROM POINTER
	MOVE W, R0, AUX0
	XOR W, R4, AUX1	;COMPARE CHR_ROM POINTER TO CHR_ROM_END POINTER
	LIM W, AUX0, $01
	BRNZ BCP_LOOP_WORD
	ADD W, R0, R0	;INCREMENT CHR_ROM POINTER

	RET
	NOP



COPY_PATTERN_TO_MEM
	LIM H, AUX1, `LH CHR_PATTERN
	LIM L, AUX1, `LL CHR_PATTERN
	LIM H, AUX2, `LH CHR_PATTERN_END
	LIM L, AUX2, `LL CHR_PATTERN_END

	MOVE W, AUX1, R1	;POINTER TO CHR_PATTERN IN R1
	MOVE W, AUX2, R2	;POINTER TO CHR_PATTERN END IN R2

	LIM H, AUX1, `LH XGRI_PAR
	LIM L, AUX1, `LL XGRI_PAR
	MOVE W, AUX1, IAL				; Set I/O addr to addr of PAR
	LIM W, AUX0, $00
	MOVE W, AUX0, ID				; Fill PAR w/ 0
	MOVE W, AUX0, DAH				; Zero DAH

	LIM H, AUX1, `LH XGRI_STATUS	
	LIM L, AUX1, `LL XGRI_STATUS
	MOVE W, AUX1, R0				; KEEP IO POINTER TO XGRI_STATUS IN R0

	LIM H, AUX1, `LH XGRI_PDR	
	LIM L, AUX1, `LL XGRI_PDR
	MOVE W, AUX1, R3				; KEEP IO POINTER TO XGRI_PDR IN R3

	MOVE W, R0, IAL	;ADDRESS XGRI_STATUS

CPTM_LOOP
	BITT L, ID, 3		; CHECK P_FULL FLAG
	BRNZ CPTM_LOOP		; IF P_FULL IS SET KEEP WAITING
	MOVE W, R1, DAL		; Set DAL to "address of current pattern in CHR_PATTERN"

	MOVE W, R3, IAL		; ADDRESS XGRI_PDR
	MOVE W, R1, AUX0
	MOVE W, DD, ID		; Move pattern from CHR_PATTERN to PDR (PAR=n --> PAR=n+1)
	MOVE W, R0, IAL		; ADDRESS XGRI_STATUS
			
	; CHECK IF WE ARE DONE AND 
	; INCREMENT CHR_PATTERN POINTER
	XOR W, R2, AUX1		; COMPARE CHR_PATTERN POINTER TO CHR_PATTERN_END POINTER
	LIM W, AUX0, $01
	BRNZ CPTM_LOOP
	ADD W, R1, R1		; INCREMENT CHR_PATTERN POINTER

	RET
	NOP



SET_ATTRIBUTE
	LIM H, AUX1, `LH XGRI_STATUS	
	LIM L, AUX1, `LL XGRI_STATUS
	MOVE W, AUX1, R0	;KEEP IO POINTER TO XGRI_STATUS IN R0

	LIM H, AUX1, `LH XGRI_ADR	
	LIM L, AUX1, `LL XGRI_ADR
	MOVE W, AUX1, R3	;KEEP IO POINTER TO XGRI_ADR IN R3

	LIM H, AUX1, `LH XGRI_AAR
	LIM L, AUX1, `LL XGRI_AAR
	MOVE W, AUX1, R2	;KEEP IO POINTER TO XGRI_AAR IN R2
	MOVE W, AUX1, IAL	; Set I/O addr to addr of AAR
	LIM W, AUX0, $00
	MOVE W, AUX0, ID	; ZERO AAR

	LIM H, AUX2, $20 
	LIM L, AUX2, $04	;KEEP EOT CHAR IN AUX2

	LIM H, AUX1, `LH 19200
	LIM L, AUX1, `LL 19200
	MOVE W, AUX1, R1	;START LOOP COUNTER AT 19200

	MOVE W, R0, IAL		;ADDRESS XGRI_STATUS
	LIM W, AUX0, $01

SET_ATTRIBUTE_LOOP
	BITT L, ID, 1		;XGRI HAS 2 CYCLE DATA TO STATUS LATENCY
	BITT L, ID, 1		;CHECK A_FULL FLAG
	BRNZ SET_ATTRIBUTE_LOOP		;IF A_FULL IS SET KEEP WAITING
	NOP

	MOVE W, R3, IAL		;ADDRESS XGRI_ADR
	MOVE W, AUX2, ID 	;WRITE ATTRIBUTE

	SUB W, R1, R1
	BRNZ SET_ATTRIBUTE_LOOP
	MOVE W, R0, IAL		;ADDRESS XGRI_STATUS

	;SET CORNERS TO A DIFFERENT TILE
	;TOP LEFT
	BITT L, ID, 0			;XGRI HAS 2 CYCLE DATA TO STATUS LATENCY
SET_ATTRIBUTE_TL
	BITT L, ID, 0			;CHECK A_EMPTY FLAG
	BRZ SET_ATTRIBUTE_TL	;IF A_EMPTY IS CLEARED KEEP WAITING
	NOP

	MOVE W, R2, IAL		;ADDRESS XGRI_AAR
	LIM W, AUX1, $00
	MOVE W, AUX1, ID	;SET AAR TO 0
	MOVE W, R3, IAL		;ADDRESS XGRI_ADR
	LIM H, AUX0, $50
	LIM L, AUX0, $2A	;PUT ATTRIBUTE IN AUX0
	MOVE W, AUX0, ID	;WRITE ATTRIBUTE
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID	;FILL REST OF ATTRIBUTE BUFFER

	;TOP RIGHT
	MOVE W, R0, IAL		;ADDRESS XGRI_STATUS
	BITT L, ID, 0		;XGRI HAS 2 CYCLE DATA TO STATUS LATENCY
SET_ATTRIBUTE_TR
	BITT L, ID, 0			;CHECK A_EMPTY FLAG
	BRZ SET_ATTRIBUTE_TR	;IF A_EMPTY IS CLEARED KEEP WAITING
	NOP

	MOVE W, R2, IAL		;ADDRESS XGRI_AAR
	LIM W, AUX1, 76
	MOVE W, AUX1, ID	;SET AAR TO 76
	MOVE W, R3, IAL		;ADDRESS XGRI_ADR
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID	;FILL REST OF ATTRIBUTE BUFFER
	MOVE W, AUX0, ID	;WRITE ATTRIBUTE

	;BOTTOM LEFT
	MOVE W, R0, IAL		;ADDRESS XGRI_STATUS
	BITT L, ID, 0		;XGRI HAS 2 CYCLE DATA TO STATUS LATENCY
SET_ATTRIBUTE_BL
	BITT L, ID, 0			;CHECK A_EMPTY FLAG
	BRZ SET_ATTRIBUTE_BL	;IF A_EMPTY IS CLEARED KEEP WAITING
	NOP

	MOVE W, R2, IAL		;ADDRESS XGRI_AAR
	LIM H, AUX1, `LH 9440
	LIM L, AUX1, `LL 9440
	MOVE W, AUX1, ID	;SET AAR TO 9440
	MOVE W, R3, IAL		;ADDRESS XGRI_ADR
	MOVE W, AUX0, ID	;WRITE ATTRIBUTE
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID	;FILL REST OF ATTRIBUTE BUFFER

	;BOTTOM RIGHT
	MOVE W, R0, IAL		;ADDRESS XGRI_STATUS
	BITT L, ID, 0		;XGRI HAS 2 CYCLE DATA TO STATUS LATENCY
SET_ATTRIBUTE_BR
	BITT L, ID, 0			;CHECK A_EMPTY FLAG
	BRZ SET_ATTRIBUTE_BR	;IF A_EMPTY IS CLEARED KEEP WAITING
	NOP

	MOVE W, R2, IAL		;ADDRESS XGRI_AAR
	LIM H, AUX1, `LH 9516
	LIM L, AUX1, `LL 9516
	MOVE W, AUX1, ID	;SET AAR TO 9516
	MOVE W, R3, IAL		;ADDRESS XGRI_ADR
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID	;FILL REST OF ATTRIBUTE BUFFER
	MOVE W, AUX0, ID	;WRITE ATTRIBUTE

	;MIDDLE
	MOVE W, R0, IAL		;ADDRESS XGRI_STATUS
	BITT L, ID, 0		;XGRI HAS 2 CYCLE DATA TO STATUS LATENCY
SET_ATTRIBUTE_MTL
	BITT L, ID, 0			;CHECK A_EMPTY FLAG
	BRZ SET_ATTRIBUTE_MTL	;IF A_EMPTY IS CLEARED KEEP WAITING
	NOP

	MOVE W, R2, IAL		;ADDRESS XGRI_AAR
	LIM H, AUX1, `LH 4676
	LIM L, AUX1, `LL 4676
	MOVE W, AUX1, ID	;SET AAR TO 4676
	MOVE W, R3, IAL		;ADDRESS XGRI_ADR
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID
	MOVE W, AUX2, ID	;FILL REST OF ATTRIBUTE BUFFER
	MOVE W, AUX0, ID	;WRITE ATTRIBUTE

	RET
	NOP



XGRI_TEST
	LIM H, AUX1, `LH XGRI_AAR
	LIM L, AUX1, `LL XGRI_AAR
	MOVE W, AUX1, IAL
	LIM W, AUX0, 16
	MOVE W, AUX0, ID

	LIM H, AUX1, `LH XGRI_ADR
	LIM L, AUX1, `LL XGRI_ADR
	MOVE W, AUX1, IAL
	LIM H, AUX0, `LH $002A
	LIM L, AUX0, `LL $002A
	MOVE W, AUX0, ID

	MOVE W, AUX0, ID
	MOVE W, AUX0, ID
	MOVE W, AUX0, ID

	RET
	NOP



LOAD_TEST_STRING
	LIM W, AUX0, $00
	MOVE W, AUX0, DAH
	LIM H, AUX1, `LH TEST_STRING
	LIM L, AUX1, `LL TEST_STRING
	LIM H, AUX2, `LH TEXT_BUFFER
	LIM L, AUX2, `LL TEXT_BUFFER
	LIM W, AUX0, $01
LOAD_TEST_STRING_LOOP
	MOVE W, AUX1, DAL
	ADD W, AUX1, AUX1
	MOVE W, DD, R0
	MOVE W, AUX2, DAL
	ADD W, AUX2, AUX2
	MOVE W, R0, DD
	BRNZ LOAD_TEST_STRING_LOOP
	NOP
LOAD_TEST_STRING_DONE
	RET
	NOP



DRAW_TEXT
	NOP
	IADDR XGRI_AAR					; SET IO ADDRESS TO XGRI ATTRIBUTE ADDRESS REGISTER
	LDADDR DRAW_TEXT_OFFSET			; SET DATA ADDRESS TO DRAW TEXT OFFSET
	MOVE W, DD, ID					; MOVE DRAW_TEXT_OFFSET TO XGRI_AAR
	SPLOAD DRAW_TEXT_PALETTE, R1	; PUT PALETTE FOR TEXT IN R1
	SDADDR TEXT_BUFFER				; SET DATA ADDRESS TO START OF TEXT BUFFER
	ROR W, R1, R1					; PUT PALETTE FOR TEXT IN HIGH 4 BITS OF R1		
	ROR W, R1, R1
	ROR W, R1, R1
	ROR W, R1, R1
DRAW_TEXT_CHECK
	MOVE W, DD, R0			; PUT CHAR IN R0
	BRNZ DRAW_TEXT_WAIT		; CHECK IF CHAR IS NULL
	NOP
	RET						; DONE IF CHAR IS NULL
	NOP
DRAW_TEXT_WAIT
	NOP
	IADDR XGRI_STATUS		; SET IO ADDR TO STATUS 
	BITT L, ID, 1			; Status[1] (a_full) -> Z Reg
	BRNZ DRAW_TEXT_WAIT
	NOP
DRAW_TEXT_SET
	IADDR XGRI_ADR			; SET IO ADDR TO ATTRIBUTE DATA
	MOVE W, R1, AUX0		
	OR W, R0, R0			; R0[0000,CHAR] | R1/AUX0[PALT,0000 0000 0000]
	MOVE W, R0, ID			; PUT CHAR IN ATTRIBUTE TABLE

	LIM W, AUX0, $01
	ADD W, DAL, DAL			; INCREMENT CHAR INDEX
	BRA DRAW_TEXT_CHECK	
	NOP



HALT
	LIM W, AUX0, $01
HALT_LOOP
	ADD W, R0, R0
	LIM W, AUX0, $00
	ADDC W, R1, R1
	ADDC W, R2, R2
	ADDC W, R3, R3
	BRA HALT_LOOP
	LIM W, AUX0, $01



DIV16
; R2 = R0 / R1
; R3 USED AS INDEX
	TEST W, R1
	BRNZ DIV16_NZ
	LIM W, AUX0, $00
	RET
	NOP
DIV16_NZ
	LIM W, AUX1, $01
	MOVE W, AUX0, R2	;CLEAR RESULT
	MOVE W, AUX1, R3	;SET INDEX TO 1
SHIFT_IT16
	BITT W, R1, 15
	BRNZ DIV16_LOOP	;IF MSB OF DENOMINATOR IS SET GOTO DIV16_LOOP
	NOP
	ROL W, R1, R1
	BRA SHIFT_IT16
	ROL W, R3, R3	;SHIFT LEFT DENOMINATOR AND INDEX
DIV16_LOOP
	MOVE W, R1, AUX0
	SUB W, R0, AUX3	;SUBTRACT DENOMINATOR FROM NUMERATOR
	BRN FINAL16	;IF THE RESULT IS NEGATIVE GOTO FINAL16
	MOVE W, R3, AUX0	;PUT INDEX IN AUX0
	MOVE W, AUX3, R1	;MAKE THE SUBTRACTION RESULT THE NEW DENOMINATOR
	ADD W, R2, R2	;ADD INDEX TO THE RESULT
FINAL16
	LIM H, AUX0, $7F
	LIM L, AUX0, $FF
	ROR W, R3, R3
	ROR W, R1, R1
	AND W, R3, R3	;SHIFT RIGHT INDEX
	BRNZ DIV16_LOOP
	AND W, R1, R1	;SHIFT RIGHT DENOMINATOR
	RET
	NOP



MUL16
; R2 = R0 * R1
	LIM W, AUX3, $00
	MOVE W, AUX3, R2	;CLEAR RESULT
MACC16
; R2 = R0 * R1 + R2
	BITT W, R0, 0
	BRZ MACC16_SHIFT	;IF LSB OF R0 IS CLEARED SKIP ADDING
	MOVE W, R1, AUX0
MACC16_ADD
	ADD W, R2, R2	;ADD R1 TO THE RESULT
MACC16_SHIFT
	LIM W, AUX0, $FE
	ROL W, R1, R1
	AND W, R0, R0
	ROR W, R0, R0	;SHIFT RIGHT R0
	BRNZ MACC16
	AND W, R1, R1	;SHIFT LEFT R1
	RET
	NOP



TEST_STRING
	DATA 'H'
	DATA 'E'
	DATA 'L'
	DATA 'L'
	DATA 'O'
	DATA '\S'
	DATA 'W'
	DATA 'O'
	DATA 'R'
	DATA 'L'
	DATA 'D'
	DATA '!'
	DATA $00



	ORG $0C00	;PLACE CHR ROM
INCLUDE "NF_CHR.INC"
