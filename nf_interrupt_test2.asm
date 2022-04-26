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
;* NF_INTERRUPT_TEST.ASM           SOURCE FILE    *
;* Copyright (C) 2022 Esteban Looser-Rojas.       *
;* Contains test program for the NeonFox          *
;* processor validation platform.                 *
;**************************************************

INCLUDE "NEONFOX.INC"

;VARIABLES
;$0000 TO $0FFF IS RESERVED FOR SYSTEM ROM SHADOW

INDICATOR_VALUE	EQU $1000	;COPY OF VALUE SHOWN IN HEX DISPLAY

;$020000 TO $02FFFF IS RESERVED FOR GRAPHICS HARDWARE (PATTERN TABLE)
;$030000 TO $034AFF IS RESERVED FOR GRAPHICS HARDWARE (ATTRIBUTE TABLE)

	ORG 0
	BRA INT_RESET
	NOP
	BRA INT_VSYNC	;HSYNC
	NOP
	BRA INT_VSYNC	;VSYNC
	NOP
	RET	;UART RX
	NOP
	RET	;UART TX
	NOP
	RET	;KB RX
	NOP
	RET	;TIMER
	NOP
	BRA INT_TEST	;INT7
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

INT_TEST
	NOP
	NOP
	NOP
	NOP

	NOP
	NOP
	NOP
	NOP

	RET
	NOP


INT_VSYNC
	;SAVE REGISTERS
	MOVE W, STATUS, R12
	MOVE W, CAL, R13
	MOVE W, CAH, R14
	MOVE W, RAL, R15
	MOVE W, RAH, AUX3
	MOVE W, AUX0, R11
	MOVE W, IAL, R9

	;DO THE THING
	LIM H, AUX0, `HH HEX_INC_MOD
	LIM L, AUX0, `HL HEX_INC_MOD
	MOVE W, AUX0, CAH
	LIM H, AUX0, `LH HEX_INC_MOD
	LIM L, AUX0, `LL HEX_INC_MOD
	MOVE W, AUX0, CAL
	CALL
	NOP

	LIM H, AUX0, `LH INTERRUPT_STATUS
	LIM L, AUX0, `LL INTERRUPT_STATUS
	MOVE W, AUX0, IAL

	;RESTORE REGISTERS
	MOVE W, R13, CAL
	MOVE W, R14, CAH
	MOVE W, R15, RAL
	MOVE W, AUX3, RAH
	MOVE W, R11, AUX0
	MOVE W, ZERO, ID
	MOVE W, R9, IAL
	RET
	MOVE W, R12, STATUS


INT_RESET
;SET ALL REGISTERS TO ZERO
	MOVE W, ZERO, AUX0
	MOVE W, ZERO, AUX1
	MOVE W, ZERO, AUX2
	MOVE W, ZERO, AUX3
	MOVE W, ZERO, R0
	MOVE W, ZERO, R1
	MOVE W, ZERO, R2
	MOVE W, ZERO, R3
	MOVE W, ZERO, R4
	MOVE W, ZERO, R5
	MOVE W, ZERO, R6
	MOVE W, ZERO, R7
	MOVE W, ZERO, R8
	MOVE W, ZERO, R9
	MOVE W, ZERO, R10
	MOVE W, ZERO, R11
	MOVE W, ZERO, R12
	MOVE W, ZERO, R13
	MOVE W, ZERO, R14
	MOVE W, ZERO, R15
	MOVE W, ZERO, DAH
	MOVE W, ZERO, DAL
	MOVE W, ZERO, IAL
	MOVE W, ZERO, DD
	MOVE W, ZERO, ID
	MOVE W, ZERO, CAH
	MOVE W, ZERO, CAL
	MOVE W, ZERO, RAH
	MOVE W, ZERO, RAL
	MOVE W, ZERO, STATUS

	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM H, AUX1, `LH INDICATOR_VALUE
	LIM L, AUX1, `LL INDICATOR_VALUE
	MOVE W, AUX1, DAL
	LIM W, AUX0, $01
	MOVE W, AUX0, ID
	MOVE W, AUX0, DD

	LIM H, AUX0, `LH INTERRUPT_CONTROL
	LIM L, AUX0, `LL INTERRUPT_CONTROL
	MOVE W, AUX0, IAL
	;LIM W, AUX0, 5
	LIM H, AUX0, $00
	LIM L, AUX0, $81
	MOVE W, AUX0, ID

	BRA MAIN
	NOP

HEX_INC_MOD
	LIM W, AUX0, $01
	ADD W, R8, R8
	LIM W, AUX0, $01
	XOR W, R8, AUX0
	BRZ HEX_INC_UPDATE
	NOP
	RET
	NOP
HEX_INC_UPDATE
	MOVE W, ZERO, R8
	LIM H, AUX1, `LH HEX_INDICATORS
	LIM L, AUX1, `LL HEX_INDICATORS
	MOVE W, AUX1, IAL
	LIM H, AUX1, `LH INDICATOR_VALUE
	LIM L, AUX1, `LL INDICATOR_VALUE
	MOVE W, AUX1, DAL
	LIM W, AUX0, $01
	ADD W, DD, AUX0
	MOVE W, AUX0, ID
	MOVE W, AUX0, DD
	RET
	NOP

MAIN
	NOP
	NOP
	NOP
	BRA MAIN2	;p miss here

	LIM W, AUX0, $01
	NOP
	NOP
	NOP

	NOP
	NOP
	NOP
	NOP
MAIN2
	NOP	;p miss here
	NOP
	NOP
	NOP	;p miss here

	NOP
	NOP
	NOP
	NOP	;p miss here

	BRA MAIN3
	LIM W, AUX0, $02
	NOP
	NOP	;p miss here

	NOP
	NOP
	NOP
	NOP
MAIN3
	NOP
	NOP
	NOP
	NOP	;p miss here

	NOP
	NOP
	NOP
	NOP	;p miss here

	NOP
	BRA MAIN4
	LIM W, AUX0, $03
	NOP	;p miss here

	NOP
	NOP
	NOP
	NOP
MAIN4
	NOP
	NOP
	NOP
	NOP	;p miss here

	NOP
	NOP
	NOP
	NOP	;p miss here

	NOP
	NOP
	BRA MAIN5
	LIM W, AUX0, $04	;p miss here

	NOP
	NOP
	NOP
	NOP
MAIN5
	NOP	;p miss here
	NOP
	NOP
	NOP	;p miss here

	NOP
	NOP
	NOP
	NOP	;p miss here

	BRA MAIN
	NOP

	NOP
	NOP	;p miss here
	NOP
	NOP

	NOP
	NOP
	NOP
	NOP

	NOP
	NOP
	NOP
	NOP

	NOP
	NOP
	NOP
	NOP

	RET
	NOP

;MODIFIES REGISTERS: AUX0, AUX1, AUX2, R0, R1, R2, R3
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
	SUB W, R0, AUX2	;SUBTRACT DENOMINATOR FROM NUMERATOR
	BRN FINAL16	;IF THE RESULT IS NEGATIVE GOTO FINAL16
	MOVE W, R3, AUX0	;PUT INDEX IN AUX0
	MOVE W, AUX2, R0	;MAKE THE SUBTRACTION RESULT THE NEW NUMERATOR
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

	ORG $0FF8	;PAD FILE TO 4K WORDS
	LIM H, AUX0, `HH INT_RESET
	LIM L, AUX0, `HL INT_RESET
	LIM H, AUX1, `LH INT_RESET
	LIM L, AUX1, `LL INT_RESET
	MOVE W, AUX0, CAH
	MOVE W, AUX1, CAL
	JMP
	NOP