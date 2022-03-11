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
;* Copyright (C) 2021 Esteban Looser-Rojas.       *
;* Contains test program for the NeonFox          *
;* processor validation platform.                 *
;**************************************************

INCLUDE "NEONFOX.INC"

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
	BRA HALT
	NOP

	NOP
	NOP
	NOP
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

	ORG $0FF8	;PAD FILE TO 4K WORDS
	LIM H, AUX0, `HH INT_RESET
	LIM L, AUX0, `HL INT_RESET
	LIM H, AUX1, `LH INT_RESET
	LIM L, AUX1, `LL INT_RESET
	MOVE W, AUX0, CAH
	MOVE W, AUX1, CAL
	JMP
	NOP
