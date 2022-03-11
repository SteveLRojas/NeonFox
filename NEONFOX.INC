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
;* NEONFOX.INC        HEADER FILE                 *
;* Copyright (C) 2021 Esteban Looser-Rojas.       *
;* Contains register defines the NeonFox CPU      *
;* and address defines for platform peripherals.  *
;* Addresses are for the NeonFox test platform.   *
;**************************************************

; Address map for IO space:
; 0x0000 to 0x0FFF video memory (read write)
; 0x1000 to 0xFFEF	reserved
; 0xFFF0 to 0xFFF3	timer module
; 0xFFF4 to 0xFFF5 keyboard module (read write)
; 0xFFF6 to 0xFFF7 video mode register (write only)
; 0xFFF8 to 0xFFFB memory subsystem control registers	(write only)
; 0xFFFC to 0xFFFD	HEX display registers	(write only)
; 0xFFFE to 0xFFFF	RS-232 module	(read write)

; MSC Address map
; 0x0 program space control register
; 	bit0	reset bit (LSB)
; 	bit1	reserved
; 	bit2	reserved
; 	bit3	control enable bit
; 0x1 program space page register
; 0x2 data space control register
; 	bit0	reset bit (LSB)
; 	bit1	flush bit
; 	bit2	reserved
; 	bit3	control enable bit
; 0x3 data space page register

; RS-232 module address map
; 0 data register
; 1 status register
;		bit 0: TX overwrite (LSB)
;		bit 1: RX overwrite
;		bit 2: TX ready
;		bit 3: RX ready
;		bit 4: TX queue empty
;		bit 5: RX queue full

; keyboard module address map
; 0 data register
; 1 status register
;		bit 0: TX overwrite (LSB)
;		bit 1: RX overwrite
;		bit 2: TX ready
;		bit 3: RX ready
;		bit 4: TX queue empty
;		bit 5: RX queue full

; Timer module address map
; 0 counter bits 7:0
; 1 counter bits 15:8
; 2 counter bits 23:16
; 3 status
;		bit 0: counter 7:0 not zero
;		bit 1: counter 15:8 not zero
;		bit 2: counter 23:16 not zero
;		bit 3: counter 23:0 not zero
;		bit 4: VSYNC
;		bit 5: HSYNC

; Interrupt controller address map
; 0 status register
; 1 control register

; Address map for data space:
; 0x00000000 to 0xFFFFFFFF	active data memory page (cached)

; Address map for program space:
; 0x00000000 to 0xFFFFFFFF	active program memory page (cached)

; REMEMBER: PROGRAM AND DATA MEMORIES ARE BOTH WORD ADDRESSABLE!
; THE TWO MEMORY SPACES ARE SHARED THROUGH THE CACHE CONTROLLERS, BOTH CACHES MUST BE FLUSHED FOR CHANGES TO THE DATA SPACE TO SHOW UP IN THE PROGRAM SPACE.

AUX0	EQU R16
AUX1 	EQU R17
AUX2 	EQU R18
AUX3	EQU R19
DAL		EQU R20
DAH		EQU R21
IAL		EQU R22
IAH		EQU R23
DD		EQU R24
ID		EQU R25
CAL		EQU R26
CAH 	EQU R27
MULL 	EQU R28
MULH 	EQU R29
STATUS 	EQU R30
CONTROL EQU R31

TIMER_COUNT_0 EQU $FFF0
TIMER_COUNT_1 EQU $FFF1
TIMER_COUNT_2 EQU $FFF2
TIMER_STATUS EQU $FFF3

KB_DATA EQU $FFF4
KB_STATUS EQU $FFF5

V_MODE EQU $FFF6
VGA_MODE EQU $FFF7	;THESE TWO ARE THE SAME REGISTER

MSC_P_CONTROL EQU $FFF8
MSC_P_PAGE EQU $FFF9
MSC_D_CONTROL EQU $FFFA
MSC_D_PAGE EQU $FFFB

HEX_LOW EQU $FFFC
HEX_HIGH EQU $FFFD

UART_DATA EQU $FFFE
UART_STATUS EQU $FFFF

INTERRUPT_STATUS EQU $FFEE
INTERRUPT_CONTROL EQU $FFEF
