; Sub routines to manage Overscan
; Many thanks to Alien (ST Connexion), Troed (Sync), Ijor and Evil 
; May the force be with you guys !
; I have especially many used the http://www.atari-wiki.com/index.php/ST_STE_Scanlines page 
; and all the links mentionned at the beginning of this page
; and ST Synclock demosystem v1.0 by Evil !http://files.dhs.nu/files_source/stsys10.zip
; I (uko) have coded from scratch this code for overscan, based on all the above links
; but I must admit it finally looks like very much to Evil's code !
; I have also done routines for Full Timer A overscan, but not enough CPU left, so it is not
; included here. I talk a little more about it in http://www.atari-forum.com/viewtopic.php?t=5423  
; The Artic Land (T.AL) 2019
; Contact: uko.tal@gmail.com or uko at http://www.atari-forum.com


	TEXT
	
	; This file must be included at the top of the main code
	; but also contains code... so we directly go to the main code !
	bra main
	
	; Warning, more than 10 macro parameters are required
	; vasmm68k_mot -Ftos -monst -align -allmp

; -----------------------------------------------------------------------------
; Constants
; -----------------------------------------------------------------------------	
; Useful nops values ;-)
OVERSCAN_NOPS_END_TOP_BORDER		equ 71
OVERSCAN_NOPS_BEFORE_LEFT_BORDER	equ 26
OVERSCAN_NOPS_AFTER_LEFT_BORDER		equ 90
OVERSCAN_BOTTOM_BORDER_NBSCANLINE	equ 228
OVERSCAN_NOPS_BEFORE_LOW_BORDER		equ 23
OVERSCAN_NOPS_AFTER_LOW_LEFT_BORDER	equ 87
OVERSCAN_ENDSCREEN_NBSCANLINE		equ 44

; -----------------------------------------------------------------------------
; Macros
; -----------------------------------------------------------------------------	

; Launch the Timer A to remove the top border and start the fullscreen stuff
; To be called at the very beginning of the VBL !
; \1 = timer A routine
overscan_callTimA macro
	; Launch timer A to remove top border
	; 4 = 1/50 = 49152 Hz, roughly 100 * VBL frequency, so roughly every 3 lines (313 lines in a vbl)
	; 101 = 32 = good. Marche de 99 à 101
	; 94	522,893617		29,92960612
	; 95	517,3894737		30,24800618
	; 96	512		30,56640625
	; 97	506,7216495		30,88480632
	; 98	501,5510204		31,20320638
	; 99	496,4848485		31,52160645
	; 100	491,52		31,84000651
	; 101	486,6534653		32,15840658
	; 102	481,8823529		32,47680664
	; 103	477,2038835		32,79520671
	; 104	472,6153846		33,11360677
	; 105	468,1142857		33,43200684
	; 106	463,6981132		33,7504069
	; 107	459,364486		34,06880697
	; 108	455,1111111		34,38720703
	timer_enable "A",\1
	timer_start "A",4,100
	endm

; Timer A routine that removes the top border
; WARNING: d7 can no longer be used for demo (only for overscan)
overscan_upborder macro
	move.w #$2100,sr	; Wait for next HBL interrupt
	stop #$2100		; Because there is not yet any video counter
	move.w #$2700,sr	; Stop all interrupts
	
	timer_stop "A"
	nops 84-8
	movem.l d1-d2/a0,-(sp)	; 8+3*8=32 = 8 NOPS 

	; Open Upper Border
	; 60/50 Hz switch
	move.b #0,(HW_SHIFTER_ADR+$A).w
	nops 9
	move.b #2,(HW_SHIFTER_ADR+$A).w
	
	; Wait for the begin of the displayed screen
	lea (HW_SHIFTER_ADR+$9).w,a0
	moveq #63,d1
	moveq #2,d7	; 2=50Hz but also HiRes
	
.sync\@:	move.b (a0),d2 ;8 
	beq.s .sync\@; 8 (10) and 56 at border end
	; Then sync to the electron ray
	sub.b d2,d1 ;4
	lsr.b d1,d1 ;132
	; We are at 8 + 146 + 56 = 210

	; It should be 284, but we remove the nops before left border to coherent with other scanlines
	nops OVERSCAN_NOPS_END_TOP_BORDER-OVERSCAN_NOPS_BEFORE_LEFT_BORDER-9
	movem.l (sp)+,d1-d2/a0	; 12+3*8=36 = 9 NOPS
	; 210+284= 494
	endm
	
; Opens the left border
overscan_left_border macro
	; Pos = -18 (pos = 4: IF(71) H = TRUE)
	move.b d7,(HW_SHIFTER_ADR+$60).w	;switch mono for left border
	move.w d7,(HW_SHIFTER_ADR+$60).w
	endm

; Opens the right border
overscan_right_border macro
	; Pos = 366 (372: IF(60) H = FALSE, 376:IF(50) H = FALSE). Should be at 362 in fact... 4 cycles too much...
	; Anyway... it works
	move.w d7,(HW_SHIFTER_ADR+$A).w	; 60/50 Hz switch for right border
	move.b d7,(HW_SHIFTER_ADR+$A).w
	endm

overscan_low_left_border macro
	move.w d7,(HW_SHIFTER_ADR+$A).w	; 60Hz to start opening bottom border
	overscan_left_border
	move.b d7,(HW_SHIFTER_ADR+$A).w		; go back to 50Hz, the bottom border is opened
	endm
	
; -----------------------------------------------------------------------------
; Sub routines
; -----------------------------------------------------------------------------	



; -----------------------------------------------------------------------------	
	DATA

	
	
; -----------------------------------------------------------------------------	
	BSS
	

