; STE MOD Player Example
; DO NOT FORGET to update the sizing of mp_buffer according to mp_init_song_debug displayed results
; The Arctic Land (T.AL) 2019
; Contact: uko.tal@gmail.com or uko at http://www.atari-forum.com

	OUTPUT MPL.TOS

	OPT D+
	OPT X+
	; vasmm68k_mot -Ftos -monst -align -allmp


; Mod Play values to be set before includes
; Replay frequency (0=6258, 1=12517, 2=25033, 3=50066)
; MP_PLAY_FRQ	equ x
; Number of VBL buffers (power of 2 required)
; MP_NB_BUFFERS	equ x
; Mode of replay (0=Classical no volume, 1= Classical with volume,
;	  2=Oversampling no volume, 3=Oversampling with volume
;	  4=Extreme no volume, 5=Extreme with volume
;	  6=check (debug))
; MP_MODE 	equ x
MP_PLAY_FRQ	equ 1
MP_NB_BUFFERS	equ 4
MP_MODE 	equ 5

	INCDIR "..\Include\"
	INCLUDE "Symbols.s"
	INCLUDE "System.s"
	INCLUDE "MODPlay.s"
	
	TEXT

main:	; Supexec
	pea main_super
	move.w #XBIOS_SUPEXEC,-(SP)
	trap #14 
	addq.w #6,SP

	; Exit
	clr.w -(SP)
	trap #1
	
main_super:
	bsr assertSTE
	bsr save_system
	bsr init_system
		
	; Resolution & Scrs
	lea screen+255,a0	; Alignment
	move.l a0,d0
	and.l #~$FF,d0
	
	move.w #0,-(sp)
	move.l d0,-(sp)
	move.l d0,-(sp)
	move.w #XBIOS_SETSCREEN,-(sp)
	trap #14
	add.w #12,sp
	
	bsr init_system_cont
	
	; Start sound
	bsr mp_init
	
	; Initialise the Song
	move.w #$050,(HW_ST_PAL_ADR).w
	lea modfile,a0
	lea mp_buffer,a1
	bsr mp_init_song
	bsr mp_init_song_debug
	
	
	; A nice picture :-D
	bsr fill_image

	; Preload buffer
	REPT MP_NB_BUFFERS/2
	bsr mp_read_song
	bsr mp_premix
	bsr mp_mix
	ENDR
		
	; Set VBL
	syncVBL
	move.l #my_vbl,HW_SYS_VBL_VEC.w
	move.w #$2300,sr
	bsr mp_playtheblues
	
	; Wait key 
	bsr wait_space
	
	bsr restore_system
	
	rts


my_vbl:
	move.w #$000,(HW_ST_PAL_ADR).w
	; Wait for top border displayed
	nops (512/4)*40
	move.w #$700,(HW_ST_PAL_ADR).w
	jsr mp_read_song
	;move.w #$000,(HW_ST_PAL_ADR).w
	;nops (512/4)*5
	move.w #$070,(HW_ST_PAL_ADR).w	
	jsr mp_premix
	move.w #$000,(HW_ST_PAL_ADR).w
	
	; Wait for the beginning of the screen
	moveq #10,d0
	lea (HW_SHIFTER_ADR+$9).w,a0
.sync	move.b (a0),d1	; 8
	beq.s .sync	; 8 (10)
	sub.b d1,d0	; 4
	lsr.b d0,d0	; 6 + 2n
	move.w #$007,(HW_ST_PAL_ADR).w	
	bsr mp_mix
	move.w #$000,(HW_ST_PAL_ADR).w

	rte



; Fill the screen with dots to measure the CPU time
nbLines	equ 5
fill_image:
	move.l $44e,a1
	REPT 200/nbLines
	move.w #$FFFF,(a1)
	REPT 9
	lea 16(a1),a1
	move.w #$8000,(a1)
	ENDR
	lea 8(a1),a1
	move.w #$FFFF,(a1)
	lea 8(a1),a1
	lea SCRLINE_LEN*(nbLines-1)(a1),a1
	ENDR

	lea -SCRLINE_LEN(a1),a1
	move.w #$FFFF,(a1)
	REPT 9
	lea 16(a1),a1
	move.w #$8000,(a1)
	ENDR
	lea 8(a1),a1
	move.w #$FFFF,(a1)
	
	rts


; -----------------------------------------------------------------------------	
	DATA
; -----------------------------------------------------------------------------	
	even
;modfile	incbin "..\Mod\HARLYMUS.MOD"
modfile	incbin "..\Mod\buena vista.MOD"
;modfile	incbin "..\Mod\ILLUSION.MOD"

; -----------------------------------------------------------------------------
	BSS
; -----------------------------------------------------------------------------
; Buffer that will contain the mod for the replay routine
; Here is the max value for the 3 sample mods (get for Illusion) AND for the Extreme mode (most demanding one) 
mp_buffer	ds.l ($E183E+4)/4
mp_buffer_end

screen	ds.l 40*200+256/4,0
