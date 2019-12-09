; Fullscreen MOD Player sample for Atari STE
; Uses my "Extreme Oversampling" mode (faster !)
; The integration within the fullscreen core is of course not optimal here, and macro provided as examples
; should be split to avoid letting nops unused in each scanline !
; DO NOT FORGET to update the sizing of mp_buffer according to mp_init_song_debug displayed results 
; The Artic Land (T.AL) 2019
; Contact: uko.tal@gmail.com or uko at http://www.atari-forum.com

	OUTPUT MPFSEX.TOS

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
MP_PLAY_FRQ	equ 1	; 12.5 kz
MP_NB_BUFFERS	equ 4
MP_MODE 	equ 4	; Extreme no volume. Extreme modes only are managed here (4 or 5)

	INCDIR "..\Include\"
	INCLUDE "Symbols.s"
	INCLUDE "System.s"
	INCLUDE "Overscan.s"
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
	
	; Screen buffers
	; with alignment (for Falcon compatibility for example, but above all for fullscreen sync)
	move.l #scrBuff,d0
	add.l #255,d0
	and.l #~$FF,d0

	
	; Resolution & Scrs
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

	; Preload buffer
	REPT MP_NB_BUFFERS/2
	bsr mp_read_song
	bsr mp_premix
	bsr mp_mix
	ENDR
		
	; A nice picture :-D
	jsr clr_scr
	jsr fill_dotsimage
		
	; Set VBL
	syncVBL
	move.l #my_vbl,HW_SYS_VBL_VEC.w
	move.w #$2300,sr
	bsr mp_playtheblues
	
	; Wait key 
	bsr wait_space
	
	; Restore all
	bsr restore_system
	
	rts


my_vbl:
	overscan_callTimA my_timA
	set_palette #scrPalette
	
	; Reads the song, and prepare the mixing phase
	; We change bk colors but it should never appear to you, else there is a big trouble because this should be done
	; before the top border opening	!!!
	move.w #$700,(HW_ST_PAL_ADR).w
	bsr mp_read_song
	move.w #$070,(HW_ST_PAL_ADR).w	
	bsr mp_premix
	move.w #$000,(HW_ST_PAL_ADR).w

	rte
	
	ifeq MP_MODE-4
; Extreme No Volume
nb_mixs_per_line	equ OVERSCAN_NOPS_AFTER_LEFT_BORDER/MP_NOPS_MIX_EXT_MIX
nb_lines_for_mix	equ (MP_BUFF_SLICE/4)/nb_mixs_per_line
nb_mixs_missing		equ (MP_BUFF_SLICE/4)-(nb_lines_for_mix*nb_mixs_per_line)

my_timA:
	overscan_upborder
	
	; Start MOD replay
	; First line is special because we must initialise the mix process
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER-4
	move.w #$007,(HW_ST_PAL_ADR).w
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-MP_NOPS_MIX_EXT_INIT
	mp_mix_ext_init_m
	overscan_right_border	

	; Do the maximum mixes possible
	REPT nb_lines_for_mix
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-(nb_mixs_per_line)*MP_NOPS_MIX_EXT_MIX
	mp_mix_ext_mix_m nb_mixs_per_line
	overscan_right_border
	ENDR

	; Do the remaining mixes
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-(nb_mixs_missing)*MP_NOPS_MIX_EXT_MIX
	mp_mix_ext_mix_m nb_mixs_missing
	overscan_right_border

	; Now end the mix process
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-MP_NOPS_MIX_EXT_END-4
	mp_mix_ext_end_m
	move.w #$000,(HW_ST_PAL_ADR).w
	overscan_right_border
	
	REPT OVERSCAN_BOTTOM_BORDER_NBSCANLINE-1-nb_lines_for_mix-2
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER
	overscan_right_border
	ENDR
	
	
	; We are at scanline 228, we open the low border
	nops OVERSCAN_NOPS_BEFORE_LOW_BORDER
	overscan_low_left_border
	nops OVERSCAN_NOPS_AFTER_LOW_LEFT_BORDER
	overscan_right_border
	
	; Now continue normally
	REPT OVERSCAN_ENDSCREEN_NBSCANLINE
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER
	overscan_right_border
	ENDR
	set_palette #blackPalette
	
.exit	timer_eos "A"
	rte
	endif

	ifeq MP_MODE-5
; Extreme With Volume
nb_mixs_per_line	equ OVERSCAN_NOPS_AFTER_LEFT_BORDER/MP_NOPS_MIX_EXT_VOL_MIX
nb_lines_for_mix	equ (MP_BUFF_SLICE/4)/nb_mixs_per_line
nb_mixs_missing		equ (MP_BUFF_SLICE/4)-(nb_lines_for_mix*nb_mixs_per_line)

my_timA:
	overscan_upborder

	; Start MOD replay
	; First line is special because we must initialise the mix process
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER-4
	move.w #$007,(HW_ST_PAL_ADR).w
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-MP_NOPS_MIX_EXT_VOL_INIT_LEFT
dbg1	mp_mix_ext_vol_init_left_m
	overscan_right_border	

	; Do the maximum left mixes possible
	REPT nb_lines_for_mix
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-(nb_mixs_per_line)*MP_NOPS_MIX_EXT_VOL_MIX
	mp_mix_ext_vol_mix_m nb_mixs_per_line
	overscan_right_border
	ENDR

	; Do the remaining left mixes
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-(nb_mixs_missing)*MP_NOPS_MIX_EXT_VOL_MIX
	mp_mix_ext_vol_mix_m nb_mixs_missing
	overscan_right_border

	; Now initialise the mix process for the right channel
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-MP_NOPS_MIX_EXT_VOL_INIT_RIGHT
dbg2	mp_mix_ext_vol_init_right_m
	overscan_right_border	

	; Do the maximum right mixes possible
	REPT nb_lines_for_mix
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-(nb_mixs_per_line)*MP_NOPS_MIX_EXT_VOL_MIX
	mp_mix_ext_vol_mix_m nb_mixs_per_line
	overscan_right_border
	ENDR

	; Do the remaining right mixes
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-(nb_mixs_missing)*MP_NOPS_MIX_EXT_VOL_MIX
	mp_mix_ext_vol_mix_m nb_mixs_missing
	overscan_right_border

	; Now end the mix process
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER-MP_NOPS_MIX_EXT_VOL_END-4
	mp_mix_ext_vol_end_m
	move.w #$000,(HW_ST_PAL_ADR).w
	overscan_right_border
	
	REPT OVERSCAN_BOTTOM_BORDER_NBSCANLINE-1-nb_lines_for_mix-2-nb_lines_for_mix-2
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER
	overscan_right_border
	ENDR	
	
	; We are at scanline 228, we open the low border
	nops OVERSCAN_NOPS_BEFORE_LOW_BORDER
	overscan_low_left_border
	nops OVERSCAN_NOPS_AFTER_LOW_LEFT_BORDER
	overscan_right_border
	
	; Now continue normally
	REPT OVERSCAN_ENDSCREEN_NBSCANLINE
	nops OVERSCAN_NOPS_BEFORE_LEFT_BORDER
	overscan_left_border
	nops OVERSCAN_NOPS_AFTER_LEFT_BORDER
	overscan_right_border
	ENDR
	set_palette #blackPalette
	
.exit	timer_eos "A"
	rte
	endif


; Buffer sizes to copy image or show debug screen
nbBytesDest	equ 400/2	; 416 in fullscreen, we remove 16 pixels to have a symmetrical display
		; (overscans shifts display 8 pixels to the right)
nbBytesSrc	equ 416/2	; 416 for overscan picture
nbBytesBuff	equ 448/2	; 448 in fullscreen
nbLines	equ 270	; for debug only

stepLine	equ 5
fill_dotsimage:
	move.l $44e,a1
	; First Line is sync line
	move.w #$FFFF,(a1)
	REPT 9
	lea 16(a1),a1
	move.w #$8000,(a1)
	ENDR
	lea 8(a1),a1
	move.w #$AAAA,(a1)
	lea 8(a1),a1 
	
	REPT nbLines/stepLine
	move.w #$FFFF,(a1)
	REPT nbBytesSrc/16-1
	lea 16(a1),a1
	move.w #$8000,(a1)
	ENDR
	lea 8(a1),a1
	move.w #$FFFF,(a1)
	lea 8(a1),a1
	lea (nbBytesBuff-nbBytesSrc)(a1),a1
	lea nbBytesBuff*(stepLine-1)(a1),a1
	ENDR

	lea -nbBytesBuff(a1),a1
	move.w #$FFFF,(a1)
	REPT nbBytesSrc/16-1
	lea 16(a1),a1
	move.w #$8000,(a1)
	ENDR
	lea 8(a1),a1
	move.w #$FFFF,(a1)
	rts

clr_scr	move.l #scrBuff,a0
	move.l #scrBuff_end,a1
	moveq #0,d0
.loop	move.l d0,(a0)+
	cmp.l a0,a1
	bne.s .loop
	rts
	
; -----------------------------------------------------------------------------	
	DATA
; -----------------------------------------------------------------------------	
	even
scrPalette	dc.w $0000,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777,$0777
blackPalette	dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000

modfile	incbin "..\Mod\HARLYMUS.MOD"
;modfile	incbin "..\Mod\buena vista.MOD"

	BSS
; -----------------------------------------------------------------------------
; Screen buffer
scrBuff	ds.l ((224*274+256)/4)
scrBuff_end	ds.l 1

; Buffer that will contain the mod for the replay routine
; The size (module dependant) is given by mp_init_song_debug
; Here is the max value for the 2 sample mods (get for buena vista)
mp_buffer	ds.l ($5852E+4)/4
mp_buffer_end