; Sub routines to play MODs on STE specially conceived for fullscreen demos
; The Arctic Land (T.AL) 2019
; Contact: uko.tal@gmail.com or uko at http://www.atari-forum.com
;
; You are free to use these routines for your own productions, or to modify them as you want
; Just be kind and mention the credits !
;
; These sub routines have been designed to fulfill 2 main objectives:
; 1/ Allow me to code from scratch a replay routine (ok it has no interest for you ;-), but I never took the time
; in the golden age to implement one)
; 2/ Have a fast (hope so !) and above all fixed execution time routine that is easy to integrate in a fullscreen demo
; (static routine with NOPS replacement, not an unsynchronous Timer A based one like Ziggy's one)
;
; The second objective leads to have a lot of compromises versus memory, sound quality and
; compatibility (especially only few effects are managed, and some could not even be managed
; using the approach I have taken).
; It can be compared to some kind of optimised sprite routine that only works with some
; sizes, bitplane numbers, ... 
; In no case it is dedicated to be the ultimate replay routine !
; If you look for a very fast and compatible routine, please use the Lance/Paulo Simoes one that is impressive !
; (cf http://www.atari-forum.com/viewtopic.php?f=68&t=24718 for example)
; I could have tried to convert this Lance/Paulo routine in a fullcreen compatible one, but I think this would
; have been very difficult, since the execution time depends on the note played 
; and moreover this would have not meet Objective 1 ! :-D 
; Like for "beatdis" sprite routine where you will ask your graphist to respect some rules, using this routine
; will need you to do the same with your musician ! In my case this is not a difficult task, since I am also the guy
; that composes the musics :-p
; In the following, I will compare my routine performance with this Lance/Paulo routine. Please consider this comparison
; as some rough order of magnitude comparison. The objective is not to have some kind of "beatdis", but only to give
; reference figures for people that do not have them in mind. And Lance/Paulo routine is far more compatible with MODs
; than the current routines. In MOD Replay performance, often it is the average time that is presented. Here since I focus on
; replaying MODs in fullscreen demo, it is more the Peak time that will be of interest for a comparison.
; Remember that I want to have fixed execution time.
;
; It is only dedicated to STE, not ST, because of the better sounding of the STE.
; It may use a lot of memory according to choosing mode (bee below) and according to the module
; that is played (mp_init_song_debug will tell you how much), and
; especially it may require to break compatibilty with a 520STE (with other demo stuff at least a 1040STE may 
; be required). But since STE can be memory expanded easily (one of the first thing I did in the past), and that emulators
; are so accurate now (tested with Hatari and Steem SSE), it should not cause too much trouble.
;
; These routines can work at several replay frequencies by setting the MP_PLAY_FRQ at compilation time
; Replay frequency (0=6258, 1=12517, 2=25033, 3=50066)
; MP_PLAY_FRQ	equ x
; In order to have a correct behaviour, the routines shall be called in a 50 Hz VBL (simplest way if compatible) or using
; an equivalent timer if you prefer (Falcon users ? ;-))
;
; These routines use the STE DMA Replay, and therefore fills a buffer that is played by the DMA DAC
; In order to avoid any jitter in the buffer filling or any gaps of sound, the buffer contains several "slices", and at each 50 Hz call
; one of these slices is filled. The number of slices is defined by setting the MP_NB_BUFFERS variable at comilation time
; Number of VBL buffers (power of 2 required)
; MP_NB_BUFFERS	equ 4
; Theoretically a value of 2 should be enough to avoid any glitch or gap, but it you plan to change VBL hander during the demo to change
; of demo-screen, and if you have some time consuming operations between the VBL changes, you can increase this number of "slices" and call the
; replay routine several times consecutively to be sure all slices are filled. Then you can perform the time consuming operations, and there
; will be any sound stop during as many VBL as time buffers defined. Do not worry if you try to fill it while it is already filled, the routine
; will detect it, and do a fake filling (to be sure that the filling has always a fixed time execution, important in fullscreen)
;
; Typical usage of the routines (source examples can be found in the archive containg the current file):
; 1- Set MP_PLAY_FRQ, MP_NB_BUFFERS and MP_MODE
; 2- Call the sub routines to prepare the Replay
; 	- mp_init
; 	- mp_init_song
; 	(-mp_init_song_debug)
; 3- At each VBL call: 
;	mp_readSong, mp_premix
;	These 2 routines have variable execution time and therefore shall be called before reaching the top border
;	opening line when used in a fullscreen demo. The duration is about 3-4 scanlines max.
; 4- Still for each VBL call:
;	mp_mix
;	This routine performs the most consuming operations of the replay. But the execution time is perfectly constant once
;	the mode and frequency set (MP_PLAY_FRQ & MP_MODE). It can then be put in the core of a fullscreen routine (you know
;	when you replace the NOPS ;-)) If you do a fullscreen demo, of course you will not be able to directly call the mp_mix,
;	but you will have to split it accoring to the NOPS. Hopefully I have also provided macros that will help you for this !
;
; And before launching the VBL (so after step 2) you can prefill some buffers by calling as many times as needed the sequence:
; mp_readSong, mp_premix, mp_mix
;
;
; I initially planned to do only one kind of implementation ( the Extreme one), but step after step I had the envy to experiment
; more and more and finally my first idea has been the last implemented one !
; So the replay can be done using several modes and techniques that can be choosen using the MP_MODE variable at compilation
; Mode of replay (0=Classical no volume, 1= Classical with volume,
;	  2=Oversampling no volume, 3=Oversampling with volume
;	  4=Extreme no volume, 5=Extreme with volume
;	  6=check, for debug purposes only)
; MP_MODE 	equ x
;
; All these modes have limitations that are described below. Some of them could be removed (i.e. I have been too lazy to implement
; stuff I did not plan to use). But some of them are part of the DNA of the routine, and would require to change the technique used.
; Especially only the $F (speed) and $C (volume) commands are managed today
;
; As you can see some modes can manage volume and others not. In fact "with volume" means on the fly volume management (command $C).
; If you choose mode without volume, the static instrument volume will anyway be managed.
;
; ----------------------------------------------------------------
; 0 & 1 : Classical (without or with on the fly volume management)
; ----------------------------------------------------------------
; Very classical, and not the most efficient way to replay a MOD. The mixing puts each voice sample in the destination buffer at
; specific places corresponding to the voice frequency (the played note).
; In order to decide if a byte sample must be repeated or skipped to simulate the PAULA pitch shift, I have developed a Python script
; to generate the corresponding tables. This script is also delivered in the archive. The generated table is included in "MODFrqTable.s"
; Only the 36 classical notes are managed by this table, so no finetune nor possible portamento. But by generating a larger table this could
; be possible. I simply did not do it.
; Here is the typical code to perform the frq shift and the mixing (a1 and a2 points on the sample data, a3 and a4 onto the shift tables)
; 	move.b (a1),d0
; 	add.w (a3)+,a1
; 	add.b (a2),d0
; 	add.w (a4)+,a2
; 	move.b d0,(sp)+  (sp has a special behaviour it always stays even !)
; This implementation requires 96 cycles per output buffer sample (stereo word) -> 5.3 samples per scanline
; it is also available with a full on the fly volume management (mode 1), and it then requires
; 144 cycles per output buffer sample (word) -> 3.55 samples per scanline
;
; Here is comparison of this classical mode with Hacking Lance v10 on Illusion.mod
; Note that the Lance PEAK performance may vary according to the module wheras the current routine won't 
; FRQ                    %CPU        %CPU L/P(Peak)    %CPU L/P(Avg)
; 12.5 kHz (NoVol)        15%             15%              12.5%
; 12.5 kHz   (Vol)        23%             23%              18%
; 25.0 kHz (NoVol)        31%             20%              16%
; 25.0 kHz   (Vol)        46%             30%              22%
; 50.0 kHz (NoVol)        61%             27%              22%
; 50.0 kHz   (Vol)        92%             37%              28%
;
; Except @12,5 kHz, it's hard to face the generated code approach ! :-D
; So it's diffucult envisaging including this "classical" routine in a Full Screen demo
; above 25 kHz No Volume ! 

; ----------------------------------------------------------------
; 2 & 3 : Oversampling (without or with on the fly volume management)
; ----------------------------------------------------------------
; We you try to optimize such a replay routine, you can try to optimize the mixing part or the frequency shift part
; At first sight, there is no much room to optimize the mixing part that moreover has a constant execution time: read bytes and add them...
; but the frequency shift part can be optimised. That's the purpose of Hacking Lance generated code.
; Another posibility is to precalculate the shifting, as for sprites ! So let's precalculate samples by oversampling (or resampling) them
; for each frequency !
; What ?? Am I crazy ???? This will take too much memory !! there are 36 notes to play, portamento effects, finetune, ....
; Let's forget finetune, and portamento. I never use them... Ok that's not an excuse :-D
; There remain 36 possible notes ! Sure but in most musical scales only 8 on 12 notes are used, and you will probavly never use the 3 octaves
; because it would sound too bad for a same sample...
; Moreover some intruments are always played with the same note (drums !)
; And there are no more than 3 or 4 chords (if you use chord samples) in a song most of the time...
; So I did it ! It takes few seconds of pre-computation, and a lot of memory... too much in fact
; and this because of notes with low frequency (high period) that are very long !
; But is is highly probable that you will have in the same time long note duration and use a lot of notes for an instrument
; so I do a fake play of the module at the initialisation, and get the maximum used length for each sample note !
; And the result is that you will multiply by 3 the size of the module in memory. Still possible althought it will probably prohibit
; the use of a 520 STE...
; Using this technique, this leads to 48 cycles per output sample (word) ! Or 96 with volume.
;
; Here is comparison of my oversampling mode with Hacking Lance v10 on Illusion.mod 
; Note that the Lance PEAK performance may vary according to the module wheras the current routine won't 
; FRQ                    %CPU        %CPU L/P(Peak)    %CPU L/P(Avg)
; 12.5 kHz (NoVol)         8%             15%              12.5%
; 12.5 kHz   (Vol)        15%             23%              18%
; 25.0 kHz (NoVol)        15%             20%              16%
; 25.0 kHz   (Vol)        31%             30%              22%
; 50.0 kHz (NoVol)        31%             27%              22%
; 50.0 kHz   (Vol)        61%             37%              28%
;
; Hey, hey ! A lot better ! Faster than Hacking Lance up to 25kz No Volume !
; and equivalent up to 50 kHz No volume
; Definitively fullscreen demo compatible ! ;-)
; If you have enough memory left !!! :-D
;
; It was initially planned to add some basic filtering to the samples. Since we loose time & memory beacuse of precomutation
; why not adding more processing to improve the sound ? For example to interpolate values during frequency shifting
; which corresponds to some filtering. The method has been developped (commented at end of the source: mpint_process_spl_per_note_ovs)
; but results were not good (should have used more precision), and it really increased the time of computation,
; so I abandonned it. I have only kept the interpolation for higher frequencies, but honestly, I am not sure that it can be heard...
; so there is the MP_INTERPOLATE variable that allows to choose... I have kept this variable internal to this file (not as MP_MODE) 

; ----------------------------------------------------------------
; 4 & 5 : Extreme (without or with on the fly volume management)
; ----------------------------------------------------------------
; Here we are, this was my first idea of implementation (and the last I did !)
; The main basis is the same than "Oversampling" mode. But still pushed further (so the "Extreme" naming)
; We have seen that we cannot optimise more the CPU time to perform frequency shift because it is already precomputed
; So the only room for improvement is now for the mixing part. Not simple, we have to read a byte, then add it with another byte...
; Maybe we could load several bytes at a time ??? Using a move.L or move.W ? Yes but the execution time gained this way will
; be lost to extract the single bytes (swap and ROL required....) before adding them. Unless we do not extract them, and
; that we directly add the long words ! 4 bytes will be added at the same time ! And movep can save us for interleaving them next !
; 	move.l (a1)+,d0	; Get 4 bytes for Voice 1
;	add.l (a2)+,d0	; Add the 4 bytes of Voice 2
;	movep.l d0,(a5) ; Spread the bytes onto alternate destination bytes
;
; There are conditions for making this addition possible: the byte values must be unsigned (positive), and the result of the addition
; (for each byte) must not "set" the MSB (else the DAC will consider it as negative).
; So instead of converting samples to 7 bits, we'll have to convert them to 6 bits unsigned positive. Reducing bitdepth implies adding noise
; (not really a problem when you test), but also to reduce the output volume..... That's the price to pay if we want to optimize...
; and it's fast, because it makes 104 cycles per FOUR output buffer sample (word), so 26 per sample (word) -> 19.7 samples per scanline 
; almost twice faster than the Oversampling mode !
;
; Some points of attention: all sample addresses must be multiple of 4 ; moreover since all samples will be centered
; around the value 32, then all buffers containing zeros, must now be filled with 32, else you can hear crack when a sample is finished
; (because you will suddenly go from a sample centered around 32 to a value of zero)
; and the volume table has to be modified accordingly
;
; Now how can we manage on the fly volume ??? Not with a mapping table as previously (too much combinations with a long word !)
; First it is important to understand that as long as we apply a volume on a voice (in this case it is also applied on
; the empty buffer, see just above) , the volume can be reduced by reducing the bitdepth dynamic of the samples,
; no matter how they are centered. This means that samples varying between 0 and 63 (center = 32) can be volumed down of a half
; either between 16 and 47, or either for example between 0 and 31. It is equivalent as soon as the translation is identical
; for all the samples of the voice of course.
; My first idea was to reduce the dynamic using AND and/or OR masks. I have tried "logical" masks, and even brute force to
; find the best masks, but this didn't provide a good result: I managed to reduce the dynamic, but this also added a stepping
; effect that logically has generated noise and distorsion. So very bad sounding :-(
; The only solution I found is to shift right the samples. So only 7 levels of volume instead of 65... Anyway the result is not
; too bad for the modules I have tested.
; Of course shifting raises 2 problems:
; - first, the upper bytes will be shifted over the lower bytes: I solved this issue by masking (AND) these bits after shifting
; - secondly, the execution time depends on the number of shifts... not good for a routine which is supposed to have a fixed
;   execution time... So it is necessary to do an additional fake shifting that compensates the time of the initial shift.
; This produces the following code:
;	move.l (a1)+,d0		; read source data
;	move.l (a2)+,d1
;	
;	lsr.l d2,d0		; shift source data to reduce the volume
;	lsr.l d3,d1
;	and.l d4,d0		; mask the bits that go to the lower bytes
;	and.l d5,d1
;	
;	add.l d1,d0		; mix the data
;	movep.l d0,0(a5)	; copy the mixed data to the buffer
;
;	lsr.l d6,d1		; Compensate previous lsr.l to have fixed CPU time (48)
;
; Here is comparison of this extreme mode with Hacking Lance v10 on Illusion.mod 
; Note that the Lance PEAK performance may vary according to the module wheras the current routine won't 
; FRQ                    %CPU        %CPU L/P(Peak)    %CPU L/P(Avg)
; 12.5 kHz (NoVol)         4%             15%              12.5%
; 12.5 kHz   (Vol)        10%             23%              18%
; 25.0 kHz (NoVol)         9%             20%              16%
; 25.0 kHz   (Vol)        20%             30%              22%
; 50.0 kHz (NoVol)        18%             27%              22%
; 50.0 kHz   (Vol)        41%             37%              28%
;
; Really fast ! If you have enough memory and accept the (major) constraints... It's up to you to decide !
; But it is also easy to integrate in a fullscreen code !
; 
;
; ----------------------------------------------------------------
; MISC
; ----------------------------------------------------------------
; Using the AMIGA frequencies tables conversion, there is no note which replay frequency is exactly one of the STE DMA
; replay frequencies. If one has a sample which contains not a simple instrument but a music loop, there will be glitches
; unless having a 50kHz replay frequency. I have therefore generated tables which allow this. This only imply a few
; hertz of shifting, but it will greatly increase the sound quality ! The corresponding note is G-1 @ 6kHz, G-2 @ 12kHz
; and G-3 above.  This facility can be used by setting the MP_STE_FRQ variable (not set by default)
;
; ----------------------------------------------------------------
; Changes:
; 2019/12/23-FIX: In classical mode, samples with length in bytes > word size were cut...  
; 2020/01/04: Add the MP_STE_FRQ parameter to slightly shift the notes replay frequencies so as to be multiple
;             of STE frequencies (see MISC chapter)
; 2020/12/23-FIX: In Extreme mode, there was a little scratch for the first loop of an instrument


; -----------------------------------------------------------------------------
; TO BE SET IN MAIN PROGRAM
; -----------------------------------------------------------------------------	
; To be set in the main program 
; Replay frequency (0=6258, 1=12517, 2=25033, 3=50066)
; MP_PLAY_FRQ	equ 1
; Number of VBL buffers (power of 2 required)
; MP_NB_BUFFERS	equ 4
; Mode of replay (0=Classical no volume, 1= Classical with volume,
;	  2=Oversampling no volume, 3=Oversampling with volume
;	  4=Extreme no volume, 5=Extreme with volume
;	  6=check (debug))
; MP_MODE 	equ 0
; (Optional) Force the note replay frequencies to be adjusted vs STE Replay frequencies
; MP_STE_FRQ	equ 1 

; -----------------------------------------------------------------------------

	TEXT
	
	; This file must be included at the top of the main code
	; but also contains code... so we directly go to the main code !
	bra main
	
	; Warning, more than 10 macro parameters are required
	; vasmm68k_mot -Ftos -monst -align -allmp

; -----------------------------------------------------------------------------
; Constants
; -----------------------------------------------------------------------------	
; Conversion of the previous MP_PLAY_FRQ into real frequency and
; numbers of samples to play per VBL 
	ifeq MP_PLAY_FRQ-0
MP_PLAY_FRQVAL	equ 6258
MP_BUFF_SLICE	equ 128
mp_msg_frq	dc.b "Replay @ 6 kHz",$a,$d,0
	endif
	ifeq MP_PLAY_FRQ-1
MP_PLAY_FRQVAL	equ 12517
MP_BUFF_SLICE	equ 256
mp_msg_frq	dc.b "Replay @12 kHz",$a,$d,0
	endif
	ifeq MP_PLAY_FRQ-2
MP_PLAY_FRQVAL	equ 25033
MP_BUFF_SLICE	equ 512
mp_msg_frq	dc.b "Replay @25 kHz",$a,$d,0
	endif
	ifeq MP_PLAY_FRQ-3
MP_PLAY_FRQVAL	equ 50066
MP_BUFF_SLICE	equ 1024
mp_msg_frq	dc.b "Replay @50 kHz",$a,$d,0
	endif
	even
	
; Conversion of MP_MODE
; Initialises mp_mix to the corresponding routine
; Also fills MP_SPL_PER_NOTE (one sample copy per note (oversampling) OR not)
; MP_DYN_VOL (dynamic volume) and MP_MOVEL (uses MOVE.L reads)
	ifeq MP_MODE-6
mp_mix	equ mp_mix_check
MP_SPL_PER_NOTE	equ 0
MP_DYN_VOL	equ 0
MP_MOVEL	equ 0
mp_msg_mode	dc.b "Mode: Check",$a,$d,0
	endif
	ifeq MP_MODE-5
mp_mix	equ mp_mix_ext_vol
MP_SPL_PER_NOTE	equ 1
MP_DYN_VOL	equ 2	; Extreme mode for volume
MP_MOVEL	equ 1
mp_msg_mode	dc.b "Mode: Extreme + Volume",$a,$d,0
	endif
	ifeq MP_MODE-4
mp_mix	equ mp_mix_ext
MP_SPL_PER_NOTE	equ 1
MP_DYN_VOL	equ 0
MP_MOVEL	equ 1
mp_msg_mode	dc.b "Mode: Extreme (No volume)",$a,$d,0
	endif
	ifeq MP_MODE-3
mp_mix	equ mp_mix_ovs_vol
MP_SPL_PER_NOTE	equ 1
MP_DYN_VOL	equ 1
MP_MOVEL	equ 0
mp_msg_mode	dc.b "Mode: Oversampling + Volume",$a,$d,0
	endif
	ifeq MP_MODE-2
mp_mix	equ mp_mix_ovs
MP_SPL_PER_NOTE	equ 1
MP_DYN_VOL	equ 0
MP_MOVEL	equ 0
mp_msg_mode	dc.b "Mode: Oversampling (No volume)",$a,$d,0
	endif
	ifeq MP_MODE-1
mp_mix	equ mp_mix_std_vol
MP_SPL_PER_NOTE	equ 0
MP_DYN_VOL	equ 1
MP_MOVEL	equ 0
mp_msg_mode	dc.b "Mode: Standard + Volume",$a,$d,0
	endif
	ifeq MP_MODE-0
mp_mix	equ mp_mix_std
MP_SPL_PER_NOTE	equ 0
MP_DYN_VOL	equ 0
MP_MOVEL	equ 0
mp_msg_mode	dc.b "Mode: Standard (No volume)",$a,$d,0
	endif
	even

; To add some interpolation in Oversampling mode (for frequencies higher than the replay one)
MP_INTERPOLATE	equ 1
	
; Force STE Replay Frequency
	ifnd MP_STE_FRQ
MP_STE_FRQ	equ 0
	endif
	
; -----------------------------------------------------------------------------
; Internal Buffer description
; First Samples table
MP_SPL_TBL_O	equ 0
; For each instrument (31)
; And for each note (36)
; 0.W Note
; 2.W Volume
; 4.L Start Address
; 8.L End Adress
MP_SPL_SO_NOTE	equ 0
MP_SPL_SO_VOL	equ 2
MP_SPL_SO_SADR	equ 4
MP_SPL_SO_EADR	equ 8
MP_SPL_SO_LADR	equ 12
MP_SPL_SO_LEN	equ MP_SPL_SO_LADR+4
MP_SPL_INST_LEN	equ MP_SPL_SO_LEN*36

; Then the Sequence of patterns
; That contains 128.L : adress of each pattern or 0 if the song is finished. We add 2 items (so 130), to be sure there are zeros
MP_PAT_SEQ_O	equ MP_SPL_TBL_O+31*36*MP_SPL_SO_LEN
; Then the patterns desciption
MP_PAT_TBL_O	equ MP_PAT_SEQ_O+(128+2)*4
; For each of the patterns
; 64 lines of 4 Rows of:
; 0.L Sample Address in the MP_SPL_TBL
; 4.B Command
; 5.B Command Value
MP_PATLV_SO_SADR equ 0
MP_PATLV_SO_CMD	equ 4
MP_PATLV_SO_VAL	equ 5
MP_PATLV_SO_LEN	equ MP_PATLV_SO_VAL+1

MP_PATL_SO_LEN	equ MP_PATLV_SO_LEN*4
MP_PAT_SO_LEN	equ MP_PATL_SO_LEN*64


; -----------------------------------------------------------------------------
; Macros
; -----------------------------------------------------------------------------	
; Set the DMA frequency according to MP_PLAY_FRQ, and mode is STEREO
; \1 = register containing DMA Base address
set_dma_stereo_frq: macro 
	and.b #$7C,HW_SND_MODFRQ_O(\1)
	or.b #MP_PLAY_FRQ,HW_SND_MODFRQ_O(\1)
	endm
	
mpint_setval4chn: macro
	REPT 4
	move.l \1,(\2)+
	ENDR
	endm
; -----------------------------------------------------------------------------
; Sub routines
; -----------------------------------------------------------------------------
; The call order for the routines is	
; 1- mp_init
; 2- mp_init_song
; (3-mp_init_song_debug)
; Then before VBL or at each VBL:
; mp_readSong, mp_premix, mp_mix

; -----------------------------------------------------------------------------
; Initialise the DMA Sound, and the mix player parameters
; -----------------------------------------------------------------------------
mp_init:	lea HW_SND_ADR.w,a0
	
	; Infinite loop on buffer
	move.l #mp_sndbuff,d0
	move.w d0,d1
	swap d0
	move.b d0,HW_SND_H_SADR_O(a0)
	movep.w d1,HW_SND_M_SADR_O(a0)
	
	move.l #mp_end_sndbuff,d0
	move.w d0,d1
	swap d0
	move.b d0,HW_SND_H_EADR_O(a0)	
	movep.w d1,HW_SND_M_EADR_O(a0)

	; Stero only
	set_dma_stereo_frq a0

	; Slices counters
	move.w #0,mp_1stFreeSlice		; First slice to fill	
	move.w #MP_NB_BUFFERS,mp_curSlice 	; we indicate that the read is not started yet
	
	; Set all voices to an existing buffer (fake) in case nothing to play
	move.l #mp_fake_sndbuff,d0
	; Start
	lea mp_vceSrcPos,a0
	move.l d0,(a0)+
	move.l d0,(a0)+
	move.l d0,(a0)+
	move.l d0,(a0)+
	; Loop at the start
	lea mp_vceSrcLoop,a0
	move.l d0,(a0)+
	move.l d0,(a0)+
	move.l d0,(a0)+
	move.l d0,(a0)+
	; Duration = 1 slice
	add.l #MP_BUFF_SLICE,d0
	lea mp_vceSrcEnd,a0
	move.l d0,(a0)+
	move.l d0,(a0)+
	move.l d0,(a0)+
	move.l d0,(a0)+

	; Prepare the inverted period table
	; to get the index corresponding to the note period
	lea mp_period_table,a0
	lea mp_inv_period,a1
	moveq #0,d0
	moveq #36-1,d7
.loopInvPeriod	move.w (a0)+,d1
	move.b d0,(a1,d1.w)
	addq.b #1,d0
	dbf d7,.loopInvPeriod

	; Creates the volume table
	bsr mpint_make_voltab
	
	ifne MP_MOVEL
	; Fills the fake buffer with shifted 0 samples (i.e. 32)
	move.l #mp_end_fake_sndbuff-mp_fake_sndbuff-1,d7
	lea mp_fake_sndbuff,a0
.zeroFake	move.b #32,(a0)+
	dbf d7,.zeroFake
	endif

	rts

; -----------------------------------------------------------------------------
; Initialises the song
; i.e. convert the mod into a specific format with more usable data
; and expanded samples (according to the mode)
; a0 = address of the MOD
; a1 = address of the destination buffer
; Returns:
; d0 = size of the buffer, or 0 if an error occured
; -----------------------------------------------------------------------------
mp_init_song:
	moveq #0,d0
		
	; Test the module type
	move.l 1080(a0),d7
	cmp.l #'M.K.',d7
	bne .end

	; The buffer will at least contain the sample description buffers
	; And the patterns sequence
	move.l #MP_PAT_TBL_O,d0	; TO BE KEPT
	move.l a1,mp_bufferAddr
	
	; Read pattern sequence
	moveq #0,d7
	moveq #0,d5
	move.b 950(a0),d7	; Song length
	subq.b #1,d7
	lea 952(a0),a5		; Sonq sequence
	lea MP_PAT_SEQ_O(a1),a6	; idem in buffer
.loopSeq	moveq #0,d6
	move.b (a5)+,d6		; Pattern number
	cmp.b d5,d6		; and we search for the highest pattern number
	blo.s .noMax
	move.b d6,d5		; d5=Max pattern number
.noMax	mulu #MP_PAT_SO_LEN,d6	; d6=pattern address offset
	add.l d0,d6		; we add the offset of the pattern list
	add.l a1,d6		; and the buffer address
	move.l d6,(a6)+
	dbf d7,.loopSeq
 
	; Convert Patterns
	addq.b #1,d5		; pattern number in d5 is included
	lsl.l #6,d5		; 64 lines per pattern
	subq.l #1,d5
	lea 1084(a0),a5		; Pattern description
	move.l d0,a6		; Patterns start in buffer
	add.l a1,a6		; And address of the buffer
	moveq #0,d1		; Clean
	moveq #0,d2
	moveq #0,d3
	moveq #0,d4
	moveq #0,d6
	lea mp_inv_period,a2

.loopPatterLine	moveq #3,d7		; 4 voices per line
.loopVoice	moveq #0,d1
	moveq #0,d2
	move.b (a5)+,d1		; 4 bytes of voice description
	move.b (a5)+,d2
	move.b (a5)+,d3
	move.b (a5)+,d4
	; Decode the voice
	; sample_index = ((val1 & 0xF0) | (val3 >> 4)) - 1
	; period = val2 | ((val1 & 0x0F) << 8)
	; command = val3 & 0x0F
	; command_value = val4

	; Period
	move.b d1,d6
	and.b #$F,d6
	lsl.w #8,d6
	or.w d6,d2		; d2 = period
	beq.s .noNote

	; Sample index
	move.b d3,d6
	lsr.b #4,d6
	and.b #$F0,d1
	or.b d6,d1
	beq.s .noNote		; No instrument -> No note to avoid crash
	subq.b #1,d1		; d1 = sample index
	mulu #MP_SPL_INST_LEN,d1	; Offset of the sample table 
	add.l a1,d1		; More the buffer address
	
	moveq #0,d6
	move.b (a2,d2.w),d6	; Index according to period
	mulu #MP_SPL_SO_LEN,d6
	add.l d6,d1		; address of the note within the sample
	move.l d1,MP_PATLV_SO_SADR(a6)	; Put in the buffer
	move.l d1,a4
	move.w d2,MP_SPL_SO_NOTE(a4)	; Write the period in the sample table
	
.noNote	; Command
	and.w #$F,d3
	cmp.b #$F,d3		; Speed
	beq.s .cmdSpeed
	
	ifne MP_DYN_VOL
	cmp.b #$C,d3		; Volume
	beq.s .cmdSet
	endif
	
	; Treat here all other commands
	; or ignore them
	lea mp_fx_met,a3
	add.w d3,a3
	cmp.b #0,d3		; Specific case of Arpeggio
	bne.s .used
	cmp.b #0,d4
	beq.s .endVoice
	
.used	move.b #1,(a3)	
	bra.s .endVoice	

.cmdSpeed	cmp.b #$20,d4		; >$20 is BPM and not Speed
	bhs.s .endVoice		; Not managed
	subq.b #1,d4		; 0 is the tick when the row is read

.cmdSet	move.b d3,MP_PATLV_SO_CMD(a6)	; Put in the buffer
	move.b d4,MP_PATLV_SO_VAL(a6)
	
.endVoice	lea MP_PATLV_SO_LEN(a6),a6
	dbf d7,.loopVoice
	dbf d5,.loopPatterLine
	move.l a6,d0		
	sub.l a1,d0		; TO BE KEPT

	; At this stage d0 is updated with the patterns description length
	; And a5 points to the begin of the source samples area
	; a6 also points to the begin of the destination samples area
 
	; Process the samples
	lea 20(a0),a0		; Current source sample description	
	ifeq MP_SPL_PER_NOTE
	bsr mpint_process_spl_global
	else
	bsr mpint_analyse_song
	bsr mpint_process_spl_per_note
	endif

 	move.l a6,d0		
	sub.l a1,d0		; TO BE KEPT
 	
	; Default speed
	move.w #6-1,mp_curSpeed
	move.w #0,mp_curTick
	
	; Start position
	lea MP_PAT_SEQ_O(a1),a0
	move.l a0,mp_curSeqPos
	move.w #0,mp_curRow
	
	; WARNING d0 returns the size of the buffer
.end	rts

; Internal sub routine to process samples and have a copy of each sample for each note
; it is the one taht do the "oversampling" job
; a0 (IO) = Address in the source mod pointing to sample descriptions 
; a1 (I) = Address of the buffer
; a5 (IO) = Address in the source mod pointing to the sample data
; a6 (IO) = Address in the buffer where to copy sample data to

; Internals
; a2 = volume table
; a3 = backup a6
; a4 = Address if the destination sample description
; d0 = period of the note
; d1 = loop start
; d2 = loop length
; d4 = sample length
; d5 = volume address
; d6 = Note dbf
; d7 = Sample dbf

mpint_process_spl_per_note:
	; Process the samples
	lea MP_SPL_TBL_O(a1),a4	; Beginning of the buffer samples list desciption
	moveq #31-1,d7		; Max number of samples

	ifne MP_MOVEL
	addq.l #3,a6		; Prepare to set sample address to multiple of 4 (see below)
	endif

.nextSample	moveq #0,d4
	moveq #0,d1
	moveq #0,d2
	move.w 22(a0),d4	; Length of the source sample in words
	lsl.l #1,d4		; in bytes now
	move.w 26(a0),d1	; Start loop in words
	lsl.l #1,d1
	move.w 28(a0),d2	; Loop length in words
	lsl.l #1,d2	
	add.l d1,d2		; Compute loop end
	add.l a5,d2
	
	moveq #36-1,d6		; Number of notes per sample
.nextNote	move.w MP_SPL_SO_NOTE(a4),d0	; Period of the note
	beq .noteNotUsed
	
	; Copy sample
	ifne MP_MOVEL
	move.l a6,d5		; Set sample address to multiple of 4
	and.b #$FC,d5
	move.l d5,a6
	endif
	move.l a6,a3		; a3=beginning of the sample in the buffer
	tst.l d4
	beq .noCopy
	
	move.l mp_volume_tab,a2	; Volume table in case of no dynamic volume
	moveq #0,d5
	move.b 25(a0),d5	; Sample volume
	lsl.w #8,d5
	add.l a2,d5	

	lea mp_inv_period,a2	; Period->Idx table
	moveq #0,d3
	move.b (a2,d0.w),d3
	move.w d3,d0
	lsl.w #2,d3
	move.w d0,MP_SPL_SO_NOTE(a4)	; Index according to period for acccessing long word table
	lea mp_frqtable,a2
	move.l (a2,d3.w),a2	; Adress of the frequency table for the note

	; If no looping, we simply copy the sample to its end
	; If looping, we first copy to the loop starting point, then a second time to the loop end 
	tst.l d1		; Loop sample ?
	beq.s .noLoop
	move.l d1,d3		; First copy to the loop start
	subq.l #1,d3		; (Excluded)
	bra.s .copySample

.noLoop	move.l d4,d3

.copySample	add.l a5,d3
	movem.l d4/d7/a0/a1/a3/a5,-(sp)
	move.l a2,a1		; Backup a2
	
	; Get the maximum oversampled note length 
	move.l MP_SPL_SO_EADR(a4),a0	; This value has been temporarly used
	add.l a6,a0
	move.l #0,MP_SPL_SO_EADR(a4)
	moveq #0,d7
	
	move.l #MP_BUFF_SLICE-1,d0
.copySampleByte	ifne MP_INTERPOLATE
	move.b d5,d4		; Backup d5
	endif
	move.b (a5),d5
	
	ifeq MP_MOVEL
	asr.b #1,d5		; Convert to 7 bits
	else		
	asr.b #2,d5		; Convert to 6 bits
	add.b #32,d5		; Unsigned
	endif
	
	ifeq MP_DYN_VOL
	move.l d5,a3
	move.b (a3),d5
	endif
	
	ifne MP_INTERPOLATE
	cmp.b #1,d7		; Loop if previous jump > 1
	bls.s .doCopy
	add.b d5,d4		; Compute and push the median value
	asr.b #1,d4
	move.b d4,(a6)+
	bra.s .getJump
	endif
	
.doCopy	move.b d5,(a6)+
.getJump	move.w (a2)+,d7
	add.w d7,a5
	dbf d0,.noFrqTableEnd	; The table frequency has a limited length, we must loop on it
	move.l a1,a2
	move.l #MP_BUFF_SLICE-1,d0
	
.noFrqTableEnd	cmp.l a0,a6
	bhs.s .noLoopCtd
	cmp.l d3,a5
	bls.s .copySampleByte

	; Test if we exit the loop for the second time (i.e. start loop to end)
	move.l MP_SPL_SO_LADR(a4),a3	; Starting point
	cmp.l #0,a3
	bne.s .startCopyLoop 	; We have already set the start of the loop -> we were in the 2nd loop

	; We have finished the first copy part 
	; Test if the copy was up to the looping start
	tst.l d1
	beq.s .noLoopCtd	; No looping
	
	ifne MP_MOVEL
	move.l a6,d3		; Set looping point multiple of 4
	and.b #$FC,d3
	move.l d3,a6
	endif
	move.l a6,MP_SPL_SO_LADR(a4)
	move.l d2,d3		; Copy up to loop end
	moveq #0,d7
	bra.s .copySampleByte

	; Manage loop (cont'd) : Fills the buffer just after the sample with the begin of the loop
.startCopyLoop	move.l #(MP_BUFF_SLICE+4)-1,d3	; Copy the loop
.copyLoop	move.b (a3)+,(a6)+
	dbf d3,.copyLoop
	
	; Correct the pointers
	sub.l #(MP_BUFF_SLICE+4),a6
	ifne MP_MOVEL	
	bra.s .restore
	endif

.noLoopCtd	; If we jump directly to here, then there is no looping
	ifne MP_MOVEL	
	move.l #(MP_BUFF_SLICE+4)-1,d3	; Fills the margin buffer with 0 shifted (i.e. 32)
	move.b #32,d4
.loopZero	move.b d4,(a6)+
	dbf d3,.loopZero
	sub.l #(MP_BUFF_SLICE+4),a6
.restore	
	endif
	movem.l (sp)+,d4/d7/a0/a1/a3/a5

.noCopy	move.l a3,MP_SPL_SO_SADR(a4)	; Start address	
	ifne MP_MOVEL
	move.l a6,d5		; Set looping point multiple of 4
	addq.l #3,d5
	and.b #$FC,d5
	move.l d5,MP_SPL_SO_EADR(a4)	; Warning: the sample end is here, and not after the margin (cf below)
	else
	move.l a6,MP_SPL_SO_EADR(a4)	; Warning: the sample end is here, and not after the margin (cf below)
	endif
	moveq #0,d0
	move.b 25(a0),d0	; Sample volume
	move.w d0,MP_SPL_SO_VOL(a4)		
	
	; to manage slice length and potential optimisation of read per 4 bytes
	lea (MP_BUFF_SLICE+4)(a6),a6
	
.noteNotUsed	lea MP_SPL_SO_LEN(a4),a4
	dbf d6,.nextNote
	
	add.l d4,a5
	lea 30(a0),a0
	dbf d7,.nextSample
	
	rts

; Internal sub routine to process samples and use the same sample for each note
; (Classical modes)
; a0 (IO) = Address in the source mod pointing to sample descriptions 
; a1 (I) = Address of the buffer
; a5 (IO) = Address in the source mod pointing to the sample data
; a6 (IO) = Address in the buffer where to copy sample data to
mpint_process_spl_global:
	; Process the samples
	lea MP_SPL_TBL_O(a1),a4	; Beginning of the buffer samples list desciption
	moveq #31-1,d7		; Max number of samples
.nextSample	moveq #0,d4
	moveq #0,d1
	moveq #0,d2
	move.w 22(a0),d4	; Length of the source sample in words
	lsl.l #1,d4		; in bytes now
	move.w 26(a0),d1	; Start loop in words
	lsl.l #1,d1
	move.w 28(a0),d2	; Loop length in words
	lsl.l #1,d2
	
	move.l a6,a3		; a3=beginning of the sample in the buffer
	tst.l d4
	beq.s .noCopy

	move.l mp_volume_tab,a2	; Volume table in case of no dynamic volume
	moveq #0,d5
	move.b 25(a0),d5	; Sample volume
	lsl.w #8,d5
	add.l a2,d5

	tst.l d1		; Loop sample ?
	beq.s .noLoop
	add.l d1,d2
	move.l d2,d3		; Sample end = sample loop end
	bra.s .copySample
	
.noLoop	move.l d4,d3

.copySample	add.l a5,d3	
.copySampleByte	move.b (a5)+,d5
	asr.b #1,d5
	ifeq MP_DYN_VOL
	move.l d5,a2
	move.b (a2),(a6)+
	else
	move.b d5,(a6)+
	endif
	cmp.l d3,a5
	bls.s .copySampleByte	; do not use dbcc because it is word limited
	
	; Manage loop (cont'd) : Fills the buffer just after the sample with the begin of the loop
	tst.l d1
	beq.s .noLoopCtd
	move.l a3,a2		; Go to the start loop point
	add.l d1,a2
	move.l #(2*MP_BUFF_SLICE+4)-1,d3	; Copy the loop
.copyLoop	move.b (a2)+,(a6)+
	dbf d3,.copyLoop
	; Correct the pointers
	sub.l d2,d4	; loop end can be before sample end
	add.l d4,a5
	sub.l #(2*MP_BUFF_SLICE+4),a6
	
.noLoopCtd	nop		; To keep relevant label names

	; Set the corresponding pointers to each note
.noCopy	lea mp_inv_period,a2	; Period->Idx table
	moveq #36-1,d6		; Number of notes per sample
.nextNote	move.w MP_SPL_SO_NOTE(a4),d5	; Period of the note
	beq.s .noteNotUsed
	
	; Replace the Period by its index
	moveq #0,d4
	move.b (a2,d5.w),d4
	lsl.w #2,d4
	move.w d4,MP_SPL_SO_NOTE(a4)	; Index according to period for acccessing long word table
	move.l a3,MP_SPL_SO_SADR(a4)	; Start address	
	move.l a6,MP_SPL_SO_EADR(a4)	; Warning: the sample end is here, and not after the margin (cf below)
	move.b 25(a0),d4	; Sample volume
	move.w d4,MP_SPL_SO_VOL(a4)		
	tst.l d1		; looping ?
	beq.s .noteNotUsed
	move.l a3,MP_SPL_SO_LADR(a4)	
	add.l d1,MP_SPL_SO_LADR(a4)	; Loop address
	
.noteNotUsed	lea MP_SPL_SO_LEN(a4),a4
	dbf d6,.nextNote
	
	; to manage slice length and potential optimisation of read per 4 bytes
	; wheras for the per_note routine, we add 2 BUFF_SLICE because frequency shift will override the first buffer
	lea (2*MP_BUFF_SLICE+4)(a6),a6
	
	lea 30(a0),a0
	dbf d7,.nextSample
	rts

; Internal sub routine to generate a volume table
; mp_volume_tab points to $2100 words but only $2000 required from $100 boundary
; THIS SUB ROUTINE IS DIRECTLY COPIED FROM Hacking_Lance_13 pt_src50_2013.s source file
; i.e. the replay routine modified by Paulo Simoes and originally by Lance
; http://www.atari-forum.com/viewtopic.php?f=68&t=24718
; It was useless to redo something that is already perfectly done !
; I only adapted the specific case of MP_MOVEL (6 bits unsigned case)
mpint_make_voltab:	
	lea mp_volume_tab,a0
	move.l (a0),d0		; get pointer
	andi.w #$ff00,d0	; clear low byte
	add.l #$100,d0		; round to upword 256 bytes boundary
	move.l d0,(a0)		; set new pointer
	move.l d0,a0		; a0 points to start of volume table
	moveq #0,d0		; volume levels 0 to $40
	moveq #0,d1		; first samples from 00 to $3F

.mt_clop0	
	move.w d1,d2		; D1 counter
	ext.w d2		; D2 = D1 with sign
	ifne MP_MOVEL
	sub.w #32,d2		; we are in 6 bits unsigned, 0 is shifted to 32
	endif
	muls.w d0,d2		; * volume level
	asr.w #6,d2		; / 64	6bit*6bit / 64 = 12 bit / 6bit = max $3F
	ifne MP_MOVEL
	add.b #32,d2		; 0 is shifted to 32
	endif
	move.b d2,(a0)+		; store adjusted result to byte
	addq.w #1,d1
	cmp.w #$40,d1
	bne.s .mt_clop0
	lea $80(a0),a0		; now the same for negative values
	move.w #$c0,d1
.mt_clop1	
	move.w d1,d2		; from $C0 to $FF
	ext.w d2
	muls.w d0,d2
	asr.w #6,d2
	move.b d2,(a0)+
	addq.w #1,d1
	cmp.w #$100,d1
	bne.s .mt_clop1

	moveq #0,d1
	addq.w #1,d0
	cmp.w #$41,d0
	bne.s .mt_clop0
	rts


; Internal sub routine to fake playing the song
; in order to get some additional optimisations
; especially when oversampling, we try to reduce the length of each resampled instrument
; note to what is really necessary for the song 
; Internal:
; a0 = mp_buffer
; a1 = current sequence position
; a2 = current line address
; a3 = tmp
; a6 = temporary buffer
; d4 = tmp
; d5 = current row
; d6 = current speed
; d7 = current tick
mpint_analyse_song:
	movem.l d0-d7/a0-a6,-(sp)

	lea mp_buffer,a0
	lea mpint_analyse_song_buff,a6

	; Default speed
	moveq #6-1,d6		; mp_curSpeed
	moveq #0,d7		; mp_curTick
	
	; Start position
	lea MP_PAT_SEQ_O(a0),a1	; mp_curSeqPos	
	moveq #0,d5		; mp_curRow

	; We only read a row every curSpeed ticks
.loopRead	tst.w d7
	bne .decreaseTick
	move.w d6,d7		; Reset tick with speed value
	
	; Get the pattern & row
	move.l (a1),a2		; Current Pattern address
	cmp.l #0,a2		; End of the song ?
	beq .endSong		; If pattern address is null, then this is the end
	moveq #0,d4
	move.w d5,d4
	mulu #MP_PATL_SO_LEN,d4
	add.l d4,a2		; a2 = current line address
	
	; Increment row for the next loop
	addq.w #1,d5
	and.w #64-1,d5		; 64 row per pattern
	bne.s .noPatternChnge
	
	; Next pattern
	lea 4(a1),a1
	
.noPatternChnge	
	; Read all voices
	lea mpint_analyse_song_buff,a6
	moveq #3,d4
	
.loopVoice	move.l MP_PATLV_SO_SADR(a2),a3	; Sample description address
	cmp.l #0,a3
	beq.s .doCmd	; There is no note to play, so we let the on-going sample playing if there is one
	
	move.l a3,(a6)	; Set sample address
	move.l #0,16(a6) ; Duration
		
.doCmd	; Look for command
	move.b MP_PATLV_SO_CMD(a2),d0
	; Speed ?
	cmp.b #$F,d0
	bne.s .nextVoice
	move.b MP_PATLV_SO_VAL(a2),d6	; Change speed
	move.w d6,d7		; And set tick accordingly

.nextVoice	; Next voice
	lea MP_PATLV_SO_LEN(a2),a2
	lea 4(a6),a6
		
.dbf	dbf d4,.loopVoice
	bsr mpint_analyse_song_process_durations	
	bra.s .loopRead

.decreaseTick	subq.w #1,d7
	bsr mpint_analyse_song_process_durations
	bra.s .loopRead
	
.endSong	movem.l (sp)+,d0-d7/a0-a6
	rts

mpint_analyse_song_buff
	ds.l 8	; 4L for sample addresses, and 4L for sample current duration

; Increase the durations, and check if current duration is maximum duration 
mpint_analyse_song_process_durations:
	movem.l d0-d1/d7/a0-a1,-(sp)
	lea mpint_analyse_song_buff,a0
	moveq #3,d7
.loopVoice	move.l (a0),a1	; Sample note description address
	cmp.l #0,a1
	beq.s .nextVoice
	
	move.l 16(a0),d0	; current duration
	add.l #MP_BUFF_SLICE,d0	; increase duration
	move.l d0,16(a0)
	
	move.l MP_SPL_SO_EADR(a1),d1	; Get max duration (temporary use of MP_SPL_SO_EADR)
	cmp.l d0,d1
	bhi.s .nextVoice
	move.l d0,MP_SPL_SO_EADR(a1)
	
.nextVoice	lea 4(a0),a0
	dbf d7,.loopVoice

	movem.l (sp)+,d0-d1/d7/a0-a1
	rts


; -----------------------------------------------------------------------------	
; Manages the error of mp_init_song
; or displays the required memory size for the buffer
; -----------------------------------------------------------------------------
mp_init_song_debug:
	tst.l d0
	beq .quit

	; Display size
	dxToText d0,(mp_size_msg1+1)
	pea mp_size_msg
	move.w #GEMDOS_CCONWS,-(sp)
	trap #1
	addq.l #6,sp
	
	; Display FX found in the MOD but not managed by the current routine
	lea mp_fx_met,a0
	lea mp_fx_msg1,a1
	moveq #16-1,d7
.nextFx	tst.b (a0)+
	beq.s .found
	add.l #1,a1
	bra.s .next
.found	move.b #' ',(a1)+
.next	dbf d7,.nextFx

	pea mp_fx_msg
	move.w #GEMDOS_CCONWS,-(sp)
	trap #1
	addq.l #6,sp

	; Display Frequency
	pea mp_msg_frq
	move.w #GEMDOS_CCONWS,-(sp)
	trap #1
	addq.l #6,sp

	; Display mode
	pea mp_msg_mode
	move.w #GEMDOS_CCONWS,-(sp)
	trap #1
	addq.l #6,sp
	
	bsr wait_space	
	
	bra.s .end
	
.quit	bsr restore_system

	pea mp_err_msg
	move.w #GEMDOS_CCONWS,-(sp)
	trap #1
	addq.l #6,sp
	
	bsr wait_space
	
	clr.w -(sp)
	trap #1

.end	rts


; -----------------------------------------------------------------------------
; Hear that sound !
; -----------------------------------------------------------------------------
mp_playtheblues: or.b #3,(HW_SND_ENABLE_O+HW_SND_ADR).w	; Start & Loop
	move.w #0,mp_curSlice	; Read is starting, logically with first slice
	rts

; -----------------------------------------------------------------------------
; Read the pattern lines
; -----------------------------------------------------------------------------
mp_read_song:	lea mp_base,a0
	; We only read a row every curSpeed ticks
	move.w (mp_curTick-mp_base)(a0),d7
	bne .decreaseTick
	move.w (mp_curSpeed-mp_base)(a0),(mp_curTick-mp_base)(a0)
	
	; Get the pattern & row
	move.l (mp_curSeqPos-mp_base)(a0),a1
	move.l (a1),a2		; Current Pattern address
	cmp.l #0,a2		; End of the song ?
	beq .endSong		; If pattern address is null, then this is the end
	move.w (mp_curRow-mp_base)(a0),d7
	moveq #0,d6
	move.w d7,d6
	mulu #MP_PATL_SO_LEN,d6
	add.l d6,a2		; a2= current line address
	
	; Increment row for the next mp_readSong call
	addq.w #1,d7
	and.w #64-1,d7		; 64 row per pattern
	bne.s .noPatternChnge
	
	; Next pattern
	lea 4(a1),a1
	move.l a1,(mp_curSeqPos-mp_base)(a0)
	
.noPatternChnge	move.w d7,(mp_curRow-mp_base)(a0)
	
	; Read all voices
	; Song voices are LRRL whereas mixer ones are LLRR, so we start at the 4th voice data
	lea 3*MP_PATLV_SO_LEN(a2),a2

	lea (mp_vceSrcPos-mp_base)(a0),a4	; Long word size pointer
	lea (mp_vcePeriodIdx-mp_base)(a0),a5	; Word size pointer
	moveq #0,d0
	moveq #3,d7
	
.loopVoice	move.l MP_PATLV_SO_SADR(a2),a3	; Sample description address
	cmp.l #0,a3
	beq.s .doCmd	; There is no note to play, so we let the on-going sample playing if there is one
		
	move.l MP_SPL_SO_SADR(a3),(a4)			; Start Address
	move.l MP_SPL_SO_EADR(a3),(mp_vceSrcEnd-mp_vceSrcPos)(a4)	; End Address
	move.w MP_SPL_SO_NOTE(a3),(a5)			; Index of the period
	move.l MP_SPL_SO_LADR(a3),(mp_vceSrcLoop-mp_vceSrcPos)(a4)	; Loop Address
	ifne MP_DYN_VOL
	move.w MP_SPL_SO_VOL(a3),(mp_vceVol-mp_vcePeriodIdx)(a5)	; Default sample volume is set for each note ON
	endif
	
.doCmd	; Look for command
	move.b MP_PATLV_SO_CMD(a2),d0
	; Speed ?
	cmp.b #$F,d0
	bne.s .noSpeedCmd
	move.b MP_PATLV_SO_VAL(a2),d0
	move.w d0,(mp_curSpeed-mp_base)(a0)	; Change speed
	move.w d0,(mp_curTick-mp_base)(a0)	; And set tick accordingly
	
.noSpeedCmd	ifne MP_DYN_VOL
	; Volume ?
	cmp.b #$C,d0
	bne.s .noVolCmd
	move.b MP_PATLV_SO_VAL(a2),d0
	move.w d0,(mp_vceVol-mp_vcePeriodIdx)(a5)
	
.noVolCmd	
	endif


.nextVoice	; Next voice
	lea 4(a4),a4
	lea 2(a5),a5
	lea MP_PATLV_SO_LEN(a2),a2
	
	; Manage stereo (cf above)
	cmp.w #3,d7
	bne.s .dbf
	lea -4*MP_PATLV_SO_LEN(a2),a2
	
.dbf	dbf d7,.loopVoice
		
	bra.s .end
	
.endSong	; We prepare a fake copy so as to have a fixed execution time for mp_mix
	move.l #mp_fake_sndbuff,d0
	lea (mp_copySAddr-mp_base)(a0),a1
	mpint_setval4chn d0,a1
	lea (mp_vceSrcPos-mp_base)(a0),a1
	mpint_setval4chn d0,a1
	add.l #MP_BUFF_SLICE,d0
	lea (mp_vceSrcEnd-mp_base)(a0),a1
	mpint_setval4chn d0,a1	
	moveq #0,d0
	lea (mp_vceSrcLoop-mp_base)(a0),a1
	mpint_setval4chn d0,a1
	lea (mp_vcePeriodIdx-mp_base)(a0),a1
	mpint_setval4chn d0,a1
	
	move.l #mp_vceSrcPosFoo,(mp_copySPosPtr-mp_base)(a0)	; See mp_mix description

	bra.s .end
	
.decreaseTick	subq.w #1,d7
	move.w d7,(mp_curTick-mp_base)(a0)
	
.end	rts

; -----------------------------------------------------------------------------
; Prepare the mix of samples. This sub shall contain all variable CPU time instructions
; Whereas mp_mix shall have a fixed duration
; -----------------------------------------------------------------------------
mp_premix:	lea mp_base,a0
	moveq #0,d0

	; Get the slice number inside the buffer
	move.w (mp_1stFreeSlice-mp_base)(a0),d0
	move.w (mp_curSlice-mp_base)(a0),d1
	cmp.w #MP_NB_BUFFERS,d1	; If play not started, we do not read counter
	beq.s .endCounterRead
	
	; Read the DMA counter
	moveq #0,d2
	lea HW_SND_ADR.w,a2
	move.b HW_SND_H_CADR_O(a2),d2
	swap d2
	movep.w HW_SND_M_CADR_O(a2),d2	
	
	; Now find the slice where the DMA counter is in
	sub.l #mp_sndbuff,d2
	moveq #0,d1
	move.w #MP_BUFF_SLICE*2,d3
.loopCounter	cmp.w d3,d2
	blo.s .counterRead
	addq.w #1,d1
	sub.w d3,d2
	bra.s .loopCounter

.counterRead	and.w #MP_NB_BUFFERS-1,d1	; Necessary if we get just at the looping time (end of buffer)	
	
.endCounterRead	move.w d1,(mp_curSlice-mp_base)(a0)
	cmp.w d0,d1	; Compare DMA slice and the next slice to fill
	beq .fakeCopy
	
	; the counter is not on the same slice that the first to fill
	; increment the number of the next slice to fill (for the next call)
	move.w d0,d1
	addq.w #1,d1
	and.w #MP_NB_BUFFERS-1,d1
	move.w d1,(mp_1stFreeSlice-mp_base)(a0)
	
	; and get the slice address
	mulu #MP_BUFF_SLICE*2,d0
	add.l #mp_sndbuff,d0
	move.l d0,(mp_copyDAddr-mp_base)(a0)
	  
	; Then for each of the 4 voices, we prepare the job for the mp_mix
	; i.e. compute source address in case of looping
	lea (mp_copySAddr-mp_base)(a0),a1
	lea (mp_vceSrcPos-mp_base)(a0),a2	; Long word pointer
	lea (mp_vcePeriodIdx-mp_base)(a0),a3	; Word pointer
	ifeq MP_DYN_VOL-1
	move.l mp_volume_tab,a5		; Volume table in case of dynamic volume
	endif
	ifeq MP_DYN_VOL-2
	lea mp_vol_ext_tab,a5		; Extreme Volume table in case of dynamic volume
	endif	
	ifeq MP_SPL_PER_NOTE
	lea mp_frqtable,a6		; Frequency table in case of standard pitch shifting
	endif
	moveq #3,d0	; 4 voices
	
.loopVoice	move.l (a2),d2
	move.l (mp_vceSrcEnd-mp_vceSrcPos)(a2),d3
	move.l (mp_vceSrcLoop-mp_vceSrcPos)(a2),d4
	
	cmp.l d2,d3
	bhi.s .notEnd
	; We have gone further than the end, must we loop ?
	tst.l d4
	beq.s .noLoop
	
	sub.l d3,d2
	add.l d4,d2
	bra.s .notEnd

.noLoop	move.l #mp_fake_sndbuff,d2
	move.l d2,(a1)
	move.l d2,(a2)
	move.l #0,(mp_vceSrcLoop-mp_vceSrcPos)(a2)
	add.l #MP_BUFF_SLICE,d2
	move.l d2,(mp_vceSrcEnd-mp_vceSrcPos)(a2)
	bra.s .nextVoice
	
.notEnd	move.l d2,(a1)

.nextVoice	; Manage frequency shift if the mode requires it
	ifeq MP_SPL_PER_NOTE
	move.w (a3),d2				; Period index
	move.l (a6,d2.w),(mp_copyFrqAddr-mp_copySAddr)(a1)	; Sequence of delta sources	
	endif
	; Dynamic Volume
	ifeq MP_DYN_VOL-1
	moveq #0,d5
	move.w (mp_vceVol-mp_vcePeriodIdx)(a3),d5	; Voice volume
	lsl.w #8,d5
	add.l a5,d5
	move.l d5,(mp_copyVolAddr-mp_copySAddr)(a1)	; Address of volume line table
	endif
	; Dynamic volume for Extreme mode
	ifeq MP_DYN_VOL-2
	moveq #0,d5
	move.w (mp_vceVol-mp_vcePeriodIdx)(a3),d5	; Voice volume
	lsl.w #3,d5
	move.l (a5,d5.w),(mp_copyVolShift-mp_copySAddr)(a1) ; Volume shift
	move.l 4(a5,d5.w),(mp_copyVolMask-mp_copySAddr)(a1) ; Volume mask
	endif

	lea 4(a1),a1
	lea 4(a2),a2
	lea 2(a3),a3
	dbra d0,.loopVoice 

	move.l #mp_vceSrcPos,(mp_copySPosPtr-mp_base)(a0)	; See mp_mix description
	bra.s .end
	
.fakeCopy	; the counter is on the same slice that the first to fill, so all the buffer if full !
	; we prepare a fake copy so as to have a fixed execution time for mp_mix
	;move.w #$777,(HW_ST_PAL_ADR).w	
	move.l #mp_fake_sndbuff,d0
	lea (mp_copySAddr-mp_base)(a0),a1
	move.l d0,(a1)+
	move.l d0,(a1)+
	move.l d0,(a1)+
	move.l d0,(a1)+
	move.l #mp_fake_dstbuff,(mp_copyDAddr-mp_base)(a0)
	move.l #mp_vceSrcPosFoo,(mp_copySPosPtr-mp_base)(a0)	; See mp_mix description
	
.end	rts

; -----------------------------------------------------------------------------
; Do the samples mix. It is important that this part is fast and with
; fixed CPU time (to be included in Overscan)
;
; When entering the routine, mp_copySPosPtr = mp_vceSrcPos or its fake version
; it is important to get how far we have gone in the source, because for some
; modes, some bytes could be repeated, and we cannot rely on
; adding MP_BUFF_SLICE in mp_premix
; -----------------------------------------------------------------------------

; Simple version for DEBUG, no volume, requires 1 asr per sample
; No frequency shift
; 48 cycles per output buffer sample (stereo word) -> 10 samples per scanline 
	ifeq MP_MODE-6
mp_mix_check:	; Get the Start Address
	lea mp_copySAddr,a0
	move.l (a0)+,a1
	move.l (a0)+,a2
	move.l (a0)+,a3
	move.l (a0)+,a4

	move.l mp_copyDAddr,a5
	
	; Copy
	REPT MP_BUFF_SLICE
	move.b (a1)+,d0	; 8	
	add.b (a2)+,d0	; 8
	move.b d0,(a5)+	; 8
	move.b (a3)+,d0	; 8
	add.b (a4)+,d0	; 8
	move.b d0,(a5)+	; 8
	ENDR

	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0
	move.l a1,(a0)+
	move.l a2,(a0)+
	move.l a3,(a0)+
	move.l a4,(a0)+
	rts
	endif
	
; -----------------------------------------------------------------------------
; Classical version, no volume, requires 1 asr per sample
; Manages frequency shift
; 96 cycles per output buffer sample (stereo word) -> 5.3 samples per scanline
; 
MP_NOPS_MIX_STD_INIT_LEFT	equ 25
mp_mix_std_init_left_m macro		; 100 cycles = 25 NOPS
	; Get the Start Address
	lea mp_copySAddr,a0	; 12
	lea mp_copyFrqAddr,a5	; 12
	move.l mp_copyDAddr,d3	; 20
	; Backup registers
	move.l sp,d6		; 4
	
	; Left Voice first, else there are not enough registers :-(	
	; Source data
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	; Frequency shifts
	move.l (a5)+,a3	; Sequence of delta sources	; 12
	move.l (a5)+,a4		; 12

	; Set destination
	move.l d3,sp		; 4
	endm
	
MP_NOPS_MIX_STD_MIX	equ 12
mp_mix_std_mix_m macro		; 48 cycles = 12 NOPS
	; Copy
	REPT \1
	move.b (a1),d0	; 8
	add.w (a3)+,a1	; 12
	add.b (a2),d0	; 8
	add.w (a4)+,a2	; 12
	move.b d0,(sp)+	; 8
	ENDR
	endm
	
MP_NOPS_MIX_STD_INIT_RIGHT	equ 19	
mp_mix_std_init_right_m macro		; 76 cycles = 19 NOPS
	; Backup a1 and a2 (should not pair, so 8 cycles each)
	exg a1,d4		; 8
	exg a2,d5		; 8
	
	; Right Voice now	
	; Source data
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	; Frequency shifts
	move.l (a5)+,a3	; Sequence of delta sources	; 12
	move.l (a5)+,a4		; 12
	
	; Shift destination address
	addq.l #1,d3		; 8
	; And set destination
	move.l d3,sp		; 4
	endm

MP_NOPS_MIX_STD_END	equ 18	
mp_mix_std_end_m macro		; 72 cycles = 18 cycles
	; Restore stack pointer
	move.l d6,sp		; 4

	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0	; 20
	move.l d4,(a0)+		; 12
	move.l d5,(a0)+		; 12
	move.l a1,(a0)+		; 12
	move.l a2,(a0)+		; 12
	endm

	ifeq MP_MODE
mp_mix_std	mp_mix_std_init_left_m
	mp_mix_std_mix_m MP_BUFF_SLICE
	mp_mix_std_init_right_m
	mp_mix_std_mix_m MP_BUFF_SLICE
	mp_mix_std_end_m
	rts
	endif
	
; -----------------------------------------------------------------------------
; Classical version, with volume, requires 1 asr per sample
; Manages frequency shift
; 144 cycles per output buffer sample (stereo word) -> 3.55 samples per scanline 
; 
MP_NOPS_MIX_STD_VOL_INIT_LEFT	equ 33
mp_mix_std_vol_init_left_m macro	; 132 cycles = 33 NOPS
	; Get the Start Address
	lea mp_copySAddr,a0	; 12
	lea mp_copyFrqAddr,a5	; 12
	move.l mp_copyDAddr,d3	; 20
	; Backup registers
	move.l sp,d6		; 4
	
	; Left Voice first, else there are not enough registers :-(	
	; Source data
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	; Frequency shifts
	move.l (a5)+,a3	; Sequence of delta sources	; 12
	move.l (a5)+,a4		; 12
	; Volume tables addresses
	move.l (mp_copyVolAddr-mp_copySAddr-8)(a0),d1	; 16
	move.l (mp_copyVolAddr-mp_copySAddr-4)(a0),d2	; 16

	; Set destination
	move.l d3,sp		; 4
	endm

MP_NOPS_MIX_STD_VOL_MIX	equ 18
mp_mix_std_vol_mix_m macro		; 72 cycles = 18 NOPS
	; Copy
	REPT \1
	move.b (a1),d1	; 8
	move.b (a2),d2	; 8
	move.l d1,a6	; 4
	move.b (a6),d0	; 8
	move.l d2,a6	; 4
	add.b (a6),d0	; 8
	add.w (a3)+,a1	; 12
	add.w (a4)+,a2	; 12
	move.b d0,(sp)+	; 8
	ENDR
	endm

MP_NOPS_MIX_STD_VOL_INIT_RIGHT	equ 27	
mp_mix_std_vol_init_right_m macro	; 108 cycles = 27 NOPS
	; Backup a1 and a2 (should not pair, so 8 cycles each)
	exg a1,d4		; 8
	exg a2,d5		; 8
	
	; Right Voice now	
	; Source data
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	; Frequency shifts
	move.l (a5)+,a3	; Sequence of delta sources	; 12
	move.l (a5)+,a4		; 12
	; Volume tables addresses
	move.l (mp_copyVolAddr-mp_copySAddr-8)(a0),d1	; 16
	move.l (mp_copyVolAddr-mp_copySAddr-4)(a0),d2	; 16
	
	; Shift destination address
	addq.l #1,d3		; 8
	; And set destination
	move.l d3,sp		; 4
	endm

MP_NOPS_MIX_STD_VOL_END	equ 18	
mp_mix_std_vol_end_m macro		; 72 cycles = 18 cycles
	; Restore stack pointer
	move.l d6,sp		; 4

	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0	; 20
	move.l d4,(a0)+		; 12
	move.l d5,(a0)+		; 12
	move.l a1,(a0)+		; 12
	move.l a2,(a0)+		; 12
	endm

	ifeq MP_MODE-1
mp_mix_std_vol	mp_mix_std_vol_init_left_m
	mp_mix_std_vol_mix_m MP_BUFF_SLICE
	mp_mix_std_vol_init_right_m
	mp_mix_std_vol_mix_m MP_BUFF_SLICE
	mp_mix_std_vol_end_m
	rts
	endif
	
; -----------------------------------------------------------------------------
; Oversampling version, without volume, requires 1 asr per sample
; 48 cycles per output buffer sample (stereo word) -> 10.6 samples per scanline 
;
MP_NOPS_MIX_OVS_INIT	equ 20
mp_mix_ovs_init_m macro		; 80 cycles = 20 NOPS
	; Get the Start Address
	lea mp_copySAddr,a0	; 12
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	move.l (a0)+,a3		; 12
	move.l (a0)+,a4		; 12

	move.l mp_copyDAddr,a5	; 20
	endm

MP_NOPS_MIX_OVS_MIX	equ 12
mp_mix_ovs_mix_m macro		; 48 cycles = 12 NOPS
	; Copy
	REPT \1
	move.b (a1)+,d0	; 8	
	add.b (a2)+,d0	; 8
	move.b d0,(a5)+	; 8
	move.b (a3)+,d0	; 8
	add.b (a4)+,d0	; 8
	move.b d0,(a5)+	; 8
	ENDR
	endm

MP_NOPS_MIX_OVS_END	equ 17
mp_mix_ovs_end_m macro		; 68 cycles = 17 NOPS
	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0	; 20
	move.l a1,(a0)+		; 12
	move.l a2,(a0)+		; 12
	move.l a3,(a0)+		; 12
	move.l a4,(a0)+		; 12
	endm

	ifeq MP_MODE-2
mp_mix_ovs	
	mp_mix_ovs_init_m
	mp_mix_ovs_mix_m MP_BUFF_SLICE
	mp_mix_ovs_end_m
	rts
	endif	


; -----------------------------------------------------------------------------
; Oversampling version, with volume, requires 1 asr per sample
; 96 cycles per output buffer sample (stereo word) -> 5.3 samples per scanline 
;
MP_NOPS_MIX_OVS_VOL_INIT	equ 36
mp_mix_ovs_vol_init_m macro		; 144 cycles = 36 NOPS
	;Get the Start Address
	lea mp_copySAddr,a0	; 12
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	move.l (a0)+,a3		; 12
	move.l (a0)+,a4		; 12

	move.l mp_copyDAddr,a5	; 20
	
	; Volume tables addresses
	move.l (mp_copyVolAddr-mp_copySAddr-16)(a0),d1	; 16
	move.l (mp_copyVolAddr-mp_copySAddr-12)(a0),d2	; 16
	move.l (mp_copyVolAddr-mp_copySAddr-8)(a0),d3	; 16
	move.l (mp_copyVolAddr-mp_copySAddr-4)(a0),d4	; 16
	endm

MP_NOPS_MIX_OVS_VOL_MIX	equ 24
mp_mix_ovs_vol_mix_m macro		; 96 cycles = 24 NOPS
	; Copy
	REPT \1
	move.b (a1)+,d1	; 8
	move.b (a2)+,d2	; 8
	move.b (a3)+,d3	; 8
	move.b (a4)+,d4	; 8
	move.l d1,a6	; 4
	move.b (a6),d0	; 8
	move.l d2,a6	; 4
	add.b (a6),d0	; 8
	move.b d0,(a5)+	; 8
	move.l d3,a6	; 4
	move.b (a6),d0	; 8
	move.l d4,a6	; 4
	add.b (a6),d0	; 8
	move.b d0,(a5)+	; 8
	ENDR
	endm

MP_NOPS_MIX_OVS_VOL_END	equ 17
mp_mix_ovs_vol_end_m macro		; 68 cycles = 17 NOPS
	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0	; 20
	move.l a1,(a0)+		; 12
	move.l a2,(a0)+		; 12
	move.l a3,(a0)+		; 12
	move.l a4,(a0)+		; 12
	endm

	ifeq MP_MODE-3
mp_mix_ovs_vol; 
	mp_mix_ovs_vol_init_m
	mp_mix_ovs_vol_mix_m MP_BUFF_SLICE
	mp_mix_ovs_vol_end_m
	rts	
	endif


; -----------------------------------------------------------------------------
; Extreme (Oversampling + long word) version, without volume, requires 2 asr per sample
; 112 cycles per FOUR output buffer sample (4 stereo words) -> 18.3 samples per scanline 
;
MP_NOPS_MIX_EXT_INIT	equ 20
mp_mix_ext_init_m macro		; 80 cycles = 20 NOPS
	; Get the Start Address
	lea mp_copySAddr,a0	; 12
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	move.l (a0)+,a3		; 12
	move.l (a0)+,a4		; 12

	move.l mp_copyDAddr,a5	; 20
	endm

MP_NOPS_MIX_EXT_MIX	equ 28
mp_mix_ext_mix_m macro		; 112 cycles = 28 NOPS
	; Copy 
	; WARNING 4 bytes are treated each time ! 
	REPT \1 
	move.l (a1)+,d0		; 12
	add.l (a2)+,d0		; 16
	movep.l d0,0(a5)	; 24
	move.l (a3)+,d0		; 12
	add.l (a4)+,d0		; 16
	movep.l d0,1(a5)	; 24
	
	lea 8(a5),a5		;8 
	ENDR
	endm

MP_NOPS_MIX_EXT_END	equ 17
mp_mix_ext_end_m macro		; 68 cycles = 17 NOPS
	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0	; 20
	move.l a1,(a0)+		; 12
	move.l a2,(a0)+		; 12
	move.l a3,(a0)+		; 12
	move.l a4,(a0)+		; 12
	endm

	ifeq MP_MODE-4
mp_mix_ext
	mp_mix_ext_init_m
	mp_mix_ext_mix_m MP_BUFF_SLICE/4	; WARNING 4 bytes are treated each time !	
	mp_mix_ext_end_m
	rts
	endif

; -----------------------------------------------------------------------------
; Extreme (Oversampling + long word) version, with volume, requires 2 asr per sample
; 128+128=256 cycles per FOUR output buffer sample (4 stereo words) -> 8 samples per scanline 
;
MP_NOPS_MIX_EXT_VOL_INIT_LEFT	equ 44
mp_mix_ext_vol_init_left_m macro	; 176 cycles = 44 NOPS
	; Get the Start Address
	lea mp_copySAddr,a0	; 12
	; Destination address
	move.l mp_copyDAddr,a6	; 20
	; Volume
	lea mp_copyVolShift,a3	; 12
	lea mp_copyVolMask,a4	; 12
	
	; Left Voice first, else there are not enough registers :-(
	; Source data
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12
	; Volume
	move.l (a3)+,d2		; 12
	move.l (a3)+,d3		; 12
	move.l (a4)+,d4		; 12
	move.l (a4)+,d5		; 12
	; Calculate the compensation for the shifts
	; Odds shifts take as much time as even ones...
	moveq #12,d6		; 4

	move.w d2,d0		; 4
	addq.w #1,d0		; 4
	and.w #$FE,d0		; 8
	sub.w d0,d6		; 4

	move.w d3,d0		; 4
	addq.w #1,d0		; 4
	and.w #$FE,d0		; 8
	sub.w d0,d6		; 4

	; Left destination address
	move.l a6,a5		; 4
	endm

MP_NOPS_MIX_EXT_VOL_MIX	equ 32
mp_mix_ext_vol_mix_m macro		; 128 cycles = 32 NOPS
	; Copy 
	; WARNING 4 bytes are treated each time ! 
	REPT \1 
	move.l (a1)+,d0		; 12
	move.l (a2)+,d1		; 12
	
	lsr.l d2,d0		; 8+2n
	lsr.l d3,d1		; 8+2n
	and.l d4,d0		; 8
	and.l d5,d1		; 8
	
	add.l d1,d0		; 8
	movep.l d0,0(a5)	; 24

	lsr.l d6,d1		; Compensate previous lsr.l to have fixed CPU time (48)
	
	lea 8(a5),a5		;8 
	ENDR
	endm


MP_NOPS_MIX_EXT_VOL_INIT_RIGHT	equ 34
mp_mix_ext_vol_init_right_m macro	; 136 cycles = 34 NOPS
	; Right Voice now
	; Volume
	move.l (a3)+,d2		; 12
	move.l (a3)+,d3		; 12
	move.l (a4)+,d4		; 12
	move.l (a4)+,d5		; 12
	; Calculate the compensation for the shifts
	; Odds shifts take as much time as even ones...
	moveq #12,d6		; 4

	move.w d2,d0		; 4
	addq.w #1,d0		; 4
	and.w #$FE,d0		; 8
	sub.w d0,d6		; 4

	move.w d3,d0		; 4
	addq.w #1,d0		; 4
	and.w #$FE,d0		; 8
	sub.w d0,d6		; 4

	; Backup a1 & a2
	move.l a1,a3		; 4
	move.l a2,a4		; 4

	; Source data
	move.l (a0)+,a1		; 12
	move.l (a0)+,a2		; 12

	; Right destination address
	addq.l #1,a6		; 8
	move.l a6,a5		; 4
	endm

MP_NOPS_MIX_EXT_VOL_END	equ 17
mp_mix_ext_vol_end_m macro		; 68 cycles = 17 NOPS
	; Indicate where the copy has stopped
	move.l mp_copySPosPtr,a0	; 20
	move.l a3,(a0)+		; 12
	move.l a4,(a0)+		; 12
	move.l a1,(a0)+		; 12
	move.l a2,(a0)+		; 12
	endm

	
	ifeq MP_MODE-5
mp_mix_ext_vol
	mp_mix_ext_vol_init_left_m
	mp_mix_ext_vol_mix_m MP_BUFF_SLICE/4	; WARNING 4 bytes are treated each time !	
	mp_mix_ext_vol_init_right_m
	mp_mix_ext_vol_mix_m MP_BUFF_SLICE/4	; WARNING 4 bytes are treated each time !	
	mp_mix_ext_vol_end_m
	rts
	endif


; Lance / Paulo
; 72 cycles per output buffer (word) -> 7 bytes per line
; 
;	move.b	(a0)+,d2	; 8
;	move.b	(a1)+,d1	; 8
;	move.l	d1,a2	; 4
;	add.b	(a2)+,d2	; 8
;	move.b	d2,(sp)+	; 8


; -----------------------------------------------------------------------------	
	DATA
mp_period_table	dc.w 856,808,762,720,678,640,604,570,538,508,480,453 ; C-1 to B-1
	dc.w 428,404,381,360,339,320,302,285,269,254,240,226 ; C-2 to B-2
	dc.w 214,202,190,180,170,160,151,143,135,127,120,113 ; C-3 to B-3

mp_volume_tab	dc.l mp_vol_tab

	; Extreme volume table (Shift, Mask)
mp_vol_ext_tab	dc.l 6,$00000000	; Volume 0
	dc.l 5,$01010101	; Volume 1-2
	dc.l 5,$01010101
	dc.l 4,$03030303	; Volume 3-4
	dc.l 4,$03030303
	REPT 4
	dc.l 3,$07070707	; Volume 5-8
	ENDR
	REPT 8
	dc.l 2,$0F0F0F0F	; Volume 9-16
	ENDR
	REPT 16
	dc.l 1,$1F1F1F1F	; Volume 17-32
	ENDR
	REPT 32
	dc.l 0,$3F3F3F3F	; Volume 33-64
	ENDR	
	

mp_err_msg	dc.b "An error occured while conveting the MOD file",$d,$a
	dc.b "Press the space bar and pray !", $a,$d, 0

mp_size_msg	dc.b "The size required for the mod buffer is:",$d,$a
mp_size_msg1	dc.b "$AAAAAAAA Bytes", $a,$d, 0

mp_fx_msg	dc.b "FXs found but not managed:",$d,$a
mp_fx_msg1	dc.b "0123456789ABCDEF ", $a,$d, 0


; -----------------------------------------------------------------------------	
	BSS
; The replay buffer, that is composed of several slices in order to avoid loop problems
; but also in case of demo screen changing that could stop the VBL, if we do CPU consuming
; activities, we'll be happy to have a good sound ;-)
mp_sndbuff	ds.w MP_NB_BUFFERS*MP_BUFF_SLICE
mp_end_sndbuff

; Fake Source Buffer (2 bytes per sample and we want 2 consecutive buffers)
mp_fake_sndbuff	ds.l MP_BUFF_SLICE
mp_end_fake_sndbuff

; Fake Destination Buffer
mp_fake_dstbuff	ds.w MP_BUFF_SLICE*2


; All variables required for MOD Play (during replay) are consecutive to mp_base
mp_base
; General
mp_bufferAddr	ds.l 1	; Adress of the song buffer
; Mix variables
mp_1stFreeSlice	ds.w 1	; Next slice number to be filled
mp_curSlice	ds.w 1 	; Slice currently being read by the DMA (or equals to MP_NB_BUFFERS is not yet started)
mp_vceSrcPos	ds.l 4	; Current position in source samples (incremented each slice)
mp_vceSrcPosFoo	ds.l 4	; Fake mp_vceSrcPos for the case no buffer must be filled
mp_vceSrcEnd	ds.l 4	; End postion of the source samples (stable)
mp_vceSrcLoop	ds.l 4	; Loop postion of the source samples (stable)
mp_vcePeriodIdx	ds.w 4	; Index of the period of the last note played 
mp_vceVol	ds.w 4	; Volume level for each voice
mp_copySAddr	ds.l 4	; Copy from... 4 sources, most of the time a copy of mp_vceSrcPos
mp_copyDAddr	ds.l 1	; The slice address in the buffer
mp_copyFrqAddr	ds.l 4	; The address of the frequency table for each voice
mp_copyVolAddr	ds.l 4	; Address of the volume table line for each voice
mp_copyVolShift	ds.l 4	; Shift value for each voice in Extreme volume mode 
mp_copyVolMask	ds.l 4	; Mask value for each voice in Extreme volume mode 

mp_copySPosPtr	ds.l 1	; points onto mp_vceSrcPos or mp_vceSrcPosFoo

; Song variables
mp_curSpeed	ds.w 1	; Current replay speed (set by $F command)
mp_curTick	ds.w 1	; Current tick (read a row every curSpeed ticks)
mp_curSeqPos	ds.l 1	; Address of the current position in the sequence
mp_curRow	ds.w 1	; Current row number

; Other variables
mp_inv_period	ds.b 1024
mp_fx_met	ds.b 16
mp_vol_tab	ds.w $4200/2

; -----------------------------------------------------------------------------	

	ifeq MP_STE_FRQ
	INCLUDE "MODFrqTable.s"
	printt "MODPlay: Include AMIGA frequencies table"
	else
	INCLUDE "MODFrqTableSTE.s"
	printt "MODPlay: Include STE frequencies table"
	endif

; -----------------------------------------------------------------------------	


; KEPT IN CASE... (See explanations at the beginning of the file)
; Internal sub routine to process samples and have a copy of each sample for each note
; And also performs interpolation
; a0 (IO) = Address in the source mod pointing to sample descriptions 
; a1 (I) = Address of the buffer
; a5 (IO) = Address in the source mod pointing to the sample data
; a6 (IO) = Address in the buffer where to copy sample data to

; a2 = volume table
; a3 = backup a6
; a4 = Address if the destination sample description

; Internals
; d0 = period of the note
; d1 = loop start
; d2 = loop length
; d4 = sample length
; d5 = volume address
; d6 = Note dbf
; d7 = Sample dbf

; mpint_cpyrepeat_m macro
	; tst.w d6
	; beq.s .noCopy\@
	; cmp.l d3,a5
	; beq.s .loopRepeat\@
	; addq.b #1,d6
	; moveq #0,d4
	; move.b 1(a5),d4
	; sub.b d5,d4
	; ext.w d4
	; ext.l d4
	; divs.w d6,d4
	; subq.b #2,d6
	
; .loopItpol\@	add.b d4,d5
	; move.b d5,(a6)+
	; cmp.l a0,a6
	; bhs.s .noLoopCtd
	; dbf d6,.loopItpol\@
	; moveq #0,d6
	; bra.s .noCopy\@
	
; .loopRepeat\@	move.b d5,(a6)+
	; cmp.l a0,a6
	; bhs.s .noLoopCtd
	; dbf d6,.loopRepeat\@
	; moveq #0,d6
; .noCopy\@	
	; endm

; mpint_process_spl_per_note_ovs
	; ; Process the samples
	; lea MP_SPL_TBL_O(a1),a4	; Beginning of the buffer samples list desciption
	; moveq #31-1,d7		; Max number of samples
; .nextSample	moveq #0,d4
	; moveq #0,d1
	; moveq #0,d2
	; move.w 22(a0),d4	; Length of the source sample in words
	; lsl.l #1,d4		; in bytes now
	; move.w 26(a0),d1	; Start loop in words
	; lsl.l #1,d1
	; move.w 28(a0),d2	; Loop length in words
	; lsl.l #1,d2	
	; add.l d1,d2		; Compute loop end
	; add.l a5,d2
	
	; moveq #36-1,d6		; Number of notes per sample
; .nextNote	move.w MP_SPL_SO_NOTE(a4),d0	; Period of the note
	; beq .noteNotUsed
	
	; ; Copy sample
	; move.l a6,a3		; a3=beginning of the sample in the buffer
	; tst.l d4
	; beq .noCopy
	
	; move.l mp_volume_tab,a2	; Volume table in case of no dynamic volume
	; moveq #0,d5
	; move.b 25(a0),d5	; Sample volume
	; lsl.w #8,d5
	; add.l a2,d5	

	; lea mp_inv_period,a2	; Period->Idx table
	; moveq #0,d3
	; move.b (a2,d0.w),d3
	; move.w d3,d0
	; lsl.w #2,d3
	; move.w d0,MP_SPL_SO_NOTE(a4)	; Index according to period for acccessing long word table
	; lea mp_frqtable,a2
	; move.l (a2,d3.w),a2	; Adress of the frequency table for the note

	; ; If no looping, we simply copy the sample to its end
	; ; If looping, we first copy to the loop starting point, then a second time to the loop end 
	; tst.l d1		; Loop sample ?
	; beq.s .noLoop
	; move.l d1,d3		; First copy to the loop start
	; subq.l #1,d3		; (Excluded)
	; bra.s .copySample

; .noLoop	move.l d4,d3

; .copySample	add.l a5,d3
	; movem.l d4/d6/d7/a0/a1/a3/a5,-(sp)	; Uses d0,d1,d2,d3,d5,
	; move.l a2,a1		; Backup a2
	
	; ; Get the maximum oversampled note length 
	; move.l MP_SPL_SO_EADR(a4),a0	; This value has been temporarly used
	; add.l a6,a0
	; move.l #0,MP_SPL_SO_EADR(a4)
	
	; move.l #MP_BUFF_SLICE-1,d0
	; moveq #0,d7	; jump
	; moveq #0,d6	; nb steps
	; moveq #0,d4	; Interpolated value

; .copySampleByte	move.b d5,d4	; Backup previous sample
	; move.b (a5),d5	; Read & write sample
	; asr.b #1,d5
	; ifeq MP_DYN_VOL
	; move.l d5,a3
	; move.b (a3),d5
	; endif
	; cmp.w #1,d7	
	; bls.s .oneSample ; Have we jumped more than one sample ?
	; add.b d5,d4
	; lsr.b #1,d4
	; move.b d4,(a6)+
	; bra.s .loopRepeat
	
; .oneSample	move.b d5,(a6)+

; .loopRepeat	move.w (a2)+,d7	; Get jump to next source sample
	; bne.s .noRepeat	; If jump is 0, this means we repeat
	; addq.w #1,d6

	; dbf d0,.noFrqTableEndR	; The table frequency has a limited length, we must loop on it
	; move.l a1,a2
	; move.l #MP_BUFF_SLICE-1,d0
	
; .noFrqTableEndR	cmp.l d3,a5
	; bls.s .loopRepeat

	
; .noRepeat	mpint_cpyrepeat_m
	; add.w d7,a5
	; dbf d0,.noFrqTableEndN	; The table frequency has a limited length, we must loop on it
	; move.l a1,a2
	; move.l #MP_BUFF_SLICE-1,d0
	
; .noFrqTableEndN	cmp.l a0,a6
	; bhs.s .noLoopCtd
	; cmp.l d3,a5
	; bls.s .copySampleByte

	; ; Test if we exit the loop for the second time (i.e. start loop to end)
	; move.l MP_SPL_SO_LADR(a4),a3	; Starting point
	; cmp.l #0,a3
	; bne.s .startCopyLoop 	; We have already set the start of the loop -> we were in the 2nd loop

	; ; We have finished the first copy part 
	; ; Test if the copy was up to the looping start
	; tst.l d1
	; beq.s .noLoopCtd	; No looping
	; move.l a6,MP_SPL_SO_LADR(a4)
	; move.l d2,d3		; Copy up to loop end
	; bra .copySampleByte

	; ; Manage loop (cont'd) : Fills the buffer just after the sample with the begin of the loop
; .startCopyLoop	move.l #(MP_BUFF_SLICE+4)-1,d3	; Copy the loop
; .copyLoop	move.b (a3)+,(a6)+
	; dbf d3,.copyLoop
	
	; ; Correct the pointers
	; sub.l #(MP_BUFF_SLICE+4),a6
	
; .noLoopCtd	movem.l (sp)+,d4/d6/d7/a0/a1/a3/a5

; .noCopy	move.l a3,MP_SPL_SO_SADR(a4)	; Start address	
	; move.l a6,MP_SPL_SO_EADR(a4)	; Warning: the sample end is here, and not after the margin (cf below)
	; moveq #0,d0
	; move.b 25(a0),d0	; Sample volume
	; move.w d0,MP_SPL_SO_VOL(a4)		
	
	; ; to manage slice length and potential optimisation of read per 4 bytes
	; lea (MP_BUFF_SLICE+4)(a6),a6
	
; .noteNotUsed	lea MP_SPL_SO_LEN(a4),a4
	; dbf d6,.nextNote
	
	; add.l d4,a5
	; lea 30(a0),a0
	; dbf d7,.nextSample
	
	; rts

