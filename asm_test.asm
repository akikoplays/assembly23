;------------------------------
; Example inspired by Photon's Tutorial:
;  https://www.youtube.com/user/ScoopexUs
;
;---------- Includes ----------
              incdir      "include"
              include     "hw.i"
              include     "funcdef.i"
              include     "exec/exec_lib.i"
              include     "graphics/graphics_lib.i"
              include     "hardware/cia.i"
              include     "exec/memory.i"
;---------- Const ----------

CIAA            = $00bfe001
COPPERLIST_SIZE = 1000                  ;Size of the copperlist
LINE            = 100                   ;<= 255

; WaitLMB macro definition
; Test with beq.b for LMB pressed, or bne.b for !pressed
WaitLMB:      MACRO
              btst        #CIAB_GAMEPORT0,CIAA+ciapra
              ENDM

init:
              movem.l     d0-a6,-(sp)
              move.l      4.w,a6        ; execbase
              clr.l       d0                      

killsys:
; Open libraries
              move.l      #gfxname,a1   ; librairy name
              jsr         _LVOOldOpenLibrary(a6) 
              move.l      d0,a1                   
              move.l      38(a1),d4     ; copper list pointer to save
              move.l      d4,CopperSave
              jsr         _LVOCloseLibrary(a6)
              lea         CUSTOM,a6     ; adresse de base
              move.w      INTENAR(a6),INTENARSave ; Copie de la valeur des interruptions 
              move.w      DMACONR(a6),DMACONSave ; sauvegarde du dmacon 
              move.w      #$138,d0      ; wait for eoframe paramètre pour la routine de WaitRaster - position à attendre
              bsr.w       WaitRaster    ; Appel de la routine wait raster - bsr = jmp,mais pour des adresses moins distantes
              move.w      #$7fff,INTENA(a6) ; désactivation de toutes les interruptions bits : valeur + masque sur 7b
              move.w      #$7fff,INTREQ(a6) ; disable all bits in INTREQ
              move.w      #$7fff,INTREQ(a6) ; disable all bits in INTREQ
              move.w      #$7fff,DMACON(a6) ; disable all bits in DMACON
              move.w      #%1000001111000000,DMACON(a6)              ; Activation classique pour démo

setupScreen1:
              ; bsr         AllocScreen1  ; a0 contains pointer to allocated bitplanes
              ; move.l      a0,screen1    ; cache location
clr_buffer:
              lea         screen1,a0    ; Load the address of the buffer into A0
              move.l      #5*10240-1,d0 ; Set up a counter for the number of bytes to clear
clear_loop:
              clr.b       (a0)+         ; Clear the byte at the address in A0, then increment A0
              dbra        d0,clear_loop ; Decrement D0 and branch if not yet zero (DBRA branches if D0 != -1)
setup_bplptrs:                
              lea         screen1,a0
              lea         bmapptrs,a1   ; bmapptrs (copper struct) to a1
              move.l      #40*256,d1    ; size of dst bitplane in bytes to d1
              moveq       #5,d2         ; number of bitplanes to copy to d2
              bsr         SetupBitplanes
setup_colors:              
              lea         colors,a0
              add         #2,a0
              
              lea         checkerData,a1
              clr.l       d1
              move.b      4(a1),d1      ; get number of colors to d1
              subq        #1,d1         ; -1 for dbf
              add         #6,a1         ; move ptr to colors section
.color_loop:
              move.w      (a1)+,(a0)+
              addq        #2,a0
              dbf         d1,.color_loop

copybitmap:              
              move.l      #40*119,d0
              lea         checkerImg,a0
              lea         screen1,a1
              bsr         CopyCPU

              move.l      #40*119,d0
              add.l       #40*137,a1
              bsr         CopyCPU

              move.l      #40*119,d0
              add.l       #40*137,a1
              bsr         CopyCPU

              move.l      #40*119,d0
              add.l       #40*137,a1
              bsr         CopyCPU

              move.l      #40*119,d0
              add.l       #40*137,a1
              bsr         CopyCPU

; draw_hline:
;               lea         screen1,a0
;               add.l       #40*50,a0
;               moveq       #5-1,d1
; .loopbpls:              
;               move.l      #40-1,d0
; .plot:
;               move.b      #$ff,(a0)+
;               dbf         d0,.plot

; .increaseptr:
;               add.l       #255*40,a0
;               dbf         d1,.loopbpls

; Activate Copper list
              lea         CUSTOM,a6
              move.l      #Copper,COP1LC(a6)
              move.w      #$7fff,COPJMP1(a6)

******************************************************************	
mainloop:
; Wait for vertical blank
              move.w      #$0c,d0       ;No buffering, so wait until raster
              bsr.w       WaitRaster    ;is below the Display Window.

checkmouse:
              WaitLMB
              bne.b       mainloop
            
exit:
              move.w      #$7fff,DMACON(a6) ; disable all bits in DMACON
              or.w        #$8200,(DMACONSave) ; Bit mask inversion for activation
              move.w      (DMACONSave),DMACON(a6) ; Restore values
              move.l      (CopperSave),COP1LC(a6) ; Restore values
              or          #$c000,(INTENARSave)         
              move        (INTENARSave),INTENA(a6) ; interruptions reactivation
              movem.l     (sp)+,d0-a6
              clr         d0            ; Return code of the program
              rts                       ; End

WaitRaster:                                     ;Wait for scanline d0. Trashes d1.
.l:           
              move.l      $dff004,d1
              lsr.l       #1,d1
              lsr.w       #7,d1
              cmp.w       d0,d1
              bne.s       .l            ;wait until it matches (eq)
              rts

; a0 pointer to framebuffer memory
; a1 pointer to copper bitplanes structure
; d1 bitplane size in bytes
; d2 number of bitplanes
SetupBitplanes:
              movem.l     d0/d1/d2/a1,-(sp)
              subq        #1,d2         ; number of bitplanes - 1 (because of dbf)
              move.l      a0,d0         ; address of src to d0
.bplloop:
              swap        d0            ; get high word of src bitplane #n
              move.w      d0,2(a1)      ; copy to copper bmapptr low word of bitplane #n
              swap        d0            ; get low word
              move.w      d0,6(a1)      ; copy to copper bmapptr high word of bitplane #n
              addq        #8,a1         ; increment to next copper bmapptr #n+1
              ; swap        d0            ; revert d0
              add.l       d1,d0         ; increment pointer to framebuffer by one bitplane size
              dbf         d2,.bplloop   ; loop until all bitplanes have been set

              movem.l     (sp)+,d0/d1/d2/a1
              rts

; Allocates memory in chip ram for 5 320x256 bitplanes
; return a0 pointer to allocated memory
AllocScreen1:
              movem.l     d0/d1/a6,-(sp)
              move.l      #40*256*5,d0
              move.l      #MEMF_CHIP|MEMF_CLEAR,d1
              move.l      4.w,a6
              jsr         _LVOAllocMem(a6)
              move.l      d0,a0
              movem.l     (sp)+,d0/d1/a6
              rts

; a0 source bitplane (will be incremented by d0)
; a1 destinaton bitplane (will be incremented by d0)
; d0 number of bytes
CopyCPU:
              subq        #1,d0
.copybyte:
              move.b      (a0)+,(a1)+
              dbf         d0,.copybyte
              rts
******************************************************************	
gfxname:
              GRAFNAME                  ; inserts the graphics library name
              even

; struct BitmapData
checkerData:   
              dc.w        320           ; width in pixels
              dc.w        119           ; height in pixels
              dc.b        19   
              dc.b        5             ; num colors
;	palette for: checker-floor
              dc.w        $0004
              dc.w        $0005
              dc.w        $0006
              dc.w        $0007
              dc.w        $0009
              dc.w        $000a
              dc.w        $000b
              dc.w        $000c
              dc.w        $000d
              dc.w        $0115
              dc.w        $0337
              dc.w        $0449
              dc.w        $055a
              dc.w        $066b
              dc.w        $077c
              dc.w        $099d
              dc.w        $0aae
              dc.w        $0bbf
              dc.w        $0ccf
;	bitplane data for: checker-floor
checkerImg:   
              incbin      "checker-floor_bitplanes_320x119x5.bin"    ; bitplane data
              even

brownImg:     
              incbin      "brownlake320x160-8.png.bpls"
              even
brownPal:     
              incbin      "brownlake320x160-8.png.pal_hi"
              even

screen1:      ds.b        10240*5

DMACONSave:   dc.w        1
CopperSave:   dc.l        1
INTENARSave:  dc.w        1
waitras1:     dc.l        0
waitras2:     dc.l        0
copperlist:   dc.l        0

even

Copper:
              dc.w        BPLCON0,$5200 ; bit plane control reg.0
              dc.w        BPLCON1,$0000 ; scroll value
              dc.w        BPLCON2,$0000 ; blp/sprite priority reg.
              dc.w        BPL1MOD,$0000 ; odd bitplane modulo value
              dc.w        BPL2MOD,$0000 ; even bitplane modulo value
              dc.w        BPLCON3,$0c00
              dc.w        $01fc,$0000
              dc.w        DIWSTRT,$2c81 ; upper left corner of disp. window
              dc.w        DIWSTOP,$2cc1 ; lower right corner of disp. window
              dc.w        DDFSTRT,$0038 ; start of bpl. (horizontal)
              dc.w        DDFSTOP,$00d0 ; end of bpl. (horizontal)
bmapptrs:
              dc.w        $00e0,$0000   ; adr of bplane 1 (long - 2 words)
              dc.w        $00e2,$0000   ; low word of bplane 1 adr
              dc.w        $00e4,$0000
              dc.w        $00e6,$0000
              dc.w        $00e8,$0000
              dc.w        $00ea,$0000
              dc.w        $00ec,$0000
              dc.w        $00ee,$0000
              dc.w        $00f0,$0000
              dc.w        $00f2,$0000
bmapmods:
              dc.w        $0108,$0000
              dc.w        $010a,$0000
        
sprptrs:
              dc.w        $0120,$0000
              dc.w        $0122,$0000
              dc.w        $0124,$0000
              dc.w        $0126,$0000
              dc.w        $0128,$0000
              dc.w        $012a,$0000
              dc.w        $012c,$0000
              dc.w        $012e,$0000
              dc.w        $0130,$0000
              dc.w        $0132,$0000
              dc.w        $0134,$0000
              dc.w        $0136,$0000
              dc.w        $0138,$0000
              dc.w        $013a,$0000
              dc.w        $013c,$0000
              dc.w        $013e,$0000
colors: 
              dc.w        COLOR00,$0000 ; color00 ..
              dc.w        COLOR01,$0000 ; color01 ..
              dc.w        COLOR02,$0000 ; color02 ..
              dc.w        COLOR03,$0000 ; color03 ..
              dc.w        COLOR04,$0000 ; color04 ..
              dc.w        COLOR05,$0000 ; color05 ..
              dc.w        COLOR06,$0000 ; color06 ..
              dc.w        COLOR07,$0000 ; color07 ..
              dc.w        COLOR08,$0010 ; color08 ..
              dc.w        COLOR09,$0020 ; color09 ..
              dc.w        COLOR10,$0030 ; color10 ..
              dc.w        COLOR11,$0040 ; color11 ..
              dc.w        COLOR12,$0050 ; color12 ..
              dc.w        COLOR13,$0060 ; color13 ..
              dc.w        COLOR14,$0070 ; color14 ..
              dc.w        COLOR15,$0080 ; color15 ..
              dc.w        COLOR16,$0090 ; color16 ..
              dc.w        COLOR17,$00a0 ; color17 ..
              dc.w        COLOR18,$00b0 ; color18 ..
              dc.w        COLOR19,$00c0 ; color19 ..
              dc.w        COLOR20,$00d0 ; color20 ..
              dc.w        COLOR21,$00e0 ; color21 ..
              dc.w        COLOR22,$00f0 ; color22 ..
              dc.w        COLOR23,$0100 ; color23 ..
              dc.w        COLOR24,$0200 ; color24 ..
              dc.w        COLOR25,$0300 ; color25 ..
              dc.w        COLOR26,$0500 ; color26 ..
              dc.w        COLOR27,$0600 ; color27 ..
              dc.w        COLOR28,$0700 ; color28 ..
              dc.w        COLOR29,$080f ; color29 ..
              dc.w        COLOR30,$0f00 ; color30 ..
              dc.w        COLOR31,$0f00 ; color31 ..
              dc.w        $ffff,$fffe
CopperEnd:              
              even

          
          
