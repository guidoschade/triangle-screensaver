.model small
.stack 200h
.data

deltax          dw      ?                       ; Differenzen der Koordinaten                   
deltay          dw      ?                       ; bei LINE 
incx            dw      ?                       ; Increment X  Y -
incy            dw      ?                       ; Werte bei LINE
inca            dw      ?                       ; \
incb            dw      ?                       ;  Parameter fuer Fehlerrech-
deltaab         dw      ?                       ;  nung bei LINE
lfarbe          db      ?                       ; Pixelfarbe

text		db "[SPACE]-Lîschen, 1,2-Anzahl, 3-Schweif  4-einfarbig, 5-mehrfarbig, 6,7-Dreiecke pro Farbe, 8,9-Farbe +/-, [ESC]-Ende$"

x11		dw	8			; erste x-position Dreieck 1
x21		dw	60			; erste x-position Dreieck 2
dirx11		db	1			; erste x-richtung Dreieck 1
dirx21		db	0			; erste x-richtung Dreieck 2
x12		dw	250			; zweite x-position Dreieck 1
x22		dw	100			
dirx12		db	0
dirx22		db	1
x13		dw	200
x23		dw	300
dirx13		db	1
dirx23		db 	0
y11		dw	100			; erste y-position Dreieck 1
y21		dw	5
diry11		db      0
diry21		db	1
y12		dw	80
y22		dw	160
diry12		db	1
diry22		db	0
y13		dw	150
y23		dw	100
diry13		db	0
diry23		db	1
col1		db 	5			; farbe1
col2		db	10			; farbe2
usecol		db      1			; farben aendern ?
anzahl 		db	5			; anzahl dreiecke mit gl. farbe
two		db	0			; zwei dreiecke ?
clear		db	0			; dreieck wieder lîschen ?

.code

bsmode          proc    near                    ; Beginn Unterprogramm
		push    ax                      ; AX retten
		xor     ah,ah                   ; BIOS-Fkt 00h in AH
		int     10h                     ; BIOS-Interupt Bildschirm
		pop     ax                      ; AX holen
		ret                             ; zurueck zum Aufrufer
bsmode          endp                            ; Ende Unterprogramm

pset            proc    near                    ; Beginn Unterprogramm
		push    ax                      ; AX retten
		mov     ah,0ch                  ; BIOS-Fkt 0CH in AH
		int     10h                     ; BIOS-Aufruf Bildschirm
		pop     ax                      ; AX wieder holen
		ret                             ; zurueck zum Aufrufer
pset            endp                            ; Ende Unterproramm


line            proc    near                    ; Beginn Unterprogramm
		push    ax
		push    cx                      ; \
		push    dx                      ;  benutzte Register retten
		push    bx                      ;  /
		push    di                      ; /
		push 	si
		mov     lfarbe,al               ; Farbe speichern
		mov     incx,1                  ; INCX = 1
		mov     ax,bx                   ; AX benutzen, Werte erhalten
		sub     ax,cx                   ; Delta X bilden
		jns     tauschex                ; X2 > X1 ?
		neg     ax                      ; dann negiere Delta X
		neg     incx                    ; und INCX = -1
tauschex:       mov     deltax,ax               ; speichern
		mov     incy,1                  ; INCY = +1
		mov     ax,di                   ; AX benutzen
		sub     ax,dx                   ; Delta Y bilden
		jns     negans                  ; negativer Anstieg ?
		neg     ax                      ; dann negiere Delta Y
		neg     incy                    ; INCY = -1
negans:         mov     deltay,ax               ; speichern
		sub     ax,deltax               ; Delta Y - Delta X
		jns     steilan                 ; wenn Anstieg > 1 
;
; -----  Programmteil fuer Anstieg kleiner 1 -----
;
		call    rechnen                 ; Fehler berechnen
setpunkt:       mov     al,lfarbe               ; Farbe des Punktes laden
		call    pset                    ; und setzen
		call    pruefen                 ; naechsten Punkt suchen
		dec     bx                      ; Zaehler - 1
		jnz     setpunkt                ; wenn letzter Punkt, dann
		jmp     lineend                 ; zum Ende d. U-progr.
;
; --- Programmteil fuer | Anstieg | groesser 1 ---
;
steilan:        neg     ax                      ; negiere ( DY - DX )
		push    deltax                  ; \
		push    deltay                  ;   DX und DY ueber
		pop     deltax                  ;   Stack tauschen
		pop     deltay                  ; /
		push    incx                    ; \
		push    incy                    ;  Incremente ueber
		pop     incx                    ;  Stack tauschen
		pop     incy                    ; /
		call    rechnen                 ; Fehler berechnen
setpunkt2:      mov     al,lfarbe               ; Farbe an Bios-Fkt. ueberg.
		call    pset                    ; Unterprogramm Punkt setzen
		xchg    cx,dx                   ; X und Y tauschen
		call    pruefen                 ; naechsten Punkt suchen 
		xchg    cx,dx                   ; X und Y zuruecktauschen
		dec     bx                      ; Zaehler - 1
		jnz     setpunkt2               ; wenn letzter Punkt, dann
lineend:	pop 	si
	        pop     di                      ; \
		pop     bx                      ;  \
		pop     dx                      ;   alte Registerinhalte holen
		pop     cx                      ;  /
		pop     ax                      ; /
		ret                             ; Unterprogrammende
;
; --- Unterprocedure Fehlerberechnung ----
;
rechnen         proc near
		mov     bx,deltax               ; BX-Reg. als Zaehler
		shl     ax,1                    ; 2 * ( DY - DX )
		mov     inca,ax                 ; = INCA
		mov     ax,deltay
		shl     ax,1                    ; 2 * DY
		mov     incb,ax                 ; = INCB
		sub     ax,deltax               ; 2 * DY - DX
		mov     deltaab,ax              ; = DELTA AB
		ret                             ; zurueck zum Aufrufer
rechnen         endp                            ; Procedurende
;
; --- Unterprocedure Punkt mit kleinstem Fehler suchen ----
;
pruefen         proc    near
		add     cx,incx                 ; naechste Spalte
		mov     ax,deltaab
		cmp     ax,00h                  ; Delta AB > 0 ?
		js      lmarke1                 ; ja, dann
		add     dx,incy                 ; addiere INCY
		add     ax,inca                 ; Delta AB + INCA
		jmp     lmarke2
lmarke1:        add     ax,incb                 ; addiere INCB
lmarke2:        mov     deltaab,ax              ; wieder speichern
		ret
pruefen         endp

line            endp                            ; Ende Unterprogramm LINE

warte MACRO zeit				; warteschleife
      LOCAL loop1,loop2				; wird nicht mehr benîtigt
	push cx
	push ax
	mov ax,zeit
loop1:  mov cx,07fffh
loop2:  loop loop2
	dec ax
	jnz loop1       
	pop ax
	pop cx
ENDM

rechteck MACRO x1,y1,x2,y2,farben
	push ax
	push bx
	push cx
	push dx
	push di

	mov al,farben
	mov cx,x1
	mov dx,y1
	mov bx,x1
	mov di,y2
	call line
	mov cx,x2
	mov dx,y2
	call line
	mov bx,x2
	mov di,y1
	call line
	mov cx,x1
	mov dx,y1
	call line

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
ENDM

dreieck MACRO x1,y1,x2,y2,x3,y3,farben		; Dreieck zeichnen
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov cx,x1
	mov dx,y1
	mov bx,x2
	mov di,y2
	mov al,farben
	call line

	mov cx,x2
	mov dx,y2
	mov bx,x3
	mov di,y3
	mov al,farben
	call line

	mov cx,x3
	mov dx,y3
	mov bx,x1
	mov di,y1
	mov al,farben
	call line

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
ENDM

NewStep MACRO dir,reg,max		; nÑchsten Schritt berechnen
	LOCAL up,down,ready
	cmp dir,0
	jz down

up:	inc reg				; incrementieren
	cmp reg,max
	jnz ready
	mov dir,0			; Richtung tauschen
	jmp ready

down:	dec reg				; decrementieren
	cmp reg,1
	jnz ready
	mov dir,1			; Richtung tauschen
	
ready:
ENDM

start:	mov ax,@data				; Programmstart
	mov ds,ax				; datensegment initialisieren
	mov ax,40h			
	mov es,ax				; 40h nach es laden
	mov bx,5				; 5 Dreiecke je Farbe

again:	mov al,19				; 320x200,256 Farben
	call bsmode

	rechteck 0,0,319,171,9				; Rahmen zeichnen

	mov bh,0
	mov dh,22
	mov dl,0
	mov ah,2
	int 10h					; cursor positionieren

	mov ah,09				; text ausgeben
	mov dx,offset text
	int 21h

	mov si,es:1ah				; keyb_head nach si

schleife:
	dreieck x11,y11,x12,y12,x13,y13,col1	; 1.dreieck zeichnen
	cmp two,0				; ,wenn nîtig, dann
	jz mark1
	dreieck x21,y21,x22,y22,x23,y23,col2	; 2.dreieck zeichnen

mark1:	cmp clear,0
	jnz mark7
	jmp mark5
mark7:	dreieck x11,y11,x12,y12,x13,y13,0	; 1.dreieck lîschen

	cmp two,0				; wenn nîtig, dann
	jnz mark6
	jmp mark5
mark6:	dreieck x21,y21,x22,y22,x23,y23,0	; 2.dreieck lîschen
		
mark5:	cmp usecol,0
	jz nocol				; farbe Ñndern ?

	dec bl					; mit farbwechsel warten		
	jnz nocol
	inc col1				; nÑchste farbe
	dec col2
	mov bl,anzahl				

nocol:	NewStep dirx11,x11,318			; neue position berechnen
	NewStep diry11,y11,170			; ( Dreieck 1 )
	NewStep dirx12,x12,318
	NewStep diry12,y12,170
	NewStep dirx13,x13,318
	NewStep diry13,y13,170

	cmp two,0				; beide Dreiecke ?
	jnz mark2				; nein > mark3
	jmp mark3				
mark2:	NewStep dirx21,x21,318			; neue position berechnen
	NewStep diry21,y21,170			; ( Dreieck 2 )
	NewStep dirx22,x22,318
	NewStep diry22,y22,170
	NewStep dirx23,x23,318
	NewStep diry23,y23,170

mark3:	cmp si,es:1ch				; taste gedrÅckt ?
	jnz was
	jmp schleife

was:	mov ah,8				; zeichen auslesen
	int 21h
	cmp al,34h				; 4 ... einfarbig
	jnz was2
	mov usecol,0
	jmp ohneclear	

was2:   cmp al,35h				; 5 ... mehrfarbig
	jnz was3
	mov usecol,1
	jmp ohneclear

was3:	cmp al,36h				; 6 ... anzahl --
	jnz was4
	mov usecol,1
	dec anzahl
	cmp anzahl,0
	jnz w1
	mov anzahl,1
w1:	jmp ohneclear

was4:	cmp al,37h				; 7 ... anzahl ++
	jnz was5
	mov usecol,1
	inc anzahl
	jmp ohneclear

was5:	cmp al,38h				; 8 ... farbe erniedrigen
	jnz was6
	dec col1 
	jmp ohneclear

was6:   cmp al,39h				; 9 ... farbe erhîhen
	jnz was7
	inc col1
	jmp ohneclear

was7:	cmp al,33h				; mit/ohne lîschen
	jnz was8
	cmp clear,1
	jz  setzero
	mov clear,1
	jmp ohneclear
setzero:mov clear,0
	jmp ohneclear

was8:   cmp al,31h				; 1 ... 1 Dreieck
	jnz was9
	mov two,0
	jmp ohneclear

was9:	cmp al,32h				; 2 ... 2 Dreiecke
	jnz was10
	mov two,1
	jmp ohneclear

was10:	cmp al,20h				; SPACE - Lîschen
	jnz was11
	jmp again

was11:  cmp al,27				; ESCape - Ende
	jz  ende
	jmp ohneclear

ende:	mov al,3				; normaler Text-Modus
	call bsmode

	mov ah,4ch
	int 21h					; zurÅck zu DOS

ohneclear:
	mov si,es:1ah				; wieder kbd_head lesen
	jmp schleife				; zurÅck

end start
