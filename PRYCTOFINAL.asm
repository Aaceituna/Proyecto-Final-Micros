#include "p16f887.inc"

; CONFIG1
; __config 0xE0F4
 __CONFIG _CONFIG1, _FOSC_INTRC_NOCLKOUT & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF & _LVP_OFF
; CONFIG2
; __config 0xFFFF
 __CONFIG _CONFIG2, _BOR4V_BOR40V & _WRT_OFF
;*******************************************************************************
   GPR_VAR        UDATA
   W_TEMP         RES        1      ; w register for context saving (ACCESS)
   STATUS_TEMP    RES        1      ; status used for context saving
   DELAY1	  RES	     1
   DELAY2	  RES	     1
   CCP1		  RES	     1	    ;VARIABLE SERVO 1
   CCP2		  RES	     1	    ;VARIABLE SERVO 2
   CCP3		  RES	     1	    ;VARIABLE SERVO 3
   CCP4		  RES	     1	    ;VARIABLE SERVO 4
   CCP31	  RES	     1	    ;SERVO3 MANUAL
   CCP41	  RES	     1	    ;SERVO4 MANUAL
   SERVO3	  RES	     1	    ;Variables que me servirán para darle un 
   SERVO4	  RES	     1	    ;límite al avance del servo
   CONTADOR	  RES	     1      ;Variable control
   CONTADOR1	  RES	     1
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

;*******************************************************************************
ISR       CODE    0x0004           ; interrupt vector location
       PUSH:
    MOVWF   W_TEMP
    SWAPF   STATUS, 0
    MOVWF   STATUS_TEMP
;*******************************************************************************

CHECKT0IF:
    BTFSC   INTCON, T0IF
    CALL    BANDERA_TMR0
    GOTO    POP
    
    
    
;*******************************************************************************
POP:
    SWAPF   STATUS_TEMP, 0
    MOVWF   STATUS
    SWAPF   W_TEMP, 1
    SWAPF   W_TEMP, 0
    RETFIE
     
;**************************Funcion de la interrupción***************************
     BANDERA_TMR0:
    MOVLW   .254
    MOVWF   TMR0  
    BCF	    INTCON, T0IF ;RST AL TMR0
    MOVLW   .0
    SUBWF   CCP31, 0
    BTFSC   STATUS,Z
    CALL    PARAR
    CALL    MOVER
    REVISION:
    MOVF    CCP31,W
    SUBWF   CONTADOR
    BTFSS   STATUS,C
    CALL    DETENER
    
    MOVLW   .0
    SUBWF   CCP41,0
    BTFSC   STATUS,Z
    CALL    PARAR1
    CALL    MOVER1
    REVISION1:
    MOVF    CCP41,W
    SUBWF   CONTADOR1
    BTFSS   STATUS,C
    CALL    DETENER1
    RETURN
     
;-------------------------------------------------------------------------------
    PARAR:
     BCF    PORTD,RD0
     GOTO   REVISION

    MOVER:
    INCF    CONTADOR
    BSF	    PORTD,RD0
    RETURN
    
    DETENER:
    BCF	    PORTD,RD0
    RETURN
    
     PARAR1:
     BCF    PORTD,RD1
     GOTO   REVISION1
    
    MOVER1:
    INCF    CONTADOR1
    BSF	    PORTD,RD1
    RETURN
    
    DETENER1:
    BCF	    PORTD,RD1
    RETURN
     
;*******************************************************************************
; MAIN PROGRAM
    MAIN_PROG CODE                      ; let linker place main program

START
;*******************************************************************************
    CALL    CONFIG_RELOJ	    ; RELOJ INTERNO DE 2Mhz
    CALL    CONFIG_IO
    CALL    CONFIG_TX_RX	    ; 10417hz
    CALL    CONFIG_ADC		    ; canal 0, fosc/8, adc on, justificado a la izquierda, Vref interno (0-5V)
    CALL    CONFIG_PWM1
    CALL    CONFIG_PWM2		    ;Configuración para los primeros 2 servos
    CALL    CONFIG_TMR0
    CALL    CONFIG_INTERRUPT
    BANKSEL PORTA
;*******************************************************************************
    CLRF    CCP1
    CLRF    CCP2
    CLRF    CCP3
    CLRF    CCP4
    
    
    LOOP:
    BCF	    ADCON0, CHS3	    ; CANAL 0 PARA LA CONVERSIÓN
    BCF	    ADCON0, CHS2
    BCF	    ADCON0, CHS1
    BCF	    ADCON0, CHS0
    CALL    DELAY_50MS
    BSF	    ADCON0, GO		    ; EMPIEZA LA CONVERSIÓN
CHECK_AD:
    BTFSC   ADCON0, GO		    ; revisa que terminó la conversión
    GOTO    CHECK_AD
    BCF	    PIR1, ADIF		    ; borramos la bandera del adc
    MOVFW   ADRESH
    MOVWF   CCP1		    ; MOVEMOS EL VALOR HACIA VARIABLE CCP1

    
    BCF	    ADCON0, CHS3	    ; CANAL 1 PARA LA CONVERSION
    BCF	    ADCON0, CHS2
    BCF	    ADCON0, CHS1
    BSF	    ADCON0, CHS0
    CALL    DELAY_50MS
    BSF     ADCON0, GO		    ; EMPIECE LA CONVERSIÓN
CHECKADC1:
    BTFSC   ADCON0, GO		    ; LOOP HASTA QUE TERMINE DE CONVERTIR
    GOTO    CHECKADC1
    BCF	    PIR1, ADIF		    ; BORRAMOS BANDERA DE INTERRUPCION
    MOVFW   ADRESH
    MOVWF   CCP2		    ; MOVEMOS EL VALOR HACIA VARIABLE	CCP1
    
    
    BCF	    ADCON0, CHS3	    ; CANAL 2 PARA LA CONVERSION
    BCF	    ADCON0, CHS2
    BSF	    ADCON0, CHS1
    BCF	    ADCON0, CHS0
    CALL    DELAY_50MS
    BSF     ADCON0, GO		    ; EMPIECE LA CONVERSIÓN
CHECKADC2:
    BTFSC   ADCON0, GO		    ; LOOP HASTA QUE TERMINE DE CONVERTIR
    GOTO    CHECKADC2
    BCF	    PIR1, ADIF		    ; BORRAMOS BANDERA DE INTERRUPCIÓN
    MOVFW   ADRESH
    MOVWF   CCP3		    ; MOVEMOS EL VALOR HACIA VARIABLE	CCP1
    
    
    BCF	    ADCON0, CHS3	    ; CANAL 3 PARA LA CONVERSION
    BCF	    ADCON0, CHS2
    BSF	    ADCON0, CHS1
    BSF	    ADCON0, CHS0
    CALL    DELAY_50MS
    BSF     ADCON0, GO		    ; EMPIECE LA CONVERSIÓN
CHECKADC3:
    BTFSC   ADCON0, GO		    ; LOOP HASTA QUE TERMINE DE CONVERTIR
    GOTO    CHECKADC3
    BCF	    PIR1, ADIF		    ; BORRAMOS BANDERA DE INTERRUPCION
    MOVFW   ADRESH
    MOVWF   CCP4		    ; MOVEMOS EL VALOR HACIA VARIABLE	CCP1
	
    
    CHECK_RCIF:		    ; RECIBE EN RX y lo manda al registro que controla al servo
    BTFSS   PIR1, RCIF
    GOTO    CHECK_TXIF
    MOVF    RCREG, W
    MOVWF   CCPR1L
    CALL    DELAY_500US
    MOVF    RCREG, W
    MOVWF   CCPR2L
    CALL    DELAY_500US
    MOVF    RCREG, W
    MOVWF   CCP31
    CALL    DELAY_500US
    MOVF    RCREG, W
    MOVWF   CCP41
  
    
    
CHECK_TXIF: 
    BTFSS   PIR1, TXIF
    GOTO    CHECK_TXIF
    MOVFW   CCP1		    ; ENVÍA CCP1 POR EL TX
    MOVWF   TXREG
    CALL    DELAY_500US
    MOVFW   CCP2		    ; ENVÍA CCP2 POR EL TX
    MOVWF   TXREG
    CALL    DELAY_500US
    MOVFW   CCP3		    ; ENVÍA CCP3 POR EL TX
    MOVWF   TXREG
    CALL    DELAY_500US
    MOVFW   CCP4		    ; ENVÍA CCP4 POR EL TX
    MOVWF   TXREG
    CALL    DELAY_500US
    MOVLW   .13		    ; ENVÍA 13 POR EL TX. Así los servos sabrán 
    MOVWF   TXREG		    ;que valor tomar sin traslaparse
   
    
    GOTO LOOP
;*******************************************************************************
    CONFIG_RELOJ
    BANKSEL TRISA
    
    BSF OSCCON, IRCF2
    BCF OSCCON, IRCF1
    BSF OSCCON, IRCF0		    ; FRECUECNIA DE 2MHz
    RETURN
 
 ;--------------------------------------------------------
    CONFIG_TX_RX
    BANKSEL TXSTA
    BCF	    TXSTA, SYNC		    ; ASINCRÓNO
    BSF	    TXSTA, BRGH		    ; LOW SPEED
    BANKSEL BAUDCTL
    BSF	    BAUDCTL, BRG16	    ; 8 BITS BAURD RATE GENERATOR
    BANKSEL SPBRG
    MOVLW   .51	    
    MOVWF   SPBRG		    ; CARGAMOS EL VALOR DE BAUDRATE CALCULADO
    CLRF    SPBRGH
    BANKSEL RCSTA
    BSF	    RCSTA, SPEN		    ; HABILITAR SERIAL PORT
    BCF	    RCSTA, RX9		    ; SOLO MANEJAREMOS 8BITS DE DATOS
    BSF	    RCSTA, CREN		    ; HABILITAMOS LA RECEPCIÓN 
    BANKSEL TXSTA
    BSF	    TXSTA, TXEN		    ; HABILITO LA TRANSMISION
    
    RETURN
;--------------------------------------
CONFIG_IO
    BANKSEL TRISA
    CLRF    TRISA
    BSF	    TRISA, RA0	; RA0 COMO ENTRADA
    BSF	    TRISA, RA1	; RA1 COMO ENTRADA
    BSF	    TRISA, RA2	; RA2 COMO ENTRADA
    BSF	    TRISA, RA3	; RA3 COMO ENTRADA
    CLRF    TRISC
    CLRF    TRISD
    CLRF    TRISB
    BANKSEL ANSEL
    CLRF    ANSEL
    BSF	    ANSEL, 0	; ANS0 COMO ENTRADA ANALÓGICA
    BSF	    ANSEL, 1	; ANS1 COMO ENTRADA ANALÓGICA
    BSF	    ANSEL, 2	; ANS0 COMO ENTRADA ANALÓGICA
    BSF	    ANSEL, 3	; ANS1 COMO ENTRADA ANALÓGICA
    CLRF    ANSELH
    BANKSEL PORTA
    CLRF    PORTA
    CLRF    PORTC
    CLRF    PORTB
    RETURN    
;-----------------------------------------------
    CONFIG_ADC
    BANKSEL PORTA
    BCF ADCON0, ADCS1
    BSF ADCON0, ADCS0		; FOSC/8 RELOJ TAD
    	
    BANKSEL TRISA
    BCF ADCON1, ADFM		; JUSTIFICACIÓN A LA IZQUIERDA
    BCF ADCON1, VCFG1		; VSS COMO REFERENCIA VREF-
    BCF ADCON1, VCFG0		; VDD COMO REFERENCIA VREF+
    BANKSEL PORTA
    BSF ADCON0, ADON		; ENCIENDO EL MÓDULO ADC
    
    RETURN
;-----------------------------------------------
DELAY_50MS
    MOVLW   .50		    ; 1US 
    MOVWF   DELAY2
    CALL    DELAY_500US
    DECFSZ  DELAY2		    ;DECREMENTA CONT1
    GOTO    $-2			    ; IR A LA POSICION DEL PC - 1
    RETURN
    
DELAY_500US
    MOVLW   .250		    ; 1US 
    MOVWF   DELAY1	    
    DECFSZ  DELAY1		    ;DECREMENTA CONT1
    GOTO    $-1			    ; IR A LA POSICION DEL PC - 1
    RETURN
    ;---------------------------------------------------------------
    CONFIG_PWM1
    BANKSEL TRISC
    BSF	    TRISC, RC1		; ESTABLEZCO RC1 / CCP2 COMO ENTRADA
    MOVLW   .255
    MOVWF   PR2			; COLOCO EL VALOR DEL PERIODO DE MI SEÑAL 2.5mS
    
    BANKSEL PORTA
    BSF	    CCP2CON, CCP2M3
    BSF	    CCP2CON, CCP2M2
    BSF	    CCP2CON, CCP2M1
    BSF	    CCP2CON, CCP2M0		    ; MODO PWM
    

    
    MOVLW   B'00011011'
    MOVWF   CCPR2L		    ; MSB   DEL DUTY CICLE
    BSF	    CCP2CON, DC2B0
    BSF	    CCP2CON, DC2B1	    ; LSB del duty cicle
    
    BCF	    PIR1, TMR2IF
    
    BSF	    T2CON, T2CKPS1
    BSF	    T2CON, T2CKPS0	    ; PRESCALER 1:16
    
    BSF	    T2CON, TMR2ON	    ; HABILITAMOS EL TMR2
    BTFSS   PIR1, TMR2IF
    GOTO    $-1
    BCF	    PIR1, TMR2IF
    
    BANKSEL TRISC
    BCF	    TRISC, RC1		    ; RC1 / CCP2 SALIDA PWM
    RETURN
    
;-----------------------------------------------------------------------------
    CONFIG_PWM2
    BANKSEL TRISC
    BSF	    TRISC, RC2		; ESTABLEZCO RC2 / CCP1 COMO ENTRADA
    MOVLW   .255
    MOVWF   PR2			; COLOCO EL VALOR DEL PERIODO DE MI SEÑAL 20mS
    
    BANKSEL PORTA
    BCF	    CCP1CON, P1M0
    BCF	    CCP1CON, P1M1
    BSF	    CCP1CON, CCP1M3
    BSF	    CCP1CON, CCP1M2
    BCF	    CCP1CON, CCP1M1
    BCF	    CCP1CON, CCP1M0		    ; MODO PWM
    

    
    MOVLW   B'00011011'
    MOVWF   CCPR1L		    ; MSB   DEL DUTY CICLE
    BSF	    CCP1CON, DC1B0
    BSF	    CCP1CON, DC1B1	    ; LSB del duty cicle
    
    BCF	    PIR1, TMR2IF
    
    BSF	    T2CON, T2CKPS1
    BSF	    T2CON, T2CKPS0	    ; PRESCALER 1:16
    
    BSF	    T2CON, TMR2ON	    ; HABILITAMOS EL TMR2
    BTFSS   PIR1, TMR2IF
    GOTO    $-1
    BCF	    PIR1, TMR2IF
    
    BANKSEL TRISC
    BCF	    TRISC, RC2		    ; RC1 / CCP1 SALIDA PWM
    RETURN
    
    ;---------------------------------------------------------------------------
    CONFIG_TMR0:
    BANKSEL TRISA
    BCF OPTION_REG, T0CS    ; TMR0 se pone como temporizador
    BCF OPTION_REG, PSA	    ; ASIGNAMOS PRESCALER A TMR0
    
    BSF OPTION_REG, PS2
    BSF OPTION_REG, PS1
    BSF OPTION_REG, PS0		; El prescaler se pone en 256
    
    BCF STATUS, 6
    BCF STATUS, 5	    ; Banco cero 
    MOVLW .254
    MOVWF TMR0			; Se carga el valor de N para un desborde de 500 ms
    BCF	INTCON, T0IF
    RETURN
    ;---------------------------------------------------------------------------
    CONFIG_INTERRUPT:
    BANKSEL PORTA
    BSF	    INTCON, GIE
    BSF	    INTCON, T0IE
    BCF	    INTCON, T0IF
    BCF	    INTCON, PEIE
    BCF	    INTCON, INTE
    BCF	    INTCON, RBIE
    BCF	    INTCON, INTF
    BCF	    INTCON, RBIF
    RETURN
    
    END
    jjjjjjj
