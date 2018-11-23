;*************************************************************
; ANTONIO ALTUNA. ADRIAN AYALA 
; UVG
; PROYECTO PWM
;*************************************************************
#include "p16f887.inc"

    ;probando 
    ; version pwm 
; CONFIG1
; __config 0xFCD4
 __CONFIG _CONFIG1, _FOSC_INTRC_NOCLKOUT & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF & _LVP_OFF
; CONFIG2
; __config 0xFFFF
 __CONFIG _CONFIG2, _BOR4V_BOR40V & _WRT_OFF
 
 
GPR_VAR	    UDATA
DELAY1	    RES	1
W_TEMP	    RES	1
STATUS_TEMP RES	1

RES_VECT    CODE    0x0000		; processor reset vector
    GOTO    START			; go to beginning of program

ISR_VECT    CODE    0x0004
PUSH:
    MOVWF   W_TEMP
    SWAPF   STATUS, W
    MOVWF   STATUS_TEMP
ISR:
    ; acá se colocaría el código en la interrupcion 
POP:
    SWAPF   STATUS_TEMP, W
    MOVWF   STATUS
    SWAPF   W_TEMP, F
    SWAPF   W_TEMP, W
    RETFIE
; TODO ADD INTERRUPTS HERE IF USED
;-------------------------------PRINCIPAL---------------------------------------
MAIN_PROG CODE                      ; let linker place main program

START
    CALL    CONFIG_OSCILATOR
    CALL    CONFIG_IO
    CALL    CONFIG_ADC
    CALL    CONFIG_PWM
    CALL    CONFIG_PWM2
    BANKSEL PORTA
    
LOOP
    CALL    CHANNEL1
    CALL    DELAY_500US
    BSF	    ADCON0, GO		; EMPIECE LA CONVERSIÓN
CHECKADC:
    BTFSC   ADCON0, GO		; LOOP HASTA QUE TERMINE DE CONVERTIR
    GOTO    CHECKADC
    MOVF    ADRESH, W
    MOVWF   PORTB		; MUEVE LA CONVERSION AL PUERTO B
    BCF	    PIR1, ADIF		; BORRAMOS BANDERA DE
    
    RRF	    ADRESH, F		
    RRF	    ADRESH, F
    RRF	    ADRESH, W		; LE QUITAMOS LOS 3 BITS MENOS SIGNIFICATIVOS A LA CONVERSION
    ANDLW   B'00011111'		
    MOVWF   CCPR2L		; MOVEMOS EL VALOR HACIA EL PERÍODO DEL PWM ROTAMOS PARA LOGRAR SOLO USAR EL TIEMPO EN ALTO DEL PWM
  
    CALL    CHANNEL2
    
    CALL    DELAY_500US
    BSF	    ADCON0, GO		; EMPIECE LA CONVERSIÓN
CHECKADC2:
    BTFSC   ADCON0, GO		; LOOP HASTA QUE TERMINE DE CONVERTIR
    GOTO    CHECKADC2
    MOVF    ADRESH, W
    MOVWF   PORTB		; MUEVE LA CONVERSION AL PUERTO B
    BCF	    PIR1, ADIF		; BORRAMOS BANDERA DE
    
    RRF	    ADRESH, F		
    RRF	    ADRESH, F
    RRF	    ADRESH, W		; LE QUITAMOS LOS 3 BITS MENOS SIGNIFICATIVOS A LA CONVERSION
    ANDLW   B'00011111'		
    MOVWF   CCPR1L		; MOVEMOS EL VALOR HACIA EL PERÍODO DEL PWM ROTAMOS PARA LOGRAR SOLO USAR EL TIEMPO EN ALTO DEL PWM
    
    GOTO    LOOP                          
;------------------------------SUBRUTINAS---------------------------------------
CHANNEL1:
    BANKSEL PORTA
    BCF ADCON0, CHS3		; CANAL 0 PARA LA CONVERSION
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BCF ADCON0, CHS0
    RETURN
CHANNEL2:
    BANKSEL PORTA
    BCF ADCON0, CHS3		; CANAL 0 PARA LA CONVERSION
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BSF ADCON0, CHS0
    RETURN
;*******************************************************************************    
DELAY_500US
    MOVLW   .250		    ; 1US 
    MOVWF   DELAY1	    
    DECFSZ  DELAY1		    ;DECREMENTA CONT1
    GOTO    $-1			    ; IR A LA POSICION DEL PC - 1
    RETURN
;----------------------------CONFIGURACIONES----------------------------------    
CONFIG_IO
    BANKSEL TRISA
    CLRF    TRISA
    BSF	    TRISA, RA0	; RA0 COMO ENTRADA
    BSF	    TRISA, RA1  ; RA1 COMO ENTRADA
    CLRF    TRISB
    CLRF    TRISC
    CLRF    TRISD
    CLRF    TRISE
    BANKSEL ANSEL
    CLRF    ANSEL
    CLRF    ANSELH
    BSF	    ANSEL, 0	; ANS0 COMO ENTRADA ANALÓGICA
    BSF	    ANSEL, 1	; ANS1 COMO ENTRADA ANALÓGICA
    BANKSEL PORTA
    CLRF    PORTA
    CLRF    PORTB
    CLRF    PORTC
    CLRF    PORTD
    RETURN

CONFIG_INTERRUPT
    BCF	    INTCON, GIE		; NO HAY INTERRUPCIONES ACTIVAS
    BCF	    INTCON, T0IE
    BCF	    INTCON, T0IF
    RETURN

CONFIG_ADC
    BANKSEL PORTA
    BCF ADCON0, ADCS1
    BSF ADCON0, ADCS0		; FOSC/8 RELOJ TAD
    
    BCF ADCON0, CHS3		; CANAL 0 PARA LA CONVERSION
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BCF ADCON0, CHS0	
    BANKSEL TRISA
    BCF ADCON1, ADFM		; JUSTIFICACIÓN A LA IZQUIERDA
    BCF ADCON1, VCFG1		; VSS COMO REFERENCIA VREF-
    BCF ADCON1, VCFG0		; VDD COMO REFERENCIA VREF+
    BANKSEL PORTA
    BSF ADCON0, ADON		; ENCIENDO EL MÓDULO ADC
    RETURN

CONFIG_OSCILATOR
    BANKSEL TRISA
    BCF OSCCON, IRCF2
    BSF OSCCON, IRCF1
    BSF OSCCON, IRCF0		; FRECUECNIA DE 500kHz
    RETURN
    
CONFIG_PWM
    BANKSEL TRISC
    BSF	    TRISC, RC1		    ; ESTABLEZCO RC1 / CCP2 COMO ENTRADA
    MOVLW   .155
    MOVWF   PR2			    ; COLOCO EL VALOR DEL PERIODO DE MI SEÑAL 20mS
    
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
    
CONFIG_PWM2
    BANKSEL TRISC
    BSF	    TRISC, RC2		    ; ESTABLEZCO RC1 / CCP2 COMO ENTRADA
    MOVLW   .155
    MOVWF   PR2			    ; COLOCO EL VALOR DEL PERIODO DE MI SEÑAL 20mS
    
    BANKSEL PORTA
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
    BCF	    TRISC, RC2		    ; RC1 / CCP2 SALIDA PWM
    RETURN	
END
