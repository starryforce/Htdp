#lang htdp/isl

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 


; An NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Boolean)
; interpretation non-empty list of Boolean values


; An [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NEList-of ITEM])
; interpretation non-empty list of Items

; [NEList-of CTemperature]

; [NEList-of Boolean]