; ------------------------------------------------
; Precarga de datos ------------------------------
;-------------------------------------------------

(setq grafo 
	'(
		(a(b f)) (b(a c)) (c(b d)) (d(c n e)) (e(d)) (f(g))
		(g(h)) (h(i l)) (i(m j)) (j( k)) (k(o))
		(l (b f)) (m (l c)) (n (j m)) (o(e n))
	)
)

(setq diccionario
	'(
		(a (PaseoColon Independencia))
		(b (PaseoColon Chile))
		(c (PaseoColon Mexico ))
		(d (PaseoColon Venezuela))
		(e (PaseoColon Belgrano))
		(f (Independencia Balcarce))
		(g (Independencia Defensa))
		(h (Defensa Chile))
		(i (Defensa Mexico))
		(j (Defensa Venezuela))
		(k (Defensa Belgrano ))
		(l (Balcarce Chile ))
		(m (Balcarce Mexico))
		(n (Balcarce Venezuela))
		(o (Balcarce Belgrano))
	)
)

; ------------------------------------------------
; Programa principal -----------------------------
;-------------------------------------------------

(
	defun GPS (inicial final)

	(setq ini (obtenerNodos inicial diccionario))

	(setq fin (obtenerNodos final diccionario))

	(GPSbase ini fin grafo)
)

(
    defun GPSbase (ini fin grafo &optional (trayectoria (list (list ini))))

   	(if (null trayectoria) nil
        (if (eq (caar trayectoria) fin) (reverse (car trayectoria))
            (GPSbase ini fin grafo
                (append (mapcar (lambda (x) (cons x (car trayectoria)))
                    (diferencia (vecinos (caar trayectoria) grafo)
                        (car trayectoria)
                    )
                    )
                    (cdr trayectoria)
                    )
                )
        )
    )
)

; ------------------------------------------------
; Devuelve los nodos vecinos del nodo parámetro --
;-------------------------------------------------
(
	defun vecinos (nodo lista)
	(if (null nodo) nil
		(if (eq (caar lista) nodo) (cadar lista)
			(vecinos nodo (cdr lista))
		)
	)
)

; ------------------------------------------------
; Traductor --------------------------------------
;-------------------------------------------------
(
	defun traductor ()
	(if (null nodo) nil
		(if (eq (caar lista) nodo) (cadar lista)
			(vecinos nodo (cdr lista))
		)
	)
)

; ------------------------------------------------
; Diferencia de conjuntos ------------------------
;-------------------------------------------------
(
	defun diferencia (lista1 lista2 &optional dif)
		(if (null lista1) dif
			(if (null lista2) lista1
				(if (pertenece (car lista1) lista2) (diferencia (cdr lista1) lista2)
					(append (cons (car lista1) dif) (diferencia (cdr lista1) lista2))
				)
			)
		)
)

; ------------------------------------------------
; Función pertenece ------------------------------
;-------------------------------------------------
(
	defun pertenece (e lista)
		(if (null lista) nil
			(if (eq e (car lista)) T
				(pertenece e (cdr lista))
			)
		)
)

; ------------------------------------------------
; Obtiene la letra que identif. al nodo ----------
;-------------------------------------------------
(
	defun obtenerNodos (listaEsquina dic)
	(if (null listaEsquina) nil
		(if 
			(and 	(eq (car listaEsquina) (caadar dic)) 
					(eq (cadr listaEsquina) (cadadr (car dic)))) (caar dic)
			; No coinciden las dos calles
			(obtenerNodos listaEsquina (cdr dic))
		)
	)
)
