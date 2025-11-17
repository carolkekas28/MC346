;; =============================
;; Arquivo: render_recipe.scm
;; Infraestrutura da DSL OvenFlow
;; =============================

(use-modules (srfi srfi-1)) ; Para funções de lista como acons, remove, etc.

;; ============================================================
;; Funções auxiliares
;; ============================================================

(define (build-step-result ingredients text-parts)
  (let* ((steps (acons 'steps (string-join (reverse text-parts) " ") '()))
         (with-ingredients (acons 'ingredients (reverse ingredients) steps)))
    with-ingredients))

(define (get-name recipe)
  (cdr (assv 'name recipe)))

(define (get-ingredients recipe)
  (cdr (assv 'ingredients recipe)))

(define (get-steps recipe)
  (cdr (assv 'steps recipe)))

(define (translate-unit unit-sym)
  (case unit-sym
    ((kg) "kg")
    ((g) "g")
    ((l) "l")
    ((ml) "ml")
    ((cup) " xícara(s)")
    ((teaspoon) " colher(es) de chá")
    ((tablespoon) " colher(es) de sopa")
    ((can) " lata(s)")
    ((un) " unidade(s)")
    ((min) " minuto(s)")
    ((h) " hora(s)")
    ((celsius) "°C")
    ((fahrenheit) "°F")
    (else (symbol->string unit-sym))))

(define (is-ingredient? x)
  (and (pair? x) (eq? (car x) 'ingredient)))

(define (is-time? x)
  (and (pair? x) (eq? (car x) 'time)))

(define (is-temp? x)
  (and (pair? x) (eq? (car x) 'temp)))

;; ============================================================
;; Processamento de steps
;; ============================================================

(define (process-step s)
  (let ((elements (cdr s)))
    (let loop ((rest elements)
               (ingredients '())
               (text-parts '()))
      (if (null? rest)
          (build-step-result ingredients text-parts)
          (let ((el (car rest)))
            (cond
             ((is-ingredient? el)
              (let* ((qtd (cadr el))
                     (unit-sym (caddr el))
                     (name (cadddr el))
                     (qtd-text (number->string qtd))
                     (unit-text (translate-unit unit-sym)))
                (loop (cdr rest)
                      (cons el ingredients)
                      (cons (string-append qtd-text unit-text " de " name)
                            text-parts))))

             ((is-time? el)
              (let* ((qtd (cadr el))
                     (unit-sym (caddr el))
                     (qtd-text (number->string qtd))
                     (unit-text (translate-unit unit-sym)))
                (loop (cdr rest)
                      ingredients
                      (cons (string-append qtd-text unit-text)
                            text-parts))))

             ((is-temp? el)
              (let* ((val (cadr el))
                     (unit-sym (caddr el))
                     (val-text (number->string val))
                     (unit-text (translate-unit unit-sym)))
                (loop (cdr rest)
                      ingredients
                      (cons (string-append val-text unit-text)
                            text-parts))))

             ((string? el)
              (loop (cdr rest)
                    ingredients
                    (cons el text-parts)))

             (else
              (error "Passo inválido" el))))))))

;; ============================================================
;; Tratamento da lista de ingredientes
;; ============================================================

(define (update-nested-ingredient name unit qtd summarized)
  (let ((ingredient (assoc name summarized)))
    (if ingredient
        ;; ingrediente já existe
        (let* ((old-unit-list (cdr ingredient))
               (existing-unit (assoc unit old-unit-list))
               (without-name (remove (lambda (p) (equal? (car p) name))
                                     summarized)))
          (if existing-unit
              ;; unidade já existe → somar
              (let* ((new-qtd (+ qtd (cdr existing-unit)))
                     (filtered (remove (lambda (p) (equal? (car p) unit))
                                       old-unit-list))
                     (new-unit-list (acons unit new-qtd filtered)))
                (acons name new-unit-list without-name))
              ;; unidade nova
              (let ((new-unit-list (acons unit qtd old-unit-list)))
                (acons name new-unit-list without-name))))
        ;; ingrediente novo
        (acons name (list (cons unit qtd)) summarized))))

(define (summarize-ingredients ingredient-list)
  (let loop ((xs ingredient-list)
             (acc '()))
    (if (null? xs)
        acc
        (let* ((ing (car xs))
               (qtd (cadr ing))
               (unit (caddr ing))
               (name (cadddr ing)))
          (loop (cdr xs)
                (update-nested-ingredient name unit qtd acc))))))

(define (format-unit-string unit-pair)
  (let ((unit (car unit-pair))
        (qtd (cdr unit-pair)))
    (string-append (number->string qtd)
                   (translate-unit unit))))

(define (format-unit-list-string units)
  (cond
   ((null? units) "")
   ((null? (cdr units)) (car units))
   ((null? (cddr units))
    (string-append (car units) " e " (cadr units)))
   (else
    (string-append (car units) ", "
                   (format-unit-list-string (cdr units))))))

(define (format-ingredient-string ing-entry)
  (let* ((name (car ing-entry))
         (units (cdr ing-entry))
         (formatted (map format-unit-string units))
         (joined (format-unit-list-string formatted)))
    (string-append joined " de " name)))

;; ============================================================
;; Manipulação de listas de passos e ingredientes
;; ============================================================

(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

(define (insert-new-steps action-sym index old-steps new-step-texts)
  (case action-sym
    ((add-step-to-start)
     (append new-step-texts old-steps))
    ((add-step-to-end)
     (append old-steps new-step-texts))
    ((add-step-after)
     (append (take old-steps index)
             new-step-texts
             (drop old-steps index)))
    (else old-steps)))

(define (find-index-from-modification action-sym action-args)
  (if (eq? action-sym 'add-step-after)
      (car action-args)
      #f))

(define (find-steps-from-modification action-sym action-args)
  (if (eq? action-sym 'add-step-after)
      (cdr action-args)
      action-args))

(define (merge-recipes base-recipe new-ingredients new-steps action-sym index)
  (let* ((old-ingredients (get-ingredients base-recipe))
         (old-steps (get-steps base-recipe))
         (ingredients (append new-ingredients old-ingredients))
         (steps (insert-new-steps action-sym index old-steps new-steps)))
    (list (cons 'name (get-name base-recipe))
          (cons 'ingredients ingredients)
          (cons 'steps steps))))

;; ============================================================
;; Construção de modificações (usado pelas macros)
;; ============================================================

(define (build-import-body x mod-sym action-sym recipe-id-stx index-stx)
  (let ((recipe-id (syntax->datum recipe-id-stx))
        (index (if index-stx (syntax->datum index-stx) #f)))
    (datum->syntax x
      `(define ,mod-sym
         (lambda (base-recipe)
           (let* ((mod-recipe ,recipe-id)
                  (new-ingredients (get-ingredients mod-recipe))
                  (new-step-texts (get-steps mod-recipe)))
             (merge-recipes base-recipe new-ingredients new-step-texts ',action-sym ,index)))))))

(define (build-literal-body x mod-sym action-sym args-stx)
  (let ((args (syntax->datum args-stx)))
    (datum->syntax x
      `(define ,mod-sym
         (lambda (base-recipe)
           (let* ((action-args ',args)
                  (action-sym ',action-sym)
                  (index (find-index-from-modification action-sym action-args))
                  (step-forms (find-steps-from-modification action-sym action-args))
                  (processed-list (map process-step step-forms))
                  (new-ingredients (apply append (map get-ingredients processed-list)))
                  (new-step-texts (map get-steps processed-list)))
             (merge-recipes base-recipe new-ingredients new-step-texts action-sym index)))))))

;; ============================================================
;; Macros da DSL (define-recipe e define-modification)
;; ============================================================

(define-syntax define-recipe
  (lambda (x)
    (syntax-case x ()
      ((_ name step-list ...)
       (let* ((recipe-name (symbol->string (syntax->datum #'name)))
              (raw-steps (syntax->datum #'(step-list ...)))
              (processed (map process-step raw-steps))
              (ingredients (apply append (map get-ingredients processed)))
              (steps (map get-steps processed)))
         (datum->syntax
           x
           `(define ,(syntax->datum #'name)
              '((name . ,recipe-name)
                (ingredients . ,ingredients)
                (steps . ,steps)))))))))

(define-syntax define-modification
  (lambda (x)
    (syntax-case x ()
      ((_ mod-name (action . args))
       (let ((mod-sym (syntax->datum #'mod-name))
             (action-sym (syntax->datum #'action)))
         (syntax-case #'args ()
           ((index recipe-id)
            (and (integer? (syntax->datum #'index))
                 (identifier? #'recipe-id))
            (build-import-body x mod-sym action-sym #'recipe-id #'index))

           ((recipe-id)
            (identifier? #'recipe-id)
            (build-import-body x mod-sym action-sym #'recipe-id #f))

           (_
            (build-literal-body x mod-sym action-sym #'args))))))))

;; ============================================================
;; Função final create-recipe (antes era macro)
;; ============================================================

(define (create-recipe final-name composition-expr)
  (let ((final-recipe composition-expr))
    (display (string-append "## " final-name "\n\n"))

    (display "### Ingredientes\n\n")
    (let* ((raw (get-ingredients final-recipe))
           (summ (summarize-ingredients raw))
           (formatted (map format-ingredient-string summ)))
      (let loop ((xs formatted))
        (when (pair? xs)
          (display (string-append "* " (car xs) "\n"))
          (loop (cdr xs)))))

    (display "\n### Modo de Preparo\n\n")
    (let loop2 ((steps (get-steps final-recipe)) (n 1))
      (when (pair? steps)
        (display (string-append (number->string n) ". " (car steps) "\n"))
        (loop2 (cdr steps) (+ n 1))))))