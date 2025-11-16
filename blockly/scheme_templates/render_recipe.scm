;;Funções auxiliadoras
(use-modules (srfi srfi-1)) ; Importa o módulo SRFI-1 para funções de lista (como 'acons', 'remove', 'append').

; retorna uma lista com duas posições: a primeira é a lista de ingredientes, a segunda é a string com o texto do passo completo.
(define (build-step-result ingredients text-parts)
  (let* ((steps (acons 'steps (string-join (reverse text-parts) " ") '())) ; Cria a entrada 'steps' juntando as partes do texto.
         (with-ingredients (acons 'ingredients (reverse ingredients) steps))) ; Cria a entrada 'ingredients' com a lista de ingredientes.
    with-ingredients)) ; Retorna o resultado no formato ((ingredients . <lista>) (steps . <string>)).

; Obtém o nome da receita de uma estrutura de receita.
(define (get-name recipe)
  (cdr (assv 'name recipe)))

; Obtém a lista de ingredientes de uma estrutura de receita.
(define (get-ingredients recipe)
  (cdr (assv 'ingredients recipe)))

; Obtém a lista de passos (texto completo) de uma estrutura de receita.
(define (get-steps recipe)
  (cdr (assv 'steps recipe)))

; Converte símbolos de unidade pré-definidos em texto legível.
(define (translate-unit unit-sym)
  (case unit-sym
    ; Unidades de peso/volume/contagem
    ((kg) "kg")
    ((g) "g")
    ((l) "l")
    ((ml) "ml")
    ((cup) " xícara(s)")
    ((teaspoon) " colher(es) de chá")
    ((tablespoon) " colher(es) de sopa")
    ((can) " lata(s)")
    ((un) " unidade(s)")

    ; Novas Unidades de TEMPO
    ((min) " minuto(s)")
    ((h) " hora(s)")
    
    ; Novas Unidades de TEMPERATURA
    ((celsius) "°C")
    ((fahrenheit) "°F")
    
    ; Fallback para qualquer outro símbolo
    (else (symbol->string unit-sym))))

; verifica se é compatível com (ingredient "alguma string") - Predicado para ingredientes.
(define (is-ingredient? x)
  (and (pair? x) (eq? (car x) 'ingredient)))

; verifica se é compatível com (time ...) - Predicado para tempo.
(define (is-time? x)
  (and (pair? x) (eq? (car x) 'time)))

; verifica se é compatível com (temp ...) - Predicado para temperatura.
(define (is-temp? x)
  (and (pair? x) (eq? (car x) 'temp)))

; processa cada (step ...) - Converte a forma do passo em uma lista de ingredientes e um texto de passo formatado.
(define (process-step s)
(let ((elements (cdr s))) ; elimina a palavra step (primeira posição) da lista a ser processada
 (let loop ((rest elements)
            (ingredients '())
            (text-parts '())) ; text-parts é tudo aquilo que foi passado dentro do step que não é um ingredient (texto puro)
   
   (if (null? rest)
       (build-step-result ingredients text-parts) ; Fim da lista, retorna o resultado.
       (let ((el (car rest)))
         (cond
          
           ; ingrediente — deve ser (ingredient <qtd> <unidade> <nome>)
           ((is-ingredient? el)
            (let* ((qtd (cadr el))
                  (unit-sym (caddr el))
                  (name (cadddr el))
                  (qtd-text (number->string qtd))
                  (unit-text (translate-unit unit-sym)))
              ; Adiciona o ingrediente à lista e a string formatada à lista de partes do texto.
              (loop (cdr rest)
                    (cons el ingredients)
                    (cons (string-append qtd-text unit-text " de " name) text-parts))))
          
          ; NOVO: Tempo — deve ser (time <qtd> <unidade>)
           ((is-time? el)
            (let* ((qtd (cadr el))
                   (unit-sym (caddr el))
                   (qtd-text (number->string qtd))
                   (unit-text (translate-unit unit-sym))
                   (time-text (string-append qtd-text unit-text)))
              ; Adiciona apenas a string formatada à lista de partes do texto (não é ingrediente).
              (loop (cdr rest)
                    ingredients
                    (cons time-text text-parts))))

          ; NOVO: Temperatura — deve ser (temp <valor> <unidade>)
           ((is-temp? el)
            (let* ((val (cadr el))
                   (unit-sym (caddr el))
                   (val-text (number->string val))
                   (unit-text (translate-unit unit-sym))
                   (temp-text (string-append val-text unit-text)))
              ; Adiciona apenas a string formatada à lista de partes do texto (não é ingrediente).
              (loop (cdr rest)
                    ingredients
                    (cons temp-text text-parts))))
          
           ; texto literal (string)
           ((string? el)
            ; Adiciona a string literal à lista de partes do texto.
            (loop (cdr rest)
                  ingredients
                  (cons el text-parts)))
          
           ; qualquer outra coisa é inválida
           (else
            (error "Passo inválido" el))))))))


; Retorna os primeiros N elementos de uma lista.
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

; Retorna a lista sem os primeiros N elementos.
(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;;;Modificações naa receitas
; Insere novos passos na lista de passos antigos com base na ação e no índice.
(define (insert-new-steps action-sym index old-steps new-step-texts)
  (case action-sym
        ((add-step-to-start)
         (append new-step-texts old-steps)) ; Adiciona no início.
        ((add-step-to-end)
         (append old-steps new-step-texts)) ; Adiciona no final.
        ((add-step-after)
         (append (take old-steps index) ; Pega os passos até o índice.
                 new-step-texts          ; Insere os novos passos.
                 (drop old-steps index))) ; Adiciona o resto dos passos.
        (else old-steps))) ; Caso de ação inválida/não suportada.

; Encontra o índice necessário para a modificação (apenas para 'add-step-after').
(define (find-index-from-modification action-sym action-args)
  (if (eq? action-sym 'add-step-after)
      (car action-args)
      #f))

; Encontra os passos (ou o ID da receita a ser importada) dos argumentos da modificação.
(define (find-steps-from-modification action-sym action-args)
  (if (eq? action-sym 'add-step-after)
      (cdr action-args) ; Se 'add-step-after', o primeiro argumento é o índice, o resto são os passos.
      action-args)) ; Caso contrário, todos os argumentos são os passos.

; Combina a receita base com novos ingredientes e passos, aplicando a ação de inserção.
(define (merge-recipes base-recipe new-ingredients new-step-texts action-sym index)
  (let* ((old-ingredients (get-ingredients base-recipe))
         (old-steps (get-steps base-recipe))
         
         (new-ingredient-list (append new-ingredients old-ingredients)) ; Combina as listas de ingredientes.
         (new-step-list (insert-new-steps action-sym index old-steps new-step-texts))) ; Insere os novos passos.
    
    ; Retorna a nova estrutura de receita combinada.
    (list (cons 'name (get-name base-recipe))
          (cons 'ingredients new-ingredient-list)
          (cons 'steps new-step-list))))

(merge-recipes '((name . "base")(ingredients . (A B C))(steps . (1 2 5))) '(D E F) '(3 4) 'add-step-after 2) ; Teste de exemplo

; Constrói o corpo da função de modificação quando a modificação importa outra receita (Caso 1 e 2 das macros).
(define (build-import-body x mod-sym action-sym recipe-id-stx index-stx)
  (let ((recipe-id-datum (syntax->datum recipe-id-stx)) ; Obtém o valor do ID da receita.
        (index-datum (if index-stx (syntax->datum index-stx) #f))) ; Obtém o valor do índice (se houver).
    
    (datum->syntax x
      `(define ,mod-sym ; Define a função de modificação.
         (lambda (base-recipe) ; Recebe a receita base.
           (let* ((mod-recipe ,recipe-id-datum) ; Obtém a receita a ser importada.
                  (new-ingredients (get-ingredients mod-recipe)) ; Extrai ingredientes da receita de importação.
                  (new-step-texts  (get-steps mod-recipe))) ; Extrai passos da receita de importação.
             (merge-recipes base-recipe new-ingredients new-step-texts ',action-sym ,index-datum))))))) ; Combina as receitas.


; teste de build-import-body - Bloco para testar a função auxiliar de construção do corpo para importação.
(let-syntax ((test-import-body (lambda (x)
                                  (syntax-case x ()
                                               ((_ mod-name (action recipe-id))
                                                (let ((mod-sym (syntax->datum #'mod-name))
                                                      (action-sym (syntax->datum #'action)))
                                                  (build-import-body x mod-sym action-sym #'recipe-id #f)))))))
  (let ((mod '((name . "mod")(ingredients . (D E))(steps . (4 5)))))
    (test-import-body teste (add-step-to-end mod))
    (teste '((name . "base")(ingredients . (A B C))(steps . (1 2 3))))))


; Constrói o corpo da função de modificação quando a modificação é definida literalmente (Caso 3 da macro).
(define (build-literal-body x mod-sym action-sym args-stx)
      (let ((action-args-datum (syntax->datum args-stx))) ; Obtém os argumentos literais da modificação.
        (datum->syntax x
          `(define ,mod-sym ; Define a função de modificação.
             (lambda (base-recipe) ; Recebe a receita base.
               (let* ((action-args ',action-args-datum)
                      (action-sym ',action-sym)
                      (index (find-index-from-modification action-sym action-args)) ; Encontra o índice.
                      (step-forms (find-steps-from-modification action-sym action-args)) ; Encontra os passos literais.
                      (processed-list (map process-step step-forms)) ; Processa cada passo para extrair ingredientes e texto.
                      (new-ingredients (apply append (map get-ingredients processed-list))) ; Concatena todos os ingredientes.
                      (new-step-texts  (map get-steps processed-list))) ; Obtém o texto formatado dos passos.
                 (merge-recipes base-recipe new-ingredients new-step-texts action-sym index))))))) ; Combina as receitas.
                            
; teste de build-literal-body - Bloco para testar a função auxiliar de construção do corpo literal.
(let-syntax ((test-literal-body (lambda (x)
                                  (syntax-case x ()
                                               ((_ mod-name (action . args))
                                                (let ((mod-sym (syntax->datum #'mod-name))
                                                      (action-sym (syntax->datum #'action)))
                                                  (build-literal-body x mod-sym action-sym #'args)))))))
  (test-literal-body teste (add-step-after 1
                            (step "Adicionar" (ingredient 3 un "cenouras médias raladas") "à mistura e bater novamente")))
  (teste '((name . "base")(ingredients . (A B C))(steps . (1 2 5)))))

;; Tratamento da lista de ingredientes
; Atualiza a lista resumida de ingredientes, agrupando por nome e por unidade.
(define (update-nested-ingredient name unit qtd summarized)
  (let ((ingredient (assoc name summarized)))
    
    (if ingredient 
        ; ingrediente já está na lista
        (let* ((old-unit-list (cdr ingredient))
               (existing-unit-pair (assoc unit old-unit-list))
               (filtered-summarized (remove (lambda (pair) (equal? (car pair) name)) summarized)))
          
           (if existing-unit-pair
               ; essa unidade já consta para o ingrediente em questão
               (let* ((new-unit-qtd (+ qtd (cdr existing-unit-pair))) ; Soma as quantidades.
                      (filtered-unit-list (remove (lambda (pair) (equal? (car pair) unit)) old-unit-list))
                      (new-unit-list (acons unit new-unit-qtd filtered-unit-list))) ; Atualiza a lista de unidades.
                 (acons name new-unit-list filtered-summarized)) ; Atualiza a lista resumida.
               
               ; primeira vez que essa unidade aparece para o ingrediente em questão
               (let ((new-unit-list (acons unit qtd old-unit-list))) ; Adiciona a nova unidade.
                 (acons name new-unit-list filtered-summarized)))) ; Atualiza a lista resumida.
        
        ; ingrediente não está na lista
        (let ((new-unit-list (acons unit qtd '()))) ; Cria uma nova lista de unidades.
          (acons name new-unit-list summarized))))) ; Adiciona o novo ingrediente à lista resumida.

; Percorre a lista de ingredientes brutos e os resume/soma.
(define (summarize-ingredients ingredient-list)
  (let loop ((remaining-list ingredient-list)
             (summary-list '()))
    (if (null? remaining-list)
        summary-list
        (let* ((current (car remaining-list))
               (qtd (cadr current))
               (unit (caddr current))
               (name (cadddr current)))
          (loop (cdr remaining-list) (update-nested-ingredient name unit qtd summary-list))))))

(summarize-ingredients '((ingredient 1 un "ovos")(ingredient 3 un "ovos")(ingredient 1 kg "ovos")(ingredient 1 un "pao"))) ; Teste de exemplo

; Formata um par (unidade . quantidade) em uma string legível.
(define (format-unit-string unit-pair)
  (let ((unit (car unit-pair))
        (qtd (cdr unit-pair)))
   (string-append (number->string qtd) (translate-unit unit))))

; Junta uma lista de strings de unidade em uma única string formatada (ex: "3 un, 1 kg e 40 ml").
(define (format-unit-list-string unit-list)
    (let join-units ((string-list unit-list))
        (let ((current (car string-list)))
            (cond
               ((null? string-list) "")
               ((null? (cdr string-list)) current) ; Último elemento.
               ((null? (cddr string-list)) (string-append current " e " (cadr string-list))) ; Penúltimo elemento.
               (else (string-append current ", " (join-units (cdr string-list)))))))) ; Elementos intermediários.

; Formata uma entrada de ingrediente resumida em uma string final (ex: "3 xícara(s) e 1 unidade(s) de açúcar").
(define (format-ingredient-string ingredient-list)
  (let* ((name (car ingredient-list))
         (unit-pairs (cdr ingredient-list))
         (formatted-units (map format-unit-string unit-pairs))
         (unit-string (format-unit-list-string formatted-units)))
      (string-append unit-string " de " name)))


;;Definindo as macros da DSL

; Macro para definir uma nova receita a partir de uma lista de passos (step ...).
(define-syntax define-recipe
  (lambda (x)
    (syntax-case x ()
      ((_ name step-list ...) ; Forma esperada: (define-recipe <nome> <step> <step> ...)
       (let* ((recipe-name (symbol->string (syntax->datum #'name)))
              (raw-steps   (syntax->datum #'(step-list ...)))) ; Obtém a lista de formas de passos.

         (let* ((processed   (map process-step raw-steps)) ; Processa cada passo.
                (ingredients (apply append (map get-ingredients processed))) ; Extrai e concatena todos os ingredientes.
                (step-texts  (map get-steps processed))) ; Extrai o texto formatado dos passos.
           (datum->syntax
            x
            `(define ,(syntax->datum #'name) ; Define a variável com o nome da receita.
               '((name . ,recipe-name)
                 (ingredients . ,ingredients) ; Armazena os ingredientes extraídos.
                 (steps . ,step-texts)))))))))) ; Armazena o texto dos passos.

; Macro para definir uma função de modificação de receita.
(define-syntax define-modification
  (lambda (x)
    (syntax-case x ()
      ((_ mod-name (action . args)) ; Forma esperada: (define-modification <nome> (<ação> <args> ...))
       (let ((mod-sym (syntax->datum #'mod-name))
             (action-sym (syntax->datum #'action)))
         
         (syntax-case #'args ()
           ; CASO 1: Importa outra receita e adiciona os passos após um índice (Ex: (add-step-after 2 receita-de-calda)).
           ((index recipe-id) (and (integer? (syntax->datum #'index)) (identifier? #'recipe-id))
            (build-import-body x mod-sym action-sym #'recipe-id #'index))

           ; CASO 2: Importa outra receita e adiciona os passos no início/fim (Ex: (add-step-to-end receita-de-calda)).
           ((recipe-id) (identifier? #'recipe-id)
            (build-import-body x mod-sym action-sym #'recipe-id #f))

           ; CASO 3: Define os passos da modificação literalmente (Ex: (add-step-to-end (step ...))).
           (_
            (build-literal-body x mod-sym action-sym #'args))
         ))))))

; Macro para executar a composição final da receita e imprimir o resultado formatado.
(define-syntax create-recipe
  (syntax-rules ()
    ((_ final-name composition-expr) ; Forma esperada: (create-recipe <título-final> <modificação-composta>)
     (let ((final-recipe composition-expr)) ; Avalia a composição das modificações na receita base.
       (display (string-append "## " final-name "\n\n")) ; Imprime o título principal.
       
       (display "### Ingredientes\n\n") ; Imprime o subtítulo de ingredientes.
       (let* ((raw-ingredients (get-ingredients final-recipe))
              (summarized (summarize-ingredients raw-ingredients)) ; Resumo a lista de ingredientes.
              (formatted (map format-ingredient-string summarized))) ; Formata cada ingrediente.
         (let ingredients-loop ((ingredients formatted)) ; Loop para imprimir a lista de ingredientes.
           (when (not (null? ingredients))
             (display (string-append "* " (car ingredients) "\n"))
             (ingredients-loop (cdr ingredients)))))
       
       (display "\n### Modo de Preparo\n\n") ; Imprime o subtítulo de modo de preparo.
       (let steps-loop ((steps (get-steps final-recipe))
                  (n 1)) ; Contador para os números dos passos.
         (when (not (null? steps)) ; Loop para imprimir os passos numerados.
           (display (string-append (number->string n) ". " (car steps) "\n"))
           (steps-loop (cdr steps) (+ n 1))))))))

