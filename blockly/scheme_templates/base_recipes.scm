;; ==============================================
;; base_recipes.scm
;; Receitas e Modificações Base para a DSL de Receitas
;; ==============================================

;; --- RECEITAS BASE ---

(define-recipe bolo-simples
  (step "Bater no liquidificador" (ingredient 3 un "ovos") "," (ingredient 1 cup "leite") "e" (ingredient 0.5 cup "óleo"))
  (step "Em uma tigela, misturar" (ingredient 2 cup "farinha de trigo") "e" (ingredient 1.5 cup "açúcar"))
  (step "Juntar as misturas e adicionar" (ingredient 1 tablespoon "fermento químico"))
  (step "Assar por" (time 30 min) "a" (temp 180 celsius)))

(define-recipe massa-de-torta
  (step "Misturar" (ingredient 2 cup "farinha de trigo") "e" (ingredient 100 g "manteiga"))
  (step "Adicionar" (ingredient 1 un "ovo") "e amassar até formar uma massa homogênea")
  (step "Levar à geladeira por" (time 15 min)))

(define-recipe recheio-creme
  (step "Misturar em fogo baixo" (ingredient 1 can "leite condensado") "," (ingredient 1 can "creme de leite") "e" (ingredient 2 tablespoon "amido de milho"))
  (step "Cozinhar até engrossar"))

(define-recipe cobertura-chocolate
  (step "Derreter em banho-maria" (ingredient 200 g "chocolate meio amargo") "com" (ingredient 1 box "creme de leite"))
  (step "Misturar até ficar liso"))

(define-recipe calda-limao
  (step "Espremer" (ingredient 2 un "limões"))
  (step "Misturar o suco com" (ingredient 1 cup "açúcar de confeiteiro")))


;; --- MODIFICAÇÕES BASE (FUNÇÕES) ---

(define-modification adicao-cenoura
  (add-step-after 1
   (step "Adicionar" (ingredient 3 un "cenouras médias raladas") "à mistura e bater novamente")))

(define-modification adicao-chocolate
  (add-step-after 2
   (step "Substituir" (ingredient 0.5 cup "farinha de trigo") "por" (ingredient 0.5 cup "chocolate em pó") "para a massa")))

(define-modification cobertura-simples
  (add-step-to-end cobertura-chocolate))

(define-modification com-recheio-creme
  (add-step-to-end recheio-creme))

(define-modification finalizacao-limao
  (add-step-to-end calda-limao))

;; --- EXEMPLO DE USO (Para testes no Guile) ---
;; (create-recipe "Bolo de Cenoura com Cobertura de Chocolate"
;;   (cobertura-simples (adicao-cenoura bolo-simples)))