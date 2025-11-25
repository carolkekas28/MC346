# DSL `OvenFlow`

> O OvenFlow é uma DSL declarativa para representar e compor receitas culinárias. Nossa linguagem consiste na manipulação de estruturas de dados para guardar uma receita e os passos necessários para a sua realização.

## Descrição Resumida da DSL

- **Contexto**: domínio de receitas e instruções culinárias.
- **Motivação**: receitas são frequentemente representadas como textos livres, difíceis de reutilizar, versionar e
transformar. O Ovenflow estrutura receitas como dados, facilitando reuso, composição e variações.
- **Relevância:** uma DSL focada no domínio reduz a ambiguidade e habilita operações de alto nível, como inserir passo
após o n-ésimo, renderizar "modo de preparo" automaticamente e listar ingredientes.
- **Escopo atual:** suporte a passos, ingredientes, definição de receitas, criação e modificações, validação de unidades no `step`, sumarização automática de ingredientes repetidos, marcação de tempo e temperatura no fluxo.

## Slides

[Link para acessar os nossos slides.](https://www.canva.com/design/DAG3gZIbsiM/ItCkigNDue6Kk7S5WM8ljA/edit?utm_content=DAG3gZIbsiM&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton)

## Notebook

[Link para acessar o nosso Notebook.](https://github.com/carolkekas28/MC346/blob/main/project-2/ovenflow.ipynb)

## Sintaxe da Linguagem

### Blocos e construtores principais
- **Passo (step)** - representa uma ação atômica no preparo:
```
(step <conteúdo-do-passo>)
```

- **Ingrediente (ingredient)** - marcador de ingrediente, utilizado dentro de um passo:
```
(ingredient QTD UNIDADE "NOME")
```

- **Definição de receita (define-recipe)** - receita base composta por uma sequência de passos:
```
(define-recipe <nome-simbólico>
  (step ...)
  (step ...)
  ...)
```

- **Formatação (create-recipe)** - realiza a composição final e imprime a receita formatada:
```
(create-recipe "<nome-de-exibição>" <expressão-de-composição>)
```

- **Modificações (define-modification)** - funções que recebem receitas e retornam receitas com passos inseridos. Variações:
  - `add-step-to-end`: adiciona passo ao fim.
  - `add-step-to-start`: adiciona passo ao início.
  - `add-step-after`: insere passo após o índice fornecido (base 1).
```
(define-modification <nome-da-mod>
  (<variação> <args...>))
```

- **Marcadores de domínio extras** - elementos opcionais usados dentro de `step`:
```
(time <quantidade> <unidade-tempo>) ; ex: (time 40 min)
(temp <valor> <unidade-temperatura>) ; ex: (temp 180 celsius)
```
> Observação: `time`e `temp` não entram na lista de ingredientes. Eles apenas compõem o texto do passo. 

### Sintaxe

#### Macro 1 - `define-recipe`

Usada para criar uma receita base nomeada, que é uma coleção de passos de preparo. 

| Macro  | Sintaxe | Elementos principais | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `define-recipe` | `(define-recipe NOME_DA_RECEITA PASSO_1 PASSO_2 ...)` | `NOME_DA_RECEITA` - símbolo (ex: `bolo`, `torta`) <br> `PASSO_N` - expressão `step` | Declara uma receita base nomeada como uma sequência de passos de preparo. |

##### Estrutura interna de um passo - `step` e `ingredient`
| Construção  | Sintaxe | Elementos | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `step` | `(step ELEMENTO_1 ELEMENTO_2 ...)` | `ELEMENTO` ∈ {string, `ingredient`, `time`, `temp`}  | Passo do modo de preparo. Permite misturar texto com elementos marcados do domínio. |
| `ingredient` | `(ingredient QTD UNIDADE "NOME")` | `QTD`: número <br> `UNIDADE`: símbolo (por exemplo, `g`, `ml`, `cup`, `un`) <br> `"NOME"`: string | Marca um ingrediente que será extraído e consolidado na lista final. |
| `time` | `(time QTD UNIDADE)` | `QTD`: número <br> `UNIDADE`: símbolo (por exemplo, `g`, `ml`, `cup`, `un`) <br> `"NOME"`: string | Marca duração usada no texto do passo (não vira ingrediente). |
| `temp` | `(temp VALOR UNIDADE)` | `VALOR`: número <br> `UNIDADE`: ∈ {`celsius`, `fahreinheit`} | Marca temperatura usada no texto do passo (não vira ingrediente). |

> ##### Validação de unidades no `step` 
> Cada `(ingredient ...)` passa por checagens:
>  - `QTD` deve ser númerica.
>  - `UNIDADE` deve pertencer ao conjunto suportado (`kg`, `g`, `l`, `ml`, `cup`, `teaspoon`, `tablespoon`, `can`, `un`).
>  - Somatório só deve ocorrer entre unidades iguais (ex: `g` + `g`).
>  Em inconsistências, a execução falha com mensagem 

#### Macro 2 - `define-modification`

Usada para criar uma função de modificação nomeada. Essa função aceita uma receita e retorna uma nova receita modificada.

| Macro  | Sintaxe | Elementos principais | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `define-modification` | `(define-modification NOME_DA_MOD (AÇÃO ARGUMENTOS))` | `NOME_DA_MOD`: símbolo (ex: `de-cenoura`) <br> `AÇÃO`: uma das ações de modificação do passo | Cria uma função de modificação nomeada que recebe uma receita e retorna uma nova receita modificada (imutável). |
##### Ações de modificação de passo
| Ação  | Sintaxe | Descrição |
| ------------- | ------------- | ------------- |
| `add-step-to-start` | `(add-step-to-start (step ...))` | Adiciona o novo passo no início da lista de passos. |
| `add-step-to-end` | `(add-step-to-end (step ...))` | Adiciona o novo passo no fim da lista de passos. |
| `add-step-after` | `(add-step-after ÍNDICE (step ...))` | Adiciona o novo passo após o passo na posição `ÍNDICE` (indexação começando em 1). |
| `add-step-after-text` | `(add-step-after-text "TERMO DE BUSCA" (step ...))` | adiciona um novo passo depois de um texto na receita (definido posteriormente). |


#### Macro 3 - `create-recipe`

Usada para compor a receita final (aplicando modificações) e imprimi-la com a lista de ingredientes e o modo de preparo numerado. 

| Macro  | Sintaxe | Elementos principais | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `create-recipe` | `(create-recipe "TÍTULO" COMPOSIÇÃO_FUNCIONAL)` | `TÍTULO`:  string visível <br> `COMPOSIÇÃO_FUNCIONAL`: expressão que aplica mods à receita base | Compõe a receita final (aplicando modificações), valida as unidades, sumariza ingredientes repetidos por unidade e imprime lista de ingredientes e modo de preparo numero. |
> ##### Sumarização de ingredientes
> Se o mesmo ingrediente aparecer em passos diferentes, as quantidades são somadas por unidade. Por exemplo, para o ingrediente "ovos", caso tenhamos `3 un` em um passo e `2 un` em outro, no final será impresso "5 unidade(s) de ovos". 


### Semântica (resumo)
- **Modelo de execução**:
  - Interpretada;
  - Receitas são estruturas imutáveis;
  - Modificações retornam novas receitas.
- **Binding**: escopo léxico para símbolos de receitas e modificações.
- **Tipos/valores**: `recipe`, `step`, `ingredient`, `string`, `number`, `time`, `temp`.
- **Efeitos**: renderização imprime Markdown, enquanto o restante é puro (sem efeitos colaterais).
- **Validação de domínio**: checagem de unidades em `ingredient`, enquanto `time` e `temp` são semânticos e não contam como ingredientes.
- **Agregação determinística**: ingredientes iguais (mesmo nome e mesma unidade) são somados na renderização. 

## Exemplos Selecionados
### 1. Receita base ("bolo")
```
(define-recipe bolo
  (step "Misturar" (ingredient 3 un "ovos") "," (ingredient 1.5 cup "açúcar") "e" (ingredient 0.5 cup "óleo"))
  (step "Adicionar" (ingredient 2 cup "farinha de trigo") "e misturar bem")
  (step "Assar em forno pré-aquecido a" (temp 180 celsius) "por" (time 40 min)))

(create-recipe "Bolo" bolo)

```
#### Saída (formato Markdown)
```
## Bolo

### Ingredientes
* 2 xícara(s) de farinha de trigo
* 0.5 xícara(s) de óleo
* 1.5 xícara(s) de açúcar
* 3 unidade(s) de ovos

### Modo de Preparo
1. Misturar 3 unidade(s) de ovos , 1.5 xícara(s) de açúcar e 0.5 xícara(s) de óleo
2. Adicionar 2 xícara(s) de farinha de trigo e misturar bem
3. Assar em forno pré-aquecido a 180°C por 40 minuto(s)
```
> O que foi alterado:
> - `ingredient` agora é `(ingredient QTD UNIDADE "NOME").
> - `time` e `temp` aparecem no passo, mas não viram ingredientes.

### 2. Inserindo um passo após o 1º (bolo de cenoura)
**Efeito**: insere o passo das cenouras como passo nº 2, enquanto a lista de ingredientes soma o açúcar (1,5 cup + 0,5 cup = 2,0 cup).
```
(define-modification de-cenoura
  (add-step-after 1
    (step "Adicionar" (ingredient 3 un "cenouras médias raladas")
                      "e" (ingredient 0.5 cup "açúcar")
                      "à mistura e bater novamente")))

(create-recipe "Bolo de Cenoura" (de-cenoura bolo))
```
#### Saída (formato Markdown)
```
## Bolo de Cenoura

### Ingredientes
* 2 xícara(s) de farinha de trigo
* 0.5 xícara(s) de óleo
* 2.0 xícara(s) de açúcar
* 3 unidade(s) de cenouras médias raladas
* 3 unidade(s) de ovos

### Modo de Preparo
1. Misturar 3 unidade(s) de ovos , 1.5 xícara(s) de açúcar e 0.5 xícara(s) de óleo
2. Adicionar 3 unidade(s) de cenouras médias raladas e 0.5 xícara(s) de açúcar à mistura e bater novamente
3. Adicionar 2 xícara(s) de farinha de trigo e misturar bem
4. Assar em forno pré-aquecido a 180°C por 40 minuto(s)
```
> O que foi alterado:
> - O novo passo usa a sintaxe estrutura de `ingredient`.
> - A sumarização aparece na lista de ingredientes (açúcar = 2,0 xícaras). 

### 3. Compondo modificações
Suponha as seguintes modificações: `de-chocolate`, `desperdicar` e `com-fermento`. 
```
(display "--- Testando add-step-after-text ---\n")

(define-modification de-chocolate
  (add-step-after-text "farinha de trigo" ; <-- Nova ação
    (step "Adicionar" (ingredient 1 cup "chocolate em pó") "e misturar")))

(define-modification desperdicar
    (add-step-after-text "Assar em forno"
        (step "Jogar tudo no lixo")))

(define-modification com-fermento
  (add-step-after-text "farinha de trigo"
    (step "Por último, adicionar" (ingredient 1 tablespoon "fermento em pó") "e misturar levemente")))

(create-recipe "Bolo de Chocolate desperdiçado"
  (com-fermento(desperdicar (de-chocolate bolo))))
```
#### Saída (trecho):
```
--- Testando add-step-after-text ---
## Bolo de Chocolate desperdiçado

### Ingredientes

* 2 xícara(s) de farinha de trigo
* 0.5 xícara(s) de óleo
* 1.5 xícara(s) de açúcar
* 3 unidade(s) de ovos
* 1 xícara(s) de chocolate em pó
* 1 colher(es) de sopa de fermento em pó

### Modo de Preparo

1. Misturar 3 unidade(s) de ovos , 1.5 xícara(s) de açúcar e 0.5 xícara(s) de óleo
2. Adicionar 2 xícara(s) de farinha de trigo e misturar bem
3. Por último, adicionar 1 colher(es) de sopa de fermento em pó e misturar levemente
4. Adicionar 1 xícara(s) de chocolate em pó e misturar
5. Assar em forno pré-aquecido a 180°C por 40 minuto(s)
6. Jogar tudo no lixo
```
> O que foi alterado:
> - A cobertura agora é uma receita reutilizável (`define-recipe`) e é adicionada com uma modificação.
> - O render mantém a sumarização global. 

### 4. Tempo, temperatura e sumarização
```
(define-recipe assar-e-finalizar
  (step "Misturar" (ingredient 3 un "ovos") "com" (ingredient 1.5 cup "açúcar"))
  (step "Assar a" (temp 180 celsius) "por" (time 40 min))
  (step "Finalizar com mais (ingredient 2 un "ovos) "batidos"))

(create-recipe "Assar e Finalizar" assar-e-finalizar) 
```
#### Saída - ingredientes (trecho):
```
* 5 unidade(s) de ovos
* 1.5 xícara(s) de açúcar
```
#### Saída - modo de preparo (trecho):
```
1. Misturar 3 unidade(s) de ovos com 1.5 xícara(s) de açúcar
2. Assar a 180°C por 40 minuto(s)
3. Finalizar com mais 2 unidade(s) de ovos batidos
```


### 5. Gerando receitas de receitas
Neste exemplo, criamos uma receita base (bolo), uma segunda receita (calda-de-chocolate) e uma modificação que adiciona a calda ao final da receita principal, o demonstra que a DSL permite composição declarativa entre receitas, resultando em uma receita de receitas.
```
(define-recipe calda-de-chocolate
    (step "Para a calda, misturar"
          (ingredient 1 can "leite condensado")
          "e"
          (ingredient 3 tablespoon "chocolate em pó")
          "em fogo baixo até engrossar"))

(define-modification com-calda-de-chocolate
  (add-step-to-end calda-de-chocolate))

(display "--- Receita 1: Bolo de Cenoura com Calda ---\n")
(create-recipe "Bolo de Cenoura com Calda de Chocolate"
  (com-calda-de-chocolate (de-cenoura bolo)))
```

#### Saída (formato Markdown)
```
## Bolo de Cenoura com Calda de Chocolate

### Ingredientes

* 2 xícara(s) de farinha de trigo
* 0.5 xícara(s) de óleo
* 2.0 xícara(s) de açúcar
* 3 unidade(s) de ovos
* 3 unidade(s) de cenouras médias raladas
* 3 colher(es) de sopa de chocolate em pó
* 1 lata(s) de leite condensado

### Modo de Preparo

1. Misturar 3 unidade(s) de ovos , 1.5 xícara(s) de açúcar e 0.5 xícara(s) de óleo  
2. Adicionar 3 unidade(s) de cenouras médias raladas e 0.5 xícara(s) de açúcar à mistura e bater novamente  
3. Adicionar 2 xícara(s) de farinha de trigo e misturar bem  
4. Assar em forno pré-aquecido a 180°C por 40 minuto(s)  
5. Para a calda, misturar 1 lata(s) de leite condensado e 3 colher(es) de sopa de chocolate em pó em fogo baixo até engrossar
```


## Discussão
Os resultados desta entrega parcial apontam que a modelagem de receitas como estruturas de dados (com `step` e `ingredient` como elementos de primeira classe) cumpre o objetivo central de reduzir complexidade acidental e tornar a edição de receitas uma tarefa previsível. No notebook, partimos de uma receita base (`bolo`) e demonstramos duas operações típicas de domínio: a inserção de passos por posição (`add-step-after`) e ao final (`add-step-to-end`) e a composição de modificações para gerar variantes, como "Bolo de cenoura com calda de chocolate" e "Torta de Cenoura". Em todos os casos, a receita original permaneceu intacta, e a versão modificada foi produzida por transformação imutável, exatamente a hipótese de composição determinística que queríamos validar. Além disso, a marcação explícita de ingredientes dentro dos passos permitiu extrair automaticamente a lista consolidada via `get-ingredients` e numerar o "modo de preparo" sem duplicação, atendendo à hipótese de apresentação automatizada.

Por que esse modelo funcionou bem aqui? Primeiro, porque o domínio de receitas favorece uma apresentação declarativa: passos são naturalmente sequenciais e ingredientes são referências textuais curtas. Ao expor ambos na sintaxe,e simplificamos o que, em uma linguagem de propósito geral, exigiria listas, loops e joins. Segundo, a decisão por imutabilidade evita efeitos colaterais difíceis de depurar, como perder um passo ao inserir outro, e torna o comportamento das modificações transparente: dado o mesmo insumo, o resultado é sempre o mesmo. Terceiro, a camada de renderização ufinica apresentação e extração, pois `get-steps` e `get-ingredients` geram material pronto para Markdown sem que o autor precise manter duas verdades (texto e lista). 


## Conclusão

Com o desenvolvimento do OvenFlow, pudemos confirmar que tratar receitas como estruturas de dados traz clareza, reuso e capacidade de transformação que textos livres não oferecem. As composições demonstradas anteriormente evidenciam que a abordagem imutável e determinística permite evoluir uma receita sem perder o histórico, além de viabilizar uma apresentação automática consistente (liste de ingredientes e numeração de passos). 

Nem tudo, porém, está resolvido. A ausência de validações de domínio (unidades, conversões e checagens de consistência) e de mensagens de erro mais orientativas ainda limita a robustez para usos mais amplos.

Podemos, enfim, concluir que separar modelo (dados) de apresentação (render) simplifica extensões e testes, enquanto a escolha por imutabilidade ajuda a manter a semântica da linguagem estável.

# Trabalhos Futuros

Os três eixos abaixo permitem fechar as principais lacunas identificadas no nosso projeto, pavimentando o caminho para avaliações quantitativas na entrega final. 

- **Sumarizar ingredientes**: quando diferentes passos utilizam a mesma matéria prima (por exemplo, 2 ovos no preparo de massa e 3 ovos na cobertura), a lista final deverá mostrar 5 ovos. 
- **Permitir modificar outros parâmetros**: adicionar novos parâmetros do domínio para serem modificados, como tempo de forno, temperatura, etc.
- **Adicionar validação de unidades de medidas**: assegurar a coerência entre grandezas, proibindo, por exemplo, somar colheres a mililitros sem algum fator de conversão. 

# Referências Bibliográficas

- Slides de 2s2025 para a disciplina MC346.
