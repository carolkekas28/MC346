# DSL `OvenFlow`

> O OvenFlow é uma DSL declarativa para representar e compor receitas culinárias. Nossa linguagem consiste na manipulação de estruturas de dados para guardar uma receita e os passos necessários para a sua realização.

## Descrição Resumida da DSL

- **Contexto**: domínio de receitas e instruções culinárias.
- **Motivação**: receitas são frequentemente representadas como textos livres, difíceis de reutilizar, versionar e
transformar. O Ovenflow estrutura receitas como dados, facilitando reuso, composição e variações.
- **Relevância:** uma DSL focada no domínio reduz a ambiguidade e habilita operações de alto nível, como inserir passo
após o n-ésimo, renderizar "modo de preparo" automaticamente e listar ingredientes.
- **Escopo atual:** suporte a passos, ingredientes, definição de receitas, criação e modificações.

## Slides

[Link para acessar os nossos slides.](https://docs.google.com/presentation/d/1QNMO3aChWYS85rvfX5D0YxKlk1Zj9BUbQlnRaAwpRmQ/edit?usp=sharing)

## Notebook

[Link para acessar o nosso Notebook.](https://github.com/carolkekas28/mc346/blob/main/ovenflow.ipynb)

## Sintaxe da Linguagem

### Blocos e construtores principais
- **Passo (step)** - representa uma ação atômica no preparo:
```
(step <conteúdo-do-passo>)
```

- **Ingrediente (ingredient)** - marcador de ingrediente, utilizado dentro de um passo:
```
(ingredient "<quantidade e item>")
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

### Sintaxe

#### Macro 1 - `define-recipe`

Usada para criar uma receita base nomeada, que é uma coleção de passos de preparo. 

| Macro  | Sintaxe | Elementos principais | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `define-recipe` | `(define-recipe NOME_DA_RECEITA PASSO_1 PASSO_2 ...)` | `NOME_DA_RECEITA` - símbolo (ex: `bolo`, `torta`) <br> `PASSO_N` - expressão `step` | Declara uma receita base nomeada como uma sequência de passos de preparo. |
##### Estrutura interna de um passo - `step` e `ingredient`
| Construção  | Sintaxe | Elementos | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `step` | `(step ELEMENTO_1 ELEMENTO_2 ...)` | `ELEMENTO_N` ∈ {string, `ingredient`} | Passo do modo de preparo. Permite misturar texto literal com ingredientes marcados. |
| `ingredient` | `(ingredient "TEXTO")` | "TEXTO" | Marca um ingrediente específico que será extraído e consolidado na lista final. |

#### Macro 2 - `define-modification`

Usada para criar uma função de modificação nomeada. Essa função aceita uma receita e retorna uma nova receita modificada.

| Macro  | Sintaxe | Elementos principais | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `define-modification` | `(define-modification NOME_DA_MOD (AÇÃO ARGUMENTOS))` | `NOME_DA_MOD` - símbolo (ex: `de-cenoura`) <br> `AÇÃO` - uma das ações de modificação do passo | Cria uma função de modificação nomeada que recebe uma receita e retorna uma nova receita modificada (imutável). |
##### Ações de modificação de passo
| Ação  | Sintaxe | Descrição |
| ------------- | ------------- | ------------- |
| `add-step-to-start` | `(add-step-to-start (step ...))` | Adiciona o novo passo no início da lista de passos. |
| `add-step-to-end` | `(add-step-to-end (step ...))` | Adiciona o novo passo no fim da lista de passos. |
| `add-step-after` | `(add-step-after ÍNDICE (step ...))` | Adiciona o novo passo após o passo na posição `ÍNDICE` (indexação começando em 1). |


#### Macro 3 - `create-recipe`

Usada para compor a receita final (aplicando modificações) e imprimi-la com a lista de ingredientes e o modo de preparo numerado. 

| Macro  | Sintaxe | Elementos principais | Descrição |
| ------------- | ------------- | ------------- | ------------- |
| `create-recipe` | `(create-recipe "TÍTULO" COMPOSIÇÃO_FUNCIONAL` | `TÍTULO` - string visível <br> `COMPOSIÇÃO_FUNCIONAL` - expressão que aplica mods à receita base | Compõe a receita final (aplica as modificações) e imprime lista de ingredientes e modo de preparo numerado. |

### Semântica (resumo)
- **Modelo de execução**:
  - Interpretada;
  - Receitas são estruturas imutáveis;
  - Modificações retornam novas receitas.
- **Binding**: escopo léxico para símbolos de receitas e modificações.
- **Tipos/valores**: `recipe`, `step`, `ingredient`, `string`, `number`.
- **Efeitos**: renderização imprime Markdown, enquanto o restante é puro (sem efeitos colaterais).

## Exemplos Selecionados
### 1. Receita base ("bolo")
```
(define-recipe bolo
(step "Misturar" (ingredient "3 ovos") "," (ingredient "1.5 xícara de açúcar") "e" (ingredient "0.5 xícara de óleo"))
(step "Adicionar" (ingredient "2 xícaras de farinha de trigo") "e misturar bem")
(step "Assar em forno pré-aquecido a 180 graus por 40 minutos"))

(create-recipe "Bolo" bolo)
```
#### Saída (formato Markdown)
```
## Bolo

### Ingredientes

* 2 xícaras de farinha de trigo
* 0.5 xícaras de óleo
* 1.5 xícara de açúcar
* 3 ovos

### Modo de preparo

1. Misturar 3 ovos , 1.5 xícara de açúcar e 0.5 xícara de óleo
2. Adicionar 2 xícaras de farinha de trigo e misturar bem
3. Assar em forno pré-aquecido a 180 graus por 40 minutos
```

### 2. Inserindo um passo após o 1º (bolo de cenoura)
**Efeito**: insere o passo das cenouras como passo nº 2, enquanto os demais passos são reindexados automaticamente.
```
(define-modification de-cenoura
(add-step-after 1
(step "Adicionar" (ingredient "3 cenouras médias raladas") "à mistura e bater novamente")))

(create-recipe "Bolo de Cenoura" (de-cenoura bolo))
```

### 3. Compondo modificações (cobertura de chocolate)
Suponha uma modificação `com-calda-de-chocolate` definida via `define-modification` que adiciona passos/ingredientes da calda.
```
(display "--- Receita 1: Bolo de Cenoura com Calda ---
")
(create-recipe "Bolo de Cenoura com Calda de Chocolate"
(com-calda-de-chocolate (de-cenoura bolo)))
```
#### Saída (trecho):
```
--- Receita 1: Bolo de Cenoura com Calda ---
## Bolo de Cenoura com Calda de Chocolate

### Ingredientes
* 2 xícaras de farinha de trigo
* 0.5 xícara de óleo
* 1.5 xícara de açúcar
* 3 ovos
* 3 cenouras médias raladas
* 3 colheres de sopa de chocolate em pó
* 1 lata de leite condensado
...
```

## Discussão
Os resultados desta entrega parcial apontam que a modelagem de receitas como estruturas de dados (com `step` e `ingredient` como elementos de primeira classe) cumpre o objetivo central de reduzir complexidade acidental e tornar a edição de receitas uma tarefa previsível. No notebook, partimos de uma receita base (`bolo`) e demonstramos duas operações típicas de domínio: a inserção de passos por posição (`add-step-after`) e ao final (`add-step-to-end`) e a composição de modificações para gerar variantes, como "Bolo de cenoura com calda de chocolate" e "Torta de Cenoura". Em todos os casos, a receita original permaneceu intacta, e a versão modificada foi produzida por transformação imutável, exatamente a hipótese de composição determinística que queríamos validar. Além disso, a marcação explícita de ingredientes dentro dos passos permitiu extrair automaticamente a lista consolidada via `get-ingredients` e numerar o "modo de preparo" sem duplicação, atendendo à hipótese de apresentação automatizada.

Por que esse modelo funcionou bem aqui? Primeiro, porque o domínio de receitas favorece uma apresentação declarativa: passos são naturalmente sequenciais e ingredientes são referências textuais curtas. Ao expor ambos na sintaxe,e simplificamos o que, em uma linguagem de propósito geral, exigiria listas, loops e joins. Segundo, a decisão por imutabilidade evita efeitos colaterais difíceis de depurar, como perder um passo ao inserir outro, e torna o comportamento das modificações transparente: dado o mesmo insumo, o resultado é sempre o mesmo. Terceiro, a camada de renderização ufinica apresentação e extração, pois `get-steps` e `get-ingredients` geram material pronto para Markdown sem que o autor precise manter duas verdades (texto e lista). 

Os nossos experimentos, contudo, expõem alguns limites bem claros. Não temos validação de unidades e medidas (por exemplo, xícara, grama e mililitros) nem checagens de consistências. Embora o custo de inserções lineares seja irrelevante para receitas comuns, coleções maiores, como livros, pedem estruturas mais persistentes.

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
