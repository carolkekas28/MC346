# MC346

Projeto para a disciplina de Paradigmas de Programação (MC346) — 2s2025.

## 👥 Equipe

| Nome                        | Ra    |
|-----------------------------|-------|
| Ana Carolina                | 246914|
| Pedro Damasceno             | 260640|
| Tabata Prado                | 250524|
| Henrique Minetto            | 172209|



## 🚀 Tecnologias Utilizadas

O projeto implementa uma Linguagem de Domínio Específico (DSL) visual com as seguintes tecnologias:

- JavaScript e HTML (Frontend): Utilizando a Interface de programação em blocos para composição de receitas. 

- Scheme/Guile (Backend): Linguagem funcional onde toda a lógica de manipulação e formatação das receitas é implementada.

## 📚 Estrutura do Repositório

O projeto segue a arquitetura de uma aplicação web (frontend + backend) que gerencia uma DSL visual.

| Caminho                        | Tipo        | Descrição                               |
|--------------------------------|-------------|-------------------------------------------|
| mc346/                         | Diretório   | Diretório Raiz                            |
| ├── blockly/                   | Diretório   | Frontend e backend                        |
| │   ├── node_modules/          | Diretório   | Dependências do Node.js                   |
| │   ├── public/                | Diretório   | Arquivos estáticos                        |
| │   │   ├── index.html         | HTML        | Página principal do Blockly               |
| │   │   ├── blocks.js          | JS          | Definição dos blocos da DSL (recipe_modification e create_final_recipe)|
| │   │   └── generators_scheme.js | JS        | Converte os blocos para uma expressão Scheme|
| │   ├── scheme_templates/      | Diretório   | Infraestrutura Scheme                     |
| │   │   ├── base_recipes.scm   | Scheme      | Banco de receitas base                    |
| │   │   ├── render_recipe.scm  | Scheme      | Macros e lógica da DSL                    |
| │   ├── server.js              | JS          | Servidor backend (Node + Guile). Recebe o código, adiciona (load "render_recipe.scm"), salva em /tmp e executa Guile com CWD em scheme_templates/        |
| │   ├── tmp/                   | Diretório   | Arquivos .scm temporários                 |
| │   ├── package.json           | JSON        | Dependências do Node.js                   |
| │   └── package-lock.json      | JSON        | Versões exatas das dependências           |
| ├── project-1             | Diretório     | Documentação e notebook da entrega parcial                 |
| ├── project-2               | Diretório    | Documentação e notebook da entrega final |
| └── README.md                  | Markdown    | Documento explicando a estrutura da do repositório/aplicação toda |


## 🛠️ Detalhamento do Fluxo

Blocos (Frontend) → Scheme → Resultado (Frontend)

O servidor recebe o código Scheme gerado pelo Blockly, acrescenta a instrução
(load "render_recipe.scm") para carregar toda a lógica da DSL, salva esse código temporariamente em /tmp e executa o interpretador Guile definindo o diretório de trabalho atual (CWD) como scheme_templates/.

Isso garante que todos os arquivos carregados via (load "...") — como render_recipe.scm e base_recipes.scm — sejam encontrados corretamente, já que o Guile passa a procurar esses arquivos dentro desse diretório.


## ▶️ Como Executar o Projeto
1. Instalação das dependências (Node.js)
```text
cd mc346/blockly
npm install
```

2. Inicialização do Servidor
```text
node server.js
```

O servidor iniciará na porta 3000.

3. Acesso

Abra no navegador:

```text
http://localhost:3000
```

Comece a compor receitas na área de trabalho do Blockly! 🍰
