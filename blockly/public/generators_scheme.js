// Cria o gerador Scheme
Blockly.Scheme = new Blockly.Generator('Scheme');

// Palavras reservadas e precedência
Blockly.Scheme.addReservedWords(
  'list quote define define-recipe define-modification create-recipe step ingredient time temp'
);
Blockly.Scheme.ORDER_NONE = 0;
Blockly.Scheme.ORDER_ATOMIC = 1;

// --- Bloco RAIZ: create_final_recipe ---
// Gera um código Scheme completo, com (load ...) + (create-recipe ...)
Blockly.Scheme.forBlock['create_final_recipe'] = function(block) {
  const finalName = block.getFieldValue('TITLE');

  // Pega a cadeia de composição gerada pelos blocos recipe_modification
    let composition =
    Blockly.Scheme.valueToCode(block, 'COMPOSITION', Blockly.Scheme.ORDER_NONE) ||
    'bolo-simples';

  if (composition) {
    composition = composition.trim();

    // valueToCode costuma adicionar um parêntese externo:
    //   (bolo-simples)              -> queremos bolo-simples
    //   ((adicao-chocolate bolo))   -> queremos (adicao-chocolate bolo)
    if (composition.startsWith('(') && composition.endsWith(')')) {
      composition = composition.slice(1, -1).trim();
    }
  }

  const code =
`;; Carrega as definições base de receitas e modificações
(load "../scheme_templates/render_recipe.scm")
(load "../scheme_templates/base_recipes.scm")

;; Execução final para imprimir a receita formatada:
(create-recipe "${finalName}"
  ${composition})
`;

  return code;
};


// --- Bloco de Composição/Base: recipe_modification ---
// Gera expressões do tipo:
//   - massa-de-torta
//   - (adicao-chocolate massa-de-torta)
//   - (cobertura-simples (adicao-cenoura bolo-simples))
Blockly.Scheme.forBlock['recipe_modification'] = function(block) {
  const name = (block.getFieldValue('COMPOUND') || '').trim();
  let nextCode =
    Blockly.Scheme.valueToCode(block, 'NEXT_RECIPE', Blockly.Scheme.ORDER_NONE);

  if (nextCode) {
    nextCode = nextCode.trim();

    // valueToCode costuma embrulhar em parênteses: (massa-de-torta) ou
    // ((adicao-chocolate massa-de-torta)). Removemos UM nível externo.
    if (nextCode.startsWith('(') && nextCode.endsWith(')')) {
      nextCode = nextCode.slice(1, -1).trim();
    }
  }

  let code;

  if (!name) {
    // Linha "separadora" ou vazia: só propaga o de baixo
    code = nextCode || '';
  } else if (nextCode) {
    // Modificação aplicada à próxima receita
    // Ex.: (adicao-chocolate massa-de-torta)
    code = `(${name} ${nextCode})`;
  } else {
    // Ponta da cadeia: receita base pura (ex.: massa-de-torta)
    code = name;
  }

  // Para blocos de valor, Blockly espera [código, ordem]
  return [code, Blockly.Scheme.ORDER_ATOMIC];
};


// --- Geração do workspace ---
// Só deixa o bloco "Gerar Receita" como topo.
Blockly.Scheme.workspaceToCode = function(workspace) {
  const blocks = workspace.getTopBlocks(true);
  const finalBlock = blocks.find(b => b.type === 'create_final_recipe');

  if (!finalBlock) {
    console.error('Nenhum bloco "Gerar Receita" encontrado no workspace.');
    return '';
  }

  // blockToCode chama forBlock['create_final_recipe'], que já monta o código inteiro.
  const code = Blockly.Scheme.blockToCode(finalBlock);
  return code || '';
};