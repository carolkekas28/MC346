// Verifica se a variável global Blockly existe e se o gerador 'Scheme' já foi criado.
// Caso contrário, cria um novo gerador com o nome 'Scheme'.
Blockly.Scheme = new Blockly.Generator('Scheme');

// Palavras reservadas e ordem de precedência
Blockly.Scheme.addReservedWords('list quote define define-recipe create-recipe step ingredient time temp');
Blockly.Scheme.ORDER_NONE = 0;
Blockly.Scheme.ORDER_ATOMIC = 1; 


// --- 1. Bloco de Composição Final (create_final_recipe) ---
// Gera: (create-recipe "Título" <cadeia-de-composicao>)
Blockly.Scheme['create_final_recipe'] = function(block) {
  const final_name = block.getFieldValue('TITLE');
  
  // Tenta capturar a cadeia de modificações/base. Se não houver nada, usa 'bolo-simples' como fallback.
  const composition_code = Blockly.Scheme.valueToCode(block, 'COMPOSITION', Blockly.Scheme.ORDER_NONE) || 'bolo-simples'; 
  
  // O código final que será executado pelo Guile
  const code = `\n;; Execução final para imprimir a receita formatada:\n(create-recipe "${final_name}"
  ${composition_code})`;
  
  // Retorna o código final, que deve ser o único bloco top-level
  return code;
};


// --- 2. Bloco de Modificação e Base (recipe_modification) ---
// Gera: (de-cenoura <proximo-elemento>) ou bolo-simples
Blockly.Scheme['recipe_modification'] = function(block) {
  const compound_name = block.getFieldValue('COMPOUND'); 
  
  // Se o COMPOUND for vazio (separador visual), retorna a próxima receita
  if (!compound_name) {
    return [Blockly.Scheme.valueToCode(block, 'NEXT_RECIPE', Blockly.Scheme.ORDER_ATOMIC) || '', Blockly.Scheme.ORDER_ATOMIC];
  }
  
  // Captura o próximo elemento na cadeia.
  const next_recipe = Blockly.Scheme.valueToCode(block, 'NEXT_RECIPE', Blockly.Scheme.ORDER_NONE); 
  
  let code;

  if (next_recipe) {
    // É uma Modificação, precisa encaixar no próximo elemento
    code = `(${compound_name} ${next_recipe})`;
  } else {
    // É o final da cadeia (Receita Base), retorna o símbolo
    code = compound_name;
  }
  
  return [code, Blockly.Scheme.ORDER_ATOMIC];
};


// --- 3. Fallback para Geração do Workspace (workspaceToCode) ---
Blockly.Scheme.workspaceToCode = function(workspace) {
  const blocks = workspace.getTopBlocks(true);
  let code = '';
  
  // 1. Carrega a infraestrutura de dados antes de tudo
  code += ';; Carrega as definições base de receitas e modificações\n';
  code += '(load "base_recipes.scm")\n\n'; 
  
  // 2. Gera o código de cada bloco top-level
  for (let b of blocks) {
    // Verifica se a função de geração existe para o tipo de bloco
    if (Blockly.Scheme[b.type]) {
        code += Blockly.Scheme.blockToCode(b) || '';
    } else {
        // Erro se o bloco não tiver gerador. Isso deve parar o processo se houver um bloco não tratado.
        console.error(`Bloco não suportado: ${b.type}`);
    }
  }
  return code;
};