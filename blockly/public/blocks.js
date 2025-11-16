// Definição dos blocos para composição de receitas
Blockly.defineBlocksWithJsonArray([
  
  // --- Bloco ÚNICO de Composição e Base ---
  // Este bloco serve para: 1. Definir a Receita Base (se for a ponta da cadeia). 
  // 2. Aplicar uma Modificação (se houver outro bloco encaixado).
  {
    "type": "recipe_modification",
    "message0": "Composição: %1 Encaixe ABAIXO: %2",
    "args0": [
      // Lista as Funções (Modificações) e os Símbolos (Bases)
      {"type": "field_dropdown", "name": "COMPOUND", "options": [
        ["--- RECEITAS BASE ---",""], // Separador visual
        ["Bolo Simples","bolo-simples"], // Receita Base
        ["Torta Básica","torta-basica"], // Receita Base
        ["--- MODIFICAÇÕES ---",""], // Separador visual
        ["Adicionar Cenoura","de-cenoura"], // Função de Modificação
        ["Adicionar Calda Chocolate","com-calda-de-chocolate"], // Função de Modificação
        ["Recheio Simples","recheio-simples"] // Função de Modificação
      ]},
      // A entrada para o próximo elemento. Se vazio, o bloco vira o SÍMBOLO.
      {"type": "input_value", "name": "NEXT_RECIPE", "check": "Recipe"} 
    ],
    "output": "Recipe", // Pode ser encaixado
    "colour": 45, 
    "tooltip": "Na ponta da cadeia, selecione a Receita Base. Em cima, selecione as Modificações.",
    "helpUrl": ""
  },

  // --- Bloco de Saída Final (create-recipe) - Raiz ---
  // Apenas inicia a execução, recebendo a cadeia completa.
  {
    "type": "create_final_recipe",
    "message0": "Gerar Receita: %1 Cadeia de Composição: %2",
    "args0": [
      {"type": "field_input", "name": "TITLE", "text": "Receita Gerada"},
      // Recebe a cadeia completa (ex: (de-cenoura bolo-simples))
      {"type": "input_value", "name": "COMPOSITION", "check": "Recipe"}
    ],
    "colour": 290,
    "tooltip": "Arraste a cadeia de composição para este bloco raiz.",
    "helpUrl": "",
    "inputsInline": true 
  }
]);