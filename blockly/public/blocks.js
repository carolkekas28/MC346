// Definição dos blocos para composição de receitas
Blockly.defineBlocksWithJsonArray([
  
  // --- Bloco ÚNICO de Composição e Base ---
  // Este bloco serve para: 1. Definir a Receita Base (se for a ponta da cadeia). 
  // 2. Aplicar uma Modificação (se houver outro bloco encaixado).
  {
    "type": "recipe_modification",
    "message0": "Composição: %1 Encaixe ABAIXO: %2",
    "args0": [
      {
        "type": "field_dropdown",
        "name": "COMPOUND",
        "options": [
          ["--- RECEITAS BASE ---", ""],                // Separador visual
          ["Bolo Simples", "bolo-simples"],            // define-recipe bolo-simples
          ["Massa de Torta", "massa-de-torta"],        // define-recipe massa-de-torta
          ["Recheio Creme", "recheio-creme"],          // define-recipe recheio-creme
          ["Cobertura de Chocolate", "cobertura-chocolate"], // define-recipe cobertura-chocolate
          ["Calda de Limão", "calda-limao"],           // define-recipe calda-limao

          ["--- MODIFICAÇÕES ---", ""],                // Separador visual
          ["Adicionar Cenoura", "adicao-cenoura"],     // define-modification adicao-cenoura
          ["Adicionar Chocolate", "adicao-chocolate"], // define-modification adicao-chocolate
          ["Cobertura de Chocolate", "cobertura-simples"], // define-modification cobertura-simples
          ["Recheio Creme", "com-recheio-creme"],      // define-modification com-recheio-creme
          ["Finalizar com Calda de Limão", "finalizacao-limao"] // define-modification finalizacao-limao
        ]
      },
      {
        "type": "input_value",
        "name": "NEXT_RECIPE",
        "check": "Recipe"
      }
    ],
    "output": "Recipe",
    "colour": 45,
    "tooltip": "Na ponta da cadeia, selecione a Receita Base. Em cima, selecione as Modificações.",
    "helpUrl": ""
  },

  // --- Bloco de Saída Final (create-recipe) - Raiz ---
  {
    "type": "create_final_recipe",
    "message0": "Gerar Receita: %1 Cadeia de Composição: %2",
    "args0": [
      { "type": "field_input", "name": "TITLE", "text": "Receita Gerada" },
      { "type": "input_value", "name": "COMPOSITION", "check": "Recipe" }
    ],
    "colour": 290,
    "tooltip": "Arraste a cadeia de composição para este bloco raiz.",
    "helpUrl": "",
    "inputsInline": true 
  }
]);