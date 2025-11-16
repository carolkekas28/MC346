const express = require('express');
const bodyParser = require('body-parser');
const fs = require('fs');
const { execFile } = require('child_process');
const { v4: uuidv4 } = require('uuid');
const path = require('path');

const app = express();
app.use(bodyParser.json());
app.use(express.static(path.join(__dirname, 'public'))); // Garante que a pasta public seja servida

const SCHEME_BIN = 'guile';

// Diretório onde os arquivos Scheme de lógica (macros) e dados (receitas base) residem
const SCHEME_TEMPLATES_DIR = path.join(__dirname, 'scheme_templates');

app.post('/run-scheme', (req, res) => {
  const { code } = req.body;
  if (typeof code !== 'string') return res.status(400).json({ success: false, error: 'Código não fornecido.' });

  const id = uuidv4();
  const tmpDir = path.join(__dirname, 'tmp');
  fs.mkdirSync(tmpDir, { recursive: true });
  const filename = path.join(tmpDir, `code-${id}.scm`);
  
  // 1. Envolver o código do usuário com o comando para carregar a infraestrutura
  // O código do usuário (gerado pelo Blockly) já contém (load "base_recipes.scm")
  // Precisamos garantir que a macro 'create-recipe' (de render_recipe.scm) exista.
  const wrapperCode = `
(load (string-append "${SCHEME_TEMPLATES_DIR}/render_recipe.scm"))
${code}
  `;

  fs.writeFileSync(filename, wrapperCode, 'utf8');

  // 2. Executar o Guile
  // Importante: Definimos o CWD (Current Working Directory) para o diretório de templates.
  // Isso garante que o (load "base_recipes.scm") do código gerado funcione.
  const options = {
    timeout: 3000,
    maxBuffer: 2000000,
    cwd: SCHEME_TEMPLATES_DIR // Onde o Guile deve procurar arquivos (load "...")
  };
  
  execFile(SCHEME_BIN, [filename], options, (err, stdout, stderr) => {
    // 3. Apagar arquivo (em background, ignora erros)
    try { fs.unlinkSync(filename); } catch(e){}

    if (err) {
      // Erro de execução ou Timeout
      const msg = (err.killed ? 'Timeout de execução (3s) atingido.' : (stderr || err.message));
      console.error("ERRO Guile:", msg);
      // Retorna o erro no stderr para o frontend
      return res.status(500).json({ success: false, error: msg.trim() });
    }
    
    // Sucesso: Retorna o que foi impresso pelo Guile
    return res.json({ success: true, output: stdout.trim() });
  });
});

const port = process.env.PORT || 3000;
app.listen(port, () => console.log('Server on http://localhost:' + port));