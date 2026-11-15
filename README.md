# Comportamento da Demanda por Carnes no Rio Grande do Sul

Repositório de códigos e procedimentos econométricos utilizados no Trabalho de Conclusão de Curso sobre o comportamento da demanda por carnes bovina, suína e de frango no Rio Grande do Sul.

---

## Estrutura do Projeto

```text
comportamento-demanda-carnes-rs/
├── dados/
│   └── Base para Estimação.xlsx      # Base de dados mensal (preços, gastos e participações)
├── scripts/
│   ├── 00_carregar_dados.R           # Leitura do .env e preparação das bases `base` e `base_sem_suino`
│   ├── 01_estacionariedade.R         # Testes de raiz unitária e estacionariedade (ADF, PP e KPSS)
│   ├── 02_cointegracao.R             # Teste de cointegração de Engle-Granger com Phillips-Perron
│   └── 03_modelo_laaids.R            # Estimação SUR do sistema LA/AIDS, elasticidades e exportação
├── resultados/
│   ├── tabelas/                      # Planilhas geradas pelo modelo (.xlsx)
│   └── graficos/                     # Gráficos e figuras gerados na análise
├── main.R                            # Script mestre que executa todas as etapas em sequência
├── .env.example                      # Modelo de configuração de caminhos locais
├── .gitignore                        # Arquivos ignorados pelo Git (inclui o .env local)
└── comportamento-demanda-carnes-rs.Rproj # Arquivo de projeto do RStudio
```

---

## Bibliotecas Utilizadas

Abaixo estão listados todos os pacotes do R utilizados no projeto e a função de cada um na análise:

| Biblioteca | Finalidade no Projeto |
| :--- | :--- |
| **`readxl`** | Leitura e importação da base de dados original em formato Excel (`.xlsx`). |
| **`dplyr`** | Manipulação, limpeza e transformação dos dados (criação de variáveis em log, índice de Stone, participações orçamentárias e filtros). |
| **`urca`** | Execução dos testes formais de estacionariedade e raiz unitária: *Augmented Dickey-Fuller* (`ur.df`), *Phillips-Perron* (`ur.pp`) e *KPSS* (`ur.kpss`). |
| **`tseries`** | Aplicação do teste de *Phillips-Perron* (`pp.test`) nas séries em nível e nos resíduos da regressão de longo prazo (etapa de cointegração de Engle-Granger). |
| **`systemfit`** | Estimação econométrica do sistema de equações de demanda **LA/AIDS** pelo método **SUR** (*Seemingly Unrelated Regressions*). |
| **`writexl`** | Exportação das tabelas finais de parâmetros, matriz Gama, elasticidades e testes de significância para um arquivo Excel (`.xlsx`) estruturado em abas. |

Para instalar todas as bibliotecas de uma única vez, execute no Console do R:

```r
install.packages(c("readxl", "dplyr", "urca", "tseries", "systemfit", "writexl"))
```

---

## Configuração Inicial (Opcional Arquivo `.env`)

O projeto já vem configurado para ler os dados da pasta `dados/` e salvar os resultados em `resultados/tabelas/` automaticamente.

Caso deseje personalizar os caminhos na sua máquina:
1. Faça uma cópia do arquivo `.env.example` na raiz do projeto e renomeie a cópia para **`.env`**.
2. Preencha as variáveis conforme desejar (ou deixe em branco para usar os caminhos padrão do projeto):
   ```ini
   INPUT_DATA_PATH="dados/Base para Estimação.xlsx"
   EXPORT_EXIT_PATH="resultados/tabelas/Resultados_Modelo_LAAIDS.xlsx"
   ```

---

## Como Executar o Projeto no R

Você não precisa copiar e colar os códigos manualmente. Escolha abaixo a forma que corresponde ao programa que você utiliza:

### Opção 1: Usando o RStudio (Recomendado)

1. Vá até a pasta do projeto no computador e dê **dois cliques** no arquivo **`comportamento-demanda-carnes-rs.Rproj`**.
   *(Isso abrirá o RStudio já configurado exatamente na pasta do projeto).*
2. **Para rodar tudo de uma só vez:**
   - No Console do RStudio, digite:
     ```r
     source("main.R")
     ```
   - *Alternativa:* Clique no arquivo `main.R` na aba *Files* (canto inferior direito) e clique no botão **Source** no topo do editor.
3. **Para rodar etapa por etapa (individualmente):**
   - Na aba *Files*, abra a pasta `scripts/` e clique no script desejado (`01_estacionariedade.R`, `02_cointegracao.R` ou `03_modelo_laaids.R`).
   - Selecione as linhas que deseja executar e pressione **`Ctrl + Enter`** para acompanhar cada resultado no Console.

---

### Opção 2: Usando o R Padrão (RGui)

Se você utiliza o programa padrão do R (sem o RStudio):

1. Abra o **RGui**.
2. No Console, defina a pasta onde o projeto está salvo no seu computador usando `setwd()` e em seguida chame o `main.R`:
   ```r
   # 1. Aponte para a pasta onde o projeto está salvo na sua máquina:
   setwd("C:/caminho/para/comportamento-demanda-carnes-rs")

   # 2. Execute todo o projeto:
   source("main.R")
   ```
3. Caso queira rodar apenas um script específico, basta substituir `"main.R"` pelo caminho do script:
   ```r
   source("scripts/01_estacionariedade.R")
   source("scripts/02_cointegracao.R")
   source("scripts/03_modelo_laaids.R")
   ```

---

## Onde Encontrar os Resultados Gerados

* **No Console do R:** Ao executar os scripts, os resultados dos testes de estacionariedade (`resultados`), da regressão de cointegração (`teste_pp_residuos`) e do modelo LA/AIDS são exibidos diretamente na tela.
* **No Excel:** Ao final da execução do script `03_modelo_laaids.R` (ou do `main.R`), é gerado automaticamente o arquivo:
  ```text
  resultados/tabelas/Resultados_Modelo_LAAIDS.xlsx
  ```
  *(Ou no caminho personalizado que você tiver definido na variável `EXPORT_EXIT_PATH` do seu `.env`).*
