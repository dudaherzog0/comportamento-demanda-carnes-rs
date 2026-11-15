# ============================================================
# 00. CONFIGURAÇÃO DE AMBIENTE E CARREGAMENTO DOS DADOS
# ============================================================

library(readxl)
library(dplyr)

# 1. Carregar variáveis do arquivo .env (se existir na raiz do projeto)
if (file.exists(".env")) {
  readRenviron(".env")
}

# 2. Definir caminho de entrada (usa o padrão em dados/ se estiver em branco no .env)
caminho_entrada <- Sys.getenv("INPUT_DATA_PATH")
if (caminho_entrada == "") {
  caminho_entrada <- "dados/Base para Estimação.xlsx"
}

if (!file.exists(caminho_entrada)) {
  stop(paste("Arquivo de dados não encontrado em:", caminho_entrada))
}

# 3. Ler a planilha original e padronizar nomes das colunas para o objeto `base`
base_bruta <- readxl::read_excel(caminho_entrada)

base <- base_bruta %>%
  rename(
    Ano_Mes    = `Ano/mês`,
    Gasto_Bov  = `Gasto Bovinos(R$)`,
    Gasto_Sui  = `Gasto Suino(R$)`,
    Gasto_Fra  = `Gasto Frango(R$)`,
    Desp_Total = `DespesaTotal(X)`,
    Part_Bov   = `Part Bov(wi)`,
    Part_Sui   = `Part Sui(wi)`,
    Part_Fra   = `Part Fra(wi)`,
    Preco_Bov  = `Preco Bov(R$/kg)`,
    Preco_Sui  = `Preco Sui(R$/kg)`,
    Preco_Fra  = `Preco Fran(R$/kg)`
  )

# 4. Criar `base_sem_suino` (sistema LA/AIDS para Carne Bovina e Carne de Frango)
# Recalcula a despesa total e as participações (w_bov + w_fra = 1)
base_sem_suino <- base %>%
  mutate(
    Desp_Total = Gasto_Bov + Gasto_Fra,
    Part_Bov   = Gasto_Bov / Desp_Total,
    Part_Fra   = Gasto_Fra / Desp_Total
  )

cat("Dados carregados com sucesso a partir de:", caminho_entrada, "\n")
