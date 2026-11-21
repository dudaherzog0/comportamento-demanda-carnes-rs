# ============================================================
# 00. CONFIGURAÇÃO DE AMBIENTE E CARREGAMENTO DOS DADOS
# ============================================================

library(readxl)
library(dplyr)

# 1. Carregar variáveis do arquivo .env (se existir na raiz do projeto)
if (file.exists(".env")) {
  readRenviron(".env")
}

# ------------------------------------------------------------
# 2. CARREGAR BASE COM AS 3 CARNES (BOVINA, SUÍNA E FRANGO)
# ------------------------------------------------------------
caminho_entrada <- Sys.getenv("INPUT_DATA_PATH")
if (caminho_entrada == "") {
  caminho_entrada <- "dados/Base para Estimação.xlsx"
}

if (file.exists(caminho_entrada)) {
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
  cat("Base (com suíno) carregada com sucesso de:", caminho_entrada, "\n")
}

# ------------------------------------------------------------
# 3. CARREGAR ARQUIVO SEM CARNE SUÍNA E CALCULAR ÍNDICE DE STONE EM NÍVEL
# ------------------------------------------------------------
caminho_sem_suino <- Sys.getenv("INPUT_DATA_SEM_SUINO_PATH")
if (caminho_sem_suino == "") {
  caminho_sem_suino <- "dados/Base_Sem_Suino.csv"
}

if (!file.exists(caminho_sem_suino)) {
  stop(paste("Arquivo sem suíno não encontrado em:", caminho_sem_suino))
}

# Suporta tanto .csv quanto .xlsx
if (grepl("\\.csv$", caminho_sem_suino, ignore.case = TRUE)) {
  base_sem_suino_bruta <- read.csv(
    caminho_sem_suino,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
} else {
  base_sem_suino_bruta <- readxl::read_excel(caminho_sem_suino)
}

base_sem_suino <- base_sem_suino_bruta %>%
  rename(
    Ano_Mes    = 1,
    Gasto_Bov  = `Gasto Bovinos(R$)`,
    Gasto_Fra  = `Gasto Frango(R$)`,
    Desp_Total = `DespesaTotal(X)`,
    Part_Bov   = `Part Bov(wi)`,
    Part_Fra   = `Part Fra(wi)`,
    Preco_Bov  = `Preco Bov(R$/kg)`,
    Preco_Fra  = `Preco Fran(R$/kg)`
  ) %>%
  arrange(Ano_Mes) %>%
  mutate(
    # --- VARIÁVEIS EM NÍVEL (27 observações) ---
    # Participações orçamentárias (w_i)
    w_bov = Part_Bov,
    w_fra = Part_Fra,
    
    # Logaritmos naturais dos preços (ln p_i)
    ln_p_bov = log(Preco_Bov),
    ln_p_fra = log(Preco_Fra),
    
    # Índice de Stone corrigido (Moschini, 1995): participações médias da
    # amostra e preços normalizados pela média, evitando que Δln P* contenha Δw
    ln_stone     = mean(w_bov) * log(Preco_Bov / mean(Preco_Bov)) +
                   mean(w_fra) * log(Preco_Fra / mean(Preco_Fra)),
    Indice_Stone = exp(ln_stone),
    
    # Gasto real em nível: ln(X / P*) = ln(X) - ln(P*)
    ln_x_real = log(Desp_Total) - ln_stone,
    
    # Preço relativo em nível: ln(P_bov) - ln(P_fra)
    lp_bov_fra = ln_p_bov - ln_p_fra,
    
    # --- VARIÁVEIS EM PRIMEIRA DIFERENÇA (Δ) ---
    dw_bov       = w_bov - dplyr::lag(w_bov),
    dw_fra       = w_fra - dplyr::lag(w_fra),
    d_ln_p_bov   = ln_p_bov - dplyr::lag(ln_p_bov),
    d_ln_p_fra   = ln_p_fra - dplyr::lag(ln_p_fra),
    d_lp_bov_fra = lp_bov_fra - dplyr::lag(lp_bov_fra),
    d_ln_x_real  = ln_x_real - dplyr::lag(ln_x_real)
  )

cat("Base sem suíno (Nível + Índice de Stone + 1ª Diferença) carregada de:", caminho_sem_suino, "\n")
