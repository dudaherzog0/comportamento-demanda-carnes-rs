# ============================================================
# 02. TESTE DE COINTEGRAÇÃO (ENGLE-GRANGER) COM PHILLIPS-PERRON (PP)
# FOCO: APENAS CARNE BOVINA E CARNE DE FRANGO
# ============================================================

# Instalar pacotes necessários (caso não tenha)
# install.packages("dplyr")
# install.packages("tseries")

library(dplyr)
library(tseries)

# Carregar dados caso ainda não estejam no ambiente
if (!exists("base")) {
  source("scripts/00_carregar_dados.R")
}

# 1. Preparação dos Dados (selecionando apenas bovina e frango)
dados_bov_fra <- base %>%
  arrange(Ano_Mes) %>%
  transmute(
    P_bov = Preco_Bov, # Carne Bovina
    P_fra = Preco_Fra  # Carne de Frango
  )


# ============================================================
# PASSO 1: Testes de Raiz Unitária nas Séries Individuais (em Nível)
# ============================================================
cat("--- TESTE DE RAIZ UNITÁRIA: CARNE BOVINA ---\n")
pp.test(dados_bov_fra$P_bov, alternative = "stationary")

cat("\n--- TESTE DE RAIZ UNITÁRIA: CARNE DE FRANGO ---\n")
pp.test(dados_bov_fra$P_fra, alternative = "stationary")
# Nota: Se o p-valor for > 0.05, as séries possuem raiz unitária em nível I(1).


# ============================================================
# PASSO 2: Regressão OLS de Longo Prazo (Bovina vs Frango)
# ============================================================
modelo_LongoPrazo <- lm(P_bov ~ P_fra, data = dados_bov_fra)

cat("\n--- SUMÁRIO DA REGRESSÃO DE LONGO PRAZO ---\n")
print(summary(modelo_LongoPrazo))


# ============================================================
# PASSO 3: Extração dos Resíduos da Regressão
# ============================================================
residuos_bov_fra <- residuals(modelo_LongoPrazo)


# ============================================================
# PASSO 4: Teste de Phillips-Perron (PP) nos Resíduos
# ============================================================
teste_pp_residuos <- pp.test(residuos_bov_fra, alternative = "stationary")

cat("\n--- TESTE DE COINTEGRAÇÃO (PP NOS RESÍDUOS) ---\n")
print(teste_pp_residuos)
# Interpretação: Se o p-valor dos resíduos for < 0.05, os resíduos são I(0) 
# e as séries são cointegradas (possuem relação de longo prazo).
