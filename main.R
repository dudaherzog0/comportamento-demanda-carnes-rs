# ============================================================
# SCRIPT PRINCIPAL - EXECUÇÃO COMPLETA DO PROJETO
# ============================================================
# Para rodar todo o projeto de uma vez no RStudio, clique em "Source"
# ou execute no console: source("main.R")

cat("=== 1. Carregando dados e configurações (.env) ===\n")
source("scripts/00_carregar_dados.R")

cat("\n=== 2. Executando Testes de Estacionariedade (ADF, PP, KPSS) ===\n")
source("scripts/01_estacionariedade.R")

cat("\n=== 3. Executando Teste de Cointegração (Engle-Granger) ===\n")
source("scripts/02_cointegracao.R")

cat("\n=== 4. Estimando Modelo LA/AIDS (SUR), Elasticidades e Exportação ===\n")
source("scripts/03_modelo_laaids.R")

cat("\n=== Execução concluída com sucesso! ===\n")
