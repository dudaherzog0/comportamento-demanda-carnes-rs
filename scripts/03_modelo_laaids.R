# ============================================================
# 03. SUR E LA/AIDS EM PRIMEIRA DIFERENÇA (FD-LA/AIDS)
# ============================================================

library(dplyr)
library(systemfit)
library(writexl)

# Carregar dados e variáveis preparadas
source("scripts/00_carregar_dados.R")

# ============================================================
# 1. PREPARAR A BASE: CALCULAR EM NÍVEL E DIFERENCIAR NO FINAL
# ============================================================

# 1.1 Cálculo em nível (27 observações: 2024/07 a 2026/09)
#
# ÍNDICE DE PREÇOS: STONE CORRIGIDO DE MOSCHINI (1995)
# O Stone original, ln P*_t = sum(w_it * ln p_it), usa a participação do
# próprio período. Ao diferenciar, Δln(X/P*) passa a conter um termo com
# Δw_bov (a variável dependente), gerando viés de simultaneidade; além
# disso, o índice depende das unidades de medida dos preços.
# Correção: usar as participações MÉDIAS da amostra (fixas no tempo) e os
# preços normalizados pela média:
#   ln P*_t = sum( w_barra_i * ln(p_it / p_barra_i) )
# Assim, Δln P*_t = sum( w_barra_i * Δln p_it ), sem nenhum termo em Δw.
# O cálculo continua em nível e a diferença é tirada só no final.

w_bov_barra <- mean(base_sem_suino$Part_Bov)
w_fra_barra <- mean(base_sem_suino$Part_Fra)

dados_nivel <- base_sem_suino %>%
  arrange(Ano_Mes) %>%
  mutate(
    # Participações orçamentárias em nível (w_i)
    w_bov = Part_Bov,
    w_fra = Part_Fra,
    
    # Logaritmos dos preços em nível (ln p_i)
    ln_p_bov = log(Preco_Bov),
    ln_p_fra = log(Preco_Fra),
    
    # Índice de Stone corrigido (Moschini): participações médias e
    # preços normalizados pela média da amostra (ÍNDICE PRINCIPAL)
    ln_stone     = w_bov_barra * log(Preco_Bov / mean(Preco_Bov)) +
                   w_fra_barra * log(Preco_Fra / mean(Preco_Fra)),
    Indice_Stone = exp(ln_stone),
    
    # Versões alternativas, usadas apenas no teste de robustez (seção 4.2):
    # (a) Stone original, com participações correntes w_t
    ln_stone_original  = w_bov * ln_p_bov + w_fra * ln_p_fra,
    # (b) Stone com participações defasadas w_{t-1}
    ln_stone_defasado  = dplyr::lag(w_bov) * ln_p_bov + dplyr::lag(w_fra) * ln_p_fra,
    
    # Gasto real em nível: ln(X / P*) = ln(X) - ln(P*)
    ln_x_real          = log(Desp_Total) - ln_stone,
    ln_x_real_original = log(Desp_Total) - ln_stone_original,
    ln_x_real_defasado = log(Desp_Total) - ln_stone_defasado,
    
    # Preço relativo em nível: ln(P_bov) - ln(P_fra)
    lp_bov_fra = ln_p_bov - ln_p_fra
  )

# 1.2 Primeira diferenciação de todas as variáveis da equação:
# w -> Δw; ln p -> Δln p; ln(X/P*) -> Δln(X/P*)
# Perde a 1ª observação (2024/07), ficando com 26 observações (2024/08 a 2026/09)
dados <- dados_nivel %>%
  mutate(
    dw_bov       = w_bov - dplyr::lag(w_bov),
    dw_fra       = w_fra - dplyr::lag(w_fra),
    d_ln_p_bov   = ln_p_bov - dplyr::lag(ln_p_bov),
    d_ln_p_fra   = ln_p_fra - dplyr::lag(ln_p_fra),
    d_lp_bov_fra = d_ln_p_bov - d_ln_p_fra,
    d_ln_x_real  = ln_x_real - dplyr::lag(ln_x_real),
    d_ln_x_real_original = ln_x_real_original - dplyr::lag(ln_x_real_original),
    d_ln_x_real_defasado = ln_x_real_defasado - dplyr::lag(ln_x_real_defasado)
  ) %>%
  filter(!is.na(dw_bov))

# Conferência: com o Stone corrigido, Δln P* = sum(w_barra * Δln p) exatamente
stopifnot(all.equal(
  dados$d_ln_x_real,
  log(dados$Desp_Total) - log(dplyr::lag(dados_nivel$Desp_Total))[-1] -
    (w_bov_barra * dados$d_ln_p_bov + w_fra_barra * dados$d_ln_p_fra)
))

cat("Número de observações em nível:", nrow(dados_nivel), "\n")
cat("Número de observações após 1ª diferença:", nrow(dados), "\n")

# ============================================================
# 2. ESPECIFICAR A EQUAÇÃO EM PRIMEIRA DIFERENÇA (SEM INTERCEPTO)
# ============================================================
# O intercepto some na diferença (0 + remove a constante para não virar tendência)
eq_bov <- dw_bov ~ 0 +
  d_lp_bov_fra +
  d_ln_x_real

sistema_aids <- list(
  bovina = eq_bov
)

# ============================================================
# 3. ESTIMAR O MODELO (SUR)
# ============================================================
modelo_aids <- systemfit(
  sistema_aids,
  method = "SUR",
  data = dados
)

# ============================================================
# 4. RESULTADOS
# ============================================================
summary(modelo_aids)

# ============================================================
# 4.1 DIAGNÓSTICOS DOS RESÍDUOS
# ============================================================
# Com uma única equação (a do frango é recuperada por adding-up),
# o SUR é idêntico ao MQO. Reestimamos com lm() para usar lmtest/sandwich.
library(lmtest)
library(sandwich)
library(urca)

modelo_mqo <- lm(dw_bov ~ 0 + d_lp_bov_fra + d_ln_x_real, data = dados)

# Conferência: coeficientes do lm() devem ser iguais aos do systemfit
stopifnot(all.equal(
  unname(coef(modelo_mqo)),
  unname(coef(modelo_aids)[c("bovina_d_lp_bov_fra", "bovina_d_ln_x_real")])
))

residuos_aids <- residuals(modelo_mqo)

# --- R² ---
# Sem intercepto, o R exibe o R² NÃO centrado (inflado). Calculamos também
# o R² centrado, que é o comparável com modelos com constante.
y_dif        <- dados$dw_bov
r2_nao_centr <- summary(modelo_mqo)$r.squared
r2_centrado  <- 1 - sum(residuos_aids^2) / sum((y_dif - mean(y_dif))^2)

# --- 1) Autocorrelação: Breusch-Godfrey com 1 e 2 defasagens ---
bg1 <- bgtest(modelo_mqo, order = 1)
bg2 <- bgtest(modelo_mqo, order = 2)

# --- 2) Heterocedasticidade: Breusch-Pagan ---
bp <- bptest(modelo_mqo)

# --- 3) Normalidade dos resíduos: Shapiro-Wilk ---
sw <- shapiro.test(residuos_aids)

# --- 4) ADF nos resíduos (sem constante e sem tendência, 1 defasagem) ---
adf_res      <- ur.df(residuos_aids, type = "none", lags = 1)
adf_res_stat <- as.numeric(adf_res@teststat[1])
adf_res_cv5  <- as.numeric(adf_res@cval[1, "5pct"])

# --- Teste da constante (sugestão: se não for significativa, deixa sem) ---
modelo_const <- lm(dw_bov ~ d_lp_bov_fra + d_ln_x_real, data = dados)
p_const      <- summary(modelo_const)$coefficients["(Intercept)", "Pr(>|t|)"]

diagnosticos <- data.frame(
  Teste = c(
    "R² não centrado (sem intercepto, saída do R)",
    "R² centrado",
    "Breusch-Godfrey (1 defasagem) - autocorrelação",
    "Breusch-Godfrey (2 defasagens) - autocorrelação",
    "Breusch-Pagan - heterocedasticidade",
    "Shapiro-Wilk - normalidade",
    "ADF nos resíduos (estat. vs crítico 5%)",
    "Intercepto no modelo em diferença"
  ),
  Estatistica = c(
    r2_nao_centr, r2_centrado,
    unname(bg1$statistic), unname(bg2$statistic),
    unname(bp$statistic), unname(sw$statistic),
    adf_res_stat, NA
  ),
  Valor_Critico_5 = c(NA, NA, NA, NA, NA, NA, adf_res_cv5, NA),
  P_Valor = c(
    NA, NA,
    bg1$p.value, bg2$p.value,
    bp$p.value, sw$p.value,
    NA, p_const
  ),
  H0 = c(
    "-", "-",
    "Sem autocorrelação", "Sem autocorrelação",
    "Homocedasticidade", "Resíduos normais",
    "Raiz unitária nos resíduos", "Intercepto = 0"
  )
)

diagnosticos$Conclusao <- c(
  "Não comparar com o modelo em nível",
  "Comparável com modelos com constante",
  ifelse(bg1$p.value < 0.05, "PROBLEMA: há autocorrelação", "OK"),
  ifelse(bg2$p.value < 0.05, "PROBLEMA: há autocorrelação", "OK"),
  ifelse(bp$p.value  < 0.05, "PROBLEMA: há heterocedasticidade", "OK"),
  ifelse(sw$p.value  < 0.05, "Resíduos não normais", "OK"),
  ifelse(adf_res_stat < adf_res_cv5, "OK: resíduos estacionários", "Resíduos NÃO estacionários"),
  ifelse(p_const < 0.05, "Constante significativa: avaliar incluir",
         "Não significativa: manter sem intercepto")
)

print(diagnosticos)

# --- Erro-padrão robusto (Newey-West) ---
# Se houver autocorrelação ou heterocedasticidade, o modelo não muda:
# apenas os erros-padrão passam a ser os robustos de Newey-West.
usar_robusto <- (bg1$p.value < 0.05) || (bg2$p.value < 0.05) || (bp$p.value < 0.05)

vcov_nw  <- NeweyWest(modelo_mqo)
coef_nw  <- coeftest(modelo_mqo, vcov = vcov_nw)
print(coef_nw)

coef_robustos <- data.frame(
  Parametro       = c("Gama bovina-bovina (g11)", "Beta bovina"),
  Estimativa      = coef_nw[, "Estimate"],
  Erro_Padrao_MQO = sqrt(diag(vcov(modelo_mqo))),
  Erro_Padrao_NW  = coef_nw[, "Std. Error"],
  Estat_t_NW      = coef_nw[, "t value"],
  P_Valor_NW      = coef_nw[, "Pr(>|t|)"],
  row.names       = NULL
)

cat("\nErros-padrão usados nas elasticidades:",
    ifelse(usar_robusto, "ROBUSTOS (Newey-West)", "MQO convencionais"), "\n")

# ============================================================
# 4.2 ROBUSTEZ: COMPARAÇÃO ENTRE AS VERSÕES DO ÍNDICE DE PREÇOS
# ============================================================
# Reestima a mesma equação trocando apenas o deflator do gasto, para mostrar
# o quanto os resultados dependem da escolha do índice.
w_bov_m <- mean(dados_nivel$w_bov)
w_fra_m <- mean(dados_nivel$w_fra)

estimar_versao <- function(var_gasto, rotulo) {
  f   <- as.formula(paste("dw_bov ~ 0 + d_lp_bov_fra +", var_gasto))
  m   <- lm(f, data = dados)
  g   <- unname(coef(m)["d_lp_bov_fra"])
  b   <- unname(coef(m)[var_gasto])
  ct  <- summary(m)$coefficients
  data.frame(
    Indice          = rotulo,
    N_obs           = nobs(m),
    Gama            = g,
    P_Gama          = ct["d_lp_bov_fra", "Pr(>|t|)"],
    Beta_bovina     = b,
    P_Beta          = ct[var_gasto, "Pr(>|t|)"],
    Elast_gasto_bov = 1 + b / w_bov_m,
    Elast_gasto_fra = 1 - b / w_fra_m,
    Marsh_bov_bov   = -1 + g / w_bov_m - b,
    Marsh_fra_fra   = -1 + g / w_fra_m + b,
    Hicks_bov_fra   = -g / w_bov_m + w_fra_m,
    BG1_p           = bgtest(m, order = 1)$p.value
  )
}

robustez_indice <- rbind(
  estimar_versao("d_ln_x_real",          "Stone corrigido - Moschini (PRINCIPAL)"),
  estimar_versao("d_ln_x_real_original", "Stone original (w_t)"),
  estimar_versao("d_ln_x_real_defasado", "Stone com w defasado (w_t-1)")
)

print(robustez_indice)

# ============================================================
# 5. RECUPERAR A VARIAÇÃO DA PARTICIPAÇÃO DO FRANGO (Δw_fra)
# ============================================================
# Na 1ª diferença, como w_bov + w_fra = 1, temos Δw_bov + Δw_fra = 0
dados$dw_fra_estimado <- -fitted(modelo_aids$eq[[1]])

# ============================================================
# 6. VERIFICAR A RESTRIÇÃO ADDING-UP
# ============================================================
# Soma das participações em nível (= 1)
summary(dados$w_bov + dados$w_fra)

# Soma das variações estimadas na 1ª diferença (= 0)
summary(
  fitted(modelo_aids$eq[[1]]) +
    dados$dw_fra_estimado
)

# ============================================================
# 7. VERIFICAR VALORES AUSENTES
# ============================================================
colSums(is.na(dados))

# ============================================================
# 8. VERIFICAR VALORES DOS PREÇOS E DESPESAS
# ============================================================
summary(dados[, c(
  "Preco_Bov",
  "Preco_Fra",
  "Desp_Total"
)])

# ============================================================
# 9. COEFICIENTES E SIGNIFICÂNCIA
# ============================================================
summary(modelo_aids)
coef(modelo_aids)

# ============================================================
# 10. ESTIMAR OS PARÂMETROS BETA E GAMA (SEM INTERCEPTO ALFA)
# ============================================================

# Extrair coeficientes estimados na 1ª diferença
coeficientes <- coef(modelo_aids)

beta_bov <- unname(coeficientes["bovina_d_ln_x_real"])
gamma    <- unname(coeficientes["bovina_d_lp_bov_fra"])

# Recuperar parâmetro beta do frango pela restrição de adding-up (beta_bov + beta_fra = 0)
beta_fra <- -beta_bov

# Matriz de parâmetros gama (homogeneidade e simetria impostas)
gama <- matrix(
  c(gamma, -gamma,
    -gamma, gamma),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Bovina", "Frango"),
    c("Bovina", "Frango")
  )
)

# Tabela dos parâmetros (sem Alfa, pois o intercepto some na 1ª diferença)
parametros <- data.frame(
  Parametro = c(
    "Beta bovina",
    "Beta frango",
    "Gama bovina-bovina (g11 = g22 = -g12)"
  ),
  Estimativa = c(
    beta_bov,
    beta_fra,
    gamma
  )
)

print(parametros)
print(gama)

# ============================================================
# 11. ELASTICIDADES-GASTO DA DEMANDA (EQ. 18)
# ============================================================
# IMPORTANTE: Usa a média do w EM NÍVEL (e não do Δw)
w_bov_med <- mean(dados_nivel$w_bov)
w_fra_med <- mean(dados_nivel$w_fra)

cat("Participação média em nível - Bovina (w_bov_med):", round(w_bov_med, 4), "\n")
cat("Participação média em nível - Frango (w_fra_med):", round(w_fra_med, 4), "\n")

# Elasticidades-gasto
eta_bov <- 1 + beta_bov / w_bov_med
eta_fra <- 1 + beta_fra / w_fra_med

# ============================================================
# 12. ELASTICIDADES-PREÇO MARSHALLIANAS (EQ. 19)
# ============================================================

# Carne bovina
e_bov_bov <- -1 + (gamma / w_bov_med) - beta_bov
e_bov_fra <- (-gamma / w_bov_med) - beta_bov * (w_fra_med / w_bov_med)

# Carne de frango
e_fra_bov <- (-gamma / w_fra_med) - beta_fra * (w_bov_med / w_fra_med)
e_fra_fra <- -1 + (gamma / w_fra_med) - beta_fra

# ============================================================
# 13. ELASTICIDADES-PREÇO HICKSIANAS / COMPENSADAS (EQ. 20)
# ============================================================

# Fórmula direta: h_ij = -delta_ij + (gamma_ij / w_i) + w_j
h_bov_bov <- -1 + (gamma / w_bov_med) + w_bov_med
h_bov_fra <- (-gamma / w_bov_med) + w_fra_med   # Cruzada: Bovina em relação ao preço do Frango

h_fra_bov <- (-gamma / w_fra_med) + w_bov_med   # Cruzada: Frango em relação ao preço da Bovina
h_fra_fra <- -1 + (gamma / w_fra_med) + w_fra_med

# Conferência pela Equação de Slutsky: h_ij = e_ij + E_i * w_j
stopifnot(
  all.equal(h_bov_fra, e_bov_fra + eta_bov * w_fra_med),
  all.equal(h_fra_bov, e_fra_bov + eta_fra * w_bov_med)
)

# ============================================================
# 14. ERROS-PADRÃO, ESTATÍSTICA t E P-VALOR DAS ELASTICIDADES
# ============================================================

# Usa Newey-West se os diagnósticos (seção 4.1) indicarem autocorrelação
# ou heterocedasticidade; caso contrário, a matriz convencional.
V <- if (usar_robusto) vcov_nw else vcov(modelo_mqo)
var_gamma <- V["d_lp_bov_fra", "d_lp_bov_fra"]
var_beta  <- V["d_ln_x_real", "d_ln_x_real"]
cov_gb    <- V["d_lp_bov_fra", "d_ln_x_real"]
gl        <- df.residual(modelo_aids$eq[[1]]) # 26 - 2 = 24 graus de liberdade

# Erros-padrão das Marshallianas (combinação linear de gamma e beta)
se_e_bov_bov <- sqrt((1 / w_bov_med)^2 * var_gamma + (-1)^2 * var_beta + 2 * (1 / w_bov_med) * (-1) * cov_gb)
se_e_bov_fra <- sqrt((-1 / w_bov_med)^2 * var_gamma + (-w_fra_med / w_bov_med)^2 * var_beta + 2 * (-1 / w_bov_med) * (-w_fra_med / w_bov_med) * cov_gb)
se_e_fra_bov <- sqrt((-1 / w_fra_med)^2 * var_gamma + (w_bov_med / w_fra_med)^2 * var_beta + 2 * (-1 / w_fra_med) * (w_bov_med / w_fra_med) * cov_gb)
se_e_fra_fra <- sqrt((1 / w_fra_med)^2 * var_gamma + (1)^2 * var_beta + 2 * (1 / w_fra_med) * (1) * cov_gb)

# Erros-padrão das Hicksianas (dependem apenas de gamma)
se_h_bov_bov <- sqrt(var_gamma) / w_bov_med
se_h_bov_fra <- sqrt(var_gamma) / w_bov_med
se_h_fra_bov <- sqrt(var_gamma) / w_fra_med
se_h_fra_fra <- sqrt(var_gamma) / w_fra_med

# Erros-padrão das Elasticidades-Gasto (dependem apenas de beta)
se_eta_bov <- sqrt(var_beta) / w_bov_med
se_eta_fra <- sqrt(var_beta) / w_fra_med

# ============================================================
# 15. TABELA FINAL COMPLETA DE ELASTICIDADES E CLASSIFICAÇÃO
# ============================================================

estimativas <- c(
  e_bov_bov, e_bov_fra, e_fra_bov, e_fra_fra,
  h_bov_bov, h_bov_fra, h_fra_bov, h_fra_fra,
  eta_bov, eta_fra
)

erros_padrao <- c(
  se_e_bov_bov, se_e_bov_fra, se_e_fra_bov, se_e_fra_fra,
  se_h_bov_bov, se_h_bov_fra, se_h_fra_bov, se_h_fra_fra,
  se_eta_bov, se_eta_fra
)

# Nota: para Elasticidade-Gasto, a hipótese nula econômica relevante é H0: E_i = 1
t_stat <- c(
  estimativas[1:8] / erros_padrao[1:8],
  (eta_bov - 1) / se_eta_bov,
  (eta_fra - 1) / se_eta_fra
)

p_valor <- 2 * pt(-abs(t_stat), df = gl)

elasticidades <- data.frame(
  Tipo = c(
    rep("Marshalliana", 4),
    rep("Hicksiana", 4),
    rep("Dispêndio (H0: E=1)", 2)
  ),
  Elasticidade = c(
    "Bovina - preço próprio",
    "Bovina - preço do frango (cruzada)",
    "Frango - preço da bovina (cruzada)",
    "Frango - preço próprio",
    "Bovina - preço próprio",
    "Bovina - preço do frango (cruzada)",
    "Frango - preço da bovina (cruzada)",
    "Frango - preço próprio",
    "Bovina - gasto",
    "Frango - gasto"
  ),
  Estimativa  = round(estimativas, 4),
  Erro_Padrao = round(erros_padrao, 4),
  Estat_t     = round(t_stat, 4),
  P_Valor     = round(p_valor, 4)
)

# Classificação econômica das elasticidades
elasticidades$Classificacao <- ifelse(
  grepl("gasto", elasticidades$Elasticidade, ignore.case = TRUE),
  ifelse(elasticidades$Estimativa > 0,
         "Bem normal",
         "Bem inferior"),
  ifelse(
    grepl("preço próprio", elasticidades$Elasticidade),
    ifelse(abs(elasticidades$Estimativa) > 1,
           "Elástica",
           ifelse(abs(elasticidades$Estimativa) < 1,
                  "Inelástica",
                  "Unitária")),
    ifelse(elasticidades$Estimativa > 0,
           "Substitutos",
           ifelse(elasticidades$Estimativa < 0,
                  "Complementares",
                  "Sem efeito cruzado"))
  )
)

print(elasticidades)

# ============================================================
# 16. SIGNIFICÂNCIA DOS PARÂMETROS DO LA/AIDS (EQUAÇÃO BOVINA)
# ============================================================

resultado <- summary(modelo_aids)

tabela_coef <- as.data.frame(
  resultado$eq[[1]]$coefficients
)

# Extrair Beta bovina e Gama (sem intercepto Alfa)
tabela_significancia <- data.frame(
  Parametro = c("Beta bovina", "Gama"),
  
  Estimativa = c(
    beta_bov,
    gamma
  ),
  
  Erro_Padrao = c(
    tabela_coef["d_ln_x_real", "Std. Error"],
    tabela_coef["d_lp_bov_fra", "Std. Error"]
  ),
  
  Estatistica_t = c(
    tabela_coef["d_ln_x_real", "t value"],
    tabela_coef["d_lp_bov_fra", "t value"]
  ),
  
  P_Valor = c(
    tabela_coef["d_ln_x_real", "Pr(>|t|)"],
    tabela_coef["d_lp_bov_fra", "Pr(>|t|)"]
  )
)

tabela_significancia$Significancia <- ifelse(
  tabela_significancia$P_Valor < 0.01, "1%",
  ifelse(
    tabela_significancia$P_Valor < 0.05, "5%",
    ifelse(
      tabela_significancia$P_Valor < 0.10, "10%",
      "Não significativo"
    )
  )
)

print(tabela_significancia)

# ============================================================
# 17. SIGNIFICÂNCIA DO PARÂMETRO BETA DO FRANGO
# ============================================================

# Erro-padrão de beta bovino
se_beta_bov <- tabela_coef["d_ln_x_real", "Std. Error"]

# Recuperar beta do frango (H0: beta_frango = 0)
beta_fra      <- -beta_bov
erro_beta_fra <- se_beta_bov
t_beta_fra    <- beta_fra / erro_beta_fra
p_beta_fra    <- 2 * pt(-abs(t_beta_fra), df = gl)

significancia_frango <- data.frame(
  Parametro     = "Beta frango",
  Estimativa    = beta_fra,
  Erro_Padrao   = erro_beta_fra,
  Estatistica_t = t_beta_fra,
  P_Valor       = p_beta_fra
)

significancia_frango$Significancia <- ifelse(
  significancia_frango$P_Valor < 0.01, "1% - altamente significativa",
  ifelse(
    significancia_frango$P_Valor < 0.05, "5% - significativa",
    ifelse(
      significancia_frango$P_Valor < 0.10, "10% - fracamente significativa",
      "Não significativa"
    )
  )
)

print(significancia_frango)

# ============================================================
# 18. EXPORTAR RESULTADOS PARA EXCEL (USANDO .ENV)
# ============================================================

parametros_excel <- as.data.frame(parametros)

gama_excel <- as.data.frame(gama)
gama_excel <- cbind(
  Produto = rownames(gama_excel),
  gama_excel
)
rownames(gama_excel) <- NULL

elasticidades_excel            <- as.data.frame(elasticidades)
significancia_parametros_excel <- as.data.frame(tabela_significancia)
significancia_frango_excel     <- as.data.frame(significancia_frango)

# Criar lista de planilhas para exportação
resultados_finais <- list(
  Base_Nivel_e_Diferenca   = as.data.frame(base_sem_suino),
  Parametros               = parametros_excel,
  Matriz_Gama              = gama_excel,
  Elasticidades            = elasticidades_excel,
  Significancia_Parametros = significancia_parametros_excel,
  Significancia_Frango     = significancia_frango_excel,
  Diagnosticos_Residuos    = diagnosticos,
  Coef_Erro_Robusto_NW     = coef_robustos,
  Robustez_Indice_Precos   = robustez_indice
)

# Se as tabelas de estacionariedade e Zivot-Andrews estiverem na sessão, exporta junto
if (exists("resultados")) {
  resultados_finais <- append(
    list(Estacionariedade_Modelo = as.data.frame(resultados)),
    resultados_finais
  )
}

if (exists("resultados_zivot_andrews")) {
  resultados_finais <- append(
    list(Zivot_Andrews_Suina = as.data.frame(resultados_zivot_andrews)),
    resultados_finais
  )
}

if (exists("resultados_precos")) {
  resultados_finais <- append(
    list(Estacionariedade_Precos = as.data.frame(resultados_precos)),
    resultados_finais
  )
}

# Conferir a estrutura
str(resultados_finais)

# Ler caminho de exportação do arquivo .env
if (file.exists(".env")) {
  readRenviron(".env")
}

caminho_saida <- Sys.getenv("EXPORT_EXIT_PATH")
if (caminho_saida == "") {
  caminho_saida <- "resultados/tabelas/Resultados_Modelo_LAAIDS.xlsx"
}

# Garantir que a pasta de destino exista
dir.create(dirname(caminho_saida), recursive = TRUE, showWarnings = FALSE)

writexl::write_xlsx(
  x = resultados_finais,
  path = caminho_saida
)

cat("Arquivo exportado com sucesso:", caminho_saida, "\n")
