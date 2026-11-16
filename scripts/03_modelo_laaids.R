# ============================================================
# 03. SUR E LA/AIDS EM NÍVEL
# ============================================================

library(dplyr)
library(systemfit)
library(writexl)

# Carregar dados caso ainda não estejam no ambiente
if (!exists("base_sem_suino")) {
  source("scripts/00_carregar_dados.R")
}

# 1. PREPARAR A BASE
dados <- base_sem_suino %>%
  arrange(Ano_Mes) %>%
  mutate(
    # Participações orçamentárias
    w_bov = Part_Bov,
    w_fra = Part_Fra,
    
    # Logaritmos dos preços
    ln_p_bov = log(Preco_Bov),
    ln_p_fra = log(Preco_Fra),
    
    # Índice de preços de Stone
    ln_stone = w_bov * ln_p_bov +
      w_fra * ln_p_fra,
    
    # Despesa real
    ln_x_real = log(Desp_Total) - ln_stone,
    
    # Preço relativo
    lp_bov_fra = ln_p_bov - ln_p_fra
  ) %>%
  na.omit()

# 2. ESPECIFICAR A EQUAÇÃO INDEPENDENTE
eq_bov <- w_bov ~
  lp_bov_fra +
  ln_x_real

sistema_aids <- list(
  bovina = eq_bov
)

# 3. ESTIMAR O MODELO
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
# 5. RECUPERAR A PARTICIPAÇÃO DO FRANGO
# ============================================================

dados$w_fra_estimado <- 1 - fitted(modelo_aids$eq[[1]])

# ============================================================
# 6. VERIFICAR A RESTRIÇÃO ADDING-UP
# ============================================================

summary(
  dados$w_bov +
    dados$w_fra_estimado
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
summary(dados$w_bov + dados$w_fra)
summary(
  fitted(modelo_aids$eq[[1]]) +
    dados$w_fra_estimado
)

# ============================================================
# 10. ESTIMAR OS PARÂMETROS ALFA, BETA E GAMA
# ============================================================

# Extrair coeficientes
coeficientes <- coef(modelo_aids)

alpha_bov <- unname(coeficientes["bovina_(Intercept)"])
beta_bov  <- unname(coeficientes["bovina_ln_x_real"])
gamma     <- unname(coeficientes["bovina_lp_bov_fra"])

# Recuperar parâmetros do frango pelas restrições
alpha_fra <- 1 - alpha_bov
beta_fra  <- -beta_bov

# Matriz de parâmetros gama
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

# Tabela dos parâmetros
parametros <- data.frame(
  Parametro = c(
    "Alfa bovina",
    "Alfa frango",
    "Beta bovina",
    "Beta frango"
  ),
  Estimativa = c(
    alpha_bov,
    alpha_fra,
    beta_bov,
    beta_fra
  )
)

print(parametros)
print(gama)

# ============================================================
# 11. ELASTICIDADES-GASTO DA DEMANDA (EQ. 18)
# ============================================================

# Participações orçamentárias médias
w_bov_med <- mean(dados$w_bov)
w_fra_med <- mean(dados$w_fra)

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

V <- vcov(modelo_aids)
var_gamma <- V["bovina_lp_bov_fra", "bovina_lp_bov_fra"]
var_beta  <- V["bovina_ln_x_real", "bovina_ln_x_real"]
cov_gb    <- V["bovina_lp_bov_fra", "bovina_ln_x_real"]
gl        <- df.residual(modelo_aids$eq[[1]])

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
# 15. TABELA FINAL COMPLETA DE ELASTICIDADES
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

print(elasticidades)


# ============================================================
# 14. CLASSIFICAR AS ELASTICIDADES
# ============================================================

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
# SIGNIFICÂNCIA DOS PARÂMETROS DO LA/AIDS (BOVINA)
# ============================================================

resultado <- summary(modelo_aids)

tabela_coef <- as.data.frame(
  resultado$eq[[1]]$coefficients
)

# Extrair cada parâmetro pela identificação da linha
tabela_significancia <- data.frame(
  Parametro = c("Alfa bovina", "Beta bovina", "Gama"),
  
  Estimativa = c(
    alpha_bov,
    beta_bov,
    gamma
  ),
  
  Erro_Padrao = c(
    tabela_coef["(Intercept)", "Std. Error"],
    tabela_coef["ln_x_real", "Std. Error"],
    tabela_coef["lp_bov_fra", "Std. Error"]
  ),
  
  Estatistica_t = c(
    tabela_coef["(Intercept)", "t value"],
    tabela_coef["ln_x_real", "t value"],
    tabela_coef["lp_bov_fra", "t value"]
  ),
  
  P_Valor = c(
    tabela_coef["(Intercept)", "Pr(>|t|)"],
    tabela_coef["ln_x_real", "Pr(>|t|)"],
    tabela_coef["lp_bov_fra", "Pr(>|t|)"]
  )
)

# Classificação
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
# SIGNIFICÂNCIA DOS PARÂMETROS ALFA E BETA DO FRANGO
# ============================================================

# Erros-padrão dos parâmetros bovinos
se_alpha_bov <- tabela_coef["(Intercept)", "Std. Error"]
se_beta_bov  <- tabela_coef["ln_x_real", "Std. Error"]

# Recuperar os parâmetros do frango
alpha_fra <- 1 - alpha_bov
beta_fra  <- -beta_bov

# Graus de liberdade residuais
gl <- nrow(dados) - length(coef(modelo_aids))

# ------------------------------------------------------------
# TESTE DE SIGNIFICÂNCIA DE ALFA DO FRANGO
# H0: alfa_frango = 0
# Equivalente a: alfa_bovina = 1
# ------------------------------------------------------------

erro_alpha_fra <- se_alpha_bov

t_alpha_fra <- (alpha_fra - 0) / erro_alpha_fra

p_alpha_fra <- 2 * pt(
  -abs(t_alpha_fra),
  df = gl
)

# ------------------------------------------------------------
# TESTE DE SIGNIFICÂNCIA DE BETA DO FRANGO
# H0: beta_frango = 0
# ------------------------------------------------------------

erro_beta_fra <- se_beta_bov

t_beta_fra <- beta_fra / erro_beta_fra

p_beta_fra <- 2 * pt(
  -abs(t_beta_fra),
  df = gl
)

# ------------------------------------------------------------
# TABELA FINAL DE SIGNIFICÂNCIA DO FRANGO
# ------------------------------------------------------------

significancia_frango <- data.frame(
  Parametro = c("Alfa frango", "Beta frango"),
  Estimativa = c(alpha_fra, beta_fra),
  Erro_Padrao = c(erro_alpha_fra, erro_beta_fra),
  Estatistica_t = c(t_alpha_fra, t_beta_fra),
  P_Valor = c(p_alpha_fra, p_beta_fra)
)

# Classificação da significância
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
# 15. EXPORTAR RESULTADOS PARA EXCEL (USANDO .ENV)
# ============================================================

# Garantir que todos os objetos sejam data frames
parametros_excel <- as.data.frame(parametros)

gama_excel <- as.data.frame(gama)
gama_excel <- cbind(
  Produto = rownames(gama_excel),
  gama_excel
)
rownames(gama_excel) <- NULL

elasticidades_excel <- as.data.frame(elasticidades)

significancia_parametros_excel <- as.data.frame(
  tabela_significancia
)

significancia_frango_excel <- as.data.frame(
  significancia_frango
)

# Criar lista de planilhas
resultados_finais <- list(
  Parametros = parametros_excel,
  Matriz_Gama = gama_excel,
  Elasticidades = elasticidades_excel,
  Significancia_Parametros = significancia_parametros_excel,
  Significancia_Frango = significancia_frango_excel
)

# Caso exista o objeto resultado_elasticidades na sessão, inclui também
if (exists("resultado_elasticidades")) {
  resultados_finais$Significancia_Elasticidades <- as.data.frame(resultado_elasticidades)
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
