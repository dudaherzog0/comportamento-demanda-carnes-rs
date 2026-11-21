# ============================================================
# 01. TESTES DE ESTACIONARIDADE (ADF, PP, KPSS) E
#     TESTE DE QUEBRA ESTRUTURAL DE ZIVOT & ANDREWS (1992)
# ============================================================

library(urca)
library(dplyr)

# Carregar dados e variáveis calculadas (base com suíno e base_sem_suino)
source("scripts/00_carregar_dados.R")

# ============================================================
# FUNÇÃO PADRÃO PARA OS TESTES ADF, PP E KPSS
# ============================================================
realizar_testes <- function(x, nome, diferenca) {
  
  x <- na.omit(as.numeric(x))
  
  # Definir especificação conforme transformação
  if (diferenca == "Nivel") {
    
    tipo_adf  <- "trend"
    modelo_pp <- "trend"
    tipo_kpss <- "tau"
    
  } else {
    
    tipo_adf  <- "drift"
    modelo_pp <- "constant"
    tipo_kpss <- "mu"
    
  }
  
  # TESTE ADF - UMA DEFASAGEM FIXA
  adf <- ur.df(
    x,
    type = tipo_adf,
    lags = 1
  )
  
  # TESTE PHILLIPS-PERRON
  pp <- ur.pp(
    x,
    type = "Z-tau",
    model = modelo_pp,
    lags = "short"
  )
  
  # TESTE KPSS
  kpss <- ur.kpss(
    x,
    type = tipo_kpss,
    lags = "short"
  )
  
  # Organizar resultados
  resultado <- data.frame(
    
    Variavel  = nome,
    Diferenca = diferenca,
    
    ADF    = round(as.numeric(adf@teststat[1]), 4),
    ADF_5  = as.numeric(adf@cval[1, "5pct"]),
    
    PP     = round(as.numeric(pp@teststat), 4),
    PP_5   = as.numeric(pp@cval[1, "5pct"]),
    
    KPSS   = round(as.numeric(kpss@teststat), 4),
    KPSS_5 = as.numeric(kpss@cval[1, "5pct"])
    
  )
  
  return(resultado)
}

# ============================================================
# PARTE A: TESTES DE RAIZ UNITÁRIA NAS SÉRIES DE PREÇOS DAS 3 CARNES
# (BOVINA, SUÍNA E FRANGO - EM NÍVEL, 1ª E 2ª DIFERENÇA - TABELA 3 DO TCC)
# ============================================================
variaveis_precos <- c("Preco_Bov", "Preco_Sui", "Preco_Fra")
resultados_precos <- data.frame()

for (v in variaveis_precos) {
  
  x <- base[[v]]
  
  # Nível
  resultados_precos <- rbind(
    resultados_precos,
    realizar_testes(x, v, "Nivel")
  )
  
  # Primeira diferença
  resultados_precos <- rbind(
    resultados_precos,
    realizar_testes(diff(x), v, "Primeira diferenca")
  )
  
  # Segunda diferença
  resultados_precos <- rbind(
    resultados_precos,
    realizar_testes(diff(x, differences = 2), v, "Segunda diferenca")
  )
}

cat("\n--- PARTE A: TESTES ADF, PP E KPSS NAS SÉRIES DE PREÇOS (3 CARNES) ---\n")
print(resultados_precos)

# ============================================================
# PARTE B: TESTE DE QUEBRA ESTRUTURAL DE ZIVOT & ANDREWS (1992)
# APLICADO À SÉRIE DE PREÇOS DA CARNE SUÍNA (TABELA 1 DO ANEXO)
# ============================================================
preco_sui_nivel <- na.omit(as.numeric(base$Preco_Sui))
preco_sui_diff1 <- diff(preco_sui_nivel)

# Modelo "both" permite quebra no intercepto e na tendência (valores críticos: -5.57, -5.08, -4.82)
za_sui_nivel <- ur.za(preco_sui_nivel, model = "both")
za_sui_diff1 <- ur.za(preco_sui_diff1, model = "both")

resultados_zivot_andrews <- data.frame(
  Serie            = c("Preco_Sui (Em nivel)", "Preco_Sui (1a diferenca)"),
  Estatistica_ZA   = round(c(as.numeric(za_sui_nivel@teststat), as.numeric(za_sui_diff1@teststat)), 4),
  Critico_1pct     = c(as.numeric(za_sui_nivel@cval[1]), as.numeric(za_sui_diff1@cval[1])),
  Critico_5pct     = c(as.numeric(za_sui_nivel@cval[2]), as.numeric(za_sui_diff1@cval[2])),
  Critico_10pct    = c(as.numeric(za_sui_nivel@cval[3]), as.numeric(za_sui_diff1@cval[3])),
  Ponto_Quebra_Obs = c(za_sui_nivel@bpoint, za_sui_diff1@bpoint),
  Resultado        = ifelse(
    c(as.numeric(za_sui_nivel@teststat), as.numeric(za_sui_diff1@teststat)) <
      c(as.numeric(za_sui_nivel@cval[2]), as.numeric(za_sui_diff1@cval[2])),
    "Estacionaria com quebra",
    "Nao estacionaria"
  )
)

cat("\n--- PARTE B: TESTE DE ZIVOT & ANDREWS (1992) - CARNE SUÍNA ---\n")
print(resultados_zivot_andrews)

# ============================================================
# PARTE C: TESTES DE ESTACIONARIDADE NAS VARIÁVEIS DA EQUAÇÃO LA/AIDS
# (EM NÍVEL E EM 1ª DIFERENÇA)
# ============================================================
variaveis <- c(
  "w_bov",
  "w_fra",
  "ln_p_bov",
  "ln_p_fra",
  "lp_bov_fra",
  "ln_x_real"
)

resultados <- data.frame()

for (v in variaveis) {
  
  x <- base_sem_suino[[v]]
  
  # Em Nível (27 observações)
  resultados <- rbind(
    resultados,
    realizar_testes(x, v, "Nivel")
  )
  
  # Em Primeira Diferença (26 observações)
  resultados <- rbind(
    resultados,
    realizar_testes(diff(x), v, "Primeira diferenca")
  )
}

cat("\n--- PARTE C: TESTES ADF, PP E KPSS NAS VARIÁVEIS DA EQUAÇÃO LA/AIDS ---\n")
print(resultados)
