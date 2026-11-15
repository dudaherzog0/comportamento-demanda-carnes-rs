# ============================================================
# 01. TESTES DE ESTACIONARIDADE: ADF, PP E KPSS
# ============================================================

library(urca)
library(dplyr)

# Carregar dados caso ainda não estejam no ambiente
if (!exists("base")) {
  source("scripts/00_carregar_dados.R")
}

# 1. Selecionar variáveis
variaveis <- c("Preco_Bov", "Preco_Sui", "Preco_Fra")

# 2. Criar tabela de resultados
resultados <- data.frame()

# 3. Função dos testes
realizar_testes <- function(x, nome, diferenca) {
  
  x <- na.omit(as.numeric(x))
  
  # Definir especificação conforme transformação
  if (diferenca == "Nivel") {
    
    tipo_adf <- "trend"
    modelo_pp <- "trend"
    tipo_kpss <- "tau"
    
  } else {
    
    tipo_adf <- "drift"
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
    
    Variavel = nome,
    Diferenca = diferenca,
    
    ADF = as.numeric(adf@teststat[1]),
    ADF_5 = as.numeric(adf@cval[1, "5pct"]),
    
    PP = as.numeric(pp@teststat),
    PP_5 = as.numeric(pp@cval[1, "5pct"]),
    
    KPSS = as.numeric(kpss@teststat),
    KPSS_5 = as.numeric(kpss@cval[1, "5pct"])
    
  )
  
  return(resultado)
}

# 4. Executar os testes
for (v in variaveis) {
  
  x <- base[[v]]
  
  # Nível
  resultados <- rbind(
    resultados,
    realizar_testes(x, v, "Nivel")
  )
  
  # Primeira diferença
  resultados <- rbind(
    resultados,
    realizar_testes(diff(x), v, "Primeira diferenca")
  )
  
  # Segunda diferença
  resultados <- rbind(
    resultados,
    realizar_testes(
      diff(x, differences = 2),
      v,
      "Segunda diferenca"
    )
  )
}

# 5. Mostrar resultados
print(resultados)
