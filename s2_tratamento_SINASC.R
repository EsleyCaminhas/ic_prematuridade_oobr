
library(dplyr)
library(tidyr)
library(data.table)
library(writexl)

### SINASC
dados_raw <- fread("f1_data-raw/dados_SINASC.csv")

# Verificando porcentagem de NA's
na_summary <- dados_raw |>
  mutate(ano = DTNASC %% 10000) |>
  group_by(ano) |>
  summarise(across(everything(),
                   list(na = ~round(mean(is.na(.)) * 100, 2))))

# Filtrando e tratando os dados 
dados_aux <- dados_raw |>
  mutate(
    
    # STTRABPART (Trabalho de parto induzido?)
    STTRABPART = case_when(
      STTRABPART == 1 ~ "Sim",
      STTRABPART == 2 ~ "Não",
      STTRABPART == 3 ~ "Não se aplica",
      STTRABPART == 9 ~ "Ignorado",
      is.na(STTRABPART) ~ "NA"
    ),
    
    # Tratamento da variável STCESPARTO (Cesárea antes do trabalho de parto?)
    STCESPARTO = case_when(
      STCESPARTO == 1 ~ "Sim",
      STCESPARTO == 2 ~ "Não",
      STCESPARTO == 3 ~ "Não se aplica",
      STCESPARTO == 9 ~ "Ignorado",
      is.na(STCESPARTO) ~ "NA"
    ),
    
    RACACORMAE = case_when(
      RACACORMAE == 1 ~ "Branca",
      RACACORMAE == 2 ~ "Preta", 
      RACACORMAE == 3 ~ "Amarela",
      RACACORMAE == 4 ~ "Parda",
      RACACORMAE == 5 ~ "Indígena",
      RACACORMAE == 9 ~ "Ignorado",
      is.na(RACACORMAE) ~ "NA"
    ),
    
    ESCMAE2010 = case_when(
      ESCMAE2010 == 0 ~ "Sem escolaridade",
      ESCMAE2010 == 1 ~ "Fundamental I",
      ESCMAE2010 == 2 ~ "Fundamental II",
      ESCMAE2010 == 3 ~ "Médio",
      ESCMAE2010 == 4 ~ "Superior incompleto",
      ESCMAE2010 == 5 ~ "Superior completo",
      ESCMAE2010 == 9 ~ "Ignorado",
      is.na(ESCMAE2010) ~ "NA"
    ),
    
    ESCMAE2010 = factor(ESCMAE2010, levels = c("Sem escolaridade", "Fundamental I", "Fundamental II", 
                                               "Médio", "Superior incompleto", "Superior completo", "Ignorado", "NA")),
    
    ESTCIVMAE = case_when(
      ESTCIVMAE %in% c(1,3,4) ~ "Solteira/Viúva/divorciada",
      ESTCIVMAE %in% c(2,5) ~ "Casada/União estável",
      ESTCIVMAE == 9 ~ "Ignorado",
      is.na(ESTCIVMAE) ~ "NA"
    ),
    
    GRAVIDEZ = case_when(
      GRAVIDEZ == 1 ~ "Única",
      GRAVIDEZ == 2 ~ "Dupla",
      GRAVIDEZ == 3 ~ "Tripla e mais",
      GRAVIDEZ == 9 ~ "Ignorado",
      is.na(GRAVIDEZ) ~ "NA"
    ),
    GRAVIDEZ = factor(GRAVIDEZ, levels = c("Única", "Dupla", "Tripla e mais", "Ignorado", "NA")),
    
    QTDPARTCES = case_when(
      QTDPARTCES == 0 ~ "0",
      QTDPARTCES == 1 ~ "1",
      QTDPARTCES > 1 & QTDPARTCES != 99 ~ "2 ou mais",
      QTDPARTCES == 99 ~ "Ignorado",
      is.na(QTDPARTCES) ~ "NA"
    ),
    QTDPARTCES = factor(QTDPARTCES, levels = c("0", "1", "2 ou mais", "Ignorado", "NA")),
    
    QTDFILMORT = case_when(
      QTDFILMORT == 0 ~ "0",
      QTDFILMORT == 1 ~ "1",
      QTDFILMORT > 1 & QTDFILMORT != 99 ~ "2 ou mais",
      QTDFILMORT == 99 ~ "Ignorado",
      is.na(QTDFILMORT) ~ "NA"
    ),
    QTDFILMORT = factor(QTDFILMORT, levels = c("0", "1", "2 ou mais", "Ignorado", "NA")),
    
    QTDFILVIVO = case_when(
      QTDFILVIVO == 0 ~ "0",
      QTDFILVIVO == 1 ~ "1",
      QTDFILVIVO > 1 & QTDFILVIVO != 99 ~ "2 ou mais",
      QTDFILVIVO == 99 ~ "Ignorado",
      is.na(QTDFILVIVO) ~ "NA"
    ),
    QTDFILVIVO = factor(QTDFILVIVO, levels = c("0", "1", "2 ou mais", "Ignorado", "NA")),
    

    N_ADEQUADO = case_when(
      SEMAGESTAC >= 22 & SEMAGESTAC <= 25 ~ 2,
      SEMAGESTAC >= 26 & SEMAGESTAC <= 29 ~ 3,
      SEMAGESTAC >= 30 & SEMAGESTAC <= 33 ~ 4,
      SEMAGESTAC >= 34 & SEMAGESTAC <= 35 ~ 5,
      SEMAGESTAC >= 36 & SEMAGESTAC <= 37 ~ 6,
      SEMAGESTAC >= 38 & SEMAGESTAC <= 39 ~ 7,
      SEMAGESTAC >= 40 ~ 8,
      SEMAGESTAC < 22 ~ NA
    ),
    
    N_PRENAT = case_when(
      CONSPRENAT >= N_ADEQUADO ~ "Adequado",
      CONSPRENAT < N_ADEQUADO ~ "Inadequado",
      is.na(CONSPRENAT) | is.na(N_ADEQUADO) ~ "NA"
    ),
    
    ############################################################################
    
    ANO = DTNASC %% 10000,
    
    NV = 1,
    
    # Prematuro
    PP = case_when(
      ((SEMAGESTAC %in% 22:36 & PESO > 500)) ~ 1,
        is.na(SEMAGESTAC) | is.na(PESO) ~ NA,
        TRUE ~ 0
      ),
    
    # PP = case_when(
    #   ((SEMAGESTAC %in% 22:36 & PESO > 500) | (GESTACAO %in% 2:4 & PESO > 500)) ~ 1,
    #   is.na(SEMAGESTAC) & is.na(GESTACAO) ~ NA,
    #   TRUE ~ 0
    # ),
    
    # Prematuro espontâneo
    PPesp = case_when(
      # Prematuro sem trabalho de parto induzido e sem cesárea antes do trabalho de parto iniciar
      PP == 1 & (STTRABPART == "Não" & STCESPARTO == "Não") ~ 1,
      is.na(PP) | (is.na(STTRABPART) & STCESPARTO == "Não") | (STTRABPART == "Não" & is.na(STCESPARTO)) ~ NA,
      TRUE ~ 0
    ),
    
    PPesp_32_37 = case_when(
      PPesp == 1 & SEMAGESTAC %in% 32:36 ~ 1,
      is.na(PPesp) | is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    PPesp_28_31 = case_when(
      PPesp == 1 & SEMAGESTAC %in% 28:31 ~ 1,
      is.na(PPesp) | is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    PPesp_28_menos = case_when(
      PPesp == 1 & SEMAGESTAC %in% 22:27 ~ 1,
      is.na(PPesp) | is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    # Prematuro eletivo
    PPel = case_when(
      # Prematuro com trabalho de parto induzido ou com cesárea antes do trabalho de parto iniciar
      PP == 1 & (STTRABPART == "Sim" | STCESPARTO == "Sim") ~ 1,
      is.na(PP) | (is.na(STTRABPART) & STCESPARTO == "Não") | (STTRABPART == "Não" & is.na(STCESPARTO)) ~ NA,
      TRUE ~ 0
    ),
    
    PPel_32_37 = case_when(
      PPel == 1 & SEMAGESTAC %in% 32:36 ~ 1,
      is.na(PPel) | is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    PPel_28_31 = case_when(
      PPel == 1 & SEMAGESTAC %in% 28:31 ~ 1,
      is.na(PPel) | is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    PPel_28_menos = case_when(
      PPel == 1 & SEMAGESTAC %in% 22:27 ~ 1,
      is.na(PPel) | is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    Termos_precoce = case_when(
      SEMAGESTAC %in% 37:38 ~ 1,
      is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    PPesp_termos_precoce = case_when(
      SEMAGESTAC %in% 37:38 & (STTRABPART == "Não" & STCESPARTO == "Não") ~ 1,
      is.na(SEMAGESTAC) | (is.na(STTRABPART) & STCESPARTO == "Não") | (STTRABPART == "Não" & is.na(STCESPARTO)) ~ NA,
      TRUE ~ 0),
    
    PPel_termos_precoce = case_when(
      SEMAGESTAC %in% 37:38 & (STTRABPART == "Sim" | STCESPARTO == "Sim") ~ 1,
      is.na(SEMAGESTAC) | (is.na(STTRABPART) & STCESPARTO == "Não") | (STTRABPART == "Não" & is.na(STCESPARTO)) ~ NA,
      TRUE ~ 0),
    
    Termos =  case_when(
      SEMAGESTAC > 37 ~ 1,
      is.na(SEMAGESTAC) ~ NA,
      TRUE ~ 0),
    
    IDADEMAE_menor_igual_15 = case_when(
      IDADEMAE <= 15 ~ 1,
      is.na(IDADEMAE) ~ NA,
      TRUE ~ 0),
    
    IDADEMAE_maior_igual_35 = case_when(
      IDADEMAE >= 35 ~ 1,
      is.na(IDADEMAE) ~ NA,
      TRUE ~ 0)
    
    )


library(dplyr)
library(writexl)

criar_aba_unica_por_ano <- function(ano) {
  
  dados_ano <- dados_aux |> 
    filter(ANO == ano)
  
  # TABELA POR RAÇA
  tabela_raca <- dados_ano |>
    group_by(RACACORMAE) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_racas <- c("Branca", "Preta", "Parda", "Amarela", "Indígena", "Ignorado", "NA")
  
  tabela_raca_filtrada <- tabela_raca |>
    mutate(
      RACACORMAE = factor(RACACORMAE, levels = ordem_racas)
    ) |>
    arrange(RACACORMAE) |>
    rename(`Categoria` = RACACORMAE) |>
    mutate(Tipo = "Raça")
  
  # TABELA POR ESCOLARIDADE
  tabela_escolaridade <- dados_ano |>
    group_by(ESCMAE2010) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_escolaridade <- c("Sem escolaridade", "Fundamental I", "Fundamental II", 
                          "Médio", "Superior incompleto", "Superior completo", "Ignorado", "NA")
  
  tabela_escolaridade_filtrada <- tabela_escolaridade |>
    mutate(
      ESCMAE2010 = factor(ESCMAE2010, levels = ordem_escolaridade)
    ) |>
    arrange(ESCMAE2010) |>
    rename(`Categoria` = ESCMAE2010) |>
    mutate(Tipo = "Escolaridade")
  
  # TABELA POR SITUAÇÃO CONJUGAL
  tabela_estcivil <- dados_ano |>
    group_by(ESTCIVMAE) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),3),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),3),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_estcivil <- c("Solteira/Viúva/divorciada", "Casada/União estável", "Ignorado", "NA")
  
  tabela_estcivil_filtrada <- tabela_estcivil |>
    mutate(
      ESTCIVMAE = factor(ESTCIVMAE, levels = ordem_estcivil)
    ) |>
    arrange(ESTCIVMAE) |>
    rename(`Categoria` = ESTCIVMAE) |>
    mutate(Tipo = "Situação conjugal")
  
  # TABELA POR MÊS DO PRÉ-NATAL
  tabela_mesprenat <- dados_ano |>
    group_by(MESPRENAT) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  tabela_mesprenat$MESPRENAT[is.na(tabela_mesprenat$MESPRENAT)] <- "NA"
  
  ordem_mesprenat <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "99", "NA")
  
  tabela_mesprenat_filtrada <- tabela_mesprenat |>
    mutate(
      MESPRENAT = factor(MESPRENAT, levels = ordem_mesprenat)
    ) |>
    arrange(MESPRENAT) |>
    rename(`Categoria` = MESPRENAT) |>
    mutate(Tipo = "Mês do pré-natal")
  
  
  # TABELA POR TIPO DE GRAVIDEZ
  tabela_gravidez <- dados_ano |>
    group_by(GRAVIDEZ) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_gravidez <- c("Única", "Dupla", "Tripla e mais", "Ignorado", "NA")
  
  tabela_gravidez_filtrada <- tabela_gravidez |>
    mutate(
      GRAVIDEZ = factor(GRAVIDEZ, levels = ordem_gravidez)
    ) |>
    arrange(GRAVIDEZ) |>
    rename(`Categoria` = GRAVIDEZ) |>
    mutate(Tipo = "Tipo de gravidez")
  
  # TABELA POR ADEQUAÇÃO DO PRÉ-NATAL
  tabela_prenat <- dados_ano |>
    group_by(N_PRENAT) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_prenat <- c("Adequado", "Inadequado", "NA")
  
  tabela_prenat_filtrada <- tabela_prenat |>
    mutate(
      N_PRENAT = factor(N_PRENAT, levels = ordem_prenat)
    ) |>
    arrange(N_PRENAT) |>
    rename(`Categoria` = N_PRENAT) |>
    mutate(Tipo = "Adequação do pré-natal")
  
  # TABELA POR NÚMERO DE CESÁREAS ANTERIORES
  tabela_cesarea <- dados_ano |>
    group_by(QTDPARTCES) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_cesarea <- c("0", "1", "2 ou mais", "Ignorado", "NA")
  
  tabela_cesarea_filtrada <- tabela_cesarea |>
    mutate(
      QTDPARTCES = factor(QTDPARTCES, levels = ordem_cesarea)
    ) |>
    arrange(QTDPARTCES) |>
    rename(`Categoria` = QTDPARTCES) |>
    mutate(Tipo = "Número de cesáreas anteriores")
  
  # TABELA POR NÚMERO DE ÓBITOS FETAL/ABORTOS
  tabela_obitos <- dados_ano |>
    group_by(QTDFILMORT) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_obitos <- c("0", "1", "2 ou mais", "Ignorado", "NA")
  
  tabela_obitos_filtrada <- tabela_obitos |>
    mutate(
      QTDFILMORT = factor(QTDFILMORT, levels = ordem_obitos)
    ) |>
    arrange(QTDFILMORT) |>
    rename(`Categoria` = QTDFILMORT) |>
    mutate(Tipo = "Número de óbitos fetais/abortos")
  
  # TABELA POR NÚMERO DE FILHOS VIVOS
  tabela_filhosvivos <- dados_ano |>
    group_by(QTDFILVIVO) |>
    summarise(
      `Nascidos vivos` = sum(NV, na.rm = TRUE),
      `Partos prematuros` = sum(PP, na.rm = TRUE),
      
      `Pindicado` = sum(PPel, na.rm = TRUE),
      `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
      `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
      `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
      `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
      `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
      `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
      
      # `Pindicado` = sum(PPel, na.rm = TRUE),
      # `P prematuro Indicado "Prematuro moderado" 32 a 37 semanas` = round(sum(PPel_32_37, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPel_28_31, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `P prematuro Indicado "Extremamente prematuro" < 28 semanas` = round(sum(PPel_28_menos, na.rm = TRUE)/sum(PPel, na.rm = TRUE),2),
      # `Pespontâneo` = sum(PPesp, na.rm = TRUE),
      # `P prematuro espontâneo "tardio" 32 a 37 semanas` = round(sum(PPesp_32_37, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = round(sum(PPesp_28_31, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      # `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = round(sum(PPesp_28_menos, na.rm = TRUE)/sum(PPesp, na.rm = TRUE),2),
      
      `Partos termos precoces (37+1 - 38+6)` = sum(Termos_precoce, na.rm = TRUE),
      `Partos termos precoces Indicados (37+1 - 38+6)` = sum(PPel_termos_precoce, na.rm = TRUE),
      `Partos termos precoces espontâneos (37+1 - 38+6)` = sum(PPesp_termos_precoce, na.rm = TRUE),
      `Partos termo (controle) > 37 +1 d` = sum(Termos, na.rm = TRUE)
    ) |>
    ungroup()
  
  ordem_filhosvivos <- c("0", "1", "2 ou mais", "Ignorado", "NA")
  
  tabela_filhosvivos_filtrada <- tabela_filhosvivos |>
    mutate(
      QTDFILVIVO = factor(QTDFILVIVO, levels = ordem_filhosvivos)
    ) |>
    arrange(QTDFILVIVO) |>
    rename(`Categoria` = QTDFILVIVO) |>
    mutate(Tipo = "Número de filhos vivos anteriores")
  
  # Combinar todas as tabelas
  tabela_combinada <- bind_rows(
    tabela_raca_filtrada,
    data.frame(NA),
    tabela_escolaridade_filtrada,
    data.frame(NA),
    tabela_estcivil_filtrada,
    data.frame(NA),
    tabela_mesprenat_filtrada,
    data.frame(NA),
    tabela_gravidez_filtrada,
    data.frame(NA),
    tabela_prenat_filtrada,
    data.frame(NA),
    tabela_cesarea_filtrada,
    data.frame(NA),
    tabela_obitos_filtrada,
    data.frame(NA),
    tabela_filhosvivos_filtrada
    ) |>
    select(Tipo, Categoria, everything())
  
  return(tabela_combinada)
}

# Definir anos e aplicar a função
anos <- 2012:2023
lista_abas_completas <- lapply(anos, criar_aba_unica_por_ano)
names(lista_abas_completas) <- 2012:2023

# Salvar em Excel
write_xlsx(lista_abas_completas, "f2_tabelas/tabela_prematuridade_por_ano.xlsx")

################################################################################

# Criar tabela resumo por ano
tabela_resumo_ano <- dados_aux |>
  filter(ANO >= 2012 & ANO <= 2023) |>
  group_by(ANO) |>
  summarise(
    # Total de nascidos vivos por residência
    `Total de nascidos vivos (NV) por residência` = sum(NV, na.rm = TRUE),
    
    # Total de partos prematuros
    `Total de partos prematuros (PP)` = sum(PP, na.rm = TRUE),
    
    # Total de PP eletivos
    `Total de PP eletivos (PPel)` = sum(PPel, na.rm = TRUE),
    
    # PP eletivos por categoria
    `P prematuro eletivo "Prematuro moderado" 32 a 37 semanas` = sum(PPel_32_37, na.rm = TRUE),
    `P prematuro eletivo "Muito prematuro" 28 semanas a 31+6d` = sum(PPel_28_31, na.rm = TRUE),
    `P prematuro eletivo "Extremamente prematuro" < 28 semanas` = sum(PPel_28_menos, na.rm = TRUE),
    
    # Total de PP espontâneos
    `Total de PP espontâneos (PPes)` = sum(PPesp, na.rm = TRUE),
    
    # PP espontâneos por categoria
    `P prematuro espontâneo "tardio" 32 a 37 semanas` = sum(PPesp_32_37, na.rm = TRUE),
    `P prematuro espontâneo "Muito prematuro" 28 semanas a 31+6d` = sum(PPesp_28_31, na.rm = TRUE),
    `P prematuro espontâneo "Extremamente prematuro" < 28 semanas` = sum(PPesp_28_menos, na.rm = TRUE),
    
    # Partos termo precoce
    `Total de partos termo precoce (PTP)` = sum(Termos_precoce, na.rm = TRUE),
    `Total de partos termo precoce eletivos (PTPel)` = sum(PPel_termos_precoce, na.rm = TRUE),
    `Total de partos termo precoce espontâneos (PTPes)` = sum(PPesp_termos_precoce, na.rm = TRUE),
    
    
    # Idade materna
    `Média de idade materna` = round(mean(IDADEMAE, na.rm = TRUE),2),
    `Idade materna com  <= 15 anos` = sum(IDADEMAE_menor_igual_15, na.rm = TRUE),
    `Idade materna com  >= 35 anos` = sum(IDADEMAE_maior_igual_35, na.rm = TRUE)
    
  ) |>
  ungroup() |>
  arrange(ANO)

# Salvar em Excel
write_xlsx(tabela_resumo_ano, "f2_tabelas/tabela_resumo_prematuridade.xlsx")











  









