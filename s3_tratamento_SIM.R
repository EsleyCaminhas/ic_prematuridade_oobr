
library(dplyr)
library(tidyr)
library(data.table)
library(writexl)

### SIM-DO
dados_raw <- fread("f1_data-raw/dados_SIM.csv")

# Verificando porcentagem de valores ausentes das variaveis por ano 

na_summary <- dados_raw |>
  mutate(ANO = DTOBITO %% 10000) |>
  group_by(ANO) |>
  summarise(across(everything(),
                   list(na = ~round(mean(is.na(.)) * 100, 2))))

# Filtrando e tratando os dados 

dados_aux <- dados_raw |> 
  mutate(ANO = DTOBITO %% 10000,
         COD_UF_RES = as.numeric(substr(as.character(CODMUNRES), 1, 2))) |>
  filter(COD_UF_RES == 35) |>
  filter((OBITOPARTO == 1 | OBITOPARTO == 2) &
           (PESO > 500) &
           (GESTACAO != 1 | SEMAGESTAC > 22)) |>
  select(
    ANO,
    ESCMAE2010,
    GRAVIDEZ,
    QTDFILMORT,
    QTDFILVIVO
  ) |>
  mutate(
    GRAVIDEZ = case_when(
      GRAVIDEZ == 1 ~ "Única",
      GRAVIDEZ == 2 ~ "Dupla",
      GRAVIDEZ == 3 ~ "Tripla e mais",
      GRAVIDEZ == 9 ~ "Ignorado",
      is.na(GRAVIDEZ) ~ "NA"
    ),
    GRAVIDEZ = factor(GRAVIDEZ, levels = c("Única", "Dupla", "Tripla e mais", "Ignorado", "NA")),
    
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
                                               "Médio", "Superior incompleto", "Superior completo", "Ignorado", "NA"))
  )

# Fazendo as contagens e criando tabela por nível das variáveis 

dados_tabela <- dados_aux |>
  pivot_longer(
    cols = c(ESCMAE2010, GRAVIDEZ, QTDFILMORT, QTDFILVIVO),
    names_to = "VARIABLE",
    values_to = "VALUE"
  ) |>
  replace_na(list(VALUE = "NA")) |>
  count(ANO, VARIABLE, VALUE, name = "COUNT") |>
  pivot_wider(
    names_from = ANO,
    values_from = COUNT
  ) |>
  mutate(VALUE = factor(VALUE, levels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", 
                                          "Superior completo", "Única", "Dupla", "Tripla e mais", "0", "1", "2 ou mais", "Ignorado", "NA")
                        )) |>
  arrange(VARIABLE, VALUE)
  
# Salvar em Excel
write_xlsx(dados_tabela, 'f2_tabelas/tabela_obitos_por_variavel.xlsx')  

################################################################################
  
# Fazendo as contagens e criando tabela por ano
tabela_obitos_ano <- dados_raw |> 
  mutate(ANO = DTOBITO %% 10000) |>
  count(ANO, name = "NUMERO_OBITOS") |>
  arrange(ANO)

# Salvar em Excel
write_xlsx(tabela_obitos_ano, 'f2_tabelas/tabela_obitos_por_ano.xlsx')
  
  

  
  
