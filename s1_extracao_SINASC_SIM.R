
# install.packages("remotes")
# remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus) 
library(data.table)

### Extração SINASC

dados <- fetch_datasus(
           year_start = 2012,
           year_end = 2023,
           uf = "SP", # Mães com UF de residência == "SP"
           information_system = "SINASC",
           vars = c(
            
             "DTNASC", # Data de nascimento
             
             "CODMUNNASC", # Código IBGE do município de nascimento
             "CODMUNRES", # Código IBGE do município de residência
             
             "IDADEMAE", # Idade da mãe
             "RACACORMAE", # Tipo de raça e cor da mãe
             "ESTCIVMAE", # Situação conjugal da mãe
             "ESCMAE2010", # Escolaridade 
             
             "QTDFILVIVO", #  Número de filhos vivos
             "QTDFILMORT", # Número de perdas fetais e abortos
             
             "PESO",
             
             "GESTACAO",
             
             "SEMAGESTAC", # Número de semanas de gestação
             
             "MESPRENAT", # Mês de gestação em que iniciou o pré‐natal
             "CONSPRENAT", # Número de consultas pré‐natal
             
             "GRAVIDEZ", # Tipo de gravidez
             
             "QTDPARTCES", # Número de partos cesáreos
             
             "STTRABPART", # Trabalho de parto induzido? 
             "STCESPARTO" # Cesárea ocorreu antes do trabalho de parto iniciar?
             
           )
          )

fwrite(dados, "f1_data-raw/dados_SINASC.csv")

### Extração SIM

dados <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  information_system = "SIM-DOFET",
  vars = c(
    
    "DTOBITO", # Data do óbito
    
    "CODMUNRES", # Código do município de residência
    "CODMUNOCOR", # Código do município da ocorrência
    
    "GESTACAO", # Semanas de gestação (intervalar)
    "SEMAGESTAC", # Semanas de gestação
    
    "OBITOPARTO", # Como foi a morte em relação ao parto
    "PESO", # Peso ao nascer em gramas 
    
    "ESCMAE", # Escolaridade em anos
    "ESCMAE2010", # Escolaridade 2010
    
    "GRAVIDEZ", # Tipo de gravidez
    "QTDFILVIVO", # Número de filhos vivos
    "QTDFILMORT" # Número de filhos mortos
    
  )
 )

fwrite(dados, "data-raw/dados_SIM.csv")









