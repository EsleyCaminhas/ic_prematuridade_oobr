# IC Prematuridade OOBR

Análise de dados sobre prematuridade no Brasil usando bases SINASC e SIM.

## Estrutura

- **`f1_data-raw/`** - Dados brutos
- **`f2_tabelas/`** - Tabelas finais
- **`f3_outros/`** - Arquivos auxiliares

## Scripts

1. **`s1_extracao_SINASC_SIM.R`** - Baixa os dados brutos
2. **`s2_tratamento_SINASC.R`** - Limpa dados SINASC e cria tabelas
3. **`s3_tratamento_SIM.R`** - Limpa dados SIM e cria tabelas
