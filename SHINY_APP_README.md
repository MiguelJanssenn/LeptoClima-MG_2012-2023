# Aplicativo Shiny - Análise de Leptospirose MG

## Descrição

Este aplicativo Shiny foi desenvolvido para resolver o problema de perda de dados ao navegar entre abas. 
Implementa um sistema de persistência de dados do paciente através de um botão "Calcular e Salvar Dados".

## Problema Resolvido

**Problema Original:** Os dados inseridos na página "Dados do Paciente" desapareciam quando o usuário 
navegava de uma aba para outra e voltava.

**Solução Implementada:** 
- Adicionado botão "Calcular e Salvar Dados" que armazena os valores em `reactiveValues`
- Os dados permanecem salvos durante toda a sessão do aplicativo
- Possibilita navegação livre entre as abas sem perda de dados
- Calcula automaticamente a data de referência (período de incubação de 14 dias)

## Funcionalidades

### 1. Aba "Dados do Paciente"
- Formulário completo para entrada de dados do paciente
- Campos:
  - ID do Paciente
  - Data de Notificação
  - Mesorregião (12 opções de MG)
  - Idade
  - Sexo
  - Município de Residência
- Botão "Calcular e Salvar Dados" para persistência
- Indicador visual de status dos dados (salvos ou não salvos)
- Resumo dos dados inseridos
- Cálculo automático da data de referência

### 2. Aba "Análise de Dados"
- Visualização dos dados salvos do paciente atual
- Tabela de informações calculadas
- Histórico completo de todos os pacientes cadastrados
- Aviso quando não há dados salvos

### 3. Aba "Visualizações"
- Gráficos dinâmicos:
  - Distribuição por mesorregião
  - Distribuição por sexo
  - Distribuição por faixa etária
- Gráficos atualizados automaticamente conforme dados são adicionados

### 4. Aba "Sobre"
- Informações sobre o projeto
- Instruções de uso
- Dados de contato do autor e orientadora

## Como Usar

1. **Instalar R e pacotes necessários:**
```R
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "lubridate", "tidyr", "DT"))
```

2. **Executar o aplicativo:**
```R
# No diretório do projeto
shiny::runApp("app.R")
```

Ou através do RStudio:
- Abrir o arquivo `app.R`
- Clicar em "Run App"

3. **Usar o aplicativo:**
   - Acesse a aba "Dados do Paciente"
   - Preencha todos os campos do formulário
   - Clique em "Calcular e Salvar Dados"
   - Navegue livremente entre as abas - os dados estarão preservados
   - Visualize análises e gráficos nas outras abas

## Estrutura Técnica

### ReactiveValues
O aplicativo usa `reactiveValues` para armazenar dados persistentes:
- `salvos`: indicador booleano se há dados salvos
- `id`, `data_notif`, `mesorregiao`, `idade`, `sexo`, `municipio`: dados do paciente atual
- `data_ref`: data de referência calculada
- `historico`: dataframe com todos os pacientes cadastrados

### Validação
- Verificação de campos obrigatórios antes de salvar
- Notificações visuais de sucesso ou erro
- Alertas informativos quando não há dados

### Cálculo Automático
- Data de referência = Data de notificação - 14 dias (período de incubação)
- Formato de data compatível com análises epidemiológicas

## Requisitos

- R (versão 4.0 ou superior)
- Pacotes R:
  - shiny
  - ggplot2
  - dplyr
  - readr
  - lubridate
  - tidyr
  - DT

## Arquivos do Projeto

- `app.R`: Aplicativo Shiny principal com UI e servidor
- `Scripts/Script-principal.R`: Script original de análise estatística
- `Dados/`: Dados brutos (SINAN, BDMEP, IBGE)
- `Resultados/`: Saídas das análises

## Autor

- **Miguel Antonio Janssen** - d202120265@uftm.edu.br
- **Orientadora:** Profa. Dra. Ana Paula Fernandes - anapaula.fernandes@uftm.edu.br

## Licença

Este projeto é parte de um estudo acadêmico sobre correlação entre precipitação e incidência 
de leptospirose em Minas Gerais (2012-2023).
