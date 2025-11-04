# Demonstração da Solução - Botão Calcular com Persistência de Dados

## Problema Original

Os dados inseridos na página "Dados do Paciente" desapareciam quando o usuário navegava entre as abas.

## Solução Implementada

### Arquitetura da Solução

```
┌─────────────────────────────────────────────────────────────┐
│                    Aplicativo Shiny                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌───────────────┐   ┌────────────────┐   ┌────────────┐  │
│  │ Aba 1:        │   │ Aba 2:         │   │ Aba 3:     │  │
│  │ Dados do      │   │ Análise de     │   │ Visualiza- │  │
│  │ Paciente      │   │ Dados          │   │ ções       │  │
│  └───────┬───────┘   └───────┬────────┘   └─────┬──────┘  │
│          │                   │                    │         │
│          └───────────────────┼────────────────────┘         │
│                              │                              │
│                    ┌─────────▼─────────┐                   │
│                    │  ReactiveValues   │                   │
│                    │  (Persistência)   │                   │
│                    └───────────────────┘                   │
│                              │                              │
│                   ┌──────────┴──────────┐                  │
│                   │  Botão "Calcular e  │                  │
│                   │  Salvar Dados"      │                  │
│                   └─────────────────────┘                  │
└─────────────────────────────────────────────────────────────┘
```

### Componentes Principais

#### 1. ReactiveValues (Armazenamento Persistente)

```r
dados_paciente <- reactiveValues(
  salvos = FALSE,           # Status: dados salvos ou não
  id = NULL,                # ID do paciente
  data_notif = NULL,        # Data de notificação
  mesorregiao = NULL,       # Mesorregião
  idade = NULL,             # Idade
  sexo = NULL,              # Sexo
  municipio = NULL,         # Município
  data_ref = NULL,          # Data de referência calculada
  historico = data.frame()  # Histórico de todos os pacientes
)
```

#### 2. Botão "Calcular e Salvar Dados"

O botão está posicionado na parte inferior do formulário e realiza:

```r
observeEvent(input$btn_calcular, {
  # 1. Validação dos campos
  req(input$id_paciente, input$data_notificacao, ...)
  
  # 2. Cálculo da data de referência
  data_ref <- input$data_notificacao - ddays(14)
  
  # 3. Salvar no reactiveValues
  dados_paciente$salvos <- TRUE
  dados_paciente$id <- input$id_paciente
  # ... outros campos
  
  # 4. Adicionar ao histórico
  dados_paciente$historico <- rbind(...)
  
  # 5. Notificação de sucesso
  showNotification("Dados salvos com sucesso!")
})
```

### Fluxo de Uso

```
┌──────────────────────────────────────────────────────────────┐
│ 1. Usuário acessa aba "Dados do Paciente"                   │
└────────────────────────┬─────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│ 2. Preenche os campos do formulário:                        │
│    - ID do Paciente                                          │
│    - Data de Notificação                                     │
│    - Mesorregião                                             │
│    - Idade                                                   │
│    - Sexo                                                    │
│    - Município                                               │
└────────────────────────┬─────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│ 3. Clica em "Calcular e Salvar Dados"                       │
│    ✓ Dados validados                                         │
│    ✓ Data de referência calculada                           │
│    ✓ Dados salvos em reactiveValues                         │
│    ✓ Notificação de sucesso exibida                         │
└────────────────────────┬─────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│ 4. Usuário navega entre as abas livremente                  │
│    ✓ Dados permanecem salvos                                │
│    ✓ Aba "Análise de Dados" mostra informações              │
│    ✓ Aba "Visualizações" mostra gráficos                    │
└──────────────────────────────────────────────────────────────┘
```

### Indicadores Visuais

#### Status dos Dados

**Antes de salvar:**
```
┌────────────────────────────────────────────────┐
│ ℹ️ Preencha os campos e clique em             │
│    'Calcular e Salvar Dados'                   │
└────────────────────────────────────────────────┘
```

**Após salvar:**
```
┌────────────────────────────────────────────────┐
│ ✓ Dados salvos com sucesso!                   │
│   Você pode navegar entre as abas.            │
└────────────────────────────────────────────────┘
```

### Validação e Segurança

1. **Validação de Campos Obrigatórios:**
   - Todos os campos devem ser preenchidos
   - Notificação de erro se campos vazios

2. **Cálculo Automático:**
   - Data de Referência = Data de Notificação - 14 dias
   - Corresponde ao período de incubação da leptospirose

3. **Persistência de Sessão:**
   - Dados mantidos durante toda a sessão do usuário
   - Possibilidade de adicionar múltiplos pacientes

### Abas do Aplicativo

#### Aba 1: Dados do Paciente
- Formulário de entrada
- Botão "Calcular e Salvar Dados"
- Status visual
- Resumo dos dados inseridos

#### Aba 2: Análise de Dados
- Tabela com dados do paciente atual
- Informações calculadas (data ref, ano, mês)
- Histórico completo de pacientes
- Aviso se não há dados salvos

#### Aba 3: Visualizações
- Gráfico: Distribuição por mesorregião
- Gráfico: Distribuição por sexo
- Gráfico: Distribuição por faixa etária
- Avisos se não há dados

#### Aba 4: Sobre
- Informações do projeto
- Instruções de uso
- Contato do autor

## Benefícios da Solução

✅ **Persistência de Dados**: Os dados não se perdem ao navegar entre abas
✅ **Validação**: Garante que todos os campos necessários estão preenchidos
✅ **Feedback Visual**: Usuário sabe quando os dados estão salvos
✅ **Cálculo Automático**: Data de referência calculada automaticamente
✅ **Histórico**: Mantém registro de todos os pacientes adicionados
✅ **Visualizações**: Gráficos dinâmicos atualizados em tempo real
✅ **Experiência do Usuário**: Interface intuitiva e informativa

## Tecnologias Utilizadas

- **Shiny**: Framework web para R
- **ReactiveValues**: Armazenamento reativo de dados
- **ggplot2**: Visualizações gráficas
- **dplyr/tidyr**: Manipulação de dados
- **DT**: Tabelas interativas
- **lubridate**: Manipulação de datas

## Como Executar

```R
# Instalar pacotes
install.packages(c("shiny", "ggplot2", "dplyr", "readr", 
                   "lubridate", "tidyr", "DT"))

# Executar aplicativo
shiny::runApp("app.R")
```

## Conclusão

A implementação do botão "Calcular e Salvar Dados" resolve completamente o problema de perda 
de dados ao navegar entre abas. A solução utiliza `reactiveValues` do Shiny para manter o 
estado dos dados durante toda a sessão, permitindo ao usuário navegar livremente entre as 
diferentes seções do aplicativo sem perder informações importantes.
