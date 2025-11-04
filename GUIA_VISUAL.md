# Guia Visual do Aplicativo Shiny - LeptoClima MG

## Interface do Usuário

### Aba 1: Dados do Paciente

```
╔════════════════════════════════════════════════════════════════════════════╗
║  Análise Leptospirose - MG (2012-2023)                                    ║
╠════════════════════════════════════════════════════════════════════════════╣
║ [Dados do Paciente] [Análise de Dados] [Visualizações] [Sobre]           ║
╠════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Entrada de Dados do Paciente                                             ║
║  ════════════════════════════════════════════════════════════════════     ║
║                                                                            ║
║  ┌──────────────────────────┐  ┌──────────────────────────────────────┐  ║
║  │ Informações do Paciente  │  │ Status dos Dados                     │  ║
║  │                          │  │                                      │  ║
║  │ ID do Paciente:          │  │  ℹ️ Preencha os campos e clique em  │  ║
║  │ [_________________]      │  │     'Calcular e Salvar Dados'       │  ║
║  │                          │  │                                      │  ║
║  │ Data de Notificação:     │  │ ────────────────────────────────────│  ║
║  │ [dd/mm/yyyy] 📅          │  │                                      │  ║
║  │                          │  │ Resumo dos Dados Inseridos:         │  ║
║  │ Mesorregião:             │  │                                      │  ║
║  │ [Selecione...     ▼]     │  │ Nenhum dado salvo ainda.            │  ║
║  │                          │  │                                      │  ║
║  │ Idade:                   │  │                                      │  ║
║  │ [0        ] anos         │  │ Data de Referência:                 │  ║
║  │                          │  │ (Data de Notificação - 14 dias)     │  ║
║  │ Sexo:                    │  │                                      │  ║
║  │ [Selecione...     ▼]     │  │ Nenhuma data calculada ainda.       │  ║
║  │                          │  │                                      │  ║
║  │ Município de Residência: │  │                                      │  ║
║  │ [_________________]      │  │                                      │  ║
║  │                          │  │                                      │  ║
║  │ ──────────────────────── │  │                                      │  ║
║  │                          │  │                                      │  ║
║  │ ┌────────────────────┐   │  │                                      │  ║
║  │ │ Calcular e Salvar  │   │  │                                      │  ║
║  │ │      Dados         │   │  │                                      │  ║
║  │ └────────────────────┘   │  │                                      │  ║
║  │                          │  │                                      │  ║
║  │ Clique em 'Calcular e    │  │                                      │  ║
║  │ Salvar Dados' para       │  │                                      │  ║
║  │ preservar as informações │  │                                      │  ║
║  │ antes de navegar para    │  │                                      │  ║
║  │ outras abas.             │  │                                      │  ║
║  └──────────────────────────┘  └──────────────────────────────────────┘  ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
```

### Após Preencher e Clicar em "Calcular e Salvar Dados"

```
╔════════════════════════════════════════════════════════════════════════════╗
║  Análise Leptospirose - MG (2012-2023)                                    ║
╠════════════════════════════════════════════════════════════════════════════╣
║ [Dados do Paciente] [Análise de Dados] [Visualizações] [Sobre]           ║
╠════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Entrada de Dados do Paciente                                             ║
║  ════════════════════════════════════════════════════════════════════     ║
║                                                                            ║
║  ┌──────────────────────────┐  ┌──────────────────────────────────────┐  ║
║  │ Informações do Paciente  │  │ Status dos Dados                     │  ║
║  │                          │  │                                      │  ║
║  │ ID do Paciente:          │  │  ✅ Dados salvos com sucesso!       │  ║
║  │ [PAC001_________]        │  │     Você pode navegar entre as abas.│  ║
║  │                          │  │                                      │  ║
║  │ Data de Notificação:     │  │ ────────────────────────────────────│  ║
║  │ [15/01/2023] 📅          │  │                                      │  ║
║  │                          │  │ Resumo dos Dados Inseridos:         │  ║
║  │ Mesorregião:             │  │                                      │  ║
║  │ [MBH              ▼]     │  │ ID do Paciente: PAC001              │  ║
║  │                          │  │ Data de Notificação: 2023-01-15     │  ║
║  │ Idade:                   │  │ Mesorregião: MBH                    │  ║
║  │ [35       ] anos         │  │ Idade: 35 anos                      │  ║
║  │                          │  │ Sexo: M                             │  ║
║  │ Sexo:                    │  │ Município: Belo Horizonte           │  ║
║  │ [Masculino        ▼]     │  │                                      │  ║
║  │                          │  │                                      │  ║
║  │ Município de Residência: │  │ Data de Referência:                 │  ║
║  │ [Belo Horizonte_]        │  │ (Data de Notificação - 14 dias)     │  ║
║  │                          │  │                                      │  ║
║  │ ──────────────────────── │  │ Data de Referência: 2023-01-01      │  ║
║  │                          │  │ (Data de Notificação - 14 dias      │  ║
║  │ ┌────────────────────┐   │  │  para período de incubação)         │  ║
║  │ │ Calcular e Salvar  │   │  │                                      │  ║
║  │ │      Dados         │   │  │                                      │  ║
║  │ └────────────────────┘   │  │                                      │  ║
║  └──────────────────────────┘  └──────────────────────────────────────┘  ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
```

### Aba 2: Análise de Dados (Após Salvar Dados)

```
╔════════════════════════════════════════════════════════════════════════════╗
║  Análise Leptospirose - MG (2012-2023)                                    ║
╠════════════════════════════════════════════════════════════════════════════╣
║ [Dados do Paciente] [Análise de Dados] [Visualizações] [Sobre]           ║
╠════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Análise dos Dados Salvos                                                 ║
║  ════════════════════════════════════════════════════════════════════     ║
║                                                                            ║
║  ┌─────────────────────────────────┐  ┌────────────────────────────────┐ ║
║  │ Dados do Paciente Salvos        │  │ Informações Calculadas         │ ║
║  ├─────────────────┬───────────────┤  ├────────────────┬───────────────┤ ║
║  │ Campo           │ Valor         │  │ Cálculo        │ Resultado     │ ║
║  ├─────────────────┼───────────────┤  ├────────────────┼───────────────┤ ║
║  │ ID              │ PAC001        │  │ Data Referência│ 2023-01-01    │ ║
║  │ Data Notificação│ 2023-01-15    │  │ Período Incub. │ 14 dias       │ ║
║  │ Mesorregião     │ MBH           │  │ Ano            │ 2023          │ ║
║  │ Idade           │ 35 anos       │  │ Mês            │ Janeiro       │ ║
║  │ Sexo            │ M             │  └────────────────┴───────────────┘ ║
║  │ Município       │ Belo Horizonte│                                     ║
║  └─────────────────┴───────────────┘                                     ║
║                                                                            ║
║  ──────────────────────────────────────────────────────────────────────   ║
║                                                                            ║
║  Histórico de Pacientes                                                   ║
║  ┌──────────────────────────────────────────────────────────────────────┐ ║
║  │ ID    │ Data Not.  │ Data Ref.  │ Mesorr.│ Idade│ Sexo│ Município   │ ║
║  ├───────┼────────────┼────────────┼────────┼──────┼─────┼─────────────┤ ║
║  │PAC001 │ 2023-01-15 │ 2023-01-01 │ MBH    │ 35   │ M   │B. Horizonte │ ║
║  │PAC002 │ 2023-02-20 │ 2023-02-06 │ ZM     │ 42   │ F   │Juiz de Fora │ ║
║  │PAC003 │ 2023-03-10 │ 2023-02-24 │ TrMG   │ 28   │ M   │Uberlândia   │ ║
║  └───────┴────────────┴────────────┴────────┴──────┴─────┴─────────────┘ ║
║  Mostrando 1 a 3 de 3 entradas                        [< 1 >]            ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
```

### Aba 3: Visualizações (Após Múltiplos Pacientes)

```
╔════════════════════════════════════════════════════════════════════════════╗
║  Análise Leptospirose - MG (2012-2023)                                    ║
╠════════════════════════════════════════════════════════════════════════════╣
║ [Dados do Paciente] [Análise de Dados] [Visualizações] [Sobre]           ║
╠════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Visualizações e Gráficos                                                 ║
║  ════════════════════════════════════════════════════════════════════     ║
║                                                                            ║
║  Distribuição por Mesorregião                                             ║
║  ┌────────────────────────────────────────────────────────────────────┐   ║
║  │                                                                    │   ║
║  │  ZM    ████████████████ (5)                                       │   ║
║  │  MBH   ████████████████████████ (8)                               │   ║
║  │  TrMG  ██████████ (3)                                             │   ║
║  │  VRD   ███████████████████ (6)                                    │   ║
║  │  CV    ████████ (2)                                               │   ║
║  │  NMG   ████████████ (3)                                           │   ║
║  │                                                                    │   ║
║  │        0      5      10     15     20     25                      │   ║
║  │                 Número de Casos                                   │   ║
║  └────────────────────────────────────────────────────────────────────┘   ║
║                                                                            ║
║  ──────────────────────────────────────────────────────────────────────   ║
║                                                                            ║
║  ┌──────────────────────────────────┐  ┌──────────────────────────────┐  ║
║  │ Distribuição por Sexo            │  │ Distribuição por Faixa Etária│  ║
║  │                                  │  │                              │  ║
║  │     ┌──────┐         ┌──────┐   │  │  ┌───┐ ┌────┐ ┌────┐ ┌───┐  │  ║
║  │     │      │         │      │   │  │  │   │ │    │ │    │ │   │  │  ║
║  │     │      │         │      │   │  │  │   │ │    │ │    │ │   │  │  ║
║  │     │  15  │         │  12  │   │  │  │ 3 │ │ 8  │ │ 10 │ │ 6 │  │  ║
║  │     │      │         │      │   │  │  │   │ │    │ │    │ │   │  │  ║
║  │     └──────┘         └──────┘   │  │  └───┘ └────┘ └────┘ └───┘  │  ║
║  │        M                F       │  │  0-17  18-29  30-44  45-59  │  ║
║  └──────────────────────────────────┘  └──────────────────────────────┘  ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
```

### Aba 4: Sobre

```
╔════════════════════════════════════════════════════════════════════════════╗
║  Análise Leptospirose - MG (2012-2023)                                    ║
╠════════════════════════════════════════════════════════════════════════════╣
║ [Dados do Paciente] [Análise de Dados] [Visualizações] [Sobre]           ║
╠════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Sobre o Sistema                                                          ║
║  ════════════════════════════════════════════════════════════════════     ║
║                                                                            ║
║  Análise da Correlação entre Precipitação e Incidência de                ║
║  Leptospirose em Minas Gerais (2012-2023)                                ║
║                                                                            ║
║  Este aplicativo permite a entrada, armazenamento e análise de dados de  ║
║  pacientes com leptospirose no estado de Minas Gerais.                   ║
║                                                                            ║
║  Funcionalidades:                                                         ║
║  • Entrada de dados do paciente com persistência entre abas              ║
║  • Cálculo automático da data de referência (período de incubação)       ║
║  • Visualização e análise dos dados salvos                               ║
║  • Histórico completo de pacientes cadastrados                           ║
║  • Gráficos de distribuição por mesorregião, sexo e idade                ║
║                                                                            ║
║  Como Usar:                                                               ║
║  1. Acesse a aba 'Dados do Paciente'                                     ║
║  2. Preencha todos os campos do formulário                               ║
║  3. Clique no botão 'Calcular e Salvar Dados'                            ║
║  4. Os dados serão preservados ao navegar entre as abas                  ║
║  5. Visualize as análises nas abas 'Análise de Dados' e 'Visualizações' ║
║                                                                            ║
║  ──────────────────────────────────────────────────────────────────────   ║
║                                                                            ║
║  Informações do Projeto:                                                  ║
║  Estudo: Correlação entre precipitação pluviométrica e incidência de     ║
║          leptospirose nas mesorregiões de Minas Gerais, 2012-2023        ║
║  Autor: Miguel Antonio Janssen - d202120265@uftm.edu.br                  ║
║  Orientadora: Profa. Dra. Ana Paula Fernandes                            ║
║               anapaula.fernandes@uftm.edu.br                              ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
```

## Fluxo de Interação do Usuário

```
    INÍCIO
      │
      ▼
┌─────────────────┐
│ Abre aplicativo │
└────────┬────────┘
         │
         ▼
┌──────────────────────┐
│ Aba "Dados Paciente" │
│ (campos vazios)      │
└────────┬─────────────┘
         │
         ▼
┌──────────────────────────┐
│ Preenche formulário      │
│ - ID: PAC001             │
│ - Data: 15/01/2023       │
│ - Mesorregião: MBH       │
│ - Idade: 35              │
│ - Sexo: M                │
│ - Município: BH          │
└────────┬─────────────────┘
         │
         ▼
┌─────────────────────────────────┐
│ Clica "Calcular e Salvar Dados" │
└────────┬────────────────────────┘
         │
         ▼
┌──────────────────────────┐      ┌─────────────────────────┐
│ ✅ Dados salvos!         │      │ Cálculo automático:     │
│ Notificação de sucesso   │      │ Data Ref = 01/01/2023   │
└────────┬─────────────────┘      └─────────────────────────┘
         │
         ▼
┌────────────────────────────┐
│ Usuário navega livremente  │
│ entre abas                 │
│ • Análise de Dados  ✓      │
│ • Visualizações     ✓      │
│ • Volta para Dados  ✓      │
└────────┬───────────────────┘
         │
         ▼
┌───────────────────────────┐
│ Dados PERMANECEM salvos   │
│ em TODAS as abas          │
│ (ReactiveValues)          │
└───────────────────────────┘
         │
         ▼
      FIM
```

## Vantagens da Solução

### ✅ Problema Resolvido
**ANTES**: Dados se perdiam ao trocar de aba
**DEPOIS**: Dados persistem durante toda a sessão

### ✅ Experiência do Usuário
- Interface intuitiva
- Feedback visual claro
- Validação de campos
- Mensagens de sucesso/erro

### ✅ Funcionalidades Avançadas
- Cálculo automático de data de referência
- Histórico de pacientes
- Visualizações dinâmicas
- Exportação de dados (através da tabela DT)

### ✅ Código Limpo e Mantível
- Estrutura modular
- ReactiveValues para estado
- Separação UI/Server
- Comentários claros
