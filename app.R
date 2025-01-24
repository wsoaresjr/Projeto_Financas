source("global.R")

# Importar módulos de interface e servidor
source("ui/categorias_ui.R")
source("ui/transacoes_ui.R")
source("ui/agendamentos_ui.R")
#source("ui/relatorios_ui.R")

source("server/categorias_server.R")
source("server/transacoes_server.R")
source("server/agendamentos_server.R")
#source("server/relatorios_server.R")

ui <- fluidPage(
  titlePanel("Gerenciador Financeiro"),
  tabsetPanel(
    categorias_ui,
    transacoes_ui,
    agendamentos_ui
    # relatorios_ui("relatorios")  # Se o módulo de relatórios existir
  )
)

# Server principal
server <- function(input, output, session) {
  # Criação do ReactiveValue global para categorias
  categories_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM categories"))
  transactions_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM transactions"))
  schedules_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM scheduled_payments"))
  
  categorias_server(input, output, session, con, categories_data)
  transacoes_server(input, output, session, con, categories_data, transactions_data)
  agendamentos_server(input, output, session, con, categories_data, schedules_data)
  #relatorios_server(input, output, session, con)
}

# Executa o app
shinyApp(ui = ui, server = server)
