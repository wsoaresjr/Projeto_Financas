
library(shiny)
library(DBI)
library(RMySQL)
library(DT)
library(dotenv)

# Arquivo .env
load_dot_env(file = "config.env")

tryCatch({
  con <- dbConnect(
    RMySQL::MySQL(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  message("Conexão ao banco de dados estabelecida com sucesso!")
}, error = function(e) {
  stop("Erro ao conectar ao banco de dados: ", e$message)
})

# UI
ui <- fluidPage(
  titlePanel("Gerenciador Financeiro"),
  
  tabsetPanel(
    tabPanel("Categorias",
             sidebarLayout(
               sidebarPanel(
                 textInput("new_category", "Nova Categoria"),
                 actionButton("add_category", "Adicionar Categoria")
               ),
               mainPanel(
                 dataTableOutput("categories_table")
               )
             )),
    tabPanel("Transações",
             sidebarLayout(
               sidebarPanel(
                 textInput("description", "Descrição"),
                 numericInput("amount", "Valor", value = 0),
                 dateInput("transaction_date", "Data", value = Sys.Date()),
                 selectInput("category", "Categoria", choices = NULL),
                 selectInput("account_type", "Tipo de Conta", choices = c("Pessoa Física", "Pessoa Jurídica")),
                 fileInput("receipt", "Comprovante (opcional)"),
                 actionButton("add_transaction", "Adicionar Transação")
               ),
               mainPanel(
                 dataTableOutput("transactions_table")
               )
             )),
    tabPanel("Agendamentos",
             sidebarLayout(
               sidebarPanel(
                 textInput("schedule_description", "Descrição"),
                 numericInput("schedule_amount", "Valor", value = 0),
                 dateInput("due_date", "Data de Vencimento", value = Sys.Date()),
                 selectInput("schedule_category", "Categoria", choices = NULL),
                 selectInput("schedule_account_type", "Tipo de Conta", choices = c("Pessoa Física", "Pessoa Jurídica")),
                 actionButton("add_schedule", "Agendar")
               ),
               mainPanel(
                 dataTableOutput("schedules_table")
               )
             )),
    tabPanel("Relatórios",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("report_range", "Período", start = Sys.Date() - 30, end = Sys.Date()),
                 actionButton("generate_report", "Gerar Relatório")
               ),
               mainPanel(
                 plotOutput("report_plot"),
                 dataTableOutput("report_table")
               )
             ))
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  categories_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM categories"))
  transactions_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM transactions"))
  schedules_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM scheduled_payments"))
  account_types <- reactiveVal(dbGetQuery(con, "SELECT * FROM account_types"))
  
  # Atualizar tipos de conta
  observe({
    updateSelectInput(session, "account_type", choices = account_types()$type_name)
  })
  
  
  # Atualizar categorias
  observe({
    updateSelectInput(session, "category", choices = categories_data()$name)
    updateSelectInput(session, "schedule_category", choices = categories_data()$name)
  })
  
  # Adicionar Categoria
  observeEvent(input$add_category, {
    req(input$new_category)
    query <- sprintf("INSERT INTO categories (name) VALUES ('%s')", dbEscapeStrings(con, input$new_category))
    dbExecute(con, query)
    categories_data(dbGetQuery(con, "SELECT * FROM categories"))
    showNotification("Categoria adicionada com sucesso!", type = "message")
  })
  
  output$categories_table <- renderDataTable({
    datatable(categories_data(), options = list(pageLength = 5))
  })
  
  # Adicionar Transação
  observeEvent(input$add_transaction, {
    req(input$description, input$amount, input$transaction_date, input$category, input$account_type)
    category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$category))$id[1]
    account_type_id <- ifelse(input$account_type == "Pessoa Física", 1, 2)
    query <- sprintf(
      "INSERT INTO transactions (amount, description, transaction_date, category_id, account_type_id) VALUES (%f, '%s', '%s', %d, %d)",
      input$amount, input$description, input$transaction_date, category_id, account_type_id
    )
    dbExecute(con, query)
    transactions_data(dbGetQuery(con, "SELECT * FROM transactions"))
    showNotification("Transação adicionada com sucesso!", type = "message")
  })
  
  output$transactions_table <- renderDataTable({
    datatable(transactions_data(), options = list(pageLength = 5))
  })
  
  # Agendar Pagamento
  observeEvent(input$add_schedule, {
    req(input$schedule_description, input$schedule_amount, input$due_date, input$schedule_category, input$schedule_account_type)
    schedule_category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$schedule_category))$id[1]
    schedule_account_type_id <- ifelse(input$schedule_account_type == "Pessoa Física", 1, 2)
    query <- sprintf(
      "INSERT INTO scheduled_payments (amount, description, due_date, category_id, account_type_id) VALUES (%f, '%s', '%s', %d, %d)",
      input$schedule_amount, input$schedule_description, input$due_date, schedule_category_id, schedule_account_type_id
    )
    dbExecute(con, query)
    schedules_data(dbGetQuery(con, "SELECT * FROM scheduled_payments"))
    showNotification("Agendamento adicionado com sucesso!", type = "message")
  })
  
  output$schedules_table <- renderDataTable({
    datatable(schedules_data(), options = list(pageLength = 5))
  })
  
  # Relatórios (Placeholder)
  observeEvent(input$generate_report, {
    showNotification("Relatórios serão implementados em breve!", type = "warning")
  })
}

# Executa o app
shinyApp(ui = ui, server = server)