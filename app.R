
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
             ))
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value para armazenar os dados das categorias
  categories_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM categories"))
  
  # Adiciona nova categoria ao banco
  observeEvent(input$add_category, {
    req(input$new_category)
    
    query <- sprintf("INSERT INTO categories (name) VALUES ('%s')", dbEscapeStrings(con, input$new_category))
    dbExecute(con, query)
    
    # Atualiza os dados das categorias após a inserção
    categories_data(dbGetQuery(con, "SELECT * FROM categories"))
    showNotification("Categoria adicionada com sucesso!", type = "message")
  })
  
  # Exibe as categorias na tabela
  output$categories_table <- renderDataTable({
    datatable(categories_data(), options = list(pageLength = 5))
  })
}

# Executa o app
shinyApp(ui = ui, server = server)