
library(shiny)
library(DBI)
library(RMySQL)
library(DT)
library(dplyr)
library(dotenv)

# Configuração do diretório para armazenar os comprovantes
comprovantes_dir <- "www/comprovantes"
if (!dir.exists(comprovantes_dir)) dir.create(comprovantes_dir, recursive = TRUE)

# Carregar variáveis de ambiente
load_dot_env(file = "config.env")

# Conexão única ao banco de dados
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
  tags$script(HTML("
    $(document).on('click', '.edit_category_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('edit_category', id, {priority: 'event'});
    });

    $(document).on('click', '.delete_category_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_category', id, {priority: 'event'});
    });

    $(document).on('click', '.edit_transaction_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('edit_transaction', id, {priority: 'event'});
    });

    $(document).on('click', '.delete_transaction_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_transaction', id, {priority: 'event'});
    });
  ")),
  
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
             ))
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  categories_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM categories"))
  transactions_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM transactions"))
  
  # Atualizar categorias no select
  observe({
    updateSelectInput(session, "category", choices = categories_data()$name)
  })
  
  # Atualizar tabela de categorias
  output$categories_table <- renderDataTable({
    datatable(
      categories_data() %>%
        mutate(
          Editar = sprintf('<button class="btn btn-primary btn-sm edit_category_btn" data-id="%d">Editar</button>', id),
          Deletar = sprintf('<button class="btn btn-danger btn-sm delete_category_btn" data-id="%d">Deletar</button>', id)
        ),
      escape = FALSE,
      options = list(pageLength = 5),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Adicionar Categoria
  observeEvent(input$add_category, {
    req(input$new_category)
    query <- sprintf("INSERT INTO categories (name) VALUES ('%s')", dbEscapeStrings(con, input$new_category))
    dbExecute(con, query)
    categories_data(dbGetQuery(con, "SELECT * FROM categories"))
    updateTextInput(session, "new_category", value = "")
    showNotification("Categoria adicionada com sucesso!", type = "message")
  })
  
  # Editar Categoria
  observeEvent(input$edit_category, {
    req(input$edit_category)
    selected_id <- input$edit_category
    selected_category <- categories_data() %>% filter(id == selected_id)
    
    showModal(modalDialog(
      title = "Editar Categoria",
      textInput("edit_category_name", "Novo Nome", value = selected_category$name),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_edit_category", "Salvar", class = "btn btn-success")
      )
    ))
  })
  
  observeEvent(input$save_edit_category, {
    req(input$edit_category_name, input$edit_category)
    query <- sprintf("UPDATE categories SET name = '%s' WHERE id = %d", dbEscapeStrings(con, input$edit_category_name), input$edit_category)
    dbExecute(con, query)
    categories_data(dbGetQuery(con, "SELECT * FROM categories"))
    removeModal()
    showNotification("Categoria editada com sucesso!", type = "message")
  })
  
  # Deletar Categoria
  observeEvent(input$delete_category, {
    req(input$delete_category)
    selected_id <- input$delete_category
    selected_category <- categories_data() %>% filter(id == selected_id)
    
    showModal(modalDialog(
      title = "Deletar Categoria",
      paste0("Tem certeza de que deseja deletar a categoria: ", selected_category$name, "?"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_delete_category", "Deletar", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_category, {
    req(input$delete_category)
    query <- sprintf("DELETE FROM categories WHERE id = %d", input$delete_category)
    dbExecute(con, query)
    categories_data(dbGetQuery(con, "SELECT * FROM categories"))
    removeModal()
    showNotification("Categoria deletada com sucesso!", type = "message")
  })
  
  # Atualizar tabela de transações
  output$transactions_table <- renderDataTable({
    datatable(
      transactions_data() %>%
        mutate(
          Comprovante = ifelse(!is.na(receipt_file_path), 
                               sprintf('<a href="/comprovantes/%s" target="_blank">Visualizar</a>', basename(receipt_file_path)), 
                               "Não enviado"),
          Editar = sprintf('<button class="btn btn-primary btn-sm edit_transaction_btn" data-id="%d">Editar</button>', id),
          Deletar = sprintf('<button class="btn btn-danger btn-sm delete_transaction_btn" data-id="%d">Deletar</button>', id)
        ),
      escape = FALSE,
      options = list(pageLength = 5),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Adicionar Transação
  observeEvent(input$add_transaction, {
    req(input$description, input$amount, input$transaction_date, input$category, input$account_type)
    
    # Obter ID da Categoria e Tipo de Conta
    category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$category))$id[1]
    account_type_id <- ifelse(input$account_type == "Pessoa Física", 1, 2)
    
    # Processar comprovante
    receipt_file_path <- NULL
    if (!is.null(input$receipt)) {
      # Corrigir nome do arquivo
      safe_file_name <- gsub("[: ]", "_", input$receipt$name)
      receipt_file_path <- file.path("www/comprovantes", safe_file_name)
      file.copy(input$receipt$datapath, receipt_file_path)
    }
    
    query <- sprintf(
      "INSERT INTO transactions (description, amount, transaction_date, category_id, account_type_id, receipt_file_path) 
     VALUES ('%s', %f, '%s', %d, %d, '%s')",
      dbEscapeStrings(con, input$description), input$amount, input$transaction_date, category_id, account_type_id, receipt_file_path
    )
    dbExecute(con, query)
    transactions_data(dbGetQuery(con, "SELECT * FROM transactions"))
    showNotification("Transação adicionada com sucesso!", type = "message")
  })
  
  # Editar Transação
  observeEvent(input$edit_transaction, {
    req(input$edit_transaction)
    selected_id <- input$edit_transaction
    selected_transaction <- transactions_data() %>% filter(id == selected_id)
    
    showModal(modalDialog(
      title = "Editar Transação",
      textInput("edit_description", "Descrição", value = selected_transaction$description),
      numericInput("edit_amount", "Valor", value = selected_transaction$amount),
      dateInput("edit_date", "Data", value = selected_transaction$transaction_date),
      selectInput("edit_transaction_category", "Categoria", choices = categories_data()$name, selected = categories_data() %>% filter(id == selected_transaction$category_id) %>% pull(name)),
      selectInput("edit_account_type", "Tipo de Conta", choices = c("Pessoa Física", "Pessoa Jurídica"), selected = ifelse(selected_transaction$account_type_id == 1, "Pessoa Física", "Pessoa Jurídica")),
      fileInput("edit_receipt", "Novo Comprovante (opcional)"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_edit_transaction", "Salvar", class = "btn btn-success")
      )
    ))
  })
  
  # Salvar Edição da Transação
  observeEvent(input$save_edit_transaction, {
    req(input$edit_description, input$edit_amount, input$edit_date, input$edit_transaction_category, input$edit_account_type, input$edit_transaction)
    
    # Obter IDs
    category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$edit_transaction_category))$id[1]
    account_type_id <- ifelse(input$edit_account_type == "Pessoa Física", 1, 2)
    
    # Processar novo comprovante, se enviado
    receipt_file_path <- NULL
    if (!is.null(input$edit_receipt)) {
      safe_file_name <- gsub("[: ]", "_", input$edit_receipt$name)
      receipt_file_path <- file.path("www/comprovantes", safe_file_name)
      file.copy(input$edit_receipt$datapath, receipt_file_path)
    } else {
      receipt_file_path <- transactions_data() %>% filter(id == input$edit_transaction) %>% pull(receipt_file_path)
    }
    
    # Atualizar transação no banco de dados
    query <- sprintf(
      "UPDATE transactions 
     SET description = '%s', amount = %f, transaction_date = '%s', category_id = %d, account_type_id = %d, receipt_file_path = '%s' 
     WHERE id = %d",
      dbEscapeStrings(con, input$edit_description), input$edit_amount, input$edit_date, category_id, account_type_id, receipt_file_path, input$edit_transaction
    )
    dbExecute(con, query)
    
    # Atualizar dados no app
    transactions_data(dbGetQuery(con, "SELECT * FROM transactions"))
    removeModal()
    showNotification("Transação editada com sucesso!", type = "message")
  })
  
  
  
  # Deletar Transação
  observeEvent(input$delete_transaction, {
    req(input$delete_transaction)
    selected_id <- input$delete_transaction
    
    showModal(modalDialog(
      title = "Deletar Transação",
      paste0("Tem certeza de que deseja deletar a transação selecionada?"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_delete_transaction", "Deletar", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_transaction, {
    req(input$delete_transaction)
    query <- sprintf("DELETE FROM transactions WHERE id = %d", input$delete_transaction)
    dbExecute(con, query)
    transactions_data(dbGetQuery(con, "SELECT * FROM transactions"))
    removeModal()
    showNotification("Transação deletada com sucesso!", type = "message")
  })
}

# Executa o app
shinyApp(ui = ui, server = server)