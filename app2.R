
library(shiny)
library(DBI)
library(RMySQL)
library(DT)
library(dplyr)
library(dotenv)
library(ggplot2)

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
    
    $(document).on('click', '.edit_schedule_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('edit_schedule', id, {priority: 'event'});
    });

    $(document).on('click', '.delete_schedule_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_schedule', id, {priority: 'event'});
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
                 dateRangeInput("report_date_range", "Período", 
                                start = Sys.Date() - 30, 
                                end = Sys.Date()),
                 selectInput("report_account_type", "Tipo de Conta", 
                             choices = c("Ambos", "Pessoa Física", "Pessoa Jurídica")),
                 actionButton("generate_report", "Gerar Relatório", class = "btn btn-primary")
               ),
               mainPanel(
                 h4("Resumo Financeiro"),
                 verbatimTextOutput("financial_summary"),
                 plotOutput("category_pie_chart"),
                 plotOutput("monthly_bar_chart"),
                 h4("Agendamentos Futuros"),
                 dataTableOutput("future_schedules_table")
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
  
  # Atualizar categorias no select
  observe({
    updateSelectInput(session, "category", choices = categories_data()$name)
  })
  
  observe({
    updateSelectInput(session, "schedule_category", choices = categories_data()$name)
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
  
  # Atualizar tabela de agendamentos
  output$schedules_table <- renderDataTable({
    datatable(
      schedules_data() %>%
        mutate(
          Editar = sprintf('<button class="btn btn-primary btn-sm edit_schedule_btn" data-id="%d">Editar</button>', id),
          Deletar = sprintf('<button class="btn btn-danger btn-sm delete_schedule_btn" data-id="%d">Deletar</button>', id)
        ),
      escape = FALSE,
      options = list(pageLength = 5),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Adicionar Agendamento
  observeEvent(input$add_schedule, {
    req(input$schedule_description, input$schedule_amount, input$due_date, input$schedule_category, input$schedule_account_type)
    
    # Obter ID da Categoria e Tipo de Conta
    category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$schedule_category))$id[1]
    account_type_id <- ifelse(input$schedule_account_type == "Pessoa Física", 1, 2)
    
    query <- sprintf(
      "INSERT INTO scheduled_payments (description, amount, due_date, category_id, account_type_id) 
       VALUES ('%s', %f, '%s', %d, %d)",
      dbEscapeStrings(con, input$schedule_description), input$schedule_amount, input$due_date, category_id, account_type_id
    )
    dbExecute(con, query)
    schedules_data(dbGetQuery(con, "SELECT * FROM scheduled_payments"))
    showNotification("Agendamento adicionado com sucesso!", type = "message")
  })
  
  # Editar Agendamento
  observeEvent(input$edit_schedule, {
    req(input$edit_schedule)
    selected_id <- input$edit_schedule
    selected_schedule <- schedules_data() %>% filter(id == selected_id)
    
    showModal(modalDialog(
      title = "Editar Agendamento",
      textInput("edit_schedule_description", "Descrição", value = selected_schedule$description),
      numericInput("edit_schedule_amount", "Valor", value = selected_schedule$amount),
      dateInput("edit_due_date", "Data de Vencimento", value = selected_schedule$due_date),
      selectInput("edit_schedule_category", "Categoria", choices = categories_data()$name, selected = categories_data() %>% filter(id == selected_schedule$category_id) %>% pull(name)),
      selectInput("edit_schedule_account_type", "Tipo de Conta", choices = c("Pessoa Física", "Pessoa Jurídica"), selected = ifelse(selected_schedule$account_type_id == 1, "Pessoa Física", "Pessoa Jurídica")),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_edit_schedule", "Salvar", class = "btn btn-success")
      )
    ))
  })
  
  observeEvent(input$save_edit_schedule, {
    req(input$edit_schedule_description, input$edit_schedule_amount, input$edit_due_date, input$edit_schedule_category, input$edit_schedule_account_type, input$edit_schedule)
    
    # Obter IDs
    category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$edit_schedule_category))$id[1]
    account_type_id <- ifelse(input$edit_schedule_account_type == "Pessoa Física", 1, 2)
    
    query <- sprintf(
      "UPDATE scheduled_payments 
       SET description = '%s', amount = %f, due_date = '%s', category_id = %d, account_type_id = %d 
       WHERE id = %d",
      dbEscapeStrings(con, input$edit_schedule_description), input$edit_schedule_amount, input$edit_due_date, category_id, account_type_id, input$edit_schedule
    )
    dbExecute(con, query)
    schedules_data(dbGetQuery(con, "SELECT * FROM scheduled_payments"))
    removeModal()
    showNotification("Agendamento editado com sucesso!", type = "message")
  })
  
  # Deletar Agendamento
  observeEvent(input$delete_schedule, {
    req(input$delete_schedule)
    selected_id <- input$delete_schedule
    
    showModal(modalDialog(
      title = "Deletar Agendamento",
      paste0("Tem certeza de que deseja deletar o agendamento selecionado?"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_delete_schedule", "Deletar", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_schedule, {
    req(input$delete_schedule)
    query <- sprintf("DELETE FROM scheduled_payments WHERE id = %d", input$delete_schedule)
    dbExecute(con, query)
    schedules_data(dbGetQuery(con, "SELECT * FROM scheduled_payments"))
    removeModal()
    showNotification("Agendamento deletado com sucesso!", type = "message")
  })
  
  
  
  # Gerar Relatórios
  observeEvent(input$generate_report, {
    req(input$report_date_range)
    
    # Filtrar transações no período e tipo de conta
    account_type_filter <- switch(input$report_account_type,
                                  "Ambos" = "",
                                  "Pessoa Física" = "AND account_type_id = 1",
                                  "Pessoa Jurídica" = "AND account_type_id = 2")
    
    transactions_in_range <- dbGetQuery(con, sprintf(
      "SELECT * FROM transactions 
     WHERE transaction_date BETWEEN '%s' AND '%s' %s",
      input$report_date_range[1], input$report_date_range[2], account_type_filter
    ))
    
    # Calcular resumo financeiro
    total_revenue <- sum(transactions_in_range$amount[transactions_in_range$amount > 0], na.rm = TRUE)
    total_expense <- sum(transactions_in_range$amount[transactions_in_range$amount < 0], na.rm = TRUE)
    balance <- total_revenue + total_expense
    
    # Atualizar Resumo Financeiro
    output$financial_summary <- renderText({
      if (nrow(transactions_in_range) == 0) {
        return("Nenhum dado encontrado para o período selecionado.")
      }
      
      total_expense <- sum(transactions_in_range$amount[transactions_in_range$amount < 0], na.rm = TRUE)
      
      paste0(
        "Despesas Totais: R$ ", format(abs(total_expense), big.mark = ".", decimal.mark = ",")
      )
    })
    
    # Gráfico de Pizza (Despesas por Categoria)
    output$category_pie_chart <- renderPlot({
      if (nrow(transactions_in_range) == 0) {
        return(NULL)
      }
      
      # Filtrar apenas despesas
      expenses <- transactions_in_range[transactions_in_range$amount < 0, ]
      if (nrow(expenses) == 0) {
        return(NULL)
      }
      
      # Relacionar categorias
      categories <- dbGetQuery(con, "SELECT id, name FROM categories")
      expenses <- merge(expenses, categories, by.x = "category_id", by.y = "id")
      
      # Agrupar despesas por categoria
      category_summary <- expenses %>%
        group_by(name) %>%
        summarise(total = sum(abs(amount)))
      
      
      ggplot(category_summary, aes(x = "", y = total, fill = name)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "Despesas por Categoria", x = NULL, y = NULL) +
        theme_void() +
        theme(legend.title = element_blank())
    })
    
    
    # Gráfico de Barras (Receitas e Despesas por Mês)
    output$monthly_bar_chart <- renderPlot({
      if (nrow(transactions_in_range) == 0) return(NULL)
      library(lubridate)
      transactions_in_range$month <- floor_date(as.Date(transactions_in_range$transaction_date), "month")
      monthly_summary <- aggregate(transactions_in_range$amount, 
                                   by = list(Month = transactions_in_range$month), 
                                   FUN = sum)
      
      ggplot(monthly_summary, aes(x = Month, y = x, fill = ifelse(x > 0, "Receitas", "Despesas"))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Receitas" = "blue", "Despesas" = "red")) +
        labs(title = "Receitas e Despesas por Mês", x = "Mês", y = "Total") +
        theme_minimal()
    })
    
    # Agendamentos Futuros
    future_schedules <- dbGetQuery(con, sprintf(
      "SELECT * FROM scheduled_payments 
   WHERE due_date >= '%s'", Sys.Date()
    ))
    
    output$future_schedules_table <- renderDataTable({
      if (nrow(future_schedules) == 0) {
        return(NULL)
      }
      
      # Relacionar tipos de conta
      account_types <- data.frame(
        id = c(1, 2),
        type_name = c("Pessoa Física", "Pessoa Jurídica")
      )
      future_schedules <- merge(future_schedules, account_types, by.x = "account_type_id", by.y = "id", all.x = TRUE)
      
      future_schedules %>%
        mutate(
          Categoria = dbGetQuery(con, sprintf(
            "SELECT name FROM categories WHERE id IN (%s)", 
            paste(unique(future_schedules$category_id), collapse = ",")
          ))$name
        ) %>%
        select(Descrição = description, Valor = amount, Data = due_date, Categoria, `Tipo de Conta` = type_name)
    })
    
  })
  
  
  
}

# Executa o app
shinyApp(ui = ui, server = server)