transacoes_server <- function(input, output, session, con, categories_data, transactions_data) {
  # Atualizar o selectInput das categorias
  observe({
    updateSelectInput(session, "category", choices = categories_data()$name)
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
      selectInput("edit_transaction_category", "Categoria", 
                  choices = categories_data()$name, 
                  selected = categories_data() %>% filter(id == selected_transaction$category_id) %>% pull(name)),
      selectInput("edit_account_type", "Tipo de Conta", 
                  choices = c("Pessoa Física", "Pessoa Jurídica"), 
                  selected = ifelse(selected_transaction$account_type_id == 1, "Pessoa Física", "Pessoa Jurídica")),
      fileInput("edit_receipt", "Novo Comprovante (opcional)"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_edit_transaction", "Salvar", class = "btn btn-success")
      )
    ))
  })
  
  observeEvent(input$save_edit_transaction, {
    req(input$edit_description, input$edit_amount, input$edit_date, input$edit_transaction_category, input$edit_account_type, input$edit_transaction)
    
    category_id <- dbGetQuery(con, sprintf("SELECT id FROM categories WHERE name = '%s'", input$edit_transaction_category))$id[1]
    account_type_id <- ifelse(input$edit_account_type == "Pessoa Física", 1, 2)
    
    receipt_file_path <- NULL
    if (!is.null(input$edit_receipt)) {
      safe_file_name <- gsub("[: ]", "_", input$edit_receipt$name)
      receipt_file_path <- file.path("www/comprovantes", safe_file_name)
      file.copy(input$edit_receipt$datapath, receipt_file_path)
    } else {
      receipt_file_path <- transactions_data() %>% filter(id == input$edit_transaction) %>% pull(receipt_file_path)
    }
    
    query <- sprintf(
      "UPDATE transactions 
       SET description = '%s', amount = %f, transaction_date = '%s', category_id = %d, account_type_id = %d, receipt_file_path = '%s' 
       WHERE id = %d",
      dbEscapeStrings(con, input$edit_description), input$edit_amount, input$edit_date, category_id, account_type_id, receipt_file_path, input$edit_transaction
    )
    dbExecute(con, query)
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
      "Tem certeza de que deseja deletar a transação selecionada?",
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
