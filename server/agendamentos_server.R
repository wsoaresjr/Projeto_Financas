# server/agendamentos_server.R
agendamentos_server <- function(input, output, session, con, categories_data, schedules_data) {
  ns <- session$ns
  
  # Atualizar o selectInput das categorias
  observe({
    updateSelectInput(session, "schedule_category", choices = categories_data()$name)
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
      textInput(ns("edit_schedule_description"), "Descrição", value = selected_schedule$description),
      numericInput(ns("edit_schedule_amount"), "Valor", value = selected_schedule$amount),
      dateInput(ns("edit_due_date"), "Data de Vencimento", value = selected_schedule$due_date),
      selectInput(ns("edit_schedule_category"), "Categoria", 
                  choices = categories_data()$name, 
                  selected = categories_data() %>% filter(id == selected_schedule$category_id) %>% pull(name)),
      selectInput(ns("edit_schedule_account_type"), "Tipo de Conta", 
                  choices = c("Pessoa Física", "Pessoa Jurídica"), 
                  selected = ifelse(selected_schedule$account_type_id == 1, "Pessoa Física", "Pessoa Jurídica")),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton(ns("save_edit_schedule"), "Salvar", class = "btn btn-success")
      )
    ))
  })
  
  observeEvent(input$save_edit_schedule, {
    req(input$edit_schedule_description, input$edit_schedule_amount, input$edit_due_date, input$edit_schedule_category, input$edit_schedule_account_type, input$edit_schedule)
    
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
      "Tem certeza de que deseja deletar o agendamento selecionado?",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton(ns("confirm_delete_schedule"), "Deletar", class = "btn btn-danger")
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
}

