categorias_server <- function(input, output, session, con, categories_data) {
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
}
