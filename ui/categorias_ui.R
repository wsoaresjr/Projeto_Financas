categorias_ui <- tabPanel(
  "Categorias",
  tags$script(HTML("
    $(document).on('click', '.edit_category_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('edit_category', id, {priority: 'event'});
    });

    $(document).on('click', '.delete_category_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_category', id, {priority: 'event'});
    });
  ")),
  sidebarLayout(
    sidebarPanel(
      textInput("new_category", "Nova Categoria"),
      actionButton("add_category", "Adicionar Categoria")
    ),
    mainPanel(
      dataTableOutput("categories_table")
    )
  )
)
