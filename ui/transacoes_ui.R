transacoes_ui <- tabPanel(
  "Transações",
  tags$script(HTML("
    $(document).on('click', '.edit_transaction_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('edit_transaction', id, {priority: 'event'});
    });

    $(document).on('click', '.delete_transaction_btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_transaction', id, {priority: 'event'});
    });
  ")),
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
  )
)
