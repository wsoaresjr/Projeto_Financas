# ui/agendamentos_ui.R
agendamentos_ui <- tabPanel(
  "Agendamentos",
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
  )
)

