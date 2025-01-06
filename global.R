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
