library(tidyverse)
library(data.table)

#' Função para carregar e processar dados
#' @param project_root Caminho raiz do projeto (opcional)
load_data <- function(project_root = ".") {
  # Define o caminho para a pasta de dados limpos
  data_path <- file.path(project_root, "data", "limpos")
  
  # Define tipos de variáveis para forçar na leitura
  var_types <- list(
    # Identificadores
    id = list(
      employee_attrition = c("EmployeeNumber")
    ),
    
    # Variáveis categóricas
    fator = list(
      employee_attrition = c(
        "JobRole", "JobInvolvement", "MaritalStatus", "Education", 
        "EducationField", "BusinessTravel", "Gender", "JobLevel", "OverTime",
        "Over18", "PerformanceRating", "EnvironmentSatisfaction",
        "RelationshipSatisfaction", "JobSatisfaction",
        "Attrition", "WorkLifeBalance", 
        "StockOptionLevel", "TrainingTimesLastYear"
      )
    ),
    
    # Variáveis numéricas
    continuas = list(
      employee_attrition = c(
        "TotalWorkingYears", "EmployeeCount", "DistanceFromHome", "StandardHours",
        "Age", "PercentSalaryHike", "NumCompaniesWorked", "MonthlyIncome",
        "DailyRate", "HourlyRate", "MonthlyRate", "JobLevel",
        "YearsAtCompany", "YearsWithCurrManager", "YearsSinceLastPromotion"
      )
    ),
    
    # Variáveis texto (se houver)
    char = list(
      employee_attrition = c()
    ),
    
    # Variáveis data (se houver)
    dates = list(
      employee_attrition = c()
    )
  )

  # Configurações para leitura
  csv_config <- list(
    sep = ",",
    dec = ".",
    na.strings = c("", "-", "NA", "#N/A", "#N/A!", "#N/D", "#DIV/0!", 
                   "#NAME?", "#NULL!", "#NUM!", "#REF!", "#VALUE!")
  )

  # Lista arquivos CSV no diretório
  csv_files <- list.files(
    path = data_path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("Nenhum arquivo CSV encontrado em ", data_path)
  }

  # Cria lista com nomes dos arquivos (sem extensão)
  files.nome <- basename(csv_files) %>%
    tools::file_path_sans_ext()

  # Função auxiliar para processar um arquivo
  process_file <- function(file_path) {
    message(glue::glue("Processando arquivo: {basename(file_path)}"))
    
    # Lê o arquivo
    df <- fread(
      file_path,
      sep = csv_config$sep,
      dec = csv_config$dec,
      na.strings = csv_config$na.strings,
      data.table = FALSE,
      encoding = "UTF-8",
      showProgress = FALSE
    ) %>%
      as_tibble()
    
    # Nome do arquivo atual (sem extensão)
    current_file <- tools::file_path_sans_ext(basename(file_path))
    
    # Força tipos das variáveis
    df %>%
      mutate(
        # Força IDs para character
        across(any_of(var_types$id[[current_file]]), as.character),
        # Força fatores
        across(any_of(var_types$fator[[current_file]]), as.factor),
        # Força numéricas
        across(any_of(var_types$continuas[[current_file]]), as.numeric),
        # Força datas (se houver)
        across(any_of(var_types$dates[[current_file]]), as.Date),
        # Força character (se houver)
        across(any_of(var_types$char[[current_file]]), as.character)
      )
  }

  # Processa todos os arquivos
  all_csvs <- csv_files %>%
    set_names(files.nome) %>%
    map(safely(process_file)) %>%
    map("result") %>%
    compact()

  # Retorna uma lista com os dados e metadados necessários
  return(list(
    data = all_csvs,          # dados processados
    files.nome = files.nome,  # nomes dos arquivos
    var_types = var_types     # tipos das variáveis
  ))
}
