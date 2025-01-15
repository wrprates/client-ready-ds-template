
library(tidyverse)
library(data.table)

#' Função para carregar e processar dados
#' @param project_root Caminho raiz do projeto (opcional)
load_data <- function(project_root = ".") {
  # Define o caminho para a pasta de dados limpos
  data_path <- file.path(project_root, "data", "limpos")
  
  # Define tipos de variáveis para forçar na leitura
  var_types <- list(
    # IDs (character)
    ids = c("emplid", "deptid"),
    
    # Fatores/Categóricas 
    factors = c(
      "cargo", "engajamento", "estado_civil", "escolaridade", 
      "formacao", "freq_viagem", "genero", "grade", "hora_extra",
      "maior_idade", "performance", "satisfacao_ambiente",
      "satisfacao_relacoes_trabalho", "satistacao_trabalho",
      "turnover", "equilibrio_vida_profissional", 
      "stockoption", "treinamentos_ultimo_ano"
    ),
    
    # Numéricas
    numeric = c(
      "anos_experiencia", "cont_emplid", "distancia", "horas_contratadas",
      "idade", "percent_gest_conseq", "qtd_emp_trabalhadas", "salario",
      "salario_diario", "salario_hora", "taxa_mensal", "tempo_cargo",
      "tempo_empresa", "tempo_mesma_lideranca", "tempo_ult_promocao"
    )
  )

  # Configurações para leitura
  csv_config <- list(
    sep = ",",
    dec = ".",
    na.strings = c("", "-", "NA", "#N/A", "#N/A!", "#N/D", "#DIV/0!", 
                   "#NAME?", "#NULL!", "#NUM!", "#REF!", "#VALUE!")
  )

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
    
    # Força tipos das variáveis
    df %>%
      mutate(
        # Força IDs para character
        across(any_of(var_types$ids), as.character),
        # Força fatores
        across(any_of(var_types$factors), as.factor),
        # Força numéricas
        across(any_of(var_types$numeric), as.numeric)
      )
  }

  # Lista arquivos CSV no diretório
  csv_files <- list.files(
    path = data_path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("Nenhum arquivo CSV encontrado em ", data_path)
  }

  # Processa todos os arquivos
  all_csvs <- csv_files %>%
    set_names(basename) %>%
    map(safely(process_file)) %>%
    map("result") %>%
    compact()

  return(all_csvs)
}

# Executa o carregamento
all_csvs <- load_data()

message(glue::glue("Tempo total de execução: {round(difftime(fim, inicio, units='mins'), 2)} minutos"))


# ##########################################
# ####      LEITURA ARQUIVOS           #####
# ##########################################
# inicio_script <- Sys.time()

# message(warn("Garanta que você está com os dados limpos em seu ambiente local"))

# # Defina o path de onde estão os arquivos de dados 
# message(note("Definindo o local onde estão os dados limpos e preparados para a análise..."))
# path <- paste0(data_path, "limpos" )

# # "Descubra" o nome de cada um dos arquivos
# files.nome <- as.list(list.files(path =  path, pattern = "*\\.csv$", ignore.case = TRUE, full.names = FALSE))

# # "Descrubra" o path para cada um dos arquivos de interesse
# files.path <- as.list(list.files(path = path, pattern = "*\\.csv$", ignore.case = TRUE, full.names = TRUE)) 

# # Renomear listas para usar posteriormente o nome do arquivo e não seu índice na lista (facilita manutenção caso add novos arquivos no path)
# names(files.path) <- files.nome
# names(files.nome) <- files.nome

# # Defina os nomes das variáveis e seus tipos 
# # No diretório /source/varnames edite o arquivo varnames_fread.R seguindo suas instruções
# # No diretório /source/vartypes edite o arquivo vartypes_fread.R seguindo suas instruções
# # Após isso, execute os scripts conforme abaixo para carregar os nomes e tipos das variáveis em ambiente
# message(note("Carregando o nome e tipo de cada variável declarado em varnames e vatypes..."))
# source(paste0(project_root_path, "/analysis/", project_name, "/sources/varnames/varnames_fread.R"), encoding = "utf-8")
# source(paste0(project_root_path, "/analysis/", project_name, "/sources/vartypes/vartypes_fread.R"), encoding = "utf-8")

# # Para facilitar o uso renomeamos as definições dos tipos e nomes das variáveis 
# names(dates) <- files.nome
# names(fator) <- files.nome
# names(char) <- files.nome
# names(id) <- files.nome
# names(continuas) <- files.nome
# names(colname) <- files.nome

# # Selecione quais ou todos os arquivos que quer fazer a leitura em conjunto:
# #   - NULL: caso queira todos arquivos do path 
# #   - list(files.path$`1987.csv`, files.path$`1991.csv`, files.path$`1998.csv`) 
# #   - list(files.nome$`1987.csv`, files.nome$`1991.csv`, files.nome$`1998.csv`)
# #   - list(colname$`1987.csv`, colname$`1991.csv`, colname$`1998.csv`)
# #   - list(dates$`1987.csv`, dates$`1991.csv`, dates$`1998.csv`) 
# #   - list(fator$`1987.csv`, fator$`1991.csv`, fator$`1998.csv`)

# interesse.path <-  NULL 
# interesse.name <- NULL 
# colname.interesse <- NULL 
# dates.interesse <- NULL 
# fator.interesse <- NULL 
# continuas.interesse <- NULL 

# # Configs para a leitura
# sep <- "," # separador de colunas
# dec <- "." # separador de decimal
# na.strings <- c(
#   "",
#   "-",
#   "NA",
#   "#N/A",
#   "#N/A!",
#   "#N/D",
#   "#DIV/0!",
#   "#NAME?",
#   "#NULL!",
#   "#NUM!",
#   "#REF!",
#   "#VALUE!"
# )

# # Execute a leitura. Atente-se para as opções da função fread. Use help(fread) para acessar o help.
# message(note("Executando a leitura dos arquivos ..."))
# inicio <- Sys.time()
# if (!is.null(interesse.path)) {
#   all.csvs <- lapply(1:length(interesse.path), function(i, files.path)
#     data.table::fread(
#       interesse.path[[i]],
#       skip = 1,
#       header = FALSE,
#       sep = sep,
#       dec = dec,
#       encoding = "UTF-8",
#       data.table = FALSE,
#       na.strings = na.strings,
#       showProgress = FALSE
#     ),
#     files.path = interesse.path)
# } else {
#   all.csvs <- lapply(1:length(files.path), function(i, files.path)
#     data.table::fread(
#       files.path[[i]],
#       skip = 1,
#       header = FALSE,
#       sep = sep,
#       dec = dec,
#       encoding = "UTF-8",
#       data.table = FALSE,
#       na.strings = na.strings,
#       showProgress = FALSE
#     ),
#     files.path = files.path)
# }
# fim <- Sys.time()
# tempo <- (as.numeric(fim) - as.numeric(inicio))/60
# message(runtime(paste("Foram necessários",round(tempo,2), "minutos para ler os arquivos.")))

# # Como declaramos nomes para variáveis anteriormente, esta epafa faz uso desses nomes para definir o nome de cada variável 
# message(note("Atribuindo o nome de cada variável em cada arquivo de dados..."))
# inicio <- Sys.time()
# if (!is.null(interesse.path)) {
#   for (i in 1:length(all.csvs)) {
#     data.table::setnames(all.csvs[[i]], colname.interesse[[i]])
#   }
# } else {
#   for (i in 1:length(all.csvs)) {
#     data.table::setnames(all.csvs[[i]], colname[[i]])
#   }
# }

# # renomear a lista com os nomes dos arquivos disponibilizados
# if (!is.null(interesse.path)) {
#   names(all.csvs) <- interesse.name
# } else {
#   names(all.csvs) <- files.nome
# }

# # Force com que variáveis de interesse tenham o tipo data
# message(note("Forçando que variáveis declaradas em vartypes como do tipo date tenham esse formato..."))
# for (i in 1:length(all.csvs)) {
#   if (!is.null(interesse.path)) {
#     if (length(dates.interesse[[i]]) == 0) {
#       next
#     } else {
#       for (j in 1:length(dates.interesse[[i]])) {
#         all.csvs[[i]][,dates.interesse[[i]][j]] <- as.Date(lubridate::fast_strptime(as.character(all.csvs[[i]][,dates.interesse[[i]][j]]), format = "%Y-%m-%d"))  
#       }       
#     }
#   } else {
#     if (length(dates[[i]]) == 0) {
#       next
#     } else {
#       for (j in 1:length(dates[[i]])) { 
#         all.csvs[[i]][,dates[[i]][j]] <- openxlsx::convertToDate(all.csvs[[i]][,dates[[i]][j]]) # Convertendo datas no formato numérico do Excel para Date do R
#       }
#     }
#   }
# }


# # Force com que variáveis de interesse tenham o tipo continuas
# message(note("Forçando que variáveis declaradas em vartypes como do tipo continuas tenham esse formato..."))
# for (i in 1:length(all.csvs)) {
#   if (!is.null(interesse.path)) {
#     if (length(continuas.interesse[[i]]) == 0) {
#       next
#     } else {
#       for (j in 1:length(continuas.interesse[[i]])) {
#         all.csvs[[i]][,continuas.interesse[[i]][j]] <- 
#           suppressWarnings(as.numeric(all.csvs[[i]][,continuas.interesse[[i]][j]]))
#       }       
#     }
#   } else {
#     if (length(continuas[[i]]) == 0) {
#       next
#     } else {
#       for (j in 1:length(continuas[[i]] )) { 
#         all.csvs[[i]][,continuas[[i]][j]] <- 
#           suppressWarnings(as.numeric(all.csvs[[i]][,continuas[[i]][j]]))
#       }
#     }
#   }
# }

# # Force com que variáveis de interesse tenham o tipo fator
# message(note("Forçando que variáveis declaradas em vartypes como do tipo fator tenham esse formato..."))
# for (i in 1:length(all.csvs)) {
#   if (!is.null(interesse.path)) {
#     if (length(fator.interesse[[i]]) == 0) {
#       next
#     } else {
#       for (j in 1:length(fator.interesse[[i]])) {
#         all.csvs[[i]][,fator.interesse[[i]][j]] <- formatC(all.csvs[[i]][,fator.interesse[[i]][j]])  
#       }       
#     }
#   } else {
#     if (length(fator[[i]]) == 0) {
#       next
#     } else {
#       for (j in 1:length(fator[[i]])) { 
#         all.csvs[[i]][,fator[[i]][j]] <- as.character(all.csvs[[i]][,fator[[i]][j]]) 
#       }
#     }
#   }
# }

# fim <- Sys.time()
# tempo <- (as.numeric(fim) - as.numeric(inicio))/60
# message(runtime(paste("Foram necessários",round(tempo,2), "minutos para atribuir nome e tipo de cada variável em cada arquivo de dados... ")))

# all.csvs <- lapply(all.csvs, tibble::as_tibble)

# message(note("Arquivos carregados para o ambiente"))

# fim_script <- Sys.time()

# tempo <- (as.numeric(fim_script) - as.numeric(inicio_script))/60
# message(runtime(paste("Foram necessários",round(tempo,2), "minutos para executar o script de loaddata.R.")))


