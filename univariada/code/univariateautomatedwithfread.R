#################################################################
#####          ANÁLISE EXPLORATÓRIA UNIVARIADA              #####
#################################################################
##########################

# Carrega função de load_data
source("sources/loaddata.R")

# Carrega os dados
inicio_script <- Sys.time()
message("Carregando dados...")
loaded_data <- load_data()

# Desempacota os objetos
all_csvs <- loaded_data$data
files.nome <- loaded_data$files.nome
var_types <- loaded_data$var_types

# Criando lista que irá receber objetos para irem para o relatório
univ <- list()

# Criando objeto auxiliar, a ser excluída após salvar RDS 
aux_univ <- list()

##########################################
####          DIMENSOES              #####
##########################################

#####
##  DESCOBRIR A QUANTIDADE DE COLUNAS EM CADA ARQUIVO
#####

# Antes de iniciar a análise é interessante verificar se a quantidade de variáveis presentes 
# em cada arquivo confere com a disponibilizada pelo cliente. Assim, fazemos a leitura automática
# de todos os arquivos e obtemos a quantidade de colunas presente em cada. 

message("Encontrando a quantidade de colunas e linhas em cada arquivo  ...")
inicio <- Sys.time()

univ$dimensoes <- 
  lapply(1:length(all_csvs), function(i) {
    Linhas <- dim(all_csvs[[i]])[1]
    Colunas <- dim(all_csvs[[i]])[2]
     base::data.frame(arquivo = files.nome[[i]],
               linhas = Linhas,
               colunas = Colunas) %>% 
      dplyr::mutate_if(is.factor, as.character) 
  }) %>% 
  dplyr::bind_rows() 

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para encontrar a quantidade de colunas e linhas do arquivo")))
##############################################
########       TIPO DE VARIÁVEL      #########
##############################################

#####
##  NUMÉRICAS
#####

# Faz um subset em cada data frame da lista separando apenas as variáveis que são numéricas
message("Separando as colunas de cada arquivo que são do tipo numéricas ...")
inicio <- Sys.time()

aux_univ$all.continuas <- lapply(1:length(all_csvs), function(i)
  tryCatch(dplyr::select(all_csvs[[i]], which(sapply(all_csvs[[i]], is.numeric))),  error = function(e) NULL))




# Faz os cálculos de interesse para as variáveis numéricas

# CORRETA
# message(note("Encontrando as estatísticas para as variáveis do tipo numéricas ..."))
# aux_univ$result.numericas <- lapply(1:length(aux_univ$all.continuas), function(i)
#   tryCatch(aux_univ$all.continuas[[i]] %>%
#              tidyr::pivot_longer(cols = everything(), names_to =  "var", values_to = "val") %>%
#              dplyr::group_by(var) %>%
#              dplyr::do({
#                qts = round(stats::quantile(.$val, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 2)
#                media = round(mean(.$val, na.rm = TRUE), 2)
#                arquivo = base::ifelse(test = !is.null(interesse.path), yes = interesse.name[[i]], no = files.nome[[i]])
#                na = sum(is.na(.$val))
#                na_perc = (na/length(.$val))*100
#                dp = sd(.$val, na.rm = TRUE)
#                amplitude = max(.$val, na.rm = TRUE) - min(.$val, na.rm = TRUE)
#                data.frame(arquivo, t(qts), media, na, na_perc, dp, amplitude)
#              }) %>%
#              dplyr::ungroup(), error = function(e) NULL))


message("Encontrando as estatísticas para as variáveis do tipo numéricas ...")
aux_univ$result.numericas <- lapply(1:length(aux_univ$all.continuas), function(i)
  tryCatch(aux_univ$all.continuas[[i]] %>%
             dplyr::select(dplyr::everything()) %>%
             tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'var', values_to = 'val') %>%
             #tidyr::gather('var', 'val') %>%
             dplyr::group_by(var) %>% 
             #dplyr::summarise(
             dplyr::do({
               qts = round(stats::quantile(.$val, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 2)#,
               #`0%` = round(stats::quantile(val, c(0), na.rm = TRUE), 2),
               #`25%` = round(stats::quantile(val, c(0.25), na.rm = TRUE), 2),
               #Mediana = round(stats::quantile(val, c(0.50), na.rm = TRUE), 2),
               #`75%` = round(stats::quantile(val, c(0.75), na.rm = TRUE), 2),
               #`100%` = round(stats::quantile(val, c(1), na.rm = TRUE), 2),
               media = round(mean(.$val, na.rm = TRUE), 2)#,
               arquivo = files.nome[[i]]#,
               na = sum(is.na(.$val))#,
               na_perc = (na/length(.$val))*100#,
               dp = sd(.$val, na.rm = TRUE)#,
               amplitude = max(.$val, na.rm = TRUE) - min(.$val, na.rm = TRUE)#,
               
               # Left Outlier
              # bp_l_out = min(boxplot(.$val, plot = FALSE)$out)
               # Left Whisker
               bp1 = graphics::boxplot(.$val, plot = FALSE)$stats[1]
               # Lower Quartile -0.7
               bp2 = graphics::boxplot(.$val, plot = FALSE)$stats[2]
               # Median -0.035
               bp3 = graphics::boxplot(.$val, plot = FALSE)$stats[3]
               # Upper Quartile 0.59
               bp4 = graphics::boxplot(.$val, plot = FALSE)$stats[4]
               # Right Whisker 2.51
               bp5 = graphics::boxplot(.$val, plot = FALSE)$stats[5]
               # Right Outlier 3.5
            #   bp_r_out = max(boxplot(.$val, plot = FALSE)$out)
               
               
               base::data.frame(arquivo, t(qts), media, na, na_perc, dp, amplitude, bp1, bp2, bp3, bp4, bp5)#, bp_l_out,bp_r_out, as.character(distribuicao))
             }) %>%
             #) %>% 
             dplyr::ungroup(), error = function(e) NULL))



# renomear a lista com os nomes dos arquivos disponibilizados
# if (!is.null(interesse.path)) {
#   names(aux_univ$result.numericas) <- gsub("\\.csv$", "", interesse.name)
# } else {
  names(aux_univ$result.numericas) <- gsub("\\.csv$", "", files.nome)
# }

# concatenando os resultados de cada data frame
univ$numericas <- dplyr::bind_rows(aux_univ$result.numericas)

# Renomeando o data frame
if (is.data.frame(univ$numericas) && nrow(univ$numericas) == 0) {
  message(warn("Você não tem nenhuma variável declarada como numérica ou idenfificada automaticamente com este tipo. Verifique as definições em /sources/vartypes/vartypes_fread.R e force variáveis de seu interesse a terem esse tipo..."))
} else {
  names(univ$numericas) <-
    c("Variavel", "Arquivo", "0%", "25%", "50%", "75%", "100%", "Media", 
      "Faltante", "Faltante (%)", "Desvio Padrão", "Amplitude",# "Distribuicao"
       "bp1", "bp2", "bp3", "bp4", "bp5") # , "bp_r_out" , "bp_l_out")
}

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
# message(runtime(paste("Foram necessários",round(tempo,2), "minutos para a análise das variáveis numéricas.")))

#####
##  CATEGÓRICAS
#####

# Faz um subset em cada data frame da lista separando apenas as variáveis que são categóricas
message("Separando as colunas de cada arquivo que são do tipo categórica ...")
inicio <- Sys.time()

aux_univ$all.categoricas  <-
  lapply(names(all_csvs),
         function(x) {
           tryCatch(
             all_csvs[[paste0(x)]] %>%  dplyr::select_if(is.character) %>% dplyr::select(-c(paste0(char[[x]]), paste0(id[[x]])))
             , error = function(e)
               NULL
           )
         })


# Faz os cálculos de interesse para as variáveis categóricas
message("Encontrando as estatísticas para as variáveis do tipo categóricas ...")
aux_univ$result.categoricas <- 
  lapply(1:length(aux_univ$all.categoricas), function(i)
    tryCatch(aux_univ$all.categoricas[[i]] %>%
               tidyr::pivot_longer(dplyr::everything(), names_to = 'var', values_to = 'value') %>%
      #tidyr::gather(aux_univ$all.categoricas[[i]], "var", "value") %>%
               dplyr::count(var, value) %>%
               dplyr::mutate(prop = prop.table(n)) %>%
               dplyr::mutate(arquivo = ifelse(!is.null(interesse.path), interesse.name[[i]], files.nome[[i]])),
             error = function(e) NULL))

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$result.categoricas) <- gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$result.categoricas) <- gsub("\\.csv$", "", files.nome)
}

# concatenando os resultados de cada data frame
univ$categoricas <- dplyr::bind_rows(aux_univ$result.categoricas)

# renomenado o data frame
if (is.data.frame(univ$categoricas ) && nrow(univ$categoricas )==0) {
  message(warn("Você não tem nenhuma variável declarada como categórica ou idenfificada automaticamente com este tipo. Verifique as definições em /sources/vartypes/vartypes_fread.R e force variáveis de seu interesse a terem esse tipo..."))
} else {
  names(univ$categoricas ) <- c("Variavel", "Fator", "N", "Proporcao", "Arquivo")
}

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para a análise das variáveis categóricas.")))


#####
##  ID'S
#####
# 
# # Faz um subset em cada data frame da lista separando apenas as variáveis que são categóricas
message("Separando as colunas de cada arquivo que são do tipo ID ...")
inicio <- Sys.time()

aux_univ$all.id  <-
  lapply(names(all_csvs),
         function(x) {
           tryCatch(
             all_csvs[[paste0(x)]] %>% dplyr::select(id[[paste0(x)]]),
             error = function(e)
               NULL
           )
         })


# Faz os cálculos de interesse para as variáveis categóricas
message("Encontrando as estatísticas para as variáveis do tipo ID ...")
aux_univ$result.id <- 
  lapply(1:length(aux_univ$all.id), function(i)
    tryCatch(aux_univ$all.id[[i]] %>%
               tidyr::pivot_longer(dplyr::everything(), names_to = "var",  values_to = "val") %>% 
               dplyr::group_by(var) %>% 
               dplyr::distinct() %>% 
               dplyr::summarise(n = dplyr::n(),
                                faltantes = sum(is.na(char)), .groups = 'drop'
                                #                  distinct = unique(val)
               ) %>% 
               dplyr::mutate(arquivo = ifelse(!is.null(interesse.path), interesse.name[[i]], files.nome[[i]])) %>% 
               dplyr::left_join(univ$dimensoes %>% dplyr::select(arquivo, linhas), by = "arquivo") %>% 
               dplyr::mutate(prop_falt = round(100*faltantes / linhas, 2)) 
             ,error = function(e) NULL)) 

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$result.id) <- gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$result.id) <- gsub("\\.csv$", "", files.nome)
}

univ$id <-
  aux_univ$result.id  %>%
  dplyr::bind_rows() %>%
  dplyr::select("Variável" = var, 
                "Qtde. de valores únicos" = n, 
                "Arquivo" = arquivo,
                "Linhas do arquivo" = linhas,
                "Linhas faltantes" = faltantes,
                "Prop. faltantes no arquivo (%)" = prop_falt 
  )


fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para a análise das variáveis do tipo ID")))



#####
##  CHAR
#####

# Faz um subset em cada data frame da lista separando apenas as variáveis que são categóricas
message("Separando as colunas de cada arquivo que são do tipo texto ...")
inicio <- Sys.time()

aux_univ$all.char  <-
  lapply(names(all_csvs),
         function(x) {
           tryCatch(
             
             all_csvs[[paste0(x)]] %>% dplyr::select(char[[paste0(x)]])
             ,
             error = function(e)
               NULL
           )
         })

# Faz os cálculos de interesse para as variáveis do tipo texto
message("Encontrando as estatísticas para as variáveis do tipo texto ...")

aux_univ$result.char <-
  lapply(1:length(aux_univ$all.char), function(i)
    tryCatch(aux_univ$all.char[[i]] %>%
               tidyr::pivot_longer(dplyr::everything(), names_to = "var",  values_to = "char") %>% 
               dplyr::mutate(nchar = stringr::str_length(char),
                             char_lt = ifelse(nchar <= 3, 1, 0)
               ) %>% 
               dplyr::group_by(var) %>% 
               dplyr::summarise(
                 max_chars = max(nchar, na.rm = TRUE),
                 min_chars = min(nchar, na.rm = TRUE),
                 median_chars = round(median(nchar, na.rm = TRUE), 2),
                 mean_chars = round(mean(nchar, na.rm = TRUE), 2),
                 sd_chars = round(sd(nchar, na.rm = TRUE), 2),
                 faltantes = sum(is.na(char)),
                 sum_char_lt = sum(char_lt, na.rm = TRUE),
                 .groups = 'drop'
               ) %>% 
               dplyr::mutate(arquivo = ifelse(!is.null(interesse.path), interesse.name[[i]], files.nome[[i]])) %>% 
               dplyr::left_join(univ$dimensoes %>% dplyr::select(arquivo, linhas), by = "arquivo") %>% 
               dplyr::mutate(prop_falt = round(100*faltantes / linhas, 2),
                             prop_char_lt = round(100*sum_char_lt/linhas, 2)
               )
             , error = function(e) NULL))

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$result.char) <- gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$result.char) <- gsub("\\.csv$", "", files.nome)
}

# concatenando os resultados de cada data frame
univ$char <- dplyr::bind_rows(aux_univ$result.char) 

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para a análise das variáveis tipo texto.")))


#####
##  DATAS
#####

# Faz um subset em cada data frame da lista separando apenas as variáveis que são datas
message("Separando as colunas de cada arquivo que são do tipo data ...")
inicio <- Sys.time()
aux_univ$all.datas <- lapply(1:length(all_csvs), function(i)
  tryCatch(dplyr::select(all_csvs[[i]], which(sapply(all_csvs[[i]], is.Date))), error = function(e) NULL))

# Faz os cálculos de interesse para as variáveis datas
message("Encontrando as estatísticas para as variáveis do tipo data ...")
aux_univ$result.datas <- 
  lapply(1:length(aux_univ$all.datas), function(i)
    tryCatch(tidyr::gather(aux_univ$all.datas[[i]], "var", "val") %>%
               dplyr::select(dplyr::everything()) %>%
               dplyr::group_by(var) %>% 
               dplyr::do({
                 min = min(.$val, na.rm = TRUE)
                 max = max(.$val, na.rm = TRUE)
                 arquivo = base::ifelse(!is.null(interesse.path), interesse.name[[i]], files.nome[[i]])
                 na = sum(is.na(.$val))
                 prop = round(100*sum(is.na(.$val))/NROW(.$val),2)
                 base::data.frame(arquivo, min, max, na, prop)
               }) %>% 
               dplyr::ungroup() %>% 
               dplyr::mutate_if(is.factor, as.character), error = function(e) NULL))

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$result.datas) <- gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$result.datas) <- gsub("\\.csv$", "", files.nome)
}

univ$datas <- aux_univ$result.datas %>% dplyr::bind_rows()

# renomeando o data frame
if (is.data.frame(univ$datas) && nrow(univ$datas)==0) {
  message(warn("Você não tem nenhuma variável declarada como date ou idenfificada automaticamente com este tipo. Verifique as definições em /sources/vartypes/vartypes_fread.R e force variáveis de seu interesse a terem esse tipo..."))
} else {
  names(univ$datas) <- c("Variavel", "Arquivo", "Data mais antiga", "Data mais recente", "Faltantes", "Proporção de Faltantes (%)"
  )
}

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para a análise das variáveis em formato de data.")))


##############################################
#######    GRÁFICOS - NUMÉRICAS     ##########
##############################################

message("Criando objeto com gráficos das análise exploratória univariada ...")
inicio <- Sys.time()
#####
##  Density chart
#####
aux_univ$density.list <-
  lapply(aux_univ$all.continuas, function(z)
    tryCatch(
      lapply(z, density_we),
      error = function(e)
        NULL
    ))

# is.null(aux_univ$result.numericas[[3]])

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$density.list) = gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$density.list) = gsub("\\.csv$", "", files.nome)
}

# Criando objeto unico para todos os graficos
univ$grid_density_numericas <- 
  lapply(
    aux_univ$density.list,
    profile_chart,
    rowheight = 200,
    ncol = 4,
    fontsize = "1em",
    js_tooltip_num = "function(){return  'Densidade (%): ' + Highcharts.numberFormat(this.y, 4);}"
  )

# Preparar gráfico para relatório
univ$chart_grid_density <- 
 htmltools::tagList(lapply(names(univ$grid_density_numericas),
                 function(x) {
                   list(tags$h5(paste("Arquivo -", x)),
                        tags$p(
                          if(is.null(aux_univ$result.numericas[[x]])) (paste0("Não há variáveis numéricas neste arquivo."))
                        ),
                        tags$div(univ$grid_density_numericas[x]))
                 }))


#####
##  Boxplot
#####

aux$box_plot_outliers = FALSE # TRUE mantém outliers, FALSE tira

aux_univ$boxplot.list <-
  lapply(aux_univ$all.continuas, function(z)
    tryCatch(
      lapply(z, boxplot_we, outliers = aux$box_plot_outliers),
      error = function(e)
        NULL
    ))

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$boxplot.list) = gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$boxplot.list) = gsub("\\.csv$", "", files.nome)
}


univ$grid_boxplot_numericas <- lapply(aux_univ$boxplot.list, boxplot_chart, ncol = 4)


univ$chart_grid_boxplot <- 
  htmltools::tagList(lapply(names(univ$grid_boxplot_numericas),
                 function(x) {
                   list(tags$h5(paste("Arquivo -", x)),
                        tags$p(
                          if(is.null(aux_univ$result.numericas[[x]])) (paste0("Não há variáveis numéricas neste arquivo."))
                        ),
                        tags$div(univ$grid_boxplot_numericas[x]))
                 }))

##############################################
#######    GRÁFICOS - CATEGÓRICAS   ##########
##############################################

#####
##  Histogramas
#####


aux$many_levels_n <- 100

aux$many_levels_cols <- 
  (univ$categoricas %>% 
     dplyr::group_by(Arquivo, Variavel) %>% 
     dplyr::tally() %>% 
     dplyr::filter(n >= aux$many_levels_n) %>% 
     dplyr::ungroup() %>% 
     dplyr::select(Variavel) %>% 
     dplyr::distinct())$Variavel



aux_univ$cat.hist.list <-
  aux_univ$all.categoricas %>% 
  # Retirando dos gráficos colunas com muitos níveis
  lapply(function(x){ 
    dplyr::select(x, - c (names(x)[names(x) %in% aux$many_levels_cols]))
  })

# renomear a lista com os nomes dos arquivos disponibilizados
if (!is.null(interesse.path)) {
  names(aux_univ$cat.hist.list) = gsub("\\.csv$", "", interesse.name)
} else {
  names(aux_univ$cat.hist.list) = gsub("\\.csv$", "", files.nome)
}

# Criando objeto unico para todos os graficos e deixando de lado variáveis que só tem NA's
univ$grid_hist_categoricas <- 
  lapply(
    lapply(aux_univ$cat.hist.list, function(x) {
      x %>% dplyr::select_if(function(y)
        all(!is.na(y)))
    }),
    #aux_univ$cat.hist.list,
    profile_chart,
    rowheight = 220,
    ncol = 3,
    #colors = aux$client_colors[2],
    fontsize = "1em"
  )

# Preparar gráfico para relatório
univ$chart_grid_hist_categoricas <- 
  htmltools::tagList(lapply(names(univ$grid_hist_categoricas),
                 function(x) {
                   list(tags$h5(paste("Arquivo -", x)),
                        tags$p(
                          if(is.null(aux_univ$result.categoricas[[x]])) (paste0("Não há variáveis categóricas neste arquivo."))
                        ),
                        tags$div(univ$grid_hist_categoricas[x]))
                 }))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para criar objetos com gráficos das análises exploratórias.")))


##############################################
#######    SALVAR RDS PARA RELATÓRIO    ######
##############################################

# Removendo objeto auxiliar 
message("Limpando objetos que não serão mais utilizados")
inicio <- Sys.time()

rm(aux_univ)

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para remover objetos que não serão mais utilizados.")))

message("Exportando resultados da análise exploratória univariada para usá-los no relatório...")
inicio <- Sys.time()

saveRDS(univ, paste0(data_path, "/outputs/univariada.rds"))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para exportar resultados da análise univariada.")))

fim_script <- Sys.time()

tempo <- (as.numeric(fim_script) - as.numeric(inicio_script))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para executar o script da análise univariada.")))
