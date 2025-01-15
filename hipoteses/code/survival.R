########################################################################################################
#####       TURNOVER SURVIVAL - ANÁLISE DAS CURVAS E TEMPO DE SOBREVIVÊNCIA DOS COLABORADORES    #######
########################################################################################################
inicio_script <- Sys.time()

######
###  CONFIGURAÇÕES
######


# se ja foi gerado o arquivo survival.rds, não refaz o processo
# Caso queria rodar novamente é necessário apagar o arquivo survival.rds" do /outputs
message(note("Verificando se a análise de sobrevivência já foi executado anteriormente.."))
if (length(list.files(paste0(data_path, "outputs/"), pattern = "survival.rds")) == 0) {

# Lista que receberá os resultados de suvrival que serão disponibilizados em relatório
survival <- list()

# Iniciar a JVM do H2O no seu computador 
message(note("Iniciando o cluster H2O ..."))
h2o::h2o.init(max_mem_size = "6g")

# Garantir que a JVM do H2O não está com qualquer dado em memória antes de começar as análises
message(note("Removendo dados antigos do cluster H2O ..."))
h2o::h2o.removeAll()

######
###  DADOS 
######

# Dados já tratados anteriormente 
message(note("Fazendo a leitura dos dados que serão usados no algoritmo de sobrevivencia ..."))
hr_data_survival_raw_survival <- readRDS(paste0(data_path,"outputs/ml_data.rds"))

# Selecionar variáveis para o modelo 
message(note("Preparando dados (renomear e selecionar variáveis bem como apenas turnover voluntário) para o algoritmo ..."))
inicio <- Sys.time()

hr_data_survival <- hr_data_survival_raw_survival %>%
  dplyr::select(-c("salario_diario", "salario_hora", "taxa_mensal", 
                   "maior_idade", "horas_contratadas", "cont_emplid", 
                   "tempo_empresa_faixa", "qtd_emp_trabalhadas_faixa",
                   "salario"), turnover_voluntario = "turnover") %>%
  dplyr::mutate(turnover_voluntario = as.factor(turnover_voluntario)) %>%
  dplyr::mutate_if(is.character, as.factor)

## agrupamento treinamentos. Clocar no join depois, só tem que ver no que mexer

hr_data_survival <- hr_data_survival %>%
  dplyr::mutate(
    treinamentos_ultimo_ano = as.factor(dplyr::case_when(
      treinamentos_ultimo_ano == "0" ~ "Nenhum",
      treinamentos_ultimo_ano == "1" | 
        treinamentos_ultimo_ano == "2"    ~ "1 a 3",
      treinamentos_ultimo_ano == "3" | 
        treinamentos_ultimo_ano == "4"   ~ "3 a 5", 
      treinamentos_ultimo_ano == "5" |
        treinamentos_ultimo_ano == "6" ~ "mais de 6"))
  )

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para preparar os dados.")))

# Colocar os dados no h2o (http://docs.h2o.ai/h2o/latest-stable/h2o-docs/architecture.html)
message(note("Colocando os dados prontos para o algoritmo no H2O ..."))

inicio <- Sys.time()

hr_data_survival_h2o <- h2o::as.h2o(hr_data_survival %>% dplyr::select(-c("emplid")))


covariaveis <- 
  list("deptid" = "deptid",
       "cargo" = "cargo",
       "escolaridade" = "escolaridade",
       "formacao" = "formacao", 
       "freq_viagem" = "freq_viagem",
       "hora_extra" = "hora_extra", 
       "equilibrio_vida_profissional" = "equilibrio_vida_profissional",
       "grade" = "grade", 
       "treinamentos_ultimo_ano" = "treinamentos_ultimo_ano"
       )

# relevel das variáveis de interesse

hr_data_survival_h2o["cargo"] <- h2o::h2o.relevel(hr_data_survival_h2o["cargo"], "Sales Representative")
hr_data_survival_h2o["equilibrio_vida_profissional"] <-h2o::h2o.relevel(hr_data_survival_h2o["equilibrio_vida_profissional"], "Excelente")
hr_data_survival_h2o["formacao"] <-h2o::h2o.relevel(hr_data_survival_h2o["formacao"], "Marketing")
hr_data_survival_h2o["treinamentos_ultimo_ano"] <-h2o::h2o.relevel(hr_data_survival_h2o["treinamentos_ultimo_ano"], "Nenhum")
hr_data_survival_h2o["formacao"] <-h2o::h2o.relevel(hr_data_survival_h2o["formacao"], "Technical Degree")

# Cria um vetor para cada descrição de perfil


survival$model_survival <-
  sapply(names(covariaveis),
         function(x){
           h2o.coxph(x = paste(x), 
           event_column = "turnover_voluntario",
           stop_column = "tempo_empresa",
          training_frame = hr_data_survival_h2o) 
      })



# salvando coeficientes 

survival$summary <-lapply(survival$model_survival, summary)
survival$coef <- lapply(survival$summary, coef)  
survival$coef <- lapply(survival$coef, unclass)

# survival fit

new_data <- list(
  "deptid" = data.frame(deptid = c("Human Resources", "Research & Development", "Sales" )),
  "cargo" = data.frame(cargo = c("Sales Representative", "Healthcare Representative", "Human Resources", "Laboratory Technician", "Manager", "Manufacturing Director", "Manufacturing Director", "Research Director","Research Scientist", "Sales Executive" )),
  "escolaridade" = data.frame(escolaridade = c("Doutorado","Ensino Médio", "Mestrado", "Nível Superior", "Nível Técnico")),
  "formacao" = data.frame(formacao = c("Technical Degree","Human Resources","Life Sciences", "Marketing", "Medical", "Other")),
  "freq_viagem" = data.frame(freq_viagem = c("Não Viaja","Viaja com frequência", "Viaja raramente")),
  "hora_extra" = data.frame(hora_extra = c("Não","Sim")),
  "equilibrio_vida_profissional" = data.frame(equilibrio_vida_profissional = c("Excelente","Bom", "Ótimo", "Baixo")),
  "grade" = data.frame(grade = c("1","2","3", "4", "5")),
  "treinamentos_ultimo_ano" = data.frame(treinamentos_ultimo_ano = c("Nenhum", "1 a 3", "3 a 5", "mais de 6"))
  
  )

survival$survival_fit <- lapply(names(survival$model_survival), 
            function(y) {
              h2o::survfit.H2OCoxPHModel(
                survival$model_survival[[paste(y)]],
                newdata = (new_data[[paste(y)]])
              )}
            )

names(survival$survival_fit) <- names(survival$model_survival)

######
###  TABELAS E GRÁFICOS PARA O RELATÓRIO 
######

##
# Gráficos Cox Regression
##

# Departamento

survival$dept <- 
  plotly::plot_ly(x = survival$survival_fit$deptid$time, y = survival$survival_fit$deptid$surv[,2],  type = 'scatter', mode = "lines", name = "Pesquisa e Desenvolvimento", line = list(color =  aux$client_colors[1], dash = 'dot'), width = 500, height = 350) %>%
  plotly::add_trace(y = ~survival$survival_fit$deptid$surv[,1], name = "Recursos Humanos", mode = "lines", line = list(color = aux$client_colors[2], dash = 'dash')) %>%
  plotly::add_trace( y = ~survival$survival_fit$deptid$surv[,3], name = "Vendas", mode = "lines", line = list(color = aux$client_colors[3], dash = 'dot'))%>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5),  title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.3)) %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                                                                      "autoScale2d", "resetScale2d"))


# Cargo 

survival$cargo <- plotly::plot_ly(x = survival$survival_fit$cargo$time, y = survival$survival_fit$cargo$surv[,2],  type = 'scatter', mode = "lines", name = "Rep. Cuidados de Saúde", line = list(color =  aux$client_colors[1]), width = 500, height =400) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,1], name = "Representante de Vendas", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,3], name = "Recursos Humanos", mode = "lines", line = list(color = aux$client_colors[3])) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,4], name = "Técnico de Laboratório", mode = "lines", line = list(color = aux$client_colors[4])) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,5], name = "Gerente", mode = "lines", line = list(color = aux$client_colors[5])) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,6], name = "Diretor de Manufatura", mode = "lines", line = list(color = aux$client_colors[6])) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,7], name = "Diretor de Pesquisa", mode = "lines", line = list(color = aux$client_colors[7])) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,8], name = "Pesquisador", mode = "lines", line = list(color = aux$client_colors[8])) %>%
  plotly::add_trace(y = ~survival$survival_fit$cargo$surv[,9], name = "Excecutivo de Vendas", mode = "lines", line = list(color = aux$client_colors[9])) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.3)) %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

# Escolaridade

survival$escol <- plotly::plot_ly(x = survival$survival_fit$escolaridade$time, y = survival$survival_fit$escolaridade$surv[,2],  type = 'scatter', mode = "lines", name = "Ensino Médio", line = list(color =  aux$client_colors[1], dash = "dot"), width = 500, height = 350) %>%
  plotly::add_trace(y = ~survival$survival_fit$escolaridade$surv[,1], name = "Doutorado", mode = "lines", line = list(color = aux$client_colors[2], dash = 'dash')) %>%
  plotly::add_trace(y = ~survival$survival_fit$escolaridade$surv[,3], name = "Mestrado", mode = "lines", line = list(color = aux$client_colors[3], dash = 'dot')) %>%
  plotly::add_trace(y = ~survival$survival_fit$escolaridade$surv[,4], name = "Ensino Superior", mode = "lines", line = list(color = aux$client_colors[4])) %>%
  plotly::add_trace(y = ~survival$survival_fit$escolaridade$surv[,5], name = "Ensino Técnico", mode = "lines", line = list(color = aux$client_colors[5])) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores", 
         legend = list(orientation = 'h', y = -0.25))  %>%
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

# Formação

survival$formacao <- plotly::plot_ly(x = survival$survival_fit$formacao$time, y = survival$survival_fit$formacao$surv[,5],  type = 'scatter', mode = "lines", name = "Medicina", line = list(color =  aux$client_colors[1]), width = 500, height = 350) %>%
  plotly::add_trace(y = ~survival$survival_fit$formacao$surv[,1], name = "Grau Técnico", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::add_trace(y = ~survival$survival_fit$formacao$surv[,4], name = "Marketing", mode = "lines", line = list(color = aux$client_colors[4], dash = 'dot')) %>%
  plotly::add_trace(y = ~survival$survival_fit$formacao$surv[,3], name = "Ciências Biológicas", mode = "lines", line = list(color = aux$client_colors[3])) %>%
  plotly::add_trace(y = ~survival$survival_fit$formacao$surv[,6], name = "Outros", mode = "lines", line = list(color = aux$client_colors[5], dash = "dot")) %>%
  plotly::add_trace(y = ~survival$survival_fit$formacao$surv[,2], name = "Recursos Humanos", mode = "lines", line = list(color = aux$client_colors[6], dash = "dot")) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.25))  %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

# Frequência Viagens

survival$viagem <- plotly::plot_ly(x = survival$survival_fit$freq_viagem$time, y = survival$survival_fit$freq_viagem$surv[,2],  type = 'scatter', mode = "lines", name = "Viaja com Frequência", line = list(color =  aux$client_colors[1]), width = 500, height = 350) %>%
  plotly::add_trace(y = ~survival$survival_fit$freq_viagem$surv[,1], name = "Não Viaja", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::add_trace(y = ~survival$survival_fit$freq_viagem$surv[,3], name = "Viaja raramente", mode = "lines", line = list(color = aux$client_colors[3])) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.25))  %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

# Horas extras

survival$hrext <- plotly::plot_ly(x = survival$survival_fit$hora_extra$time, y = survival$survival_fit$hora_extra$surv[,2],  type = 'scatter', mode = "lines", name = "Sim", line = list(color =  aux$client_colors[1]), width = 500, height = 350) %>%
  plotly::add_trace(y = ~survival$survival_fit$hora_extra$surv[,1], name = "Não", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.25))  %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

# Equilíbrio vida profissional

survival$equil <- plotly::plot_ly(x = survival$survival_fit$equilibrio_vida_profissional$time, y = survival$survival_fit$equilibrio_vida_profissional$surv[,4],  type = 'scatter', mode = "lines", name = "Baixo", line = list(color =  aux$client_colors[1]), width = 500, height = 350) %>%
  plotly::add_trace(y = ~survival$survival_fit$equilibrio_vida_profissional$surv[,1], name = "Excelente", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::add_trace(y = ~survival$survival_fit$equilibrio_vida_profissional$surv[,3], name = "Bom", mode = "lines", line = list(color = aux$client_colors[3], dash = "dot")) %>%
  plotly::add_trace(y = ~survival$survival_fit$equilibrio_vida_profissional$surv[,2], name = "Ótimo", mode = "lines", line = list(color = aux$client_colors[4], dash = "dot")) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.25))  %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

# Treinamentos último anos

survival$treinam <- plotly::plot_ly(x = survival$survival_fit$treinamentos_ultimo_ano$time, y = survival$survival_fit$treinamentos_ultimo_ano$surv[,2],  type = 'scatter', mode = "lines", name = "1 a 2 treinamentos", line = list(color =  aux$client_colors[1]), width = 500, height = 400) %>%
  plotly::add_trace(y = ~survival$survival_fit$treinamentos_ultimo_ano$surv[,1], name = "Nenhum", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::add_trace(y = ~survival$survival_fit$treinamentos_ultimo_ano$surv[,3], name = "3 a 4 treinamentos", mode = "lines", line = list(color = aux$client_colors[3])) %>%
  plotly::add_trace(y = ~survival$survival_fit$treinamentos_ultimo_ano$surv[,4], name = "mais de 5 treinamentos", mode = "lines", line = list(color = aux$client_colors[4])) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.25))  %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

## Grade

survival$grade <- plotly::plot_ly(x = survival$survival_fit$grade$time, y = survival$survival_fit$grade$surv[,2],  type = 'scatter', mode = "lines", name = "Grade 2", line = list(color =  aux$client_colors[1]), width = 500, height = 400) %>%
  plotly::add_trace(y = ~survival$survival_fit$grade$surv[,1], name = "Grade 1", mode = "lines", line = list(color = aux$client_colors[2], dash = "dash")) %>%
  plotly::add_trace(y = ~survival$survival_fit$grade$surv[,3], name = "Grade 3", mode = "lines", line = list(color = aux$client_colors[3])) %>%
  plotly::add_trace(y = ~survival$survival_fit$grade$surv[,4], name = "Grade 4", mode = "lines", line = list(color = aux$client_colors[4])) %>%
  plotly::add_trace(y = ~survival$survival_fit$grade$surv[,5], name = "Grade 5", mode = "lines", line = list(color = aux$client_colors[5])) %>%
  plotly::layout(yaxis = list(range = c(0, 1), title = "% dos colab s/ turnover voluntário", tickformat = "%"), xaxis = list (title = "tempo na empresa (em anos)", dtick = 5), title = "Curvas de Sobrevida dos colaboradores",
         legend = list(orientation = 'h', y = -0.25))  %>% 
  kowr::clear_plotly_options(keep_logo = FALSE, buttons_to_keep = c("zoomIn2d", "zoomOut2d" , "toImage", "pan2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                                              "autoScale2d", "resetScale2d"))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para rodar o modelo de survival.")))


# Código survival para criar tabela que vai para o power BI

message(note("Criando e salvando objetos para utilizar no Power BI ..."))
inicio <- Sys.time()
powerbi_survival <- list()

powerbi_survival$cargo <-
  list(
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 2], "cargo" = "Rep. Cuidados de Saúde"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 1], "cargo" = "Representante de Vendas"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 3], "cargo" = "Recursos Humanos"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 4], "cargo" = "Técnico de Laboratório"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 5], "cargo" = "Gerente"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 6], "cargo" = "Diretor de Manufatura"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 7], "cargo" = "Diretor de Pesquisa"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 8], "cargo" = "Pesquisador"),
    tibble::tibble("tempo" = survival$survival_fit$cargo$time, "perc_colab" = survival$survival_fit$cargo$surv[, 9], "cargo" = "Excecutivo de Vendas")
  ) %>% dplyr::bind_rows()

powerbi_survival$dept <-
  list(
    tibble::tibble("tempo" = survival$survival_fit$dept$time, "perc_colab" = survival$survival_fit$dept$surv[, 2], "dept" = "Pesquisa e Desenvolvimento"),
    tibble::tibble("tempo" = survival$survival_fit$dept$time, "perc_colab" = survival$survival_fit$dept$surv[, 1], "dept" = "Recursos Humanos"),
    tibble::tibble("tempo" = survival$survival_fit$dept$time, "perc_colab" = survival$survival_fit$dept$surv[, 3], "dept" = "Vendas")
  ) %>% dplyr::bind_rows()

powerbi_survival$freq_viagem <-
  list(
    tibble::tibble("tempo" = survival$survival_fit$freq_viagem$time, "perc_colab" = survival$survival_fit$freq_viagem$surv[, 2], "freq_viagem" = "Viaja com Frequência"),
    tibble::tibble("tempo" = survival$survival_fit$freq_viagem$time, "perc_colab" = survival$survival_fit$freq_viagem$surv[, 1], "freq_viagem" = "Não Viaja"),
    tibble::tibble("tempo" = survival$survival_fit$freq_viagem$time, "perc_colab" = survival$survival_fit$freq_viagem$surv[, 3], "freq_viagem" = "Viaja raramente")
  ) %>% dplyr::bind_rows()

powerbi_survival$grade <-
  list(
    tibble::tibble("tempo" = survival$survival_fit$grade$time, "perc_colab" = survival$survival_fit$grade$surv[, 2], "grade" = "Grade 2"),
    tibble::tibble("tempo" = survival$survival_fit$grade$time, "perc_colab" = survival$survival_fit$grade$surv[, 1], "grade" = "Grade 1"),
    tibble::tibble("tempo" = survival$survival_fit$grade$time, "perc_colab" = survival$survival_fit$grade$surv[, 3], "grade" = "Grade 3"),
    tibble::tibble("tempo" = survival$survival_fit$grade$time, "perc_colab" = survival$survival_fit$grade$surv[, 4], "grade" = "Grade 4"),
    tibble::tibble("tempo" = survival$survival_fit$grade$time, "perc_colab" = survival$survival_fit$grade$surv[, 5], "grade" = "Grade 5")
  ) %>% dplyr::bind_rows()

powerbi_survival$hora_extra <-
  list(
    tibble::tibble("tempo" = survival$survival_fit$hora_extra$time, "perc_colab" = survival$survival_fit$hora_extra$surv[, 2], "hora_extra" = "Sim"),
    tibble::tibble("tempo" = survival$survival_fit$hora_extra$time, "perc_colab" = survival$survival_fit$hora_extra$surv[, 1], "hora_extra" = "Não")
  ) %>% dplyr::bind_rows() 

powerbi_survival$equilibrio <-
  list(
    tibble::tibble("tempo" = survival$survival_fit$equilibrio_vida_profissional$time, "perc_colab" = survival$survival_fit$equilibrio_vida_profissional$surv[, 4], "equilibrio_vida_profissional" = "Baixo"),
    tibble::tibble("tempo" = survival$survival_fit$equilibrio_vida_profissional$time, "perc_colab" = survival$survival_fit$equilibrio_vida_profissional$surv[, 1], "equilibrio_vida_profissional" = "Excelente"),
    tibble::tibble("tempo" = survival$survival_fit$equilibrio_vida_profissional$time, "perc_colab" = survival$survival_fit$equilibrio_vida_profissional$surv[, 3], "equilibrio_vida_profissional" = "Bom"),
    tibble::tibble("tempo" = survival$survival_fit$equilibrio_vida_profissional$time, "perc_colab" = survival$survival_fit$equilibrio_vida_profissional$surv[, 2], "equilibrio_vida_profissional" = "Ótimo")
     ) %>% dplyr::bind_rows() 




writexl::write_xlsx(powerbi_survival, paste0(data_path, "outputs/powerbi/pwrbi_survival.xlsx"))


### Salvando objetos para utilizar no PowerBi
# if(dir.exists(paste0(data_path, "outputs/powerbi")) == FALSE){
#   dir.create(paste0(data_path, "outputs/powerbi"))
# }

# Salvar cada df como um objeto separado
# lapply(ls(powerbi_survival),
#        function(x){
#          assign(paste(x), powerbi_survival[[paste(x)]], envir = .GlobalEnv) 
#        }
# )

# Salva todos objetos em RData
# save(list = ls(powerbi_survival), file = 
#        paste0(data_path, "outputs/powerbi/pwrbi_turnover_survival.RData"))

# # Remove os objetos criados
# rm(list = ls(powerbi_survival))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para criar e salvar os objetos para o Power BI.")))

## Salvar os resultados para uso no relatório
message(note("Salvando arquivo rds com todas as informações de previsões que serão usadas no relatório ..."))
inicio <- Sys.time()

saveRDS(survival, paste0(data_path, "outputs/survival.rds"), compress = FALSE)

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para salvar os dados preparados em survival.rds.")))

# Garantir que a JVM do H2O não vai manter com qualquer dado em memória.
message(note("Removendo dados do cluster H2O ..."))

h2o::h2o.removeAll()

## Desligando a instância do h2o
message(note("Desligando o cluster do H2O ..."))
h2o::h2o.shutdown(prompt = FALSE)

# Aguardando 15 segundos

message(note("Aguardando 15 segundos para encerrar todos os processos do h2o ..."))
Sys.sleep(15) # necessário pois executando o scritp pelo main o mesmo estava apresentando problemas quando iniciava o ml.

} else{
  message(note("O modelo de sobrevivência já foi executado anteriormente...Carregando o arquivo de dados..."))
  
  survival <- readr::read_rds(paste0(data_path, "outputs/survival.rds"))
}

fim_script <- Sys.time()

tempo <- (as.numeric(fim_script) - as.numeric(inicio_script))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para executar o script de survival.R.")))


