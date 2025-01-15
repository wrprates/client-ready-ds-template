##############################################
#####   DADOS E GRÁFICOS P/ HIPÓTESES  #######
##############################################
inicio_script <- Sys.time()

######
###  CONFIGURAÇÕES
######

# Lendo os dados já tratados em script
dfs <- list()
powerbi <- list()

# Lê direto o output gerado pelo join. Usar no caso de testes.
message(note("Carregando dados necessários para os testes de hipóteses e criando lista para receber os gráficos e tabelas das hipóteses ..."))
dfs <- readRDS(paste0(data_path,"outputs/dfs_join.rds"))


#####
### Resumo Geral - Utilizado para inicio do relatório
#####

message(note("Criando gráficos e tabelas para "), crayon::cyan$underline$bold("informações gerais"), note(" ..."))
inicio <- Sys.time()

### Tabela com a quantidade e proporção do tipo de turnover
aux$geral_turnover <- 
  dfs$dados_pessoais_last_info %>% 
  dplyr::select(emplid, turnover) %>% 
  dplyr::group_by(turnover) %>%
  dplyr::summarise (n = dplyr::n(),  .groups = 'drop') %>%
  dplyr::mutate(proporcao = paste0(round(100 * n/sum(n), 2), "%"))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para criar gráficos e tabelas de informações gerais.")))

######
###  H. 1 - Perfil demográfico dos colaboradores que tem maior incidência de Turnover voluntário
######

# Análise: Obter o perfil demográfico dos colaboradores que tem maior incidência de *Turnover* voluntário. <br/>
# Premissa: Considerar apenas os colaboradores que tiveram *Turnover* voluntário.

message(note("Criando gráficos e tabelas para a "), crayon::cyan$underline$bold("hipótese 1 de perfil dos colaboradores que geraram turnover"), note(" ..."))
inicio <- Sys.time()

aux$h1$df_perfil <- 
  dfs$dados_pessoais_last_info %>% 
  dplyr::select(turnover, idade, escolaridade,"formação" = formacao, "distância casa e trabalho" = distancia, genero, "estado civil" = estado_civil, 
                "posição na faixa" = posicao_faixa, "qtde de empresas trabalhadas" = qtd_emp_trabalhadas_faixa, "anos de experiência" = anos_experiencia,
                "tempo de empresa" = tempo_empresa_faixa) 

aux$h1$chart_perfil_turn <- profile_chart(aux$h1$df_perfil %>%
                                              dplyr::filter(turnover == 1) %>%
                                              dplyr::select(-c("turnover")))

aux$h1$chart_perfil_ativos <- profile_chart(aux$h1$df_perfil %>%
                                              dplyr::filter(turnover == 0) %>%
                                              dplyr::select(-c("turnover")))


message(note("Rodando análise de correlação para a "), crayon::cyan$underline$bold("hipótese 1 de perfil dos colaboradores que geraram turnover"), note(" ..."))


## Correlação entre variáveis
####
## Categóricas

aux$h1$df_cor_char_ativos <-  
  dfs$dados_pessoais_last_info %>%
  dplyr::select_if(is.character) %>%
  dplyr::bind_cols(dfs$dados_pessoais_last_info %>% dplyr::select(turnover)) %>% 
  dplyr::filter(turnover == 0) %>% 
  dplyr::select(-c("turnover", "emplid", "maior_idade"))

aux$h1$df_cor_char_deslig <- 
  dfs$dados_pessoais_last_info %>%
  dplyr::select_if(is.character) %>%
  dplyr::bind_cols(dfs$dados_pessoais_last_info %>% dplyr::select(turnover)) %>% 
  dplyr::filter(turnover == 1) %>% 
  dplyr::select(-c("turnover", "emplid", "maior_idade"))

# calculando Cramer's V matrix

aux$h1$mat_cramer_ativos <- cramer_mat(aux$h1$df_cor_char_ativos)
aux$h1$mat_cramer_deslig <- cramer_mat(aux$h1$df_cor_char_deslig)

aux$h1$gt_pos_cor = .1
aux$h1$lt_neg_cor = -.1

aux$h1$chart_cramerv_ativos <-
  hchart.cor(
    aux$h1$mat_cramer_ativos %>% dplyr::select(-var),
    diagonal = 1,
    min.color.axis = 0,
    gt_pos_cor = aux$h1$gt_pos_cor, # greater than
    lt_neg_cor = aux$h1$lt_neg_cor, # less than
    cor_colr =
      list(list(0, '#F8F5F5'),
           list(1, '#2E86C1'))
  )

aux$h1$chart_cramerv_deslig <-
  hchart.cor(
    aux$h1$mat_cramer_deslig %>% dplyr::select(-var),
    diagonal = 1,
    min.color.axis = 0,
    gt_pos_cor = aux$h1$gt_pos_cor, # greater than
    lt_neg_cor = aux$h1$lt_neg_cor, # less than
    cor_colr =
      list(list(0, '#F8F5F5'),
           list(1, '#2E86C1'))
  )

## Numéricas
aux$h1$df_cor_num_ativos <-
  dfs$dados_pessoais_last_info %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::filter(turnover == 0) %>% 
  dplyr::select(-c("cont_emplid", "turnover"))


aux$h1$df_cor_num_deslig <-
  dfs$dados_pessoais_last_info %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::filter(turnover == 1) %>% 
  dplyr::select(-c("cont_emplid", "turnover"))

aux$h1$mat_corr_ativos <- 
  suppressWarnings(cor(aux$h1$df_cor_num_ativos, method = "spearman")) # Warning

aux$h1$mat_corr_deslig <- 
  suppressWarnings(cor(aux$h1$df_cor_num_deslig, method = "spearman")) # Warning

aux$h1$chart_cor_num_ativos <-
  hchart.cor(aux$h1$mat_corr_ativos,
             diagonal = 1,
             gt_pos_cor = aux$h1$gt_pos_cor, # greater than
             lt_neg_cor = aux$h1$lt_neg_cor, # less than
             )

aux$h1$chart_cor_num_deslig <-
  hchart.cor(aux$h1$mat_corr_deslig,
             diagonal = 1,
             gt_pos_cor = aux$h1$gt_pos_cor, # greater than
             lt_neg_cor = aux$h1$lt_neg_cor, # less than
  )

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para realizar os cáculos, criar gráficos e tabelas da hipótese 1.")))

######
###  H. 3 - Colaboradores com menor percentual na faixa têm maior índice de *turnover* 
######

message(note("Criando gráficos e tabelas para a "), crayon::cyan$underline$bold("hipótese 3 de relação entre turnover voluntário e posição na faixa"), note(" ..."))
inicio <- Sys.time()

aux$h3$df <- 
dfs$dados_pessoais_last_info %>% 
  dplyr::select(emplid, turnover, posicao_faixa, deptid)

aux$h3$tab_medias <- 
  aux$h3$df %>% 
  dplyr::group_by(turnover) %>% 
  dplyr::summarise(posicao_faixa = round(mean(posicao_faixa),2), .groups = "drop")

# Box plot não demonstoru muita variação, mas o resultado do teste foi singificativo. Dar mais ênfase para o gráfico de densidade.  
aux$h3$chart_box_plot <- 
  # hcboxplot2(
  highcharter::hcboxplot(
    x = aux$h3$df$posicao_faixa,
    var =  aux$h3$df$turnover,
    outliers = FALSE) %>%
  highcharter::hc_chart(type = "column") %>%  # to put box vertical
  highcharter::hc_size(width = NULL, height = 400) %>%
  highcharter::hc_title(text = "Turnover voluntário e posição na faixa") %>%
  highcharter::hc_subtitle(text = "(Ativos = 0; Turnover = 1))") %>%
  highcharter::hc_yAxis(title = list(text = "Posição na faixa")) %>%
  highcharter::hc_colors(aux$client_colors)

# Gráfico de densidade
aux$h3$chart_density <- 
  highcharter::hchart(
  density_we((dfs$dados_pessoais_last_info %>%  filter(turnover == 1))$posicao_faixa),
  name = "Turnover") %>% 
  highcharter::hc_add_series(  density_we((dfs$dados_pessoais_last_info %>%  dplyr::filter(turnover == 0))$posicao_faixa),
    type = "area", name = "Ativos")  %>% 
  highcharter::hc_size(width = NULL, height = 450) %>% 
  highcharter::hc_title(text = "Turnover voluntário e posição na faixa (%)") %>%
  highcharter::hc_yAxis(title = list(text = "Densidade")) %>%
  highcharter::hc_xAxis(title = list(text = "Posição na faixa")) %>%
  highcharter::hc_colors(aux$ey_colors)


# Teste estatístico
aux$h3$teste_wilcox <-
  stats::wilcox.test(
    # Posição na faixa para turnover voluntário
    (dfs$dados_pessoais_last_info %>%  dplyr::filter(turnover == 1))$posicao_faixa,

    # Posição na faixa para ativos
    (dfs$dados_pessoais_last_info %>%  dplyr::filter(turnover == 0))$posicao_faixa,
    alternative = "less"
  )

# Acessar o p-valor 
# aux$h3$teste_wilcox$p.value 

# Chamando arquivo com Chord Diagram
#source(paste0(project_root_path,"/analysis/", project_name, "/templates/ae/hipoteses/code/grafico_perfil.R"), encoding = encoding)

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para realizar os cáculos, criar gráficos e tabelas da hipótese 3.")))

######
###  H. 4 - Há maior turnover voluntário em colaboradores com mais tempo no cargo 
######

# Análise: Comparar o tempo na posição entre colaboradores que se desligaram voluntariamente e colaboradores ativos
# Premissa: O tempo na posição será calculado baseado no tempo no mesmo jobcode (posição) 

message(note("Criando gráficos e tabelas para a "), crayon::cyan$underline$bold("hipótese 4 de relação entre turnover voluntário e tempo na posição"), note(" ..."))
inicio <- Sys.time()

aux$h4$df <- 
dfs$dados_pessoais_last_info %>% 
  dplyr::select(emplid, tempo_cargo, turnover, deptid)

aux$h4$tab_medias <- 
  aux$h4$df %>% 
  dplyr::group_by(turnover) %>% 
  dplyr::summarise(mean_tempo_cargo = round(mean(tempo_cargo),1), .groups = "drop")

# Box plot não demonstoru muita variação, mas o resultado do teste foi singificativo. Dar mais ênfase para o gráfico de densidade.
aux$h4$chart_box_plot <-
  # hcboxplot2(
  highcharter::hcboxplot(
    x = aux$h4$df$tempo_cargo,
    var =  aux$h4$df$turnover,
    outliers = FALSE) %>%
  highcharter::hc_chart(type = "column") %>%  # to put box vertical
  highcharter::hc_size(width = NULL, height = 400) %>% 
  highcharter::hc_title(text = "Turnover voluntário e tempo no cargo") %>%
  highcharter::hc_subtitle(text = "(Ativos = 0; Turnover = 1)") %>%
  highcharter::hc_yAxis(title = list(text = "Tempo no cargo (em anos)")) %>%
  highcharter::hc_colors(aux$client_colors)

# Gráfico de densidade
aux$h4$chart_density <-
  highcharter::hchart(
    density_we((dfs$dados_pessoais_last_info %>%  dplyr::filter(turnover == 1))$tempo_cargo), 
    name = "Turnover") %>%
  hc_add_series(density_we((dfs$dados_pessoais_last_info %>%  dplyr::filter(turnover == 0))$tempo_cargo), type = "area", name = "Ativos")  %>%
  highcharter::hc_size(width = NULL, height = 400) %>% 
  highcharter::hc_title(text = "Turnover voluntário e tempo no cargo") %>%
  highcharter::hc_yAxis(title = list(text = "Densidade")) %>%
  highcharter::hc_xAxis(title = list(text = "Tempo no cargo", min = 0)) %>%
  highcharter::hc_colors(aux$ey_colors)

# Teste estatístico
aux$h4$teste_wilcox <-
  stats::wilcox.test(
    # Posição na faixa para turnover voluntário
    (dfs$dados_pessoais_last_info %>%  filter(turnover == 1))$tempo_cargo,

    # Posição na faixa para ativos
    (dfs$dados_pessoais_last_info %>%  filter(turnover == 0))$tempo_cargo,
    alternative = "less"
  )

# Acessar o p-valor
# aux$h4$teste_wilcox$p.value

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para realizar os cáculos, criar gráficos e tabelas da hipótese 4.")))

######
###  H. 5 - Quando há menor percentual de aumento na gestão de consequência o turnover voluntário é maior
######
# - Dividir a gestão de consequência em quartis
# - Comparar o primeiro e último quartis

message(note("Criando gráficos e tabelas para a "), crayon::cyan$underline$bold("hipótese 5 sobre percentual na gestão de consequência e turnover voluntário"), note(" ..."))
inicio <- Sys.time()

aux$h5$tab_quartil <-
  stats::quantile(dfs$dados_pessoais_last_info$percent_gest_conseq,
                probs = seq(0,1,.20)) %>%
  tibble::enframe(name = NULL) %>% 
  dplyr::mutate(quantil = paste0("Q", seq(1:dplyr::n())-1)) %>% 
  dplyr::rename("corte_quantil" = "value")

aux$h5$df <- 
  dfs$dados_pessoais_last_info %>% 
    dplyr::select(emplid, turnover, percent_gest_conseq) %>% 
    dplyr::mutate(baixa_gest_conseq = as.factor(dplyr::if_else(
  condition = percent_gest_conseq <= (aux$h5$tab_quartil %>% dplyr::filter(quantil == "Q2"))$corte_quantil, 
  true = 1, false = 0))) %>% 
    dplyr::mutate(alta_gest_conseq = as.factor(dplyr::if_else(
      condition = percent_gest_conseq >= (aux$h5$tab_quartil %>% dplyr::filter(quantil == "Q4"))$corte_quantil, 
      true = 1, false = 0)))

aux$h5$tab_baixa_gest_conseq <- 
  aux$h5$df %>% 
  dplyr::filter(baixa_gest_conseq == 1) %>% 
  dplyr::group_by(turnover) %>% 
  dplyr::tally() %>% 
  dplyr::mutate(total = n/sum(n),
                "Proporção" =
                    sparkline::spk_chr(
                    sparkline_data = c(n, turnover),
                    values = n,
                    type = 'pie',
                    tooltipFormat = '{{offset:offset}} ({{percent.2}}%)',
                    tooltipValueLookups = htmlwidgets::JS("{offset: {'0': 'Com turnover', '1': 'Sem turnover' }}"),
                    borderColor = aux$ey_colors[2], 
                    borderWidth = 1,
                    tooltipoFormat = spk_tool(c('0', '1', n)),
                    fillColor = "#ffffff",
                    lineColor = "#ffffff",
                    sliceColors = c (aux$ey_colors[1], aux$ey_colors[2])))





aux$h5$tab_alta_gest_conseq <- 
  aux$h5$df %>% 
  dplyr::filter(alta_gest_conseq == 1) %>% 
  dplyr::group_by(turnover) %>% 
  dplyr::tally() %>% 
  dplyr::mutate(total = n/sum(n), 
                "Proporção" =
                  sparkline::spk_chr(
                  sparkline_data = c(n, turnover),
                  values = n,
                  type = 'pie',
                  tooltipFormat = '{{offset:offset}} ({{percent.2}}%)',
                  tooltipValueLookups = htmlwidgets::JS("{offset: {'0': 'Com turnover', '1': 'Sem turnover' }}"),
                  borderColor = aux$ey_colors[2], 
                  borderWidth = 1, 
                  tooltipoFormat = spk_tool(c('0', '1', n)),
                  fillColor = "#ffffff",
                  lineColor = "#ffffff",
                  sliceColors = c (aux$ey_colors[1], aux$ey_colors[2])))

aux$h5$tab_report <-
  aux$h5$tab_baixa_gest_conseq  %>% 
  dplyr::mutate(Total = sum(n)) %>% 
  dplyr::filter(turnover == 1) %>%
  dplyr::rename("prop" = total) %>% 
  dplyr::mutate(aval = "Menor percentual") %>%
  dplyr::select(aval, n, Total, prop, `Proporção`) %>% dplyr::bind_rows(
    aux$h5$tab_alta_gest_conseq  %>%  
      dplyr::mutate(Total = sum(n)) %>% 
      dplyr::filter(turnover == 1) %>%
      dplyr::rename("prop" = total) %>% 
      dplyr::mutate(aval = "Maior percentual") %>%
      dplyr::select(aval, n, Total, prop, `Proporção`) 
  ) %>% 
  dplyr::mutate(prop = round(100*prop, 2)) %>% 
  dplyr::rename("Gestão consequência" = aval,
                "Qtde. de colaboradores (matrícula)" = n,
                "Proporção de turnover (%)" = prop, 
                "Proporção" = `Proporção`)

aux$h5$teste_prop <- 
  stats::prop.test(c((aux$h5$tab_baixa_gest_conseq %>% dplyr::filter(turnover == 1))$n, 
              (aux$h5$tab_alta_gest_conseq %>% dplyr::filter(turnover == 1))$n),
            c(sum((aux$h5$tab_baixa_gest_conseq)$n),
              sum((aux$h5$tab_alta_gest_conseq)$n)))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para realizar os cáculos, criar gráficos e tabelas da hipótese 5.")))

######
###  H. 6 - Colaboradores que fazem horas extras tendem a sair mais da companhia
######

message(note("Criando gráficos e tabelas para a "), crayon::cyan$underline$bold("hipótese 6 sobre turnover voluntário e horas extras"), note(" ..."))
inicio <- Sys.time()

aux$h6$df <- 
  dfs$dados_pessoais_last_info %>%
  dplyr::select(emplid, turnover, hora_extra) %>% 
  dplyr::left_join(dfs$dados_pessoais_last_info %>% dplyr::select(emplid, deptid), by = "emplid")

aux$h6$tab_prop <-
  aux$h6$df %>%
  dplyr::group_by(hora_extra) %>%
    dplyr::summarise(
      count_turnover = sum(turnover),
      n = dplyr::n(), 
      distribuicao =
        sparkline::spk_chr(
          values = c(count_turnover, n),
          type = 'pie',
          tooltipFormat = '{{offset:offset}} ({{percent.2}}%)',
          tooltipValueLookups = htmlwidgets::JS("{offset: {'0': 'Com turnover', '1': 'Sem turnover' }}"),
          defaultOptions = list(margin = c(0,0,0,5)),
          borderColor = aux$ey_colors[2], 
          borderWidth = 1, 
          fillColor = "#ffffff",
          lineColor = "#ffffff",
          sliceColors = c (aux$ey_colors[1], aux$ey_colors[2])), 
      .groups = "drop") %>% 
  dplyr::mutate(prop = count_turnover/(n))

aux$h6$tab_prop_deptid <-
  aux$h6$df %>% 
  dplyr::group_by(hora_extra, deptid) %>%
  dplyr::summarise(
    count_turnover = sum(turnover),
    n = dplyr::n(), .groups = "drop"
  ) %>% 
  dplyr::mutate(prop = 100*count_turnover/(n))


# Teste de proporções
aux$h6$prop_test <-
  prop.test(c(aux$h6$tab_prop$count_turnover),
            c(aux$h6$tab_prop$n))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para realizar os cáculos, criar gráficos e tabelas da hipótese 6.")))

######
###  H. 7 - Há um maior índice de turnover voluntário em colaboradores que precisam viajar a trabalho.
######

message(note("Criando gráficos e tabelas para a "), crayon::cyan$underline$bold("hipótese 7 sobre turnover voluntário e frequência de viagens"), note(" ..."))
inicio <- Sys.time()

aux$h7$df <-
  dfs$dados_pessoais_last_info %>%
  dplyr::select(emplid, turnover, freq_viagem, deptid) 

### 
# Proporções de turnover por deptid

# Porporção Geral
aux$h7$tab_prop <-  
  aux$h7$df %>% 
  dplyr::group_by(freq_viagem) %>%
  dplyr::summarise(
    count_turnover = sum(turnover),
    n = dplyr::n(),
    distribuicao =
      sparkline::spk_chr(
        values = c(count_turnover, n),
        type = 'pie',
         tooltipFormat = '{{offset:offset}} ({{percent.2}}%)',
         tooltipValueLookups = htmlwidgets::JS("{offset: {'0': 'Com turnover', '1': 'Sem turnover' }}"),
        borderColor = aux$ey_colors[2], 
        borderWidth = 1, 
        fillColor = "#ffffff",
        lineColor = "#ffffff",
        sliceColors = c (aux$ey_colors[1], aux$ey_colors[2])), 
    .groups = "drop") %>% 
  dplyr::mutate(prop = 100*(count_turnover/(n+ count_turnover))) %>% 
  dplyr::arrange(prop)

# Todas as proporções por deptid, mas sem filtros
aux$h7$df_prop_deptid <- 
  aux$h7$df %>% 
  dplyr::group_by(freq_viagem, deptid) %>%
  dplyr::summarise(
    count_turnover = sum(turnover),
    n = dplyr::n(), 
    distribuicao =
      sparkline::spk_chr(
        values = c(count_turnover, n),
        type = 'pie',
        borderColor = aux$ey_colors[2], 
        borderWidth = 1, 
        fillColor = "#ffffff",
        lineColor = "#ffffff",
        sliceColors = c (aux$ey_colors[1], aux$ey_colors[2])), .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop = 100*(count_turnover/(n))) %>% 
  dplyr::arrange(prop)
  
# Porporção Sales
aux$h7$tab_prop_sales <-  
  aux$h7$df_prop_deptid %>% 
  dplyr::filter(deptid == "Sales") %>% 
  # dplyr::select(-deptid)
  dplyr::select(freq_viagem, count_turnover, n, prop, distribuicao) # usando o select para mudar ordem das colunas

# Porporção Research & Development
aux$h7$tab_prop_rnd <-  
  aux$h7$df_prop_deptid %>% 
  dplyr::filter(deptid == "Research & Development") %>% 
# dplyr::select(-deptid)
  dplyr::select(freq_viagem, count_turnover, n, prop, distribuicao) # usando o select para mudar ordem das colunas

# Porporção Human Resources
aux$h7$tab_prop_hr <-  
  aux$h7$df_prop_deptid %>% 
  dplyr::filter(deptid == "Human Resources") %>% 
 # dplyr::select(-deptid)
  dplyr::select(freq_viagem, count_turnover, n, prop, distribuicao) # usando o select para mudar ordem das colunas


## Criando gráfico
## Este gráfico está com a seguinte mensagem de Warning (ficar atento, pois deve ser resolvido em novas versoes do highcharter
# Warning message:
#   `parse_quosure()` is deprecated as of rlang 0.2.0.
# Please use `parse_quo()` instead.
aux$h7$chart_freq_viagem <- 
  suppressWarnings( # Warning por função do highcharter.
    hchart(
    #aux$h7$tab_prop %>% 
    aux$h7$df_prop_deptid %>% 
      dplyr::rename("Frequência de viagens" = freq_viagem, "Proporção de turnover" = prop),
    type = "column",
    highcharter::hcaes(x = `Frequência de viagens`, y = `Proporção de turnover`, group = "deptid"),
    showInLegend = TRUE,
    animation = TRUE
  )) %>%
  highcharter::hc_tooltip(formatter = 
                            htmlwidgets::JS(
                 "function(){return  '<b>Prop. de turnover: ' + Highcharts.numberFormat(this.y) + '%</b> <br><hr>';}"
               ), useHTML = FALSE) %>% 
  highcharter::hc_yAxis(max = max(aux$h7$df_prop_deptid$prop)) %>% 
  highcharter::hc_title(text = "Proporção de turnover voluntário por frequência de viagem") %>%
  highcharter::hc_size(height = 380) %>%
  highcharter::hc_colors(aux$ey_colors) %>%
  highcharter::hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "horizontal")

# Testes de proporções
#####
# Geral
aux$h7$prop_test <-
  prop.test(c(aux$h7$tab_prop$count_turnover),
            c(aux$h7$tab_prop$n))

aux$h7$prop_trend_test <-
  prop.trend.test(c(aux$h7$tab_prop$count_turnover),
            c(aux$h7$tab_prop$n))

# Sales
aux$h7$prop_test_sales <-
  prop.test(c(aux$h7$tab_prop_sales$count_turnover),
            c(aux$h7$tab_prop_sales$n))

aux$h7$prop_trend_test_sales <-
  prop.trend.test(c(aux$h7$tab_prop_sales$count_turnover),
                  c(aux$h7$tab_prop_sales$n))


# Research & Development
aux$h7$prop_test_rnd <-
  prop.test(c(aux$h7$tab_prop_rnd$count_turnover),
            c(aux$h7$tab_prop_rnd$n))

aux$h7$prop_trend_test_rnd <-
  prop.trend.test(c(aux$h7$tab_prop_rnd$count_turnover),
                  c(aux$h7$tab_prop_rnd$n))

# Human Resources
aux$h7$prop_test_hr <-
  suppressWarnings(
    prop.test(c(aux$h7$tab_prop_hr$count_turnover),
            c(aux$h7$tab_prop_hr$n))) # Warning

aux$h7$prop_trend_test_hr <-
  prop.trend.test(c(aux$h7$tab_prop_hr$count_turnover),
                  c(aux$h7$tab_prop_hr$n))

fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para realizar os cáculos, criar gráficos e tabelas da hipótese 7.")))


message(crayon::cyan$underline$bold("Códigos para construção do relatório de hipóteses executados com sucesso"))


message(note("Criando e salvando objetos para utilizar no Power BI ..."))
# Código para criar tabelas que vão para o power BI
inicio <- Sys.time()

########################
# Objetos para PowerBi #
########################

powerbi$geral_turnover <-
  aux$geral_turnover %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h1_cor_cat <- 
  aux$h1$mat_cramer_ativos %>% 
    dplyr::mutate(turnover = 0) %>% 
    dplyr::bind_rows(
      aux$h1$mat_cramer_deslig %>% 
        dplyr::mutate(turnover = 1) 
    ) %>% 
  dplyr::rename(
    "Freq. de Viagem" = "freq_viagem",
    "Dept. ID" = "deptid",
    "Escolaridade" = "escolaridade",
    "Formação" = "formacao",
    "Satisfação Ambiente" = "satisfacao_ambiente",
    "Gênero" = "genero",
    "Engajamento" = "engajamento",
    "Grade" = "grade",
    "Cargo" = "cargo",
    "Satisfação Trabalho" = "satistacao_trabalho",
    "Estado Civil" = "estado_civil", 
    "Hora extra" = "hora_extra",
    "Performance" = "performance",
    "Satisfação relações trabalho" = "satisfacao_relacoes_trabalho",
    "Stock Option" = "stockoption",
    "Treinamento Último Ano" = "treinamentos_ultimo_ano",
    "Equilíbrio Vida Profissional" = "equilibrio_vida_profissional",
    "Tempo Empresa Faixa" = "tempo_empresa_faixa",
    "Qtde Emp. Trabalhadas Faixa" = "qtd_emp_trabalhadas_faixa" 
    ) %>% 
  dplyr::mutate(
    variavel = 
      dplyr::case_when(
        var == "freq_viagem" ~ "Freq. de Viagem",
        var == "deptid" ~ "Dept. ID",
        var == "escolaridade" ~ "Escolaridade",
        var == "formacao" ~ "Formação",
        var == "satisfacao_ambiente" ~ "Satisfação Ambiente",
        var == "genero" ~ "Gênero",
        var == "engajamento" ~ "Engajamento",
        var == "grade" ~ "Grade",
        var == "cargo" ~ "Cargo",
        var == "satistacao_trabalho" ~ "Satisfação Trabalho",
        var == "estado_civil" ~ "Estado Civil", 
        var == "hora_extra" ~ "Hora extra",
        var == "performance" ~ "Performance",
        var == "satisfacao_relacoes_trabalho" ~ "Satisfação relações trabalho",
        var == "stockoption" ~ "Stock Option",
        var == "treinamentos_ultimo_ano" ~ "Treinamento Último Ano",
        var == "equilibrio_vida_profissional" ~ "Equilíbrio Vida Profissional",
        var == "tempo_empresa_faixa" ~ "Tempo Empresa Faixa",
        var == "qtd_emp_trabalhadas_faixa" ~ "Qtde Emp. Trabalhadas Faixa",
        TRUE ~ var
      )
  )

powerbi$h1_cor_num <-
  aux$h1$mat_corr_ativos %>%  tibble::as_tibble(rownames = "variavel") %>%  dplyr::mutate(turnover = 0) %>% 
  dplyr::bind_rows(
    aux$h1$mat_corr_deslig %>% tibble::as_tibble(rownames = "variavel") %>% dplyr::mutate(turnover = 1) 
  ) %>% 
  dplyr::rename(
    "Idade" = idade,
    "Salário Diário" = salario_diario,
    "Distância Trab." = distancia,
    "Salário Hora" = salario_hora,
    "Salário" = salario,
    "Taxa Mensal" = taxa_mensal,
    "Qtd. Empresas Trab." = qtd_emp_trabalhadas,
    "% Gest. Conseq." = percent_gest_conseq,
    "Horas Contratadas" = horas_contratadas,
    "Anos Exp." = anos_experiencia,
    "Tempo Empresa" = tempo_empresa,        
    "Tempo Cargo" = tempo_cargo,
    "Tempo Última Prom." = tempo_ult_promocao,
    "Tempo Mesma Liderança" = tempo_mesma_lideranca,
    "Posição faixa" = posicao_faixa
    ) %>% 
  dplyr::mutate(
    variavel = 
      dplyr::case_when(
        variavel == "idade" ~ "Idade",
        variavel == "salario_diario" ~ "Salário Diário",
        variavel == "distancia" ~ "Distância Trab.",
        variavel == "salario_hora" ~ "Salário Hora",
        variavel == "salario" ~ "Salário",
        variavel == "taxa_mensal" ~ "Taxa Mensal",
        variavel == "qtd_emp_trabalhadas" ~ "Qtd. Empresas Trab.",
        variavel == "percent_gest_conseq" ~ "% Gest. Conseq.",
        variavel == "horas_contratadas" ~ "Horas Contratadas",
        variavel == "anos_experiencia" ~ "Anos Exp.",
        variavel == "tempo_empresa" ~ "Tempo Empresa",        
        variavel == "tempo_cargo" ~ "Tempo Cargo",
        variavel == "tempo_ult_promocao" ~ "Tempo Última Prom.",
        variavel == "tempo_mesma_lideranca" ~ "Tempo Mesma Liderança",
        variavel == "posicao_faixa" ~ "Posição faixa",
        variavel == "turnover" ~ "Turnover", 
        TRUE ~ variavel
      ) 
  )

powerbi$h1_df_perfil <- 
  aux$h1$df_perfil %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))


powerbi$h3_df <-
  aux$h3$df %>% dplyr::rename("Posição na Faixa (%)" = posicao_faixa) %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos")) %>% 
  dplyr::mutate("Status faixa" = 
                  dplyr::case_when(
                    `Posição na Faixa (%)` < 90 ~ "Abaixo",
                    `Posição na Faixa (%)` >= 90 ~ "Dentro"
                  ))

powerbi$h3_tab_status_fx_deptid <-
  powerbi$h3_df %>% 
  dplyr::group_by(deptid, `Ativo / Deslig.`, turnover, `Status faixa`) %>% 
  dplyr::summarise(`Qtde.` = dplyr::n(), .groups = "drop") %>% 
  dplyr::mutate("% Abaixo da Faixa" = round(100*`Qtde.`/sum(`Qtde.`),2)) %>% 
  dplyr::filter(`Status faixa` == "Abaixo") %>% 
  dplyr::ungroup()

powerbi$h3_tab_status_faixa <- 
  powerbi$h3_df %>% 
  dplyr::group_by(`Status faixa`, turnover) %>% 
  dplyr::tally() %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(turnover) %>% 
    dplyr::mutate(`% Abaixo da Faixa` = 100*n/sum(n)) %>%
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))
  

powerbi$h3_tab_medias <- aux$h3$tab_medias %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h4_df  <- aux$h4$df %>% dplyr::rename("Tempo no Cargo (anos)" = tempo_cargo) %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h4_tab_medias <- aux$h4$tab_medias %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h5_df <-
  aux$h5$df %>% 
  dplyr::rename("% Gestão de Conseq." = percent_gest_conseq,
                "Baixa Gestão de Conseq." = baixa_gest_conseq,
                "Alta Gestão de Conseq." = alta_gest_conseq 
                ) %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h5_tab <-
  aux$h5$tab_baixa_gest_conseq %>% dplyr::mutate(`Gestão consequência` = "Menor percentual") %>%
  dplyr::bind_rows(aux$h5$tab_alta_gest_conseq %>% dplyr::mutate(`Gestão consequência` = "Maior percentual")) %>%
  dplyr::mutate(total = total*100) %>%
  dplyr::rename("Qtde." = n, "Total" = total) %>% 
  dplyr::select(- `Proporção`) %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h5_tab_report <- aux$h5$tab_report %>% dplyr::select(-`Proporção`)

powerbi$h6_df <- aux$h6$df %>% dplyr::rename("Hora Extra" = hora_extra) %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h6_tab_prop  <- aux$h6$tab_prop %>% dplyr::mutate(prop = prop*100) %>% dplyr::select(-distribuicao)

powerbi$h6_tab_prop_deptid <- aux$h6$tab_prop_deptid %>% dplyr::rename("Hora Extra" = hora_extra, "Prop. (%) de Turnover" = prop)

powerbi$h7_df <- aux$h7$df %>% dplyr::rename("Freq. de Viagem" = freq_viagem, "Departamento" = deptid) %>% 
  dplyr::mutate(`Ativo / Deslig.` = dplyr::if_else(turnover == 1, true = "Desligados", false = "Ativos"))

powerbi$h7_tab_prop <- aux$h7$tab_prop %>% dplyr::select(-"distribuicao")

powerbi$h7_df_prop_deptid <- aux$h7$df_prop_deptid %>% dplyr::select(-"distribuicao")

powerbi$hipoteses_testadas <-
  dplyr::tribble(
    ~"Grupo", ~"Núm", ~"Hipótese", ~"Validada?", ~"Validada (binária)?",
    "Perfil Demográfico", 1, "Perfil demográfico/profissional dos colaboradores que se desligam voluntariamente.", "Não se aplica", NA,
    "Análise de Sobrevida", 2, "Análise de risco dos colaboradores que se desligam voluntariamente.", "Não se aplica", NA,
    "Remuneração", 3, "Colaboradores que se desligam voluntariamente estão pior posicionados dentro da faixa salarial.", "Sim", 1,
    "Carreira", 4, "Colaboradores com maior tempo no cargo estão menos inclinados a pedir demissão.", "Sim", 1,
    "Carreira", 5, "Colaboradores que recebem menor percentual na gestão de consequências (promoção) estão mais inclinados a pedir demissão.", "Não", 0,
    "Equilíbrio (vida pessoal e profissional)", 6, "Colaboradores que fazem maior quantidade de horas extras tendem a pedir demissão.", "Sim", 1,
    "Equilíbrio (vida pessoal e profissional)", 7, "Há um maior índice de turnover voluntário em colaboradores cuja frequência de viagem a trabalho é maior.", "Sim", 1 
  )

# Salvando objetos para utilizar no PowerBi
# if(dir.exists(paste0(data_path, "outputs/powerbi")) == FALSE){
#   dir.create(paste0(data_path, "outputs/powerbi"))
# }

# Salvar cada df como um objeto separado
# lapply(ls(powerbi),
#        function(x){
#          assign(paste(x), powerbi[[paste(x)]], envir = .GlobalEnv) 
#        }
# )
# 
# # Salva todos objetos em RData
# save(list = ls(powerbi), file = 
#        paste0(data_path, "outputs/powerbi/pwrbi_turnover.RData"))


# Salvando arquivo Excel para power BI

writexl::write_xlsx(powerbi, paste0(data_path, "outputs/powerbi/pwrbi_turnover.xlsx"))


# Remove os objetos criados
rm(list = ls(powerbi))

## Código para carregar dados no Power BI
# Usar este, mas com barras invertidas no Power BI --> paste0(data_path, "outputs/powerbi/pwrbi_turnover.RData")
# project_folder <- "C:\\Users\\WS432QF\\Documents\\Projetos\\pier_templates\\analysis\\turnover\\data\\outputs\\powerbi\\pwrbi_turnover.RData"
# load (file = project_folder)

message(crayon::cyan$underline$bold("Arquivos para o PowerBi criados com sucesso."))
fim <- Sys.time()
tempo <- (as.numeric(fim) - as.numeric(inicio))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para criar e salvar os objetos para o Power BI.")))


fim_script <- Sys.time()

tempo <- (as.numeric(fim_script) - as.numeric(inicio_script))/60
message(runtime(paste("Foram necessários",round(tempo,2), "minutos para executar o script da análise hipóteses.")))

