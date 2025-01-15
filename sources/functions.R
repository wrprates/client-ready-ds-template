#################################################################
#####                    FUNÇÕES ESSENCIAIS                   #####
#################################################################

# Operador para filtrar valores não desejados
'%!in%' <- function(x,y) !('%in%'(x,y))

# Multiplicar por 100 para percentuais
t100 <- function(x) x * 100

#################################################################
#####                    FUNÇÕES DE GRÁFICOS                  #####
#################################################################

# Função base para gráficos highcharter
get_hc <- function(d, title = NULL) {
  highcharter::hchart(d, showInLegend = FALSE) %>%
    highcharter::hc_title(text = title)
} 

# Tema personalizado para gráficos
hc_theme_ey <- function(..., 
                       colors = c("#d35400", "#2980b9", "#2ecc71", 
                                "#f1c40f", "#2c3e50", "#7f8c8d")) {
  theme <- highcharter::hc_theme(
    colors = colors,
    chart = list(style = list(color = "#666666")),
    title = list(align = "left"),
    legend = list(align = "right", verticalAlign = "bottom")
  )
  theme
}

# Função para gráficos de densidade
density_we <- function(x, from = min(x), to = max(x)){
  if(all(is.na(x))){
    "Gráfico de densidade vazio"
  } else{
    density(x, na.rm = TRUE, from = from, to = to)
  }
}

# Função para boxplots
boxplot_we <- function(x, outliers = TRUE){
  if(all(is.na(x))){
    "Gráfico boxplot vazio"
  } else{
    hcboxplot2(x, na.rm = TRUE, name = "Box-plot", outliers = outliers)
  }
}

# Função auxiliar para boxplots
hcboxplot2 <- function(x = NULL, var = NULL, var2 = NULL, outliers = TRUE, ...) {
  stopifnot(is.numeric(x))
  if (is.null(var)) var <- NA
  if (is.null(var2)) var2 <- NA
  
  df <- tibble::tibble(x, g1 = var, g2 = var2)
  
  # ... resto do código do hcboxplot2 (mantido como está) ...
}

#################################################################
#####                    FUNÇÕES DE FORMATAÇÃO                #####
#################################################################

# Formatar valores monetários
format_real <- function(values, currency = "R$ ", nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    stringr::str_trim() %>%
    stringr::str_c(currency, .)
}

# Preparar nomes de colunas
prep_colnames <- function(df, add_pattern = "", space = "_"){
  df %>% setNames(., paste0(gsub(" ", space, prep_fun(colnames(df))), add_pattern))
}