# Codigo general para graficar indicadores: funciones, leer antes de cada capítulo
# Autor: Maria Reyes Retana
# Divsion: SCL-SCL: Investigacion

##### Librerias #####

library(ggplot2)
library(tidyverse)
library(haven)
library(ggrepel)
library(extrafont)
library(Rttf2pt1)
library(geofacet)
library(scales)
library(RColorBrewer)
library(scldataR)
library(plotly)
library(dplyr)
library(purrr)

##### Preliminares y colores #####

colors_pal <- c('#17406D','#0F6FC6','#5FF3CB', '#009DD9','#FEA300', '#A5C249','#176A7B','#0BD0D9',
                '#10CF9B', '#FA5F00','#C8DA92','#CC0066')

barplot(rep(1,length(colors_pal)), col=colors_pal)

countries_color <- sample(colors_pal, 12, replace = FALSE)

countries_color_prom <- c("ARG"= "#17406D", "BOL" = "#17406D","CHL" = "#17406D",
                          "COL" = "#17406D", "CRI" = "#17406D", "ECU" = "#17406D",
                          "MEX" = "#17406D", "PER" = "#17406D", "PRY" = "#17406D", 
                          "SLV" = "#17406D", "Promedio" = "#009DD9", "BRA" = "#17406D")

# agregar región BID

paises <- query_indicator(indicator = 'pobreza', 
                          yearstart = 2019, 
                          yearend =  2020) %>% 
  select(isoalpha3) %>% 
  distinct() %>% 
  mutate(region_bid = case_when(isoalpha3 %in% c("ARG", "BRA", "PRY", "CHL", "URY") ~ "Cono Sur", 
                                isoalpha3 %in% c("BOL", "COL", "ECU", "PER", "VEN") ~ "Grupo Andino", 
                                isoalpha3 %in% c("BHS", "BRB", "GUY", "JAM", "SUR", "TTO") ~ "Caribe", 
                                isoalpha3 %in% c("HTI", "MEX", "PAN", "DOM", "BEL", "CRI", "SLV", "GTM", "HND", "NIC") ~ "Centroamerica", 
                                TRUE ~ "NO"))

colors_regiones <- sample(colors_pal, 5, replace = FALSE)

##### Función para generar promedios #####

grouped_mean <- function(.data, .summary_var, ...) {
  .summary_var <- enquo(.summary_var)
  
  .data %>%
    group_by(...) %>%
    mutate(value = mean(!!.summary_var)) %>% 
    mutate(isoalpha3 = "Promedio", 
           country_name_en = "Average", 
           country_name_es = "Promedio", 
           source = "scldata", 
           se = NA_character_, 
           cv = NA_character_, 
           sample = NA_character_, 
           source_es = "scldata", 
           source_en = 'scldata') %>% 
    arrange(source, year) %>% 
    distinct()
  
}

##### Función gráfica cambios promedios ya con datos #####

ggfun_prom <- function(dat,  x.var, y.var) 
  {
  ggp_prom <- ggplot(dat, aes(x = fct_reorder(x.var, y.var),  fill = x.var)) +
    geom_col(aes(y = y.var)) +
    geom_text(data = dat,
              aes(y =  y.var, label = comma(y.var, accuracy = 0.1)),  
              family = 'Century Gothic', fontface = "bold") +
    labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
    theme(legend.position="none", 
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(), 
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_fill_manual(values = countries_color_prom)
  
  print(ggp_prom)
}

##### Cambios evolución general #####

ggfun_evo <- function(dat, x.var, y.var){
  ggp_evo <- ggplot(data = dat, aes(x = x.var, 
                                    y = y.var, 
                                    color = region_bid)) +
    geom_line(size=2)+
    facet_wrap(~isoalpha3, scales = 'free_y') +
   # labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
    theme(legend.position="bottom", 
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(), 
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_color_manual(values = colors_regiones)+
    scale_y_continuous(labels = label_number(accuracy = .01))
  
  print(ggp_evo)
}

##### Gráfica género o una categoría con dos o tres niveles #####

ggfun_one <- function(dat, x.var, y.var, nivel) {
 
ggp_one <- ggplot(dat, aes(x = fct_reorder(x.var, y.var),  fill = nivel)) +
 geom_col(aes(y = y.var), position = "dodge") +
 geom_text(data = dat,
          aes(y =  y.var+.005, label = comma(y.var, accuracy = 0.1)),  
           family = 'Century Gothic', fontface = "bold", 
          position = position_dodge(width = .9), angle = 90) +
  labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
  theme(legend.position="bottom", 
        axis.title.y = element_text(color = "#000f1c", face = "bold", 
                                    family = 'Century Gothic'),
        axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 14, color = "#000f1c", face = "bold", 
                                 family = 'Century Gothic'),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold",
                                   family = 'Century Gothic'),
        text = element_text(size = 14, color = "#000f1c", face = "bold", 
                            family = 'Century Gothic')) +
  scale_fill_manual(values = colors_pal) +
  scale_y_continuous(labels = comma)
  
  print(ggp_one)
  
}

##### Cambios por edad  y promedio #####

ggfun_mul <- function(ggplot, dat){
  
ggp_mul<- ggplot +
  labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
    theme(legend.position="right",
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(),
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_color_brewer(palette = "Purples") +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values = countries_color_prom) +
    geom_hline(yintercept = 0)
    
print(ggp_mul)

}
  