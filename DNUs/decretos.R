library(tidyverse)
library (lubridate)

# Carga de la base y division por presidente ------------------------------

infojus <- read_csv("base-infoleg-normativa-nacional.csv")
options(scipen=999) #elimina notacion cientifica
infojus <- infojus %>% 
  mutate(presidente = case_when(fecha_sancion >= "1983-12-10" & fecha_sancion <= "1989-07-08" ~ "Alfonsin",
                                fecha_sancion >= "1989-07-09" & fecha_sancion <= "1999-12-09" ~ "Menem",
                                fecha_sancion >= "1999-12-10" & fecha_sancion <= "2001-12-20" ~ "De la Rua",
                                fecha_sancion >= "2001-12-21" & fecha_sancion <= "2001-12-30" ~ "Rodriguez Saa",
                                fecha_sancion >= "2001-12-31" & fecha_sancion <= "2003-05-24" ~ "Duhalde",
                                fecha_sancion >= "2003-05-25" & fecha_sancion <= "2007-12-09" ~ "Kirchner",
                                fecha_sancion >= "2007-12-10" & fecha_sancion <= "2015-12-09" ~ "Fernandez de Kirchner",
                                fecha_sancion >= "2015-12-10" & fecha_sancion <= "2019-12-10" ~ "Macri",
                                TRUE ~ "Otros"))


# Leyes y decretos desde Alfonsin -----------------------------------------

#lista de Regex (regular expresion) para detectar DNU
lista <- c("artículo 99.? inciso.? \\d?.?\\s?\\d?\\s?\\y?\\s?3", 
           "inciso.?\\s?\\d?.?\\s?\\d?.?\\s\\y?\\s?3.? del artículo 99")

#detectando DNU en cada link de Ley y Decreto a partir del 24-08-1994
infojus_leyes <- infojus %>%
  mutate(texto_original = replace(texto_original, id_norma == 43004, NA)) %>%  #eliminando el error de pagina
  filter(tipo_norma == "Ley"| tipo_norma == "Decreto",
         !is.na(texto_original),
         fecha_sancion >= "1994-08-24")  %>%       #desde la entrada en vigencia de la CN 1994
  mutate(DNU = str_detect(tolower(htmlToText(texto_original)), paste(lista, collapse = "|")))

save(infojus_leyes, file ="infojus_leyes.rdata")

#agregar DNU a tipo de norma en reemplazo de Decreto, filtrar 1995 a 2018
infojus_dnu <- infojus_leyes %>% 
  mutate(tipo_norma = case_when(tipo_norma == "Decreto" & DNU == TRUE ~ "DNU", 
                                TRUE ~ tipo_norma)) %>% 
  filter(fecha_sancion >= "1995-01-01" & fecha_sancion <= "2018-12-31") %>% 
  mutate(tipo_norma = case_when(tipo_norma == "DNU" & str_detect(texto_resumido, "OBSERVASE")~"Decreto",
                                TRUE ~ tipo_norma))

DNUs <- infojus_dnu %>% 
  filter(tipo_norma =="DNU")

# Cuadro y Grafico de DNU por presidente ----------------------------------

## DNU, Decreto y Leyes por ano y presidente
periodo_dnu <- infojus_dnu %>% 
  filter(tipo_norma != "Decreto") %>% 
  group_by(presidente, tipo_norma, year(fecha_sancion)) %>% 
  summarise(cant = n()) %>% 
  rename(Ano = `year(fecha_sancion)`) %>% 
  arrange(desc(Ano)) %>% 
  ungroup() %>% 
  group_by(tipo_norma) %>% 
  mutate(prom = round(sum(cant/(max(Ano)-min(Ano))),0))

## Graficando cuadro de Ley, Decreto, DNU por ano y presidente

ggplot(data = periodo_dnu , aes(x= Ano, y = cant))+
  geom_col(aes (fill = presidente), position = position_stack( reverse = TRUE))+
  geom_text(aes(label = cant, y = cant+2), size = 2.5, position = position_stack(vjust =1.05))+
  scale_x_continuous(breaks = seq(1995,2018,1), labels = str_sub(seq(1995, 2018,1), start = -2))+
  facet_wrap(~tipo_norma, nrow = 3, scales = "free")+
  geom_hline(aes(yintercept = prom), colour = "grey50")+
  labs(subtitle = "Leyes, Decretos y DNU por año y presidente", x = "Año", y="", 
       caption="Fuente: elaboración propia en base a datos de Infoleg.")+
  geom_text(aes(x=1994, y = prom*1.1+2, label = prom), alpha= 0.05, size = 2.5)+
  scale_fill_grey()+
  theme_bw()



