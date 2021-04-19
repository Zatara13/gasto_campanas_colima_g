## Gasto en campañas

## Cargamos librerías de trabajo
library(readr) ## Abrir bases de datos csv
library(tidyverse) ## Manipular bases de datos
library(ggsci) ## Colores bonitos para las gráficas
library(ggthemes) ## Colores bonitos para las gráficas

## Cargamos la base de datos
gastos_g <- read.csv(file = "https://raw.githubusercontent.com/Zatara13/gasto_campanas_colima_g/main/PELO_20-21_CAMPA%C3%91A_G_RUBRO_210415.csv")
## Filtramos para Colima, seleccionamos cargo a la gubernatura y seleccionamos variables de interés
gastos_c <- gastos_g %>% ## Llamamos base de datos
  filter(ESTADO.ELECCION == "COLIMA" &  ## Seleccionamos el estado
           CARGO == "GOBERNADOR ESTATAL") %>%  ## Seleccionamos el cargo
  ## Seleccionamos variables de interés, para no tener la base de datos completa
  select(NOMBRE.COMPLETO, ## Nombre del Candidato
         FINANCIEROS, ## Gastos financieros
         OPERATIVOS.DE.LA.CAMPAÑA, ## Gastos operativos
         PRODUCCIÓN.DE.LOS.MENSAJES.PARA.RADIO.Y.T.V., ## Producción de spots
         PROPAGANDA, ## Propaganda
         PROPAGANDA.EN.DIARIOS..REVISTAS.Y.OTROS.MEDIOS.IMPRESOS, ## Propaganda en medios impresos
         PROPAGANDA.EN.VÍA.PÚBLICA, ## Propaganda en vía pública
         PROPAGANDA.EXHIBIDA.EN.PÁGINAS.DE.INTERNET, ## Propaganda en internet
         PROPAGANDA.UTILITARIA) %>%   ## Total de gastos
  ## Renombramos las variables para trabajar con mayor facilidad
  rename(candidato = NOMBRE.COMPLETO,
         financieros = FINANCIEROS,
         gasto_operativo = OPERATIVOS.DE.LA.CAMPAÑA,
         produccion_spots = PRODUCCIÓN.DE.LOS.MENSAJES.PARA.RADIO.Y.T.V.,
         propaganda = PROPAGANDA,
         medios_impresos = PROPAGANDA.EN.DIARIOS..REVISTAS.Y.OTROS.MEDIOS.IMPRESOS,
         en_via_publica = PROPAGANDA.EN.VÍA.PÚBLICA,
         internet = PROPAGANDA.EXHIBIDA.EN.PÁGINAS.DE.INTERNET,
         utilitaria = PROPAGANDA.UTILITARIA) %>% ## Propaganda utilitaria
  ## Agrupamos las variables en una de conceptos y otra de gastos, para trabajar con más facilidad
  gather(concepto, gasto, financieros:utilitaria) %>%
  ## Organizamos las columnas
  select(candidato,
         concepto,
         gasto) %>% 
  ## Transformamos de NA (sin valor) a 0, asumiendo que no han gastado en eso (o no lo han reportado)
  mutate(gasto = replace_na(gasto,0)) %>%
  ## Conseguimos los gastos por candidato
  ## Debido a que la candidatura de Indira Vizcaíno es una candidatura común, sumamos los gastos de Nueva Alianza y Morena
  group_by(candidato, concepto) %>%  
  summarise(gasto = sum(gasto)) %>% 
  mutate(concepto = as.factor(concepto))
rm(gastos_g) ## Limpiamos el área de trabajo

## Visualización cuánto y en qué están gastando los candidatos
ggplot(gastos_c %>% 
         mutate(candidato = fct_relevel(candidato, ## Seleccionamos el orden de más gasto a menos gasto
                                        "LEONCIO ALFONSO MORAN SANCHEZ",
                                        "VIRGILIO MENDOZA AMEZCUA",
                                        "CLAUDIA VALERIA YAÑEZ CENTENO Y CABRERA",
                                        "INDIRA VIZCAINO SILVA",
                                        "MELY ROMERO CELIS",
                                        "AURORA ILEANA CRUZ ALCARAZ"),
                gasto = gasto / 1000), ## Configuramos el eje en miles de pesos
       aes(x = candidato, ## Seleccionamos variables de interés
           y = gasto,
           fill = concepto))+
  theme_bw()+ ## Se ven elegantes las gráficas
  geom_bar(stat = "identity", ## Seleccionamos gráfica de barras
           position = "stack")+ ## Este parámetro nos permite apilar los gastos
  scale_fill_few(labels = c("Publicidad en vía pública", ## Le ponemos nombre a los gastos y seleccionamos la paleta few
                            "Financieros",
                            "Gasto Operativo",
                            "Páginas de internet",
                            "Medios Impresos",
                            "Producción de Spots",
                            "Propaganda general",
                            "Propaganda utilitaria"),
                 palette = "Dark")+ ## Seleccionamos la paleta oscura
  labs(title = "Gastos en las campañas 2021 en Colima", ## Ponemos etiquetas a cosas
       subtitle = "Desglose de los gastos de los candidatos a la gobernatura de Colima",
       x = "Nombre de la candidata",
       y = "Miles de pesos",
       caption = "Fuente: Datos abiertos de fiscalización del INE.
       Nota: La fecha de corte de los datos de la gráfica es 15/04/2021.
       @jkvisfocri")

## Vemos el grado de contaminación de los gastos
nivel_contaminacion <- data.frame(concepto = as.factor(c("en_via_publica", ## Armamos una base de datos con la variable de concepto
                                         "financieros",
                                         "gasto_operativo",
                                         "internet",
                                         "medios_impresos",
                                         "produccion_spots",
                                         "propaganda",
                                         "utilitaria")),
                            grado_contaminacion = as.factor(c("contaminante", ## Le asignamos una de las categorías de contaminación a cada concepto
                                                    "no tan contaminante",
                                                    "indeterminado",
                                                    "no tan contaminante",
                                                    "contaminante",
                                                    "no tan contaminante",
                                                    "indeterminado",
                                                    "contaminante")))
## Unimos las etiquetas de contaminación a sus correspondientes conceptos en la base de datos de gastos
gastos_c <- merge(gastos_c,
                  nivel_contaminacion,
                  by = "concepto")
## Hacemos un resumen de los gastos por concepto de contaminación
gasto_contaminacion <- gastos_c %>% ## Llamamos la base de datos
  ungroup() %>%  ## Desagrupamos
  group_by(candidato, ## Agrupamos variables de interés
           grado_contaminacion) %>%
  summarise(gasto = sum(gasto)) %>%  ## Resumimos la información de los gastos por concepto
  ungroup() %>%  ## Desagrupamos
  group_by(candidato) %>%  ## Agrupamos por candidato
  mutate(total = sum(gasto)) %>% ## Generamos el total de gastos por candidato en una nueva variable
  mutate(pct = gasto / total * 100) ## Creamos una nueva variable de porcentaje por grado de contaminación

## Visualización cuánto y en qué están gastando los candidatos
## Graficamos el gasto en conceptos contaminantes
ggplot(gasto_contaminacion %>% ## Llamamos base de datos
         mutate(candidato = fct_relevel(candidato, ## Ordenamos los candidatos, de quien ha gastado en más publicidad contaminante a quien ha gastado menos
                                        "CLAUDIA VALERIA YAÑEZ CENTENO Y CABRERA",
                                        "LEONCIO ALFONSO MORAN SANCHEZ",
                                        "INDIRA VIZCAINO SILVA",
                                        "VIRGILIO MENDOZA AMEZCUA",
                                        "MELY ROMERO CELIS",
                                        "AURORA ILEANA CRUZ ALCARAZ")),
       aes(x = candidato, ## Seleccionamos variables de interés
           y = pct,
           fill = grado_contaminacion))+
  theme_bw()+ ## Está elegante este tema
  geom_bar(stat = "identity", ## Gráfica de barras
           position = "stack")+ ## Agrupamos conceptos uno sobre el otro
  scale_fill_startrek()+ ## Seleccionamos estos colores, porque vienen en orden rojo - azul - verde
  labs(title = "Gastos en las campañas 2021 en Colima", ## Ponemos etiquetas y leyendas adecuadas
       subtitle = "Porcentaje del gasto de campaña en conceptos contaminantes",
       fill = "Grado de contaminación",
       x = "Nombre de la candidata",
       y = "Porcentaje",
       caption = "Fuente: Datos abiertos de fiscalización del INE.
       Nota 1: La fecha de corte de los datos de la gráfica es 15/04/2021.
       Nota 2: Se considera como contaminante la publicidad en espectaculares, medios impresos y utilitaria. Se considera indeterminada los gastos operativos y la propaganda general. Se considera no tan contaminante la el gasto financiero, en portales de internet y la producción de spots.
       @jkvisfocri")
