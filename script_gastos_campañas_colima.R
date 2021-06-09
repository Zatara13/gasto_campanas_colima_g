## Gasto en campañas
## Por Zatara
## Agradecimiento a Economemes por la magia negra para ordenar en automático las gráficas y para ponerle etiqueta de partido a las candidaturas.

## En este script vemos el gasto que han hecho todos los candidatos, por cargo y distrito

## Cargamos librerías de trabajo
library(readr) ## Abrir bases de datos csv
library(tidyverse) ## Manipular bases de datos
library(ggsci) ## Colores bonitos para las gráficas
library(ggthemes) ## Colores bonitos para las gráficas
library(sf)

## Los datos los obtenemos del apartado de los datos abiertos de campañas del ámbito local de la página de rendición de cuentas del INE.
## Disponibles en https://fiscalizacion.ine.mx/web/portalsif/descarga-de-reportes
## Este script utiliza los reportes de fiscalización actualizados hasta el 26 de abril de 2021.

## Cargamos la base de datos
## Datos abiertos candidaturas locales
gastos_g_locales <- read.csv(file = "https://raw.githubusercontent.com/Zatara13/gasto_campanas_colima_g/main/PELO_20-21_CAMPA%C3%91A_G_RUBRO_210607.csv")

## Datos abiertos candidaturas federales
gastos_g_federales <- read.csv(file = "https://raw.githubusercontent.com/Zatara13/gasto_campanas_colima_g/main/PEFO_20-21_CAMPA%C3%91A_G_RUBRO_210607.csv")

## Función de filtrado para datos de candidaturas
f_filtrado <- function(df){
  df_filtrado <- df %>% 
    filter(ESTADO.ELECCION == "COLIMA") %>%  ## Seleccionamos el estado de Colima
    ## Seleccionamos variables de interés, para no tener la base de datos completa
    select(NOMBRE.COMPLETO, ## Nombre del Candidato
           SUBNIVEL.ENTIDAD, ## Distrito al que va
           CARGO, ## Cargo por el que compite
           SIGLAS, ## Partido
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
           cargo = CARGO,
           subnivel = SUBNIVEL.ENTIDAD,
           siglas = SIGLAS,
           financieros = FINANCIEROS,
           gasto_operativo = OPERATIVOS.DE.LA.CAMPAÑA,
           produccion_radio_tv = PRODUCCIÓN.DE.LOS.MENSAJES.PARA.RADIO.Y.T.V.,
           propaganda = PROPAGANDA,
           medios_impresos = PROPAGANDA.EN.DIARIOS..REVISTAS.Y.OTROS.MEDIOS.IMPRESOS,
           en_via_publica = PROPAGANDA.EN.VÍA.PÚBLICA,
           internet = PROPAGANDA.EXHIBIDA.EN.PÁGINAS.DE.INTERNET,
           utilitaria = PROPAGANDA.UTILITARIA) %>% ## Propaganda utilitaria
    ## Agrupamos las variables en una de conceptos y otra de gastos, para trabajar con más facilidad
    gather(concepto, gasto, financieros:utilitaria) %>%
    ## Organizamos las columnas
    select(candidato,
           subnivel,
           cargo,
           siglas,
           concepto,
           gasto) %>% 
    ## Transformamos de NA (sin valor) a 0, asumiendo que no han gastado en eso (o no lo han reportado los muy pillos)
    mutate(gasto = replace_na(gasto,0)) %>%
    ## Conseguimos los gastos por candidato
    ## Debido a que hay candidaturas comunes, agrupamos por candidatos
    group_by(candidato,
             cargo,
             subnivel,
             siglas,
             concepto) %>%  
    summarise(gasto = sum(gasto)) %>% ## Obtenemos el gasto por candidato
    mutate(concepto = as.factor(concepto), ## Mutamos concepto a categoría factor
           subnivel = as.factor(subnivel), ## Mutamos subnivel a categoría factor
           siglas = as.factor(siglas)) %>% ## Mutamos siglas a categoría factor
    ungroup()
}

## Filtramos la base de datos federales para Colima
gastos_c_locales <- f_filtrado(gastos_g_locales)
gastos_c_federales <- f_filtrado(gastos_g_federales)

## Unimos la base de datos en una sola
gastos_c <- bind_rows(gastos_c_federales,
                      gastos_c_locales)
## Con este comando, añadimos el nombre de partido a los nombres de candidatos. (se verá mejor en las gráficas)
gastos_c$candidato <- paste0(gastos_c$candidato,
                             "\n(",
                             gastos_c$siglas,
                             ")")

## Limpiamos el área de trabao
rm(gastos_g_locales,
   gastos_g_federales,
   gastos_c_locales,
   gastos_c_federales)


## Con esta función, vamos a seleccionar los cargos de los candidatos
f_seleccion_cargo <- function(cargo_i,
                              subnivel_i){
  df_gastos <- gastos_c %>% ## Llamamos la base de datos
    filter(cargo == cargo_i & ## Seleccionamos el cargo de interés
             subnivel == subnivel_i) ## Seleccionamos el lugar de interés
  return(df_gastos)
}

## Con esta función vamos a producir las gráficas
f_graficado <- function(df,
                        subtitulo){
  ggplot(df %>% 
           mutate(gasto = gasto / 1000), ## Ajustamos el eje y a miles de pesos
         aes(x = reorder(candidato,
                         -gasto), ## Seleccionamos variable de inteés para el eje x y ajustamos de modo que grafica en orden descendente del eje y
             y = gasto, ## seleccionamos variable de interes para el eje y
             fill = concepto))+ ## Diferenciamos el gasto por concepto)
    theme_bw()+ ## Se ven elegantes las gráficas
    theme(text = element_text(size=11), ## Ajustamos la letra del texto a 11 puntos
          plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
          axis.title.x =  element_blank()) + ## Quitamos la etiqueta del eje x (con esto funcionará el comando reorder de arriba)
    geom_bar(stat = "identity", ## Seleccionamos gráfica de barras 
             position = "stack",
             width = 0.5)+ ## Este parámetro nos permite apilar los gastos
    scale_fill_few(labels = c("Publicidad en vía pública", ## Le ponemos nombre a los gastos
                              "Financieros",
                              "Gasto Operativo",
                              "Páginas de internet",
                              "Medios Impresos",
                              "Producción para radio y tv",
                              "Propaganda general",
                              "Propaganda utilitaria"),
                   palette = "Dark") + ## Seleccionamos la paleta oscura
    ## Ponemos etiquetas a cosas
    labs(title = "Gastos en las campañas 2021 en Colima",
         subtitle = subtitulo, ## Este parámetro varía en función de lo que se ingrese al momento de grafícar las candidaturas
         y = "Miles de pesos", 
         caption = "Fuente: Datos abiertos de fiscalización del INE.
       Nota: La fecha de corte de los datos de la gráfica es 07/06/2021.
         Créditos: Página que te avisa cuanto gastan los políticos en campaña"
    )
}



## Con esta función monitoreamos el gasto de los candidatos en orden descendente
f_mayor_gasto <- function(df){
  df %>% 
    group_by(candidato) %>% 
    summarise(gastos = sum(gasto)) %>% 
    arrange(desc(gastos))
}

## Gubernatura
gubernatura <- f_seleccion_cargo("GOBERNADOR ESTATAL",
                                 "")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(gubernatura)
## Graficamos
f_graficado(gubernatura,
            "Desglose de los gastos de campaña de los candidatos a la gubernatura de Colima"
)


## Diputaciones locales
## Distrito 1 - Colima
## Base de datos
distrito_1 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 1-COLIMA")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(distrito_1)
## Gráfica
f_graficado(distrito_1,
            "Desglose de los gastos de campaña de los candidatos del distrito 1 - Colima")


## Distrito 2 - Colima  
distrito_2 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 2-COLIMA")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(distrito_2)

## Gráfica
f_graficado(distrito_2,
            "Desglose de los gastos de campaña de los candidatos del distrito 2 - Colima")

## Distrito 3 - Colima
distrito_3 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 3-COLIMA")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(distrito_3)

## Gráfica
f_graficado(distrito_3,
            "Desglose de los gastos de campaña de los candidatos del distrito 3 - Colima")

## Distrito 4 - Comala y Villa de Álvarez
distrito_4 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 4-COMALA")
## Vemos quién gastó más
f_mayor_gasto(distrito_4)
## Graficamos
f_graficado(distrito_4,
            "Desglose de los gastos de campaña de los candidatos del distrito 4 - Comala y Villa de Álvarez")

## Distrito 5 - Coquimatlan y Villa de Álvarez
distrito_5 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 5-COQUIMATLAN")
## Vemos quién gastó más
f_mayor_gasto(distrito_5)
## Graficamos
f_graficado(distrito_5,
            "Desglose de los gastos de campaña de los candidatos del distrito 5 - Coquimatlán y Villa de Álvarez")

## Distrito 6 - Colima - Cuauhtémoc
distrito_6 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 6-CUAUHTEMOC")
## Vemos quien gasta mas
f_mayor_gasto(distrito_6)
## Graficamos
f_graficado(distrito_6,
            "Desglose de los gastos de campaña de los candidatos del distrito 6 - Colima y Cuauhtémoc")

## Distrito 7 - Villa de Álvarez
distrito_7 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 7-VILLA DE ALVAREZ")
## Vemos quien gasta mas
f_mayor_gasto(distrito_7)
## Graficamos
f_graficado(distrito_7,
            "Desglose de los gastos de campaña de los candidatos del distrito 7 - Villa de Álvarez")

## Distrito 8 - Villa de Álvarez
distrito_8 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 8-VILLA DE ALVAREZ")
## Vemos quien gasta mas
f_mayor_gasto(distrito_8)
## Graficamos
f_graficado(distrito_8,
            "Desglose de los gastos de campaña de los candidatos del distrito 8 - Villa de Álvarez")

## Distrito 9 - Armería - Manzanillo
distrito_9 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 9-ARMERIA")
## Vemos quien gasta mas
f_mayor_gasto(distrito_9)
## Graficamos
f_graficado(distrito_9,
            "Desglose de los gastos de campaña de los candidatos del distrito 9 - Armería y manzanillo")

## Distrito 10 Tecoman
distrito_10 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 10-TECOMAN")
## Vemos quien gasta mas
f_mayor_gasto(distrito_10)
## Graficamos
f_graficado(distrito_10,
            "Desglose de los gastos de campaña de los candidatos del distrito 10 - Tecoman")

## Distrito 11 Manzanillo
distrito_11 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 11-MANZANILLO")
## Vemos quien gasta mas
f_mayor_gasto(distrito_11)
## Graficamos
f_graficado(distrito_11,
            "Desglose de los gastos de campaña de los candidatos del distrito 11 - Manzanillo")

## Distrito 12 Manzanillo
distrito_12 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 12-MANZANILLO")
f_mayor_gasto(distrito_12)
## Graficamos
f_graficado(distrito_12,
            "Desglose de los gastos de campaña de los candidatos del distrito 12 - Manzanillo")

## Distrito 13 Manzanillo
distrito_13 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 13-MANZANILLO")
f_mayor_gasto(distrito_13)
## Graficamos
f_graficado(distrito_13,
            "Desglose de los gastos de campaña de los candidatos del distrito 13 - Manzanillo")

## Distrito 14 Manzanillo - Minatitlan
distrito_14 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 14-MINATITLAN")
f_mayor_gasto(distrito_14)
## Graficamos
f_graficado(distrito_14,
            "Desglose de los gastos de campaña de los candidatos del distrito 14 - Manzanillo y Minatitlán")

## Distrito 15 Tecoman
distrito_15 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 15-TECOMAN")
f_mayor_gasto(distrito_15)
## Graficamos
f_graficado(distrito_15,
            "Desglose de los gastos de campaña de los candidatos del distrito 15 - Tecomán")

## Distrito 16 - Tecoman e Ixtlahuacán Distrito 16-TECOMAN
distrito_16 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 16-TECOMAN")
f_mayor_gasto(distrito_16)
## Graficamos
f_graficado(distrito_16,
            "Desglose de los gastos de campaña de los candidatos del distrito 16 - Tecomán e ixtlahuacán")
## Limpiamos este horror
rm(distrito_1,
   distrito_2,
   distrito_3,
   distrito_4,
   distrito_5,
   distrito_6,
   distrito_7,
   distrito_8,
   distrito_9,
   distrito_10,
   distrito_11,
   distrito_12,
   distrito_13,
   distrito_14,
   distrito_15,
   distrito_16)


## Presidencia municipal
## Armeria
armeria <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                             "Municipio 6-ARMERIA")
f_mayor_gasto(armeria)
f_graficado(armeria,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Armeria")
## Colima
colima <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                            "Municipio 1-COLIMA")
f_mayor_gasto(colima)
f_graficado(colima,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Colima")
## Comala
comala <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                            "Municipio 2-COMALA")
f_mayor_gasto(comala)
f_graficado(comala,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Comala")

## Coquimatlan
coqui <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                           "Municipio 3-COQUIMATLAN")
f_mayor_gasto(coqui)
f_graficado(coqui,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Coquimatlán")
## Cuauhtémoc
cuau <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                          "Municipio 4-CUAUHTEMOC")
f_mayor_gasto(cuau)
f_graficado(cuau,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Cuauhtémoc")
## Ixtlahuacan
ixtla <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                           "Municipio 7-IXTLAHUACAN")
f_mayor_gasto(ixtla)
f_graficado(ixtla,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Ixtlahuacán")
## Manzanillo
manza <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                           "Municipio 8-MANZANILLO")
f_mayor_gasto(manza)
f_graficado(manza,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Manzanillo")
## Minatitlan
mina <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                          "Municipio 9-MINATITLAN")
f_mayor_gasto(mina)
f_graficado(mina,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Minatitlán")
## Tecoman
teco <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                          "Municipio 10-TECOMAN")
f_mayor_gasto(teco)
f_graficado(teco,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Tecomán")
## Villa de Álvarez
villa <- f_seleccion_cargo("PRESIDENTE MUNICIPAL",
                           "Municipio 5-VILLA DE ALVAREZ")
f_mayor_gasto(villa)
f_graficado(villa,
            "Desglose de los gastos de campaña de los candidatos a la presidencia municipal de Villa de Álvarez")
rm(
  armeria,
  colima,
  comala,
  coqui,
  cuau,
  ixtla,
  manza,
  mina,
  teco,
  villa
)



## Diputaciones federales
## Distrito Federal 1
distrito_1_f <- f_seleccion_cargo("DIPUTACION FEDERAL MR",
                                  "Distrito 1-COLIMA")
f_mayor_gasto(distrito_1_f)
## graficamos
f_graficado(distrito_1_f,
            "Desglose de los gastos de campaña de los candidatos del distrito federal 1")
## Distrito Federal 2
distrito_2_f <- f_seleccion_cargo("DIPUTACION FEDERAL MR",
                                  "Distrito 2-MANZANILLO")
f_mayor_gasto(distrito_2_f)
## graficamos
f_graficado(distrito_2_f,
            "Desglose de los gastos de campaña de los candidatos del distrito federal 2")
rm(distrito_1_f,
   distrito_2_f)


## Gasto por Partidos y Coaliciones
g_partidos <- gastos_c %>%
  group_by(siglas,
           concepto) %>%
  summarise(gasto = sum(gasto))

## Consideramos a Nueva Alianza y Morena como gasto conjunto por las candidaturas comunes. Los asumimos aliados del mismo proyecto
## Como PRI - PAN - PRD = Va por México, también asignamos la misma etiqueta para que se grafique adecuadamente
siglas_2 <- data.frame(siglas = as.factor(c("MORENA",
                                            "MOVIMIENTO CIUDADANO",
                                            "PT",
                                            "PVEM",
                                            "VA POR MEXICO",
                                            "FS X MÉXICO",
                                            "NUEVA ALIANZA COLIMA",
                                            "PRI-PAN-PRD",
                                            "RSPPPN",
                                            "PES")),
                       siglasb = as.factor(c("MORENA",
                                             "MOVIMIENTO CIUDADANO",
                                             "PT",
                                             "PVEM",
                                             "PRI-PAN-PRD",
                                             "FS X MÉXICO",
                                             "NUEVA ALIANZA",
                                             "PRI-PAN-PRD",
                                             "RSPPPN",
                                             "PES")))

g_partidos <- merge(g_partidos,
                    siglas_2,
                    by = "siglas") %>% 
  ungroup() %>% 
  select(siglasb,
         concepto,
         gasto) %>% 
  rename(siglas = siglasb) %>%
  group_by(siglas,
           concepto) %>% 
  summarise(gasto = sum(gasto))

## Vemos cifras 
g_partidos %>% 
  ungroup() %>% 
  group_by(siglas) %>% 
  summarise(gasto = sum(gasto)) %>% 
  arrange(desc(gasto))


## Gráfica
ggplot(g_partidos %>% 
         mutate(gasto = gasto / 1000),
       aes(x = reorder(siglas,
                       -gasto), ## Seleccionamos variable de inteés para el eje x
           y = gasto, ## seleccionamos variable de interes para el eje y
           fill = concepto))+ ## Diferenciamos el gasto por concepto)
  theme_bw()+ ## Se ven elegantes las gráficas
  theme(text = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        axis.title.x =  element_blank()) +
  geom_bar(stat = "identity", ## Seleccionamos gráfica de barras
           position = "stack",
           width = 0.5) + ## Este parámetro nos permite apilar los gastos
  scale_fill_few(labels = c("Publicidad en vía pública", ## Le ponemos nombre a los gastos
                            "Financieros",
                            "Gasto Operativo",
                            "Páginas de internet",
                            "Medios Impresos",
                            "Producción para radio y tv",
                            "Propaganda general",
                            "Propaganda utilitaria"),
                 palette = "Dark") + ## Seleccionamos la paleta oscura
  labs(title = "Gastos en las campañas 2021 en Colima", ## Ponemos etiquetas a cosas
       subtitle = "Desglose de los gastos de campaña por coaliciones, candidaturas comunes y partidos contendientes",
       y = "Miles de pesos",
       caption = "Fuente: Datos abiertos de fiscalización del INE.
       Nota: La fecha de corte de los datos de la gráfica es 10/05/2021.
         Créditos: Página que te avisa cuanto gastan los políticos en campaña"
  )

## Gasto de partidos expresado como años necesarios para que un trabajador con salario mínimo consiga el mismo varo
g_partidos_sm <- g_partidos %>% 
  ungroup() %>% 
  group_by(siglas) %>% 
  summarise(gasto = sum(gasto)) %>% 
  mutate(gasto_dias = gasto / 141.7) %>% 
  mutate(gasto_años = gasto_dias / 312) %>% 
  mutate(gasto_años = round(gasto_años,
                            2)) %>% 
  mutate(colores = c("orchid1",
                     "brown4",
                     "darkorange",
                     "cyan3",
                     "darkorchid3",
                     "royalblue2",
                     "orangered3",
                     "green",
                     "azure3"))

## Gráfica gasto en días
ggplot(g_partidos_sm, ## Llamamos base de datos
       aes(x = reorder(siglas, ## Seleccionamos eje de las x y ordenamos de acuerdo a comportamiento en y
                       -gasto_años),
           y = gasto_años,
           fill = colores)) + ## Seleccionamos eje de las y
  theme_bw()+
  theme(text = element_text(size=17),
        plot.title = element_text(hjust = 0.5),
        axis.title.x =  element_blank(),
        legend.position = "none") +
  geom_bar(stat="identity") +
  geom_text(aes(label=gasto_años),
            vjust=-0.3,
            size=7) +
  scale_fill_manual(values = c("royalblue2" = "royalblue2",
                               "orangered3" = "orangered3",
                               "orchid1" = "orchid1",
                               "brown4" = "brown4",
                               "cyan3" = "cyan3",
                               "darkorange" = "darkorange",
                               "green" = "green",
                               "azure3" = "azure3",
                               "darkorchid3" = "darkorchid3"))+
  geom_hline(yintercept = 60,
             linetype="dashed",
             color = "red")+
  geom_hline(yintercept = 120,
             linetype="dashed",
             color = "red")+
  geom_hline(yintercept = 180,
             linetype="dashed",
             color = "red")+
  geom_hline(yintercept = 240,
             linetype="dashed",
             color = "red")+
  geom_hline(yintercept = 300,
             linetype="dashed",
             color = "red")+
  geom_hline(yintercept = 360,
             linetype="dashed",
             color = "red")+
  labs(title = "Gastos en las campañas 2021 en Colima", ## Ponemos etiquetas a cosas
       subtitle = "Años que un trabajador con salario mínimo tardaría en reunir el dinero que los partidos gastaron en campaña hasta el 07/06/21",
       y = "Años",
       caption = "Fuente: Datos abiertos de fiscalización del INE.
       Nota 1: Se asume que el trabajador tiene un día de descanso por semana laboral.
       Nota 2: Las líneas rojas representan el momento en que un trabajador alcanza la edad promedio de muerte en México (75 años).
       Créditos: Página que te avisa cuanto gastan los políticos en campaña"
  )