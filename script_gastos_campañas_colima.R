## Gasto en campañas
## Por Zatara

## En este script vemos el gasto que han hecho todos los candidatos, por cargo y distrito

## Cargamos librerías de trabajo
library(readr) ## Abrir bases de datos csv
library(tidyverse) ## Manipular bases de datos
library(ggsci) ## Colores bonitos para las gráficas
library(ggthemes) ## Colores bonitos para las gráficas

## Los datos los obtenemos del apartado de los datos abiertos de campañas del ámbito local de la página de rendición de cuentas del INE.
## Disponibles en https://fiscalizacion.ine.mx/web/portalsif/descarga-de-reportes
## Este script utiliza los reportes de fiscalización actualizados hasta el 19 de abril de 2021.

## Cargamos la base de datos
## Datos abiertos candidaturas locales
gastos_g_locales <- read.csv(file = "https://raw.githubusercontent.com/Zatara13/gasto_campanas_colima_g/main/PELO_20-21_CAMPA%C3%91A_G_RUBRO_210419.csv")

## Datos abiertos candidaturas federales
gastos_g_federales <- read.csv(file = "https://raw.githubusercontent.com/Zatara13/gasto_campanas_colima_g/main/PEFO_20-21_CAMPA%C3%91A_G_RUBRO_210419.csv")

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
    summarise(gasto = sum(gasto)) %>% 
    mutate(concepto = as.factor(concepto),
           subnivel = as.factor(subnivel),
           siglas = as.factor(siglas)) %>% 
    ungroup()
}

## Filtramos la base de datos federales para Colima
gastos_c_locales <- f_filtrado(gastos_g_locales)
gastos_c_federales <- f_filtrado(gastos_g_federales)

gastos_c <- bind_rows(gastos_c_federales,
                      gastos_c_locales)

rm(gastos_g_locales,
   gastos_g_federales,
   gastos_c_locales,
   gastos_c_federales)


## Con esta función, vamos a seleccionar los cargos de los candidatos
f_seleccion_cargo <- function(cargo_i, subnivel_i){
  df_gastos <- gastos_c %>% ## Llamamos la base de datos
    filter(cargo == cargo_i & ## Seleccionamos el cargo de interés
             subnivel == subnivel_i) ## Seleccionamos el lugar de interés
  return(df_gastos)
}

## Con esta función vamos a producir las gráficas
f_graficado <- function(df, refactorizacion, subtitulo){
  ggplot(df %>% 
           mutate(candidato = fct_relevel(candidato,
                                          refactorizacion),
                  gasto = gasto / 1000),
         aes(x = candidato, ## Seleccionamos variable de inteés para el eje x
             y = gasto, ## seleccionamos variable de interes para el eje y
             fill = concepto))+ ## Diferenciamos el gasto por concepto)
    theme_bw()+ ## Se ven elegantes las gráficas
    geom_bar(stat = "identity", ## Seleccionamos gráfica de barras
             position = "stack")+ ## Este parámetro nos permite apilar los gastos
    scale_fill_few(labels = c("Publicidad en vía pública", ## Le ponemos nombre a los gastos
                              "Financieros",
                              "Gasto Operativo",
                              "Páginas de internet",
                              "Medios Impresos",
                              "Producción para radio y tv",
                              "Propaganda general",
                              "Propaganda utilitaria"),
                   palette = "Dark")+ ## Seleccionamos la paleta oscura
    labs(title = "Gastos en las campañas 2021 en Colima", ## Ponemos etiquetas a cosas
         subtitle = subtitulo,
         x = "Nombre de la candidata",
         y = "Miles de pesos",
         caption = "Fuente: Datos abiertos de fiscalización del INE.
       Nota: La fecha de corte de los datos de la gráfica es 21/04/2021.
       @jkvisfocri")
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
            c("LEONCIO ALFONSO MORAN SANCHEZ",
              "VIRGILIO MENDOZA AMEZCUA",
              "CLAUDIA VALERIA YAÑEZ CENTENO Y CABRERA",
              "INDIRA VIZCAINO SILVA",
              "MELY ROMERO CELIS",
              "AURORA ILEANA CRUZ ALCARAZ"),
            "Desglose de los gastos de campaña de los candidatos a la gubernatura de Colima"
)

## Presidencias municipales

## Diputaciones locales
## Distrito 1 - Colima
## Base de datos
distrito_1 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 1-COLIMA")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(distrito_1)
## Gráfica
f_graficado(distrito_1,
            c("GABRIELA RODRIGUEZ MACIAS",
              "ALONDRA ISABEL LOPEZ ALONSO",
              "DULCE ASUCENA HUERTA ARAIZA"),
            "Desglose de los gastos de campaña de los candidatos del distrito 1 - Colima")


## Distrito 2 - Colima  
distrito_2 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 2-COLIMA")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(distrito_2)

## Gráfica
f_graficado(distrito_2,
            c("MARCO ANTONIO GARCIA CHAVIRA",
              "MARISA MESINA POLANCO"),
            "Desglose de los gastos de campaña de los candidatos del distrito 2 - Colima")

## Distrito 3 - Colima
distrito_3 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 3-COLIMA")
## Con esto vemos quien lleva más gastos
f_mayor_gasto(distrito_3)

## Gráfica
f_graficado(distrito_3,
            c("ANA ISABEL FLORES VIERA",
              "ALFREDO ALVAREZ RAMIREZ"),
            "Desglose de los gastos de campaña de los candidatos del distrito 3 - Colima")

## Distrito 4 - Comala y Villa de Álvarez
distrito_4 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 4-COMALA")
## Vemos quién gastó más
f_mayor_gasto(distrito_4)
## Graficamos
f_graficado(distrito_4,
            c("CLAUDIA MACIAS HERMOSILLO",
              "JESUS ALEJANDRO GONZALEZ GONZALEZ"),
            "Desglose de los gastos de campaña de los candidatos del distrito 4 - Comala y Villa de Álvarez")

## Distrito 5 - Coquimatlan y Villa de Álvarez
distrito_5 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 5-COQUIMATLAN")
## Vemos quién gastó más
f_mayor_gasto(distrito_5)
## Graficamos
f_graficado(distrito_5,
            c("CARLOS ANTONIO CHAVIRA GEORGE",
              "MYRIAM GUDIÑO ESPINDOLA"),
            "Desglose de los gastos de campaña de los candidatos del distrito 5 - Coquimatlán y Villa de Álvarez")

## Distrito 6 - Colima - Cuauhtémoc
distrito_6 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 6-CUAUHTEMOC")
## Vemos quien gasta mas
f_mayor_gasto(distrito_6)
## Graficamos
f_graficado(distrito_6,
            c("JAIME CESAR PEREZ RODRIGUEZ",
              "FRANCISCO JAVIER GUARDADO PLASCENCIA"),
            "Desglose de los gastos de campaña de los candidatos del distrito 6 - Colima y Cuauhtémoc")

## Distrito 7 - Villa de Álvarez
distrito_7 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 7-VILLA DE ALVAREZ")
## Vemos quien gasta mas
f_mayor_gasto(distrito_7)
## Graficamos
f_graficado(distrito_7,
            c("GERARDO ADRIANO DIAZ MARQUEZ",
              "MA GLORIA CORTES SANDOVAL"),
            "Desglose de los gastos de campaña de los candidatos del distrito 7 - Villa de Álvarez")

## Distrito 8 - Villa de Álvarez
distrito_8 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 8-VILLA DE ALVAREZ")
## Vemos quien gasta mas
f_mayor_gasto(distrito_8)
## Graficamos
f_graficado(distrito_8,
            c("LUIS ALFONSO POLANCO TERRIQUEZ",
              "RAQUEL RODRIGUEZ MORA"),
            "Desglose de los gastos de campaña de los candidatos del distrito 8 - Villa de Álvarez")

## Distrito 9 - Armería - Manzanillo
distrito_9 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 9-ARMERIA")
## Vemos quien gasta mas
f_mayor_gasto(distrito_9)
## Graficamos
f_graficado(distrito_9,
            c("GLENDA CELINA DEASIS CARRILLO",
              "ADRIANA DEL CARMEN RAMIREZ GOMEZ",
              "ANNETT GUADALUPE TAPIA CERVANTES"),
            "Desglose de los gastos de campaña de los candidatos del distrito 9 - Armería y manzanillo")

## Distrito 10 Tecoman
distrito_10 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                "Distrito 10-TECOMAN")
## Vemos quien gasta mas
f_mayor_gasto(distrito_10)
## Graficamos
f_graficado(distrito_10,
            c("LORENA CERVANTES JUAREZ"),
            "Desglose de los gastos de campaña de los candidatos del distrito 10 - Tecoman")

## Distrito 11 Manzanillo
distrito_11 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 11-MANZANILLO")
## Vemos quien gasta mas
f_mayor_gasto(distrito_11)
## Graficamos
f_graficado(distrito_11,
            c("ALFREDO VALENCIA DELGADO",
              "XIMENA FIGUEROA TENE"),
            "Desglose de los gastos de campaña de los candidatos del distrito 11 - Manzanillo")

## Distrito 12 Manzanillo
distrito_12 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 12-MANZANILLO")
f_mayor_gasto(distrito_12)
## Graficamos
f_graficado(distrito_12,
            c("MARIA ELENA VELASCO RAMIREZ"),
            "Desglose de los gastos de campaña de los candidatos del distrito 12 - Manzanillo")

## Distrito 13 Manzanillo
distrito_13 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 13-MANZANILLO")
f_mayor_gasto(distrito_13)
## Graficamos
f_graficado(distrito_13,
            c("JORGE LUIS HERRERA VALLE"),
            "Desglose de los gastos de campaña de los candidatos del distrito 13 - Manzanillo")

## Distrito 14 Manzanillo - Minatitlan
distrito_14 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 14-MINATITLAN")
f_mayor_gasto(distrito_14)
## Graficamos
f_graficado(distrito_14,
            c("GLORIA LETICIA TORRES BRIZUELA",
              "MIRNA GUADALUPE MONTES DE OCA CONTRERAS"),
            "Desglose de los gastos de campaña de los candidatos del distrito 14 - Manzanillo y Minatitlán")

## Distrito 15 Tecoman
distrito_15 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 15-TECOMAN")
f_mayor_gasto(distrito_15)
## Graficamos
f_graficado(distrito_15,
            c("CARINA DUEÑAS VALDOVINOS",
              "ANA VICTORIA LEDESMA IÑIGUEZ"),
            "Desglose de los gastos de campaña de los candidatos del distrito 15 - Tecomán")

## Distrito 16 - Tecoman e Ixtlahuacán Distrito 16-TECOMAN
distrito_16 <- f_seleccion_cargo("DIPUTADO LOCAL MR",
                                 "Distrito 16-TECOMAN")
f_mayor_gasto(distrito_16)
## Graficamos
f_graficado(distrito_16,
            c("FRANCISCO JAVIER OROZCO MEDINA"),
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

## Municipales - A la fecha, no hay datos. Lo actualizo ya que tengamos los reportes


## Diputaciones federales
## Distrito Federal 1
  distrito_1_f <- f_seleccion_cargo("DIPUTACION FEDERAL MR",
                                    "Distrito 1-COLIMA")
  f_mayor_gasto(distrito_1_f)
## graficamos
f_graficado(distrito_1_f,
            c("SERGIO AGUSTIN MORALES ANGUIANO",
              "PATRICIA ALCARAZ PULIDO",
              "RIULT RIVERA GUTIERREZ",
              "ANILU SALAZAR MEJIA"),
            "Desglose de los gastos de campaña de los candidatos del distrito federal 1")
## Distrito Federal 2
distrito_2_f <- f_seleccion_cargo("DIPUTACION FEDERAL MR",
                                  "Distrito 2-MANZANILLO")
f_mayor_gasto(distrito_2_f)
## graficamos
f_graficado(distrito_2_f,
            c("RAFAEL MENDOZA GODINEZ",
              "FRANCISCO CUEVAS MARTIN",
              "OSCAR ARMANDO AVALOS VERDUGO",
              "MARIA DE LOS ANGELES MAGAÑA FERNANDEZ"),
            "Desglose de los gastos de campaña de los candidatos del distrito federal 2")
rm(distrito_1_f,
   distrito_2_f)