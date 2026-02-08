
# install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

pacman::p_load(
  readr,        # Importar datos (ya incluido en tidyverse)
  labelled,     # Manejo de etiquetas
  naniar,       # Visualizar datos faltantes
  DataExplorer, # Gráficos de missing values
  psych,        # Estadísticas descriptivas
  rvest,        # Web scraping
  rio,          # Importar/exportar datos
  tidyverse,    # Conjunto de paquetes para tidy data (incluye dplyr, ggplot2, etc.)
  skimr,        # Resumen de datos
  visdat,       # Visualizar datos faltantes
  corrplot,     # Gráficos de correlación
  gridExtra,    # Organización de gráficos
  MASS,         # Funciones estadísticas diversas
  stargazer,    # Tablas para salida a TEX
  chromote,     # Automatización de navegador (útil para scraping avanzado)
  ggplot2,      # Gráficos (ya incluido en tidyverse)
  boot,         # Funciones de bootstrap
  patchwork,    # Combinación de gráficos
  dplyr
)

# ------------------------------------------------------------- #
# ------------------Web scraping --------------------- 
# ------------------------------------------------------------- #

if (!dir.exists("Bases")) {
  dir.create("Bases")
  cat("Carpeta 'Bases' creada.\n")
}

#Se revisa Html,pero las bases no aparecen directamente. 
# El contenido se carga dinámicamente utilizando el atributo "w3-include-html"
#Por lo que se scrapea directamente los archivos html que contienen las tablas

urls <- paste0(
  "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
  1:10,
  ".html"
)

#guardar en listas cada tabla

lista_tablas <- lapply(urls, function(x) {
  read_html(x) %>%
    html_element("table") %>%
    html_table()
})

#validar que los nombres de las columnas conincidan antes de pegar

nombres_columnas <- lapply(lista_tablas, names)
ref <- nombres_columnas[[1]]
sapply(nombres_columnas, function(x) identical(x, ref))

#Todos los nombres coinciden

#Unir las bases
base_completa <- bind_rows(lista_tablas)


# ------------------------------------------------------------- #
# ------------------limpieza de datos --------------------- 
# ------------------------------------------------------------- #

#solo nos interesan mayores de edad y ocupados

summary(base_completa$age)
summary(base_completa$ocu)

base_filtrada1 <- base_completa %>%
  filter(age >= 18, ocu == 1)

#Variable de resultado y_total_m
summary(base_completa$y_total_m)  

#El sub registro de ingreso está más asociado a informales 

prop.table(
  table(is.na(base_filtrada1$y_total_m), base_filtrada1$informal),
  margin = 2
)

# Gráfica dispersión ingresos por edad y nivel de informalidad
ggplot(data = base_filtrada1 , 
       mapping = aes(x = age , y = log(y_total_m) , group=as.factor(informal) , color=as.factor(informal))) +
  geom_point()

#Eliminamos los NAs

base_filtrada2 <- base_filtrada1 %>%
  filter(!is.na(y_total_m))





###





# Funciones para conocer la estructura general de los datos
head(GEIH_BOG_01)
tail(GEIH_BOG_01)
glimpse(GEIH_BOG_01)
skim(GEIH_BOG_01) |> head()

# Definir variables de interés
vars_interes <- c("sex", "age", "relab", "y_total_m", "ocu", "totalHoursWorked",
                  "p6210", "sizeFirm")

# Resunem estadística descriptiva
des_vars <- c("sex", "age", "relab","y_total_m", "ocu","totalHoursWorked",
              "p6210", "sizeFirm")

stargazer(as.data.frame(GEIH_BOG_01[, des_vars]), type = "text")

# Revisar valores únicos 
tabla_valores_unicos <- GEIH_BOG_01 |> 
  select(all_of(vars_interes)) |> 
  summarise(across(
    everything(),
    ~ n_distinct(.x, na.rm = TRUE)
  )) |> 
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_valores_unicos"
  )

tabla_valores_unicos

unique(GEIH_BOG_01$sex)
unique(GEIH_BOG_01$relab)
unique(GEIH_BOG_01$ocu)
unique(GEIH_BOG_01$p6210)
unique(GEIH_BOG_01$sizeFirm)

# Revisar NAs. Vemos que las variables con más proporción de NAs son:
# el ingreso laboral, el tipo de trabajo, las horas trabajadas y el
# tamaño de la firma. 

tabla_na <- GEIH_BOG_01 |> 
  select(all_of(vars_interes)) |> 
  summarise(across(
    everything(),
    ~ mean(is.na(.x)) * 100
  )) |> 
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "porcentaje_NA"
  ) |> 
  arrange(desc(porcentaje_NA))

tabla_na

# Ver correlación entre los NAs de las variables. De la gráfica se puede 
# observar que todas las correlaciones son positivas dado el color azul. 
# Además, el tamaño del círculo ayuda a entender la magnitud de la 
# correlación. Los valores faltantes de relab están altamente correlacionados
# positivamente con los ingresos laborales, y ,especialmente, con las
# horas trabajadas y el tamaño de la firma. Lo cual tiene sentido
# dado que no se observó el tipo de trabajo y tampoco ingresos laborales, 
# horas trabajadas o tamaño de la empresa. Lo que puede significar que esa persona
# no está ocupada. Los ingresos laborales están correlacionados con horas
# trabajadas, tipo de trabajo y tamaño de la firma por la razón explicada 
# anteriormente.

vars_interes_NA <- GEIH_BOG_01 |> 
  select(all_of(vars_interes))

vars_miss <-vars_interes_NA |> 
  mutate(across(everything(), ~ ifelse(is.na(.x), 1, 0)))

vars_miss <- vars_miss |> 
  select(which(apply(vars_miss, 2, sd) > 0))

mcorrmiss <- cor(vars_miss)
corrplot(mcorrmiss)

# Ver datos filtrados por edad (+18) y por ocupados y volvemos a hacer la tabla 
# de porcentaje de NAs. Después de filtrar los datos podemos observar que ahora 
# para la mayoría de variables no hay NAs y para el ingreso bajó el porcentaje 
# de NAs de 57.9% a 13.5%
df_filtrado <- GEIH_BOG_01 |> 
  filter(age >= 18, ocu == 1) |> 
  select(all_of(vars_interes))

tabla_na_filtrado <- df_filtrado |> 
  summarise(across(
    everything(),
    ~ mean(is.na(.x)) * 100
  )) |> 
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "porcentaje_NA"
  ) |> 
  arrange(desc(porcentaje_NA))

tabla_na_filtrado
