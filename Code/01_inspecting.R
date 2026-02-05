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
