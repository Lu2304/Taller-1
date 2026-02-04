# Funciones para conocer la estructura general de los datos
head(GEIH_BOG_01)
tail(GEIH_BOG_01)
glimpse(GEIH_BOG_01)
skim(GEIH_BOG_01) |> head()

# Análisis de variables de interés
unique(GEIH_BOG_01$sex)
unique(GEIH_BOG_01$age)
unique(GEIH_BOG_01$relab)
unique(GEIH_BOG_01$y_total_m)
unique(GEIH_BOG_01$totalHoursWorked)
unique(GEIH_BOG_01$p6210)
unique(GEIH_BOG_01$sizeFirm)
unique(GEIH_BOG_01$ocu)

des_vars <- c("age", "y_total_m", "totalHoursWorked")
stargazer(as.data.frame(GEIH_BOG_01[, des_vars]), type = "text")

sum(is.na(GEIH_BOG_01$sex))
sum(is.na(GEIH_BOG_01$age))
sum(is.na(GEIH_BOG_01$relab))
sum(is.na(GEIH_BOG_01$y_total_m))
sum(is.na(GEIH_BOG_01$totalHoursWorked))
sum(is.na(GEIH_BOG_01$p6210))
sum(is.na(GEIH_BOG_01$sizeFirm))
sum(is.na(GEIH_BOG_01$ocu))



