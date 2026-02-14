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

# Muestra: 18+ y ocupados
base_samp <- base_completa |>
  filter(age >= 18, ocu == 1)

# Revisar ingreso
summary(base_samp$y_total_m)

# (Opcional) Relación de NA con informalidad
prop.table(
  table(is.na(base_samp$y_total_m), base_samp$informal),
  margin = 2
)

# Filtrar ingresos válidos para log: no NA y > 0
base_clean <- base_samp |>
  filter(!is.na(y_total_m), y_total_m > 0) |>
  mutate(
    log_w = log(y_total_m),
    age2 = age^2,
    relab = factor(relab)
  )

# Gráfica rápida (ya sin -Inf)
ggplot(
  base_clean,
  aes(x = age, y = log_w, color = factor(informal))
) +
  geom_point(alpha = 0.25) +
  theme_minimal() +
  labs(color = "Informal", y = "log(ingreso mensual)", x = "Edad")

#Preparar datos (log ingreso + age²)
library(dplyr)

df_s1 <- base_clean |>
  filter(y_total_m > 0, !is.na(totalHoursWorked), !is.na(relab)) |>
  mutate(
    log_w = log(y_total_m),
    age2 = age^2,
    relab = factor(relab)
  )
#modelo condicional 
# Incondicional: log(w) ~ age + age^2
m_uncond <- lm(log_w ~ age + age2, data = df_s1)

# Condicional: + horas + tipo de empleo (SOLO estos controles)
m_cond <- lm(log_w ~ age + age2 + totalHoursWorked + relab, data = df_s1)

#Calcular “edad pico” (peak age)
peak_age <- function(model) {
  b <- coef(model)
  -b["age"] / (2 * b["age2"])
}

peak_uncond <- peak_age(m_uncond)
peak_cond <- peak_age(m_cond)

peak_uncond
peak_cond

#Bootstrap para intervalo de confianza del pico
boot_peak <- function(data, formula, R = 1000, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  
  peaks <- replicate(R, {
    idx <- sample.int(n, n, replace = TRUE)
    m <- lm(formula, data = data[idx, ])
    b <- coef(m)
    -b["age"] / (2 * b["age2"])
  })
  
  peaks <- peaks[is.finite(peaks)]
  quantile(peaks, c(0.025, 0.975))
}

ci_uncond <- boot_peak(df_s1, log_w ~ age + age2, R = 1000)
ci_cond   <- boot_peak(df_s1, log_w ~ age + age2 + totalHoursWorked + relab, R = 1000)

ci_uncond
ci_cond

#Gráfica de perfiles (incondicional vs condicional)
library(ggplot2)
library(tibble)

# Valores típicos
typ_hours <- median(df_s1$totalHoursWorked, na.rm = TRUE)
mode_relab <- df_s1 |>
  count(relab, sort = TRUE) |>
  slice(1) |>
  pull(relab)

# Creamos grid para el modelo condicional
grid_cond <- grid_age |>
  mutate(
    totalHoursWorked = typ_hours,
    relab = factor(mode_relab, levels = levels(df_s1$relab))
  )

# Predicción condicional
pred_cond <- grid_cond
pred_cond$yhat <- predict(m_cond, newdata = grid_cond)
pred_cond$modelo <- "Condicional (horas + relab)"

library(dplyr)

pred_all <- bind_rows(pred_uncond, pred_cond)

ggplot(pred_all, aes(age, yhat, color = modelo)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "Edad",
    y = "Log ingreso mensual predicho",
    color = NULL,
    title = "Perfil edad–ingreso: incondicional vs condicional"
  )


