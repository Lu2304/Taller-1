# ============================================================= #
#  TALLER 1 - SECCIÓN 1: PERFIL EDAD–INGRESO LABORAL
#  Objetivo:
#   (1) Estimar perfil incondicional (lineal y cuadrático)
#   (2) Estimar perfil condicional (solo horas y relab)
#   (3) Calcular edad pico + IC bootstrap
#   (4) Tabla para slides (R2 + pico + IC) y figura final
#   (5) Guardar outputs para presentación
# ============================================================= #

# ----------------------------- #
# 4) DATA PARA SECCIÓN 1
# ----------------------------- #
df_s1 <- base_clean |>
  filter(!is.na(totalHoursWorked), !is.na(relab)) |>
  mutate(
    age2 = age^2,
    relab = factor(relab)
  )

# ----------------------------- #
# 5) MODELOS (LO QUE PIDE EL ENUNCIADO)
#   - Lineal (para mostrar contribución del cuadrático)
#   - Cuadrático incondicional
#   - Condicional (solo horas + relab)
# ----------------------------- #
m_lin    <- lm(log_w ~ age, data = df_s1)
m_uncond <- lm(log_w ~ age + age2, data = df_s1)
m_cond   <- lm(log_w ~ age + age2 + totalHoursWorked + relab, data = df_s1)

# ----------------------------- #
# 6) EDAD PICO (peak age) + BOOTSTRAP CI
# ----------------------------- #
peak_age <- function(model) {
  b <- coef(model)
  -b["age"] / (2 * b["age2"])
}

peak_uncond <- peak_age(m_uncond)
peak_cond   <- peak_age(m_cond)

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

# ----------------------------- #
# 7) TABLA PARA SLIDES (R2 + PEAK + IC + N)
# ----------------------------- #
tabla_s1 <- tibble(
  Modelo = c("Lineal", "Cuadrático (incond.)", "Condicional (horas + relab)"),
  R2 = c(summary(m_lin)$r.squared,
         summary(m_uncond)$r.squared,
         summary(m_cond)$r.squared),
  Edad_pico = c(NA, peak_uncond, peak_cond),
  IC_95 = c(
    NA,
    sprintf("[%.1f, %.1f]", ci_uncond[1], ci_uncond[2]),
    sprintf("[%.1f, %.1f]", ci_cond[1], ci_cond[2])
  ),
  N = c(nobs(m_lin), nobs(m_uncond), nobs(m_cond))
)

print(tabla_s1)

# Guardar tabla para slides
readr::write_csv(tabla_s1, file.path(path_tables, "tabla_s1_resumen.csv"))

# ----------------------------- #
# 8) TABLA ESTILO PAPER (OPCIONAL, PERO MUY ÚTIL)
# ----------------------------- #
stargazer(
  m_lin, m_uncond, m_cond,
  type = "text",
  title = "Sección 1: Perfil edad–ingreso (log ingreso mensual)",
  column.labels = c("Lineal", "Cuadrático", "Condicional"),
  dep.var.labels = "log(ingreso mensual)",
  out = file.path(path_tables, "tabla_s1_stargazer.txt")
)

# ----------------------------- #
# 9) GRÁFICA DE PERFILES (incondicional vs condicional)
# ----------------------------- #
grid_age <- tibble(
  age = seq(min(df_s1$age, na.rm = TRUE),
            max(df_s1$age, na.rm = TRUE),
            by = 1)
) |>
  mutate(age2 = age^2)

# Predicción incondicional (cuadrático)
pred_uncond <- grid_age
pred_uncond$yhat   <- predict(m_uncond, newdata = grid_age)
pred_uncond$modelo <- "Incondicional"

# Predicción condicional: fijar horas y relab en valores típicos
typ_hours <- median(df_s1$totalHoursWorked, na.rm = TRUE)
mode_relab <- df_s1 |>
  count(relab, sort = TRUE) |>
  slice(1) |>
  pull(relab)

grid_cond <- grid_age |>
  mutate(
    totalHoursWorked = typ_hours,
    relab = factor(mode_relab, levels = levels(df_s1$relab))
  )

pred_cond <- grid_cond
pred_cond$yhat   <- predict(m_cond, newdata = grid_cond)
pred_cond$modelo <- "Condicional (horas + relab)"

pred_all <- bind_rows(pred_uncond, pred_cond)


# ============================================================= #
# 10) GRÁFICA ACADÉMICA 
# ============================================================= #

library(scales)

# Datos para anotar picos
picos_df <- tibble::tibble(
  modelo = c("Incondicional", "Condicional (horas + relab)"),
  pico = c(peak_uncond, peak_cond)
) |>
  mutate(
    lbl = paste0("Pico: ", round(pico, 1), " años"),
    y_pos = c(
      max(pred_all$yhat) + 0.05,  # uno ligeramente arriba
      max(pred_all$yhat) - 0.02   # el otro ligeramente abajo
    ),
    hjust_val = c(-0.1, 1.1)      # separación horizontal elegante
  )

# Colores sobrios académicos
cols <- c(
  "Incondicional" = "#1F4E79",              
  "Condicional (horas + relab)" = "#2E2E2E"
)

p_s1_academica <- ggplot(pred_all, aes(x = age, y = yhat, color = modelo)) +
  geom_line(linewidth = 1.3) +
  
  # Líneas verticales de los picos
  geom_vline(
    data = picos_df,
    aes(xintercept = pico, color = modelo),
    linetype = "22",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  
  # Etiquetas separadas horizontal y verticalmente
  geom_text(
    data = picos_df,
    aes(x = pico, y = y_pos, label = lbl, color = modelo),
    hjust = picos_df$hjust_val,
    size = 3.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  
  labs(
    title = "Perfil edad–ingreso laboral (Bogotá, GEIH 2018)",
    subtitle = "Modelo condicional fija horas trabajadas en la mediana y tipo de empleo en la categoría modal",
    x = "Edad",
    y = "Log del ingreso laboral mensual predicho",
    color = NULL,
    caption = "Fuente: GEIH 2018. Muestra: ocupados, 18+."
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )

p_s1_academica


ggsave(
  filename = file.path(path_figs, "fig_s1_perfil_edad_ingreso_academica.png"),
  plot = p_s1_academica,
  width = 9,
  height = 5.5,
  dpi = 400
)

# ----------------------------- #
# 10) MENSAJES CLAVE (para revisar rápido)
# ----------------------------- #
cat("\n--- CHECKS IMPORTANTES ---\n")
cat("Coef(age2) incondicional:", coef(m_uncond)["age2"], "\n")
cat("Coef(age2) condicional  :", coef(m_cond)["age2"], "\n")
cat("Peak incondicional:", peak_uncond, 
    "IC:", ci_uncond[1], "-", ci_uncond[2], "\n")
cat("Peak condicional  :", peak_cond, 
    "IC:", ci_cond[1], "-", ci_cond[2], "\n")
cat("Figura guardada en:", 
    file.path(path_figs, "fig_s1_perfil_edad_ingreso_academica.png"), "\n")

