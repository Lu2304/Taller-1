# Ver relación entre ingreso y edad
plot_ingreso_edad_fil <- ggplot(
  data = base_filtrada,
  aes(x = age, y = log(y_total_m))
) +
  geom_point(alpha = 0.25, color = "gray40") +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "blue",
    linewidth = 1
  ) +
  labs(
    x = "Edad",
    y = "Log(Ingreso laboral mensual)",
    title = "Relación entre edad e ingreso laboral"
  ) +
  theme_minimal()

plot_ingreso_edad_fil

fig1_path <- file.path(path_figures, "plot_ingreso_edad_fil.png")

ggsave(
  filename = fig1_path,
  plot = plot_ingreso_edad_fil,
  width = 8, height = 6, dpi = 300
)

# ============================================================
# Section 1 (Punto 4)
# ============================================================
base_reg <- base_filtrada |> 
  mutate(age2 = age^2, 
         log_w = log(y_total_m),
         female = if_else(sex == 0, 1, 0),
         relab  = as.factor(relab),
         informal = as.factor(informal))

head# Regresiónes 
reg_lin_nc <- lm(log_w ~ age, data = base_reg)
reg_quad_nc <- lm(log_w ~ age + age2, data = base_reg)
reg_lin_c <- lm(log_w ~ age + totalHoursWorked + relab, data = base_reg)
reg_quad_c <- lm(log_w ~ age + age2 + totalHoursWorked + relab, data = base_reg)

peak_age_fun <- function(data, index) {
  f <- lm(log_w ~ age + age2, data = data, subset = index)
  
}

peak_uncond_fn <- function(data, index) {
  f <- lm(log_w ~ age + age2, data = data, subset = index)
  coefs <- coef(f)
  b_age  <- coefs["age"]
  b_age2 <- coefs["age2"]
  peak_age <- -b_age / (2 * b_age2)
}

set.seed(123)

B <- 10000

boot_peak_uncond <- boot(data = base_reg,      statistic = peak_uncond_fn, R = B)
ci_uncond <- boot.ci(boot_peak_uncond, type = "perc")

# ============================================================
# Section 2 (Punto 5)
# ============================================================
# Especificación no condicional
reg_sex_nc <- lm(log_w ~ female, data = base_reg)
reg_y_sex_c <- lm(log_w ~ female + relab + informal, data = base_reg)

# Especificaciones condicionales
reg_sex_X <- lm(female ~ relab + informal, data = base_reg)
sex_resid <- resid(reg_sex_X)
reg_y_X <- lm(log_w ~ relab+informal, data = base_reg)
y_resid <- resid(reg_y_X)
reg_sex_c <- lm(y_resid ~ sex_resid)

coef(reg_sex_nc)["female"] 
coef(reg_y_sex_c)["female"]
coef(reg_sex_c)["sex_resid"]

# SE analítico (OLS)
se_analitico <- summary(reg_sex_c)$coefficients["sex_resid", "Std. Error"]

#SE por Bootstrap
fwl_stat <- function(data, index) {
  d <- data[index, , drop = FALSE]
  reg_sex_X_b <- lm(female ~ relab + informal, data = d)
  sex_resid_b <- resid(reg_sex_X_b)
  
  reg_y_X_b <- lm(log_w ~ relab + informal, data = d)
  y_resid_b <- resid(reg_y_X_b)
  
  reg_sex_c_b <- lm(y_resid_b ~ sex_resid_b)
  
  coef(reg_sex_c_b)[["sex_resid_b"]]
}

set.seed(123)
B_fwl <- 10000
boot_out <- boot(data = base_reg, statistic = fwl_stat, R = B)

se_boot <- sd(boot_out$t, na.rm = TRUE)
se_boot

# ============================================================
# Exportar resultados
# ============================================================
tab_p4 <- bind_rows(
  tidy(reg_lin_nc)  |>  mutate(modelo = "Lin sin controles"),
  tidy(reg_quad_nc) |> mutate(modelo = "Quad sin controles"),
  tidy(reg_lin_c)   |>  mutate(modelo = "Lin con controles"),
  tidy(reg_quad_c)  |>  mutate(modelo = "Quad con controles")
) |> 
  filter(term %in% c("(Intercept)", "age", "age2")) |> 
  select(modelo, term, estimate, std.error, statistic, p.value) |> 
  mutate(across(c(estimate, std.error, statistic, p.value), ~ round(.x, 4)))

glance_p4 <- bind_rows(
  glance(reg_lin_nc)  |>  mutate(modelo = "Lin sin controles"),
  glance(reg_quad_nc) |>  mutate(modelo = "Quad sin controles"),
  glance(reg_lin_c)   |>  mutate(modelo = "Lin con controles"),
  glance(reg_quad_c)  |>  mutate(modelo = "Quad con controles")
) |> 
  select(modelo, nobs, r.squared) |> 
  mutate(r.squared = round(r.squared, 4))

peak_hat <- boot_peak_uncond$t0
ci_peak  <- ci_uncond$percent[4:5]   # low/high percentil

tab_peak <- tibble(
  estimacion = "Peak age (quad sin controles)",
  peak_age_hat = round(peak_hat, 4),
  ci95_low_perc = round(ci_peak[1], 4),
  ci95_high_perc = round(ci_peak[2], 4),
  B = B
)

tab_p5_beta <- tibble(
  modelo = c("No condicional: log_w ~ female",
             "Condicional (directo): log_w ~ female + relab + informal",
             "Condicional (FWL): y_resid ~ sex_resid"),
  beta_female = c(
    coef(reg_sex_nc)[["female"]],
    coef(reg_y_sex_c)[["female"]],
    coef(reg_sex_c)[["sex_resid"]]
  )
) |> 
  mutate(beta_female = round(beta_female, 4))

tab_p5_se <- tibble(
  objeto = "Coeficiente género (FWL: sex_resid)",
  se_analitico = round(se_analitico, 4),
  se_bootstrap = round(se_boot, 4),
  B = B_fwl
)

doc_path <- file.path(path_tables, "Resultados_Taller_PS1.docx")

doc <- read_docx() |> 
  body_add_par("Resultados Taller - Problem Set 1", style = "heading 1") |> 
  
  # Figura
  body_add_par("Figura 1. Relación entre edad e ingreso laboral (log)", style = "heading 2") |> 
  body_add_img(src = fig1_path, width = 6.5, height = 4.8) |> 
  body_add_par(" ", style = "Normal") |> 
  
  # Punto 4
  body_add_par("Section 1 - Punto 4: Regresiones edad–ingreso", style = "heading 1") |> 
  body_add_par("Tabla 1. Coeficientes clave (Intercept, age, age2)", style = "heading 2") |> 
  body_add_flextable(flextable(tab_p4) |>  autofit() |>  theme_booktabs()) |> 
  body_add_par("Tabla 2. Ajuste del modelo (N y R²)", style = "heading 2") |> 
  body_add_flextable(flextable(glance_p4) |>  autofit() |>  theme_booktabs()) |> 
  body_add_par("Tabla 3. Peak age (bootstrap percentil 95%)", style = "heading 2") |> 
  body_add_flextable(flextable(tab_peak) |>  autofit() |>  theme_booktabs()) |> 
  
  # Punto 5
  body_add_par("Section 2 - Punto 5: Gender labor income gap", style = "heading 1") |> 
  body_add_par("Tabla 4. Coeficiente de género (no condicional vs condicional)", style = "heading 2") |> 
  body_add_flextable(flextable(tab_p5_beta) |>  autofit() |>  theme_booktabs()) |> 
  body_add_par("Tabla 5. Errores estándar del coeficiente de género (FWL)", style = "heading 2") |> 
  body_add_flextable(flextable(tab_p5_se) |>  autofit() |>  theme_booktabs())

print(doc, target = doc_path)
doc_path




