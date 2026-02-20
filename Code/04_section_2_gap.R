# ==============================================================================
# SECCIÓN 2: BRECHA DE GÉNERO (CODIFICACIÓN: 1 = MUJER, 0 = HOMBRE)
# ==============================================================================

#(CODIFICACIÓN: 1 = Hombre, 0 = Mujer)

#0. Análisis descriptivo de variables

base_filtrada$sexo_label <- ifelse(base_filtrada$sex == 1, "Hombre", "Mujer")

#revisar proporciones relab y educación (p6210)
prop.table(table(base_filtrada$relab, base_filtrada$sex), margin = 2) * 100
prop.table(table(base_filtrada$p6210, base_filtrada$sex), margin = 2) * 100


base_filtrada$relab_simple <- case_when(
  base_filtrada$relab %in% c(1,2) ~ "Asalariado",
  base_filtrada$relab == 4 ~ "Cuenta propia",
  base_filtrada$relab == 3 ~ "Empleado doméstico",
  TRUE ~ "Otros"
)

tabla_continuas <- base_filtrada %>%
  group_by(sexo_label) %>%
  summarise(
    Observaciones = n(),
    Ingreso_promedio = mean(exp(ln_y), na.rm = TRUE),
    Edad_promedio = mean(age, na.rm = TRUE),
    Horas_promedio = mean(totalHoursWorked, na.rm = TRUE),
    Tasa_informalidad = mean(informal, na.rm = TRUE) * 100
  )

prop_media_h <- base_filtrada %>%
  filter(sex == 1, p6210 != 9) %>%
  summarise(prop = mean(p6210 == 5) * 100) %>%
  pull(prop)

prop_media_m <- base_filtrada %>%
  filter(sex == 0, p6210 != 9) %>%
  summarise(prop = mean(p6210 == 5) * 100) %>%
  pull(prop)

prop_uni_h <- base_filtrada %>%
  filter(sex == 1, p6210 != 9) %>%
  summarise(prop = mean(p6210 == 6) * 100) %>%
  pull(prop)

prop_uni_m <- base_filtrada %>%
  filter(sex == 0, p6210 != 9) %>%
  summarise(prop = mean(p6210 == 6) * 100) %>%
  pull(prop)

prop_asal_h <- base_filtrada %>%
  filter(sex == 1) %>%
  summarise(prop = mean(relab_simple == "Asalariado") * 100) %>%
  pull(prop)

prop_asal_m <- base_filtrada %>%
  filter(sex == 0) %>%
  summarise(prop = mean(relab_simple == "Asalariado") * 100) %>%
  pull(prop)

prop_cp_h <- base_filtrada %>%
  filter(sex == 1) %>%
  summarise(prop = mean(relab_simple == "Cuenta propia") * 100) %>%
  pull(prop)

prop_cp_m <- base_filtrada %>%
  filter(sex == 0) %>%
  summarise(prop = mean(relab_simple == "Cuenta propia") * 100) %>%
  pull(prop)

prop_dom_h <- base_filtrada %>%
  filter(sex == 1) %>%
  summarise(prop = mean(relab_simple == "Empleado doméstico") * 100) %>%
  pull(prop)

prop_dom_m <- base_filtrada %>%
  filter(sex == 0) %>%
  summarise(prop = mean(relab_simple == "Empleado doméstico") * 100) %>%
  pull(prop)

tabla_final_slide <- data.frame(
  Variable = c("Observaciones",
               "Ingreso promedio (COP)",
               "Edad promedio",
               "Horas trabajadas",
               "Tasa de informalidad (%)",
               "Universitaria (%)",
               "Asalariado (%)",
               "Cuenta propia (%)",
               "Empleado doméstico (%)"),
  Hombres = c(
    tabla_continuas$Observaciones[tabla_continuas$sexo_label=="Hombre"],
    tabla_continuas$Ingreso_promedio[tabla_continuas$sexo_label=="Hombre"],
    tabla_continuas$Edad_promedio[tabla_continuas$sexo_label=="Hombre"],
    tabla_continuas$Horas_promedio[tabla_continuas$sexo_label=="Hombre"],
    tabla_continuas$Tasa_informalidad[tabla_continuas$sexo_label=="Hombre"],
    prop_uni_h,
    prop_asal_h,
    prop_cp_h,
    prop_dom_h
  ),
  Mujeres = c(
    tabla_continuas$Observaciones[tabla_continuas$sexo_label=="Mujer"],
    tabla_continuas$Ingreso_promedio[tabla_continuas$sexo_label=="Mujer"],
    tabla_continuas$Edad_promedio[tabla_continuas$sexo_label=="Mujer"],
    tabla_continuas$Horas_promedio[tabla_continuas$sexo_label=="Mujer"],
    tabla_continuas$Tasa_informalidad[tabla_continuas$sexo_label=="Mujer"],
    prop_uni_m,
    prop_asal_m,
    prop_cp_m,
    prop_dom_m
  )
)

tabla_gt <- tabla_final_slide %>%
  gt() %>%
  tab_header(title = "Estadísticas Descriptivas por Género") %>%
  fmt_number(columns = c(Hombres, Mujeres), decimals = 1)

tabla_gt

gtsave(tabla_gt,
       filename = file.path(path_tables, "S02_tabla_descriptiva.png"))





# 1. Definición de Modelos para la regresión incondicional y condicionales (OLS)

M1 <- as.formula("ln_y ~ sex")
M2 <- as.formula("ln_y ~ sex + age + age2 + as.factor(p6210) ")
M3 <- as.formula("ln_y ~ sex + age + age2 + as.factor(p6210) + informal + totalHoursWorked + as.factor(relab)")

#Estimación de modelos

model_incondicional <- lm(M1, data = base_filtrada)
model_cap_humano    <- lm(M2, data = base_filtrada)
model_completo       <- lm(M3, data = base_filtrada)

#2.  Estimación usando FWL

fwl_estimator <- function(formula_full, data) {
  
  # Separar variable dependiente y regresores
  y_var <- all.vars(formula_full)[1]
  x_vars <- attr(terms(formula_full), "term.labels")
  
  # Separar sex de los demás controles
  controles <- x_vars[x_vars != "sex"]
  
  # Fórmulas auxiliares
  formula_y <- as.formula(
    paste(y_var, "~", paste(controles, collapse = " + "))
  )
  
  formula_s <- as.formula(
    paste("sex ~", paste(controles, collapse = " + "))
  )
  
  # Residualizar
  y_tilde <- resid(lm(formula_y, data = data))
  s_tilde <- resid(lm(formula_s, data = data))
  
  # Regresión FWL
  coef(lm(y_tilde ~ s_tilde))["s_tilde"]
}


coef_fwl_m2 <- fwl_estimator(M2, base_filtrada)
coef_fwl_m3 <- fwl_estimator(M3, base_filtrada)

coef_ols_m2 <- coef(model_cap_humano)["sex"]
coef_ols_m3 <- coef(model_completo)["sex"]

print(c(OLS_M2 = round(coef_ols_m2,5),
        FWL_M2 = round(coef_fwl_m2,5)))

print(c(OLS_M3 = round(coef_ols_m3,5),
        FWL_M3 = round(coef_fwl_m3,5)))


# 3. Bootstrap para Errores Estándar 


boot_all_models <- function(data, indices) {
  df <- data[indices, ]
  
  m1 <- lm(M1, data = df)
  m2 <- lm(M2, data = df)
  m3 <- lm(M3, data = df)
  
  return(c(
    coef(m1)["sex"],
    coef(m2)["sex"],
    coef(m3)["sex"]
  ))
}

set.seed(123)

res_boot_all <- boot(
  data = base_filtrada,
  statistic = boot_all_models,
  R = 1000
)


#Tabla resumen 
se_boot_m1 <- sd(res_boot_all$t[,1])
se_boot_m2 <- sd(res_boot_all$t[,2])
se_boot_m3 <- sd(res_boot_all$t[,3])


se_analitico_m1 <- summary(model_incondicional)$coefficients["sex","Std. Error"]
se_analitico_m2 <- summary(model_cap_humano)$coefficients["sex","Std. Error"]
se_analitico_m3 <- summary(model_completo)$coefficients["sex","Std. Error"]

tabla_gap %>%
  gt() %>%
  tab_header(title = "Brecha Salarial: OLS y FWL") %>%
  fmt_number(
    columns = 2:4,
    rows = Variable %in% c("Hombre",
                           "SE (Analítico)",
                           "SE (Bootstrap)",
                           "R2"),
    decimals = 3
  ) %>%
  fmt_number(
    columns = 2:4,
    rows = Variable == "Num. Obs.",
    decimals = 0
  )

colnames(tabla_gap) <- c(
  "Variable",
  "(1)",
  "(2)",
  "(3)"
)

tabla_regresiones <- tabla_gap %>%
  gt() %>%
  tab_header(title = "Brecha Salarial: OLS y FWL") %>%
  fmt_number(
    columns = 2:4,
    rows = Variable %in% c("Hombre",
                           "SE (Analítico)",
                           "SE (Bootstrap)",
                           "R2"),
    decimals = 3
  ) %>%
  fmt_number(
    columns = 2:4,
    rows = Variable == "Num. Obs.",
    decimals = 0
  )


gtsave(
  tabla_regresiones,
  filename = file.path(path_tables, "S02_tabla_gap.png")
)


# Picos de edad 

model_interaccion <- lm(
  ln_y ~ sex * (age + age2) + as.factor(p6210),
  data = base_filtrada
)



boot_peak <- function(data, indices) {
  df <- data[indices, ]
  
  fit <- lm(
    ln_y ~ sex * (age + age2) + as.factor(p6210),
    data = df
  )
  
  b <- coef(fit)
  
  # Mujer (sex = 0)
  peak_m <- - b["age"] / (2 * b["age2"])
  
  # Hombre (sex = 1)
  peak_h <- - (b["age"] + b["sex:age"]) /
    (2 * (b["age2"] + b["sex:age2"]))
  
  return(c(peak_m, peak_h))
}

set.seed(123)

res_boot_peak <- boot(
  data = base_filtrada,
  statistic = boot_peak,
  R = 1000
)

peak_m_est <- res_boot_peak$t0[1]
peak_h_est <- res_boot_peak$t0[2]

p_m <- round(peak_m_est, 1)
p_h <- round(peak_h_est, 1)

#Calcular intervalos de confianza

ci_peak_m <- boot.ci(res_boot_peak, index = 1, type = "perc")
ci_peak_h <- boot.ci(res_boot_peak, index = 2, type = "perc")

ci_m <- round(ci_peak_m$percent[4:5], 1)
ci_h <- round(ci_peak_h$percent[4:5], 1)

#Gráfico

age_seq <- seq(
  min(base_filtrada$age, na.rm = TRUE),
  max(base_filtrada$age, na.rm = TRUE),
  by = 1
)

df_plot <- expand.grid(
  age = age_seq,
  sex = c(0,1),
  p6210 = levels(as.factor(base_filtrada$p6210))[1]
)

df_plot$age2 <- df_plot$age^2

df_plot$pred <- predict(model_interaccion, newdata = df_plot)

library(ggplot2)

plot_pico_edad <- ggplot(df_plot,
                     aes(x = age, y = pred, color = as.factor(sex))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("#d7191c", "#2c7bb6"),
    labels = c("Mujer", "Hombre")
  ) +
  labs(
    x = "Edad",
    y = "Log(Ingreso)",
    color = "Género",
    title = "Perfiles de ingreso por edad"
  ) +
  theme_minimal()

ggsave(
  file.path(path_figures, "S02_perfiles_edad.png"),
  plot = plot_pico_edad,
  width = 7,
  height = 4
)


#Tabla

tabla_picos <- data.frame(
  Grupo = c("Mujer", "Hombre"),
  Edad_Pico = c(p_m, p_h),
  IC_95_inf = c(ci_m[1], ci_h[1]),
  IC_95_sup = c(ci_m[2], ci_h[2])
)


tabla_picos_gt <- tabla_picos %>%
  gt() %>%
  tab_header(
    title = "Edad Pico del Ingreso por Género",
    subtitle = "Intervalos de confianza bootstrap (percentil 95%)"
  ) %>%
  fmt_number(
    columns = c(Edad_Pico, IC_95_inf, IC_95_sup),
    decimals = 1
  )



tabla_picos_gt <- tabla_picos %>%
  gt() %>%
  tab_header(
    title = "Edad Pico del Ingreso por Género",
    subtitle = "Intervalos de confianza bootstrap (percentil 95%)"
  ) %>%
  fmt_number(
    columns = c(Edad_Pico, IC_95_inf, IC_95_sup),
    decimals = 1
  )

gtsave(
  tabla_picos_gt,
  filename = file.path(path_tables, "S02_tabla_picos_edad.png")
)
