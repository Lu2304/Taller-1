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
    y = "Ingreso laboral mensual",
    title = "Relación entre edad e ingreso laboral"
  ) +
  theme_minimal()

plot_ingreso_edad_fil

# ============================================================
# Section 1 (Punto 4)
# ============================================================
base_reg <- base_filtrada |> 
  mutate(age2 = age^2, 
         log_w = log(y_total_m))

# Regresiónes 
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
