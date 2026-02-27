lista_tablas <- lapply(lista_tablas, janitor::clean_names)

base_chunks <- bind_rows(
  lapply(seq_along(lista_tablas), function(k) {
    lista_tablas[[k]] |>  mutate(chunk = k)
  })
)

base_chunks_filtrada <- base_chunks |>
  filter(age >= 18, ocu == 1, relab %in% 1:7) |>
  drop_na(y_total_m)

des_vars <- c("y_total_m", "age", "total_hours_worked", "relab", "p6210",
              "size_firm", "informal", "sex", "p6426")

stargazer(
  as.data.frame(base_chunks_filtrada[, des_vars]),
  type = "text",
  summary = TRUE,
  out = file.path(path_tables, "tabla_descriptivas_predict.txt")
)

base_chunks_filtrada <- base_chunks_filtrada %>%
  mutate(
    p6210 = factor(p6210,
                   levels = c(1,2,3,4,5,6,9),
                   labels = c(
                     "Ninguno",
                     "Preescolar",
                     "Básica primaria",
                     "Básica secundaria",
                     "Media",
                     "Superior / universitaria",
                     "No sabe / no informa"
                   )
    ),
    
    relab = factor(relab,
                   levels = 1:9,
                   labels = c(
                     "Empleado empresa privada",
                     "Empleado gobierno",
                     "Empleado doméstico",
                     "Cuenta propia",
                     "Empleador / patrón",
                     "Familiar sin remuneración",
                     "Sin remuneración en otros hogares",
                     "Jornalero / peón",
                     "Otro"
                   )
    )
  )

base_chunks_filtrada <- base_chunks_filtrada %>%
  mutate(
    size_firm = factor(size_firm,
                       levels = 1:5,
                       labels = c(
                         "Trabajador independiente",
                         "2 a 5 trabajadores",
                         "6 a 10 trabajadores",
                         "11 a 50 trabajadores",
                         "Más de 50 trabajadores"
                       )
    )
  )
tabla_bonita <- base_chunks_filtrada %>%
  droplevels() %>%
  select(all_of(des_vars)) %>%
  tbl_summary(
    label = list(
      y_total_m ~ "Ingreso laboral mensual",
      age ~ "Edad",
      total_hours_worked ~ "Horas trabajadas por semana",
      relab ~ "Ocupación",
      p6210 ~ "Nivel educativo",
      size_firm ~ "Tamaño de la empresa",
      informal ~ "Empleo informal",
      sex ~ "Sexo",
      p6426 ~ "Antigüedad en el empleo"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()
gtsave(
  as_gt(tabla_bonita),
  filename = file.path(path_figures, "tabla_descriptivas.png")
)


training   <- base_chunks_filtrada |>  filter(chunk %in% 1:7)
validation <- base_chunks_filtrada |>  filter(chunk %in% 8:10)

training <- training %>%
  mutate(
    log_w = log(y_total_m),
    age2  = age^2,
    exper_yrs = p6426 / 12,
    exper_yrs2 = exper_yrs^2,
    relab = as.factor(relab),
    informal = as.factor(informal),
    female = if_else(sex == 0, 1, 0)  
  ) %>%
  filter(is.finite(log_w))

validation <- validation %>%
  mutate(
    log_w = log(y_total_m),
    age2  = age^2,
    exper_yrs = p6426 / 12,
    exper_yrs2 = exper_yrs^2,
    relab = as.factor(relab),
    informal = as.factor(informal),
    female = if_else(sex == 0, 1, 0)
  ) %>%
  filter(is.finite(log_w))

# Especificaciones
form_s1_age   <- log_w ~ age + age2 + total_hours_worked + relab  
form_s2_gap     <- log_w ~ female + age + age2 + p6210

form_1 <- log_w ~ age + age2 + total_hours_worked + relab + p6210 + size_firm + informal + exper_yrs + exper_yrs2 + female
form_2 <- log_w ~ poly(age,3,raw=TRUE) + total_hours_worked + relab + p6210 + size_firm + informal +  poly(exper_yrs, 3, raw=TRUE) + female
form_3 <- log_w ~ poly(age,3,raw=TRUE) + total_hours_worked + relab + p6210 + size_firm + informal + poly(exper_yrs, 3, raw=TRUE):female + female
form_4 <- log_w ~ poly(age,3,raw=TRUE) + total_hours_worked + relab + p6210 + size_firm + informal +  poly(exper_yrs, 3, raw=TRUE) +
  female + relab:informal
form_5 <- log_w ~ poly(age,3,raw=TRUE) + total_hours_worked + relab + p6210 + size_firm + informal + poly(exper_yrs, 3, raw=TRUE) +
  female + relab:female + informal:female

fit_and_rmse <- function(formula, train_df, valid_df) {
  m <- lm(formula, data = train_df)
  pred <- predict(m, newdata = valid_df)
  rmse <- RMSE(pred = pred, obs = valid_df$log_w)
  list(model = m, rmse = rmse)
}

specs <- list(
  "S1_age"   = form_s1_age,
  "S2_gap"    = form_s2_gap,
  "Extra_1"   = form_1,
  "Extra_2"   = form_2,
  "Extra_3"   = form_3,
  "Extra_4"   = form_4,
  "Extra_5"   = form_5
)

results <- imap(specs, ~ fit_and_rmse(.x, training, validation))

rmse_table <- tibble(
  Spec = names(results),
  RMSE_validation = map_dbl(results, "rmse")
) |>  arrange(RMSE_validation)

rmse_table

rmse_table_export <- rmse_table |> 
  mutate(across(where(is.numeric), ~ round(., 3)))

write.csv(
  rmse_table_export,
  file = file.path(path_tables, "rmse_validation_models.csv"),
  row.names = FALSE
)

best_spec <- rmse_table$Spec[1]
best_model <- results[[best_spec]]$model
best_spec
summary(best_model)

e <- resid(best_model)
h <- hatvalues(best_model)
rmse_loocv <- sqrt(mean((e / (1 - h))^2))

rmse_loocv

rmse_compare <- tibble(
  Metric = c("Validation RMSE", "LOOCV RMSE"),
  Value  = c(rmse_table$RMSE_validation[1], rmse_loocv)
)

rmse_compare_export <- rmse_compare %>%
  mutate(Value = round(Value, 2))

write.csv(
  rmse_compare_export,
  file = file.path(path_tables, "rmse_best_model.csv"),
  row.names = FALSE
)

c(RMSE_validation_best = rmse_table$RMSE_validation[1],
  RMSE_loocv_best = rmse_loocv)


diagnostics <- model.frame(best_model) |> 
  mutate(
    pred_in = fitted(best_model),
    resid_in = resid(best_model),
    h_ii = hatvalues(best_model),
    loo_resid = resid_in / (1 - h_ii),
    loo_sqerr = loo_resid^2
  )

top_loo_neg <- diagnostics |>
  filter(loo_resid < 0) |>
  arrange(loo_resid) |>
  slice(1:10)

top_loo_neg

top_loo_neg_export <- top_loo_neg %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.csv(
  top_loo_neg_export,
  file = file.path(path_tables, "top10_loo_errors.csv"),
  row.names = FALSE
)

mf <- model.frame(best_model)
y  <- model.response(mf)
X  <- model.matrix(best_model)  

X_std <- X

if ("(Intercept)" %in% colnames(X_std)) {
  j <- which(colnames(X_std) != "(Intercept)")
  X_std[, j] <- scale(X_std[, j])
}

beta_hat <- solve(t(X_std) %*% X_std, t(X_std) %*% y)

H_diag <- diag(X_std %*% solve(t(X_std) %*% X_std) %*% t(X_std))
e_std  <- as.vector(y - X_std %*% beta_hat)

G_inv <- solve(t(X_std) %*% X_std)

N <- nrow(X_std)
influence_beta <- numeric(N)

for (i in 1:N) {
  adj <- as.numeric(e_std[i] / (1 - H_diag[i]))
  beta_minus_i <- beta_hat - adj * (G_inv %*% X_std[i, ])
  influence_beta[i] <- sum((beta_minus_i - beta_hat)^2)
}

diagnostics$beta_influence <- influence_beta

top_infl <- diagnostics %>%
  arrange(desc(beta_influence)) %>%
  slice(1:10)

top_infl

top_infl_export <- top_infl %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write.csv(
  top_infl_export,
  file = file.path(path_tables, "top10_beta_influence.csv"),
  row.names = FALSE
)

summary_tbl <- diagnostics %>%
  summarise(
    rmse_in_sample = sqrt(mean(resid_in^2)),
    rmse_loocv = sqrt(mean(loo_sqerr)),
    p95_loo_sqerr = quantile(loo_sqerr, 0.95),
    p99_loo_sqerr = quantile(loo_sqerr, 0.99),
    p95_beta_infl = quantile(beta_influence, 0.95),
    p99_beta_infl = quantile(beta_influence, 0.99)
  )

summary_tbl

summary_tbl_export <- summary_tbl %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.csv(
  summary_tbl_export,
  file = file.path(path_tables, "best_model_diagnostics_summary.csv"),
  row.names = FALSE
)

ggplot(diagnostics, aes(x = beta_influence, y = loo_sqerr)) +
  geom_point(alpha = 0.3) +
  labs(x = "||beta(-i) - beta||^2 (X estandarizado)",
       y = "LOO squared error",
       title = "Dónde falla el modelo: error LOO vs influencia en coeficientes") +
  theme_minimal()

ggsave(
  filename = file.path(path_figures, "loo_error_vs_beta_influence.png"),
  width = 8,
  height = 6,
  dpi = 300
)
