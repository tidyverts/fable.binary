test_that("logistic fits", {
  library(broom)
  # Fit models
  df <- melb_rain |> 
    mutate(
      t = dplyr::row_number(),
      sint = sin(2 * pi * t / 365.25),
      cost = cos(2 * pi * t / 365.25)
    )
  fit <- df |>
    model(logistic = LOGISTIC(Wet ~ sint + cost))
  fit_glm <- stats::glm(Wet ~ sint + cost, data = df, family = binomial)

  expect_equal(
    tidy(fit) |> dplyr::select(-.model), 
    tidy(fit_glm)
  )
  expect_equal(
    glance(fit) |> dplyr::select(
      null.deviance = null_deviance, 
      df.null = df_null, 
      logLik = log_lik, 
      AIC, BIC, deviance, df.residual, nobs), 
    glance(fit_glm)
  )
  expect_equal(
    augment(fit) |> as_tibble() |> select(.fitted, .resid, .innov),
    tibble(
      .fitted = unname(fitted(fit_glm)),
      .resid = unname(residuals(fit_glm, type = "response")),
      .innov = unname(residuals(fit_glm, type = "deviance"))
    )
  )
})

