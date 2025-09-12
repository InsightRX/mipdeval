# Simulate busulfan data for use in examples and tests:
library(dplyr)
library(tidyr)
library(PKPDsim)

if(!requireNamespace("pkbusulfanshukla", quietly = TRUE)) {
  install_default_literature_model("pk_busulfan_shukla", force = TRUE)
}

set.seed(54321)

n_ids <- 10
covariates_df <- data.frame(
  ID = 1:n_ids,
  AGE = 5 * exp(rnorm(n_ids, 0, .3)),
  WT = 20 * exp(rnorm(n_ids, 0, .2)),
  HT = 100 * exp(rnorm(n_ids, 0, .3)),
  SEX = runif(n_ids, 0.5),
  REGI = 0
)
reg <- new_regimen(amt = 50, interval = 24, t_inf = 3, n = 4, type = 'infusion')

res <- withr::with_package("pkbusulfanshukla", quietly = TRUE, {
  sim(
    ode = model(),
    parameters = parameters(),
    regimen = reg,
    covariates_table = covariates_df,
    omega = omega_matrix(),
    res_var = ruv(),
    t_obs = c(3.5, 5, 6, 8, 12) + rep(c(0, 24, 48, 72), each = 5),
    iov_bins = c(0, 24, 48, 72, 96, 9999),
    output_include = list(covariates = TRUE),
    only_obs = TRUE
  )
})

tdm <- res |>
  rename(ID = id, TIME = t, DV = y) |>
  mutate(CMT = 1, EVID = 0, MDV = 0, AMT = 0, RATE = 0) |>
  select(-comp, -obs_type)

doses <- expand.grid(1:n_ids, reg$dose_times) |>
  rlang::set_names(c("ID", "TIME")) |>
  arrange(ID, TIME) |>
  left_join(select(regimen_to_nm(reg), -ID))

nm_busulfan <- bind_rows(doses, tdm) |>
  arrange(ID, TIME, EVID)

# Save a .csv so this data can be used with PsN:
readr::write_csv(nm_busulfan, "inst/extdata/nm_busulfan.csv")

usethis::use_data(nm_busulfan, overwrite = TRUE)
