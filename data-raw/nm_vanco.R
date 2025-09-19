# Simulate vancomycin data for use in examples and tests:
library(tibble)
library(dplyr)
library(tidyr)
library(PKPDsim)

if(!requireNamespace("pkvancothomson", quietly = TRUE)) {
  install_default_literature_model("pk_vanco_thomson", force = TRUE)
}

set.seed(123456)

n_ids <- 10
covariates_df <- data.frame(
  ID = 1:n_ids,
  WT = 70 * exp(rnorm(n_ids, 0, .2)),
  CRCL = 4.5 * exp(rnorm(n_ids, 0, .2)),
  CL_HEMO = 0
)
reg <- new_regimen(amt = 1500, interval = 12, t_inf = 2, type = "infusion")

res <- withr::with_package("pkvancothomson", quietly = TRUE, {
  sim(
    ode = model(),
    parameters = parameters(),
    regimen = reg,
    covariates_table = covariates_df,
    omega = omega_matrix(),
    res_var = ruv(),
    t_obs = c(15, 23, 50, 55, 71),
    output_include = list(covariates = TRUE),
    only_obs = TRUE
  )
})

tdm <- res |>
  rename(ID = id, TIME = t, DV = y) |>
  mutate(CMT = 1, EVID = 0, MDV = 0, AMT = 0, RATE = 0) |>
  mutate(DV = round(DV, 1)) |>
  select(-comp, -obs_type) |>
  filter(DV > 0)

doses <- expand.grid(1:n_ids, reg$dose_times) |>
  rlang::set_names(c("ID", "TIME")) |>
  arrange(ID, TIME) |>
  left_join(select(regimen_to_nm(reg), -ID))

nm_vanco <- bind_rows(doses, tdm) |>
  arrange(ID, TIME, EVID) |>
  fill(c(WT, CRCL, CL_HEMO), .direction = "downup")

# Make sure output is an ungrouped tibble:
nm_vanco <- as_tibble(ungroup(nm_vanco))

# Save a .csv so this data can be used with PsN:
readr::write_csv(nm_vanco, "inst/extdata/nm_vanco.csv")

usethis::use_data(nm_vanco, overwrite = TRUE)
