## Create simulation dataset(s) for use in tests
require(dplyr)
require(PKPDsim)
regenerate <- FALSE

## Vancomycin test data:
if(!requireNamespace("pkvancothomson", quietly = TRUE)) {
  PKPDsim::install_default_literature_model("pk_vanco_thomson")
  loadNamespace("pkvancothomson")
}
if(regenerate) {
  require(pkvancothomson)
  mod <- pkvancothomson::model()
  par <- pkvancothomson::parameters()
  omega <- pkvancothomson::omega_matrix()
  n_ids <- 10
  set.seed(123456)
  covariates_df <- data.frame(
    ID = 1:n_ids,
    WT = 70 * exp(rnorm(n_ids, 0, .2)),
    CRCL = 4.5 * exp(rnorm(n_ids, 0, .2)),
    CL_HEMO = 0
  )
  reg <- new_regimen(amt = 1500, interval = 12, t_inf = 2, type = 'infusion')
  res <- sim(
    mod,
    parameters = par,
    regimen = reg,
    covariates_table = covariates_df,
    omega = omega,
    res_var = pkvancothomson::ruv(),
    t_obs = c(15, 23, 50, 55, 71),
    output_include = list(covariates = T),
    only_obs = TRUE
  )
  tdm <- res |>
    rename(ID = id, TIME = t, DV = y) |>
    mutate(CMT = 1, EVID = 0, MDV = 0, AMT = 0, RATE = 0) |>
    mutate(DV = round(DV, 1)) |>
    select(-comp, -obs_type)
  doses <- left_join(
    expand.grid(1:n_ids, reg$dose_times) |> setNames(c("ID", "TIME")) |> arrange(ID, TIME),
    PKPDsim::regimen_to_nm(reg) |> select(-ID)
  )
  nm_vanco <- bind_rows(
    doses,
    tdm |> filter(DV > 0)
  ) |>
    arrange(ID, TIME, EVID) |>
    tidyr::fill(c(WT, CRCL, CL_HEMO), .direction = "downup")
  write.csv(nm_vanco, "./inst/data/nm_vanco.csv", quote=F, row.names=F)
}
nm_vanco <- read.csv(file = system.file(package = "mipdeval", "data/nm_vanco.csv"))

## Busulfan test data
if(!requireNamespace("pkvbusulfanshukla", quietly = TRUE)) {
  PKPDsim::install_default_literature_model("pk_busulfan_shukla")
  loadNamespace("pkbusulfanshukla")
}
if(regenerate) {
  require(pkbusulfanshukla)
  mod <- pkbusulfanshukla::model()
  par <- pkbusulfanshukla::parameters()
  omega <- pkbusulfanshukla::omega_matrix()
  n_ids <- 10
  set.seed(54321)
  covariates_df <- data.frame(
    ID = 1:n_ids,
    AGE = 5 * exp(rnorm(n_ids, 0, .3)),
    WT = 20 * exp(rnorm(n_ids, 0, .2)),
    HT = 100 * exp(rnorm(n_ids, 0, .3)),
    SEX = runif(n_ids, 0.5),
    REGI = 0
  )
  reg <- new_regimen(amt = 50, interval = 24, t_inf = 3, n = 4, type = 'infusion')
  res <- sim(
    mod,
    parameters = par,
    regimen = reg,
    covariates_table = covariates_df,
    omega = omega,
    res_var = pkbusulfanshukla::ruv(),
    t_obs = c(3.5, 5, 6, 8, 12) + rep(c(0, 24, 48, 72), each = 5),
    iov_bins = c(0, 24, 48, 72, 96, 9999),
    only_obs = TRUE
  )
  tdm <- res |>
    rename(ID = id, TIME = t, DV = y) |>
    mutate(CMT = 1, EVID = 0, MDV = 0, AMT = 0, RATE = 0) |>
    select(-comp, -obs_type)
  doses <- left_join(
    expand.grid(1:n_ids, reg$dose_times) |> setNames(c("ID", "TIME")) |> arrange(ID, TIME),
    PKPDsim::regimen_to_nm(reg) |> select(-ID)
  )
  nm_busulfan <- bind_rows(doses, tdm) |>
    arrange(ID, TIME, EVID)
  write.csv(nm_busulfan, "./inst/data/nm_busulfan.csv", quote=F, row.names=F)
}
nm_busulfan <- read.csv(file = system.file(package = "mipdeval", "data/nm_busulfan.csv"))
