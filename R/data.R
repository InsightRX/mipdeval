#' Vancomycin data
#'
#' Vancomycin data simulated with the `"pkvancothomson"` model.
#'
#' @format
#' A NONMEM-style data frame with `r nrow(nm_vanco)` rows and
#' `r ncol(nm_vanco)` columns:
#'
#' \describe{
#'   \item{`ID`}{Patient ID.}
#'   \item{`TIME`}{The recorded times of dosing events and/or observations.}
#'   \item{`CMT`}{Dose compartment.}
#'   \item{`DV`}{The dependent variable or observed concentration.}
#'   \item{`AMT`}{Dose administered for dosing record.}
#'   \item{`EVID`}{Event ID (0 = observation, 1 = dose, 2 = other, 3 = reset, 4 = reset + dose).}
#'   \item{`MDV`}{Missing DV (0 = not missing, 1 = missing).}
#'   \item{`RATE`}{Rate of drug infusion.}
#'   \item{`WT`}{Patient weight at TIME of measurement.}
#'   \item{`CRCL`}{Patient creatinine clearance at TIME of measurement.}
#'   \item{`CL_HEMO`}{Additional clearance attributable to hemodialysis.}
#' }
#'
#' @references Thomson et al. J Antimicrob Chemotherap. 2009.
#'   <https://doi.org/10.1093/jac/dkp085>
"nm_vanco"

#' Busulfan data
#'
#' Busulfan data simulated with the `"pkbusulfanshukla"` model.
#'
#' @format
#' A NONMEM-style data frame with `r nrow(nm_busulfan)` rows and
#' `r ncol(nm_busulfan)` columns:
#'
#' \describe{
#'   \item{`ID`}{Patient ID.}
#'   \item{`TIME`}{The recorded times of dosing events and/or observations.}
#'   \item{`CMT`}{Dose compartment.}
#'   \item{`DV`}{The dependent variable or observed concentration.}
#'   \item{`AMT`}{Dose administered for dosing record.}
#'   \item{`EVID`}{Event ID (0 = observation, 1 = dose, 2 = other, 3 = reset, 4 = reset + dose).}
#'   \item{`MDV`}{Missing DV (0 = not missing, 1 = missing).}
#'   \item{`RATE`}{Rate of drug infusion.}
#'   \item{`AGE`}{Patient age at TIME of measurement.}
#'   \item{`WT`}{Patient weight at TIME of measurement.}
#'   \item{`HT`}{Patient height at TIME of measurement.}
#'   \item{`SEX`}{Patient sex (0 = female, 1 = male).}
#'   \item{`REGI`}{Regimen type.}
#' }
#'
#' @references Shukla et al. Front Pharmacol. 2020.
#'   <https://doi.org/10.3389/fphar.2020.00888>
"nm_busulfan"
