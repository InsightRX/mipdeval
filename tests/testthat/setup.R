# Models used in tests:
if(!requireNamespace("pkvancothomson", quietly = TRUE)) {
  suppressMessages(
    PKPDsim::install_default_literature_model("pk_vanco_thomson", force = TRUE)
  )
}

if(!requireNamespace("pkvbusulfanshukla", quietly = TRUE)) {
  suppressMessages(
    PKPDsim::install_default_literature_model("pk_busulfan_shukla")
  )
}
