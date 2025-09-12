# Models used in tests:
if(!requireNamespace("pkvancothomson", quietly = TRUE)) {
  PKPDsim::install_default_literature_model("pk_vanco_thomson", force = TRUE)
}

if(!requireNamespace("pkvbusulfanshukla", quietly = TRUE)) {
  PKPDsim::install_default_literature_model("pk_busulfan_shukla")
  loadNamespace("pkbusulfanshukla")
}
