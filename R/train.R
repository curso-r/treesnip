
.onLoad <- function(libname, pkgname){

  # Registers engines with parsnip
  add_boost_tree_lightgbm()
  add_decision_tree_tree()
  add_decision_tree_partykit()
  add_rand_forest_partykit()

}




