
.onLoad <- function(libname, pkgname){

  if (!"lightgbm" %in% parsnip::get_model_env()$boost_tree$engine) {
    add_boost_tree_lightgbm()
  }

  if (!"catboost" %in% parsnip::get_model_env()$boost_tree$engine) {
    add_boost_tree_catboost()
  }

  if (!"tree" %in% parsnip::get_model_env()$decision_tree$engine) {
    add_decision_tree_tree()
  }

}




