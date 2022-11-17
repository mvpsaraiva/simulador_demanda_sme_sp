## code to prepare `app_states` dataset goes here

app_states <- list(
  # app states
  STATE_NOTHING_SELECTED = 1,
  STATE_MB_SELECTED = 2,

  # initial state values
  INITIAL_DRE = "Todas DRE"
)


usethis::use_data(app_states, overwrite = TRUE)
