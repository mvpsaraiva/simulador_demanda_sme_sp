## code to prepare `theme` dataset goes here

# Colors and themes -------------------------------------------------------

theme <- list(
  ECHARTS_THEME <- "auritus",

  COLOR_BLUE <- "#00a2eb",
  COLOR_GREEN <- "#adb514",
  COLOR_ORANGE <- "#fd9f02",
  COLOR_PINK <- "#ce2c78",
  COLOR_RED <- "#d32d05",
  COLOR_PURPLE <- "#7522b8",
  COLOR_GREY <- "#ffffffaa",
  COLOR_DARK_GREY <- "#888888aa",
  COLOR_WHITE <- "#eee"
)

usethis::use_data(theme, overwrite = TRUE)
