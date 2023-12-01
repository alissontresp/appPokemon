#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  dados <- readr::read_rds(
    app_sys("pkmn.rds")
  )

  mod_pag_pokemon_server("pag_pokemon_1", dados)

}
