#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # le a base de dados salva na pasta inst do pacote
  dados <- readr::read_rds(
    app_sys("pkmn.rds")
  )

  mod_pag_pokemon_server("pag_pokemon_1", dados)

}
