#' pag_pokemon UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pag_pokemon_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        class = "coluna_imagem",
        uiOutput(ns("imagem1"))
      ),
      column(
        width = 3,
        selectInput(
          ns("pokemon1"),
          label = "",
          choices = c("Carregando..." = "")
        ),
        textOutput(ns("tipos1"))
      ),
      column(
        width = 3,
        selectInput(
          ns("pokemon2"),
          label = "",
          choices = c("Carregando..." = "")
        ),
        textOutput(ns("tipos2"))
      ),
      column(
        width = 3,
        class = "coluna_imagem",
        uiOutput(ns("imagem2"))
      )
    ),
    fluidRow(
      column(
        width = 3,
        bs4Dash::valueBoxOutput(
          ns("altura1")
        ),
        bs4Dash::valueBoxOutput(
          ns("peso1")
        ),
        bs4Dash::valueBoxOutput(
          ns("exp_base1")
        )
      ),
      column(
        width = 6,
        echarts4r::echarts4rOutput(ns("grafico_radar"))
      ),
      column(
        width = 3,
        bs4Dash::valueBoxOutput(
          ns("altura2")
        ),
        bs4Dash::valueBoxOutput(
          ns("peso2")
        ),
        bs4Dash::valueBoxOutput(
          ns("exp_base2")
        )
      )
    )
  )
}

#' pag_pokemon Server Functions
#'
#' @noRd
mod_pag_pokemon_server <- function(id, dados){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    updateSelectInput(
      inputId = "pokemon1",
      choices = dados$pokemon
    )

    observe({
      opcoes <- dados |>
        dplyr::filter(pokemon != input$pokemon1) |>
        dplyr::pull(pokemon)

      updateSelectInput(
        inputId = "pokemon2",
        choices = opcoes
      )
    })

    dados_filtrados1 <- reactive({
      req(input$pokemon1)
      dados |>
        dplyr::filter(pokemon == input$pokemon1)
    })

    dados_filtrados2 <- reactive({
      req(input$pokemon2)
      dados |>
        dplyr::filter(pokemon == input$pokemon2)
    })

    output$imagem1 <- renderUI({
      url <- criar_url_imagem(dados_filtrados1())
      img(src = url, width = "100%")
    })

    output$imagem2 <- renderUI({
      url <- criar_url_imagem(dados_filtrados2())
      img(src = url, width = "100%")
    })

    output$tipos1 <- renderText({
      pegar_tipos_pkmn(dados_filtrados1())
    })

    output$tipos2 <- renderText({
      pegar_tipos_pkmn(dados_filtrados2())
    })

    output$grafico_radar <- echarts4r::renderEcharts4r({
      cor1 <- dados_filtrados1()$cor_1
      cor2 <- dados_filtrados2()$cor_1

      dados_filtrados1() |>
        dplyr::bind_rows(dados_filtrados2()) |>
        dplyr::select(
          pokemon, hp:velocidade
        ) |>
        tidyr::pivot_longer(
          cols = -pokemon,
          names_to = "stats",
          values_to = "valor"
        ) |>
        dplyr::group_by(pokemon) |>
        echarts4r::e_chart(x = stats) |>
        echarts4r::e_radar(serie = valor) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_color(color = c(cor1, cor2))
    })

  })
}

## To be copied in the UI
# mod_pag_pokemon_ui("pag_pokemon_1")

## To be copied in the server
# mod_pag_pokemon_server("pag_pokemon_1")
