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
        fluidRow(
          bs4Dash::valueBoxOutput(
            ns("altura1"),
            width = 12
          ),
          bs4Dash::valueBoxOutput(
            ns("peso1"),
            width = 12
          ),
          bs4Dash::valueBoxOutput(
            ns("exp_base1"),
            width = 12
          )
        )
      ),
      column(
        width = 6,
        echarts4r::echarts4rOutput(ns("grafico_radar"))
      ),
      column(
        width = 3,
        fluidRow(
          bs4Dash::valueBoxOutput(
            ns("altura2"),
            width = 12
          ),
          bs4Dash::valueBoxOutput(
            ns("peso2"),
            width = 12
          ),
          bs4Dash::valueBoxOutput(
            ns("exp_base2"),
            width = 12
          )
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
      req(input$pokemon1)
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

      tab_1 <- dados_filtrados1() |>
        dplyr::select(
          hp:velocidade
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "stats",
          values_to = "valor1"
        )

      tab_2 <- dados_filtrados2() |>
        dplyr::select(
          hp:velocidade
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "stats",
          values_to = "valor2"
        )

      tab_1 |>
        dplyr::left_join(tab_2, by = "stats") |>
        echarts4r::e_chart(x = stats) |>
        echarts4r::e_radar(serie = valor1) |>
        echarts4r::e_radar(serie = valor2) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_color(color = c(cor1, cor2))
    })


    output$altura1 <- bs4Dash::renderbs4ValueBox({
      valor <- dados_filtrados1() |>
        dplyr::pull(altura)

      valuebox_altura(valor)
    })

    output$peso1 <- bs4Dash::renderbs4ValueBox({
      valor <- dados_filtrados1() |>
        dplyr::pull(peso)

      valuebox_peso(valor)
    })

    output$exp_base1 <- bs4Dash::renderbs4ValueBox({
      valor <- dados_filtrados1() |>
        dplyr::pull(exp_base)

      valuebox_exp_base(valor)
    })

    output$altura2 <- bs4Dash::renderbs4ValueBox({
      valor <- dados_filtrados2() |>
        dplyr::pull(altura)

      valuebox_altura(valor)
    })

    output$peso2 <- bs4Dash::renderbs4ValueBox({
      valor <- dados_filtrados2() |>
        dplyr::pull(peso)

      valuebox_peso(valor)
    })

    output$exp_base2 <- bs4Dash::renderbs4ValueBox({
      valor <- dados_filtrados2() |>
        dplyr::pull(exp_base)

      valuebox_exp_base(valor)
    })

  })
}

## To be copied in the UI
# mod_pag_pokemon_ui("pag_pokemon_1")

## To be copied in the server
# mod_pag_pokemon_server("pag_pokemon_1")
