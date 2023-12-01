criar_url_imagem <- function(tab) {
  id <- tab |>
    dplyr::pull(id) |>
    stringr::str_pad(
      width = 3,
      side = "left",
      pad = "0"
    )

  url <- glue::glue(
    "https://raw.githubusercontent.com/HybridShivam/Pokemon/master/assets/images/{id}.png"
  )
}


pegar_tipos_pkmn <- function(tab) {
  tab |>
    dplyr::mutate(
      tipo_2 = ifelse(is.na(tipo_2), "", tipo_2),
      tipos = paste(na.omit(c(tipo_1, tipo_2)), collapse = ", ")
    ) |>
    dplyr::pull(tipos) |>
    stringr::str_to_sentence()
}
