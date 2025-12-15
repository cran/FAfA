#' About UI Module
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
about_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # 1. Uygulama Hakkında (En üstte geniş kart)
    card(
      card_header(
        class = "bg-dark text-white",
        "About FAfA",
        bs_icon("info-circle")
      ),
      card_body(
        htmlOutput(ns("application_description_html"))
      )
    ),

    # 2. Alt Kısım: Geliştirici, Teşekkürler ve Atıf (3 Sütunlu Yapı)
    layout_columns(
      col_widths = c(4, 4, 4),

      # Geliştirici Bilgisi
      card(
        card_header(
          class = "bg-primary text-white",
          "Developer",
          bs_icon("person-workspace")
        ),
        card_body(
          htmlOutput(ns("developer_info_html"))
        )
      ),

      # Teşekkürler & Versiyon (EKSİK OLAN KISIM EKLENDİ)
      card(
        card_header(
          class = "bg-success text-white",
          "Acknowledgements & Version",
          bs_icon("people")
        ),
        card_body(
          htmlOutput(ns("contributors_version_html"))
        )
      ),

      # Atıf Bilgisi
      card(
        card_header(
          class = "bg-info text-white",
          "How to Cite",
          bs_icon("quote")
        ),
        card_body(
          htmlOutput(ns("citation_info_html"))
        )
      )
    )
  )
}
