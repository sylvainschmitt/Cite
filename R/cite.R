#' @importFrom RefManageR ReadBib
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny selectInput checkboxInput observeEvent stopApp paneViewer runGadget
NULL

#' @export
#'
cite <- function(){

  # Bibliography
  c <-  getActiveDocumentContext()
  if(!is.integer(grep('bibliography', c$contents))){
    stop('No bibliography in the document !')
  }
  bib <- c$contents[grep('bibliography', c$contents)]
  bib <- substr(bib, 15, nchar(bib))
  path <- strsplit(c$path, '/')[[1]]
  path <- c(path[-length(path)], bib)
  path <- paste0(path, collapse = '/')
  bib <- ReadBib(path)

  # UI
  ui <- miniPage(
    gadgetTitleBar("Cite"),
    miniContentPanel(
      selectInput("citation", "Citation:", names(bib)[order(names(bib))]),
      checkboxInput("brackets", "Brackets", T)
    )
  )

  # Server
  server <- function(input, output, session) {

    observeEvent(input$done, {
      citation <- paste0('@', input$citation)
      if(input$brackets){
        citation <- paste0('[', citation, ']')
      }
      insertText(text = citation)
      stopApp()
    })

  }

  # Viewer
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
}
