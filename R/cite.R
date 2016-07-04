#' @importFrom RefManageR ReadBib TextCite
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny selectizeInput checkboxInput textOutput renderText observeEvent stopApp paneViewer runGadget reactiveValues
NULL


#' Cite
#'
#' Addin to insert BibTex citation in Rmd documents
#'
#' @return citaion entry
#' @export
#'
#' @examples
#' # To run the addin place your cursor
#' # in a Rmd document with a defined
#' # bibliography file
#'
cite <- function(){

  # Bibliography
  c <-  getActiveDocumentContext()
  if(length(grep('bibliography', c$contents)) < 1){
    stop('No bibliography in the document !')
  }
  bib <- c$contents[grep('bibliography', c$contents)]
  bib <- substr(bib, 15, nchar(bib))
  path <- strsplit(c$path, '/')[[1]]
  path <- c(path[-length(path)], bib)
  path <- paste0(path, collapse = '/')
  bib <- ReadBib(path)
  bib.l <- names(bib)
  names(bib.l) <- unlist(lapply(bib, function(x){TextCite(x, .opts = list(cite.style = 'authortitle', max.names = 2))}))
  bib.l <- bib.l[order(bib.l)]

  # UI
  ui <- miniPage(
    gadgetTitleBar("Cite"),
    miniContentPanel(
      selectizeInput("citation", "Citation:", bib.l),
      checkboxInput("brackets", "Brackets", T),
      textOutput("preview")
    )
  )

  # Server
  server <- function(input, output, session) {

    citation <- reactiveValues(ref = NULL,
                               code = NULL,
                               entry = NULL,
                               title = NULL,
                               Journal = NULL)

    observeEvent(input$citation,{
      citation$code <- input$citation
      citation$entry <- bib[which(names(bib) == citation$code)]
      citation$ref <- TextCite(citation$entry)
      citation$title <- citation$entry$title
      citation$journal <- citation$entry$journal
      output$preview <- renderText(paste(citation$ref,
                                         citation$title,
                                         citation$journal,
                                         sep = ' ; '))
    })

    observeEvent(input$done, {
      text <- paste0('@', citation$code)
      if(input$brackets){
        text <- paste0('[', text, ']')
      }
      insertText(text = text)
      stopApp()
    })

  }

  # Viewer
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
}
