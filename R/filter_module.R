#' get RDMA
#' @export
#' @import shiny

variablesUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    id = paste0("var", id),
    fluidRow(
      column(
        width = 3,
        uiOutput(ns('filterborder'))
      ),
      column(
        width = 9,
        uiOutput(ns('add_filter'))
      )
    )
  )
}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

variablesServer <- function(input, output, session){
  ns = session$ns
  
  output$filterborder <- renderUI({
    selectInput(
      inputId = ns("filterborder"),
      label = paste0("Filter ", strsplit(x = ns(""), split = "-")),
      choices = c("Choose" = "", c('a','b','c'))
    )
  })
  
  output$add_filter <- renderUI({
    tagList(
      column(
        width = 5,
        selectInput(inputId = "opercountry", label = "Operator", choices = c("Choose" = "", c("~~","==","!~","!=")))),
      column(
        width = 5,
        textInput(inputId = "exprecountry", label = "Expression"))
      )
  })
}