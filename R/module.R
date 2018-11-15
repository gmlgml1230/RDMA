
#' @import shiny

# ==============================================================================
# Module UI
# ------------------------------------------------------------------------------

variablesUI <- function(id) {
  ns <- NS(id)

  tags$div(
    id = paste0("var", id),
    fluidRow(
      column(
        width = 4,
        uiOutput(ns('filterborder'))
      ),
      column(
        width = 4,
        uiOutput(ns('operator'))
      ),
      column(
        width = 4,
        uiOutput(ns('expression'))
      )
    )
  )
}

# ==============================================================================




# ==============================================================================
# Module Server
# ------------------------------------------------------------------------------

variablesServer <- function(input, output, session, select_filter.vec){
  ns = session$ns

  output$filterborder <- renderUI({
    selectInput(
      inputId = ns("filterborder"),
      label = paste0("Select Filters ", strsplit(x = ns(""), split = "-")),
      choices = c("Choose" = "", select_filter.vec())
    )
  })

  output$operator <- renderUI({
    selectInput(inputId = ns("operator"), label = "Operator", choices = c("~~","==","!~","!="))
  })
}

variablesServer_exp <- function(input, output, session, add_filter.func, select_filter.chr){
  ns = session$ns

  output$expression <- renderUI({
    add_filter.func(ns, select_filter.chr)
  })

}

# ==============================================================================




# ==============================================================================
# Module Function
# ------------------------------------------------------------------------------

# Dimension 선택 시 Expression 변경 함수
add_filter.func <- function(ns,select.chr){
  tagList(
    if(select.chr == ""){
      NULL
    } else {
      if(select.chr == "device"){
        selectInput(inputId = ns("expression"),label = "Expression", choices = c("","DESKTOP","MOBILE","TABLET"))
      } else if(select.chr == "searchAppearance"){
        selectInput(inputId = ns("expression"),label = "Expression", choices = c("","AMP_BLUE_LINK","RICHCARD"))
      } else {
        textInput(inputId = ns("expression"),label = "Expression")
      }
    }
  )

}

# ==============================================================================
