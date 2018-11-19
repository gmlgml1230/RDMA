
#' @import shiny

# ==============================================================================
# Module UI
# ------------------------------------------------------------------------------

variablesUI <- function(id, data_filter.log = TRUE){
  ns <- NS(id)

  if(data_filter.log){
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
  } else {
    tags$div(
      id = paste0("var_data", id),
      fluidRow(
        column(
          width = 4,
          uiOutput(ns('filterborder'))
        ),
        column(
          width = 2,
          uiOutput(ns('operator'))
        ),
        column(
          width = 4,
          uiOutput(ns('expression'))
        ),
        column(
          width = 2,
          uiOutput(ns('and_or'))
        )
      )
    )
  }
}

# ==============================================================================




# ==============================================================================
# Module Server
# ------------------------------------------------------------------------------

variablesServer <- function(input, output, session, filter.vec, data_filter.log = TRUE){
  ns = session$ns

  output$filterborder <- renderUI({
    selectInput(
      inputId = ns("filterborder"),
      label = "Select Filters ",
      choices = c("Choose" = "", filter.vec)
    )
  })

  if(data_filter.log){
    output$operator <- renderUI({
      selectInput(inputId = ns("operator"), label = "Operator", choices = c("~~","==","!~","!="))
    })
  } else {
    output$operator <- renderUI({
      selectInput(inputId = ns("operator"), label = "Operator", choices = c("~~","==","!~","!=",">=","<="))
    })

    output$expression <- renderUI({
      textInput(inputId = ns("expression"),label = "Expression")
    })
  }
}

variablesServer_exp <- function(input, output, session, add_filter.func, select_filter.chr){
  ns = session$ns

  output$expression <- renderUI({
    add_filter.func(ns, select_filter.chr)
  })

}

variablesServer_and_or <- function(input, output, session){
  ns = session$ns

  output$and_or <- renderUI({
    selectInput(inputId = ns("and_or"), label = "", choices = c("AND", "OR"), selected = "AND")
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
