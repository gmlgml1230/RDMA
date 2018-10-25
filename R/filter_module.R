#' get RDMA
#' @export
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

# ==============================================================================




# ==============================================================================
# Module Server
# ------------------------------------------------------------------------------

# variablesServer <- function(input, output, session){
#   ns = session$ns
#
#   output$filterborder <- renderUI({
#     selectInput(
#       inputId = ns("filterborder"),
#       label = paste0("Select Filters ", strsplit(x = ns(""), split = "-")),
#       choices = c("Choose" = "", c('a','b','c'))
#     )
#   })
#
#   output$add_filter <- renderUI({
#     tagList(
#       column(
#         width = 5,
#         selectInput(inputId = ns("opercountry"),
#                     label = "Operator",
#                     choices = c("Choose" = "", c("~~","==","!~","!=")))),
#       column(
#         width = 5,
#         textInput(inputId = ns("exprecountry"),
#                   label = "Expression"))
#       )
#   })
# }

variablesServer <- function(input, output, session, scfilter_list.func, select_filter.vec){
  ns = session$ns

  output$filterborder <- renderUI({
    selectInput(
      inputId = ns("filterborder"),
      label = paste0("Select Filters ", strsplit(x = ns(""), split = "-")),
      choices = c("Choose" = "", select_filter.vec)
    )
  })

  output$add_filter <- renderUI({
    scfilter_list.func(select_filter.vec[1])
  })
}

# ==============================================================================




# ==============================================================================
# Module Function
# ------------------------------------------------------------------------------


# scfilter_func <- function(filter_count.num, ){
#   select_name <- sapply(X = 1:filter_count.num,
#                         FUN = function(x){
#
#                         }
#                         )
# }



scfilter_func <- reactive({
  select_name <- sapply(1:sc_filter_add$filter, function(i){eval(parse(text=paste0("input$sclist",i)))})
  if(!is.null(select_name)){
    scfiltercode <- c(
      if(any(select_name %in% "country") && input$exprecountry != ""){paste0("country",input$opercountry,input$exprecountry)} else {NULL},
      if(any(select_name %in% "device") && input$expredevice != ""){paste0("device",input$operdevice,input$expredevice)} else {NULL},
      if(any(select_name %in% "page") && input$exprepage != ""){paste0("page",input$operpage,input$exprepage)} else {NULL},
      if(any(select_name %in% "query") && input$exprequery != ""){paste0("query",input$operquery,input$exprequery)} else {NULL},
      if(any(select_name %in% "searchAppearance") && input$expresearch != ""){paste0("searchAppearance",input$opersearch,input$expresearch)} else {NULL}
    )
    return(scfiltercode)
  } else {NULL}

})


scfilter_list.func <- function(select.chr){
  tagList(
    column(5,selectInput(inputId = ns("opercountry"), label = "Operator", choices = c("~~","==","!~","!="))),
    column(5,
           if(select.chr == "device"){
             selectInput(inputId = ns("exprecountry"),label = "Expression", choices = c("","DESKTOP","MOBILE","TABLET"))
           } else if(select.chr == "searchAppearance"){
             selectInput(inputId = ns("exprecountry"),label = "Expression", choices = c("","AMP_BLUE_LINK","RICHCARD"))
           } else {
             textInput(inputId = ns("exprecountry"),label = "Expression")
           })
  )

}

# ==============================================================================
