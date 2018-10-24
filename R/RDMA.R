#' RDMA
#' R Data Manipulation Add in
#' @export
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import RAdwords
#' @import RSiteCatalyst
#' @import shinyWidgets
#' @import shinyAce
#' @import searchConsoleR
#' @import googleAuthR
#' @import googleAnalyticsR
#' @import doParallel
#' @import foreach
#' @import openxlsx
#' @importFrom rstudioapi insertText
#' @importFrom readxl excel_sheets




RDMA <- function(){

  options(httr_oauth_cache=T)
  # Shiny에서 5MB의 제한을 잡아놓은걸 30MB로 늘린 것
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  oauth_ck <- function(auth_file){
    if(file.exists(auth_file)){
      return("OK")
    } else {
      return("NO")
    }
  }

  data_ck <- function(df_name, text_page, exr){
    if(!is.reactivevalues(df_name)){
      exr
    } else {
      showModal(text_page("데이터 추출 먼저 해야합니다."))
    }
  }

  oauth_trycatch <- function(auth_file, exr){
    if(file.exists(auth_file)){
      tryCatch({
        exr
      },
      error = function(e){
        file.remove(auth_file)
      })
    }
  }

  element_null_ck <- function(..., element_name, text_page, exr){
    null_ck <- vapply(list(...), is.null, TRUE)
    if(any(null_ck)){
      showModal(text_page(paste0(paste0(element_name[null_ck], collapse = " , "), " 을(를) 선택해야 합니다.")))
    } else {
      exr
    }
  }

  oauth_trycatch("sc.httr-oauth", {
    gar_auth("sc.httr-oauth")
    website_url <- searchConsoleR::list_websites()$siteUrl
  })

  oauth_trycatch("ga.httr-oauth", {
    googleAnalyticsR::ga_auth("ga.httr-oauth")
    ga_id <- googleAnalyticsR::ga_account_list()
    ga_metric <- googleAnalyticsR::allowed_metric_dim(type = "METRIC")
    ga_dimension <- googleAnalyticsR::allowed_metric_dim(type = "DIMENSION")
    ga_segment <- googleAnalyticsR::ga_segment_list()$items
  })


  ##### UI -----------------------------------------------------------------------------------------------------------------------------

  ui <- miniPage(
    tags$script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-117525726-1', 'auto');ga('send', 'pageview')"),
    tags$style("
               @import url(//fonts.googleapis.com/css?family=Nanum+Gothic);
               * { font-family: 'Nanum Gothic', sans-serif;}
               "
    ),
    gadgetTitleBar("Data Preparation Tool"),

    miniTabstripPanel(

      ##### Search Console -----------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Search Console",
                   miniContentPanel(
                     fluidRow(
                       column(6,
                              actionButton(inputId = "scRefresh", label = paste0("Authorization : ", oauth_ck("sc.httr-oauth"))),
                              actionButton(inputId = "scremove", label = "Remove Auth")
                       )
                     ),
                     hr(),
                     wellPanel(
                       fluidRow(
                         column(3,
                                dateRangeInput(inputId = "scstartdate", label = "Date Range", start = Sys.Date() - 7, end = Sys.Date()))
                       ),
                       fluidRow(
                         column(9,
                                selectizeInput(inputId = "scwebsite", label = "Web Site URL", choices = if(!exists("website_url")){""} else {website_url}, multiple = T)),
                         column(3,
                                selectInput(inputId = "scdimension", label = "Dimension", choices = c("date","country","device","page","query","searchAppearance"), multiple = T))
                       ),
                       shinyWidgets::materialSwitch("scfilter", "SC Filter", status = "info"),
                       conditionalPanel(condition = "input.scfilter == true",
                                        wellPanel(
                                          fluidRow(
                                            actionButton(inputId = "scfilteradd",label = "ADD"),
                                            actionButton(inputId = "scfilterdelete",label = "Delete")
                                          ),
                                          fluidRow(
                                            column(3,
                                                   uiOutput("scfilterborder")),
                                            column(9,
                                                   uiOutput("add_scfilter"))
                                          )
                                        )
                       ),
                       shinyWidgets::materialSwitch("dtfilter", "Data Filter", status = "info"),
                       conditionalPanel(condition = "input.dtfilter == true",
                                        wellPanel(
                                          fluidRow(
                                            actionButton(inputId = "dtfilteradd",label = "ADD"),
                                            actionButton(inputId = "dtfilterdelete",label = "Delete")
                                          ),
                                          fluidRow(
                                            column(3,
                                                   uiOutput("dtfilterborder")),
                                            column(9,
                                                   uiOutput("add_dtfilter"))
                                          ),
                                          actionButton(inputId = "dfstart", label = "OK")
                                        )
                       ),
                       actionButton(inputId = "scstart", label = "S&C Start"),
                       downloadButton("sc_data.xlsx", "Download")
                     ),
                     verbatimTextOutput("scfail"),
                     dataTableOutput("scdata")
                   )
      )
    )
  )

  ##### SERVER -------------------------------------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    temp_err <- reactiveValues()

    text_page <- function(text, buffer = FALSE, button = "OK"){
      if(buffer == FALSE){
        modalDialog(
          text,
          footer = tagList(
            modalButton(button)
          )
        )
      } else {
        modalDialog(text, easyClose = TRUE, footer = NULL)
      }

    }

    ##### Search Console TAP -------------------------------------------------------------------------------------------------------------

    sc_data.df <- reactiveValues()
    sc_filter_add <- reactiveValues(filter = 0)
    dt_filter_add <- reactiveValues(filter = 0)

    my_search_analytics <- function(siteURL, startDate, endDate, dimensions, dimensionFilterExp, rowLimit, walk_data){
      temp_df <- tryCatch({
        search_analytics(siteURL = siteURL,
                         startDate = startDate,
                         endDate = endDate,
                         dimensions = dimensions,
                         dimensionFilterExp = dimensionFilterExp,
                         rowLimit = rowLimit,
                         walk_data = walk_data) %>% mutate(url = siteURL)
      },
      error = function(e){
        temp_err <<- c(temp_err, siteURL)
        NULL
      })
    }

    observeEvent(input$scRefresh, {
      if(!file.exists("sc.httr-oauth")){
        searchConsoleR::scr_auth("sc.httr-oauth")
        website_url <- searchConsoleR::list_websites()$siteUrl
        updateSelectizeInput(session, "scwebsite", choices = website_url, options = list(maxOptions = length(website_url)))
        updateActionButton(session, inputId = "scRefresh", label = "Authorization : OK")
      }
    })

    observeEvent(input$scremove, {
      file.remove("sc.httr-oauth")
      updateActionButton(session, inputId = "scRefresh", label = "Authorization : NO")
    })

    # ----------------------------------------------------------------
    # SC Filter

    scfilter_name <- reactive({
      select_dimension <- input$scdimension
      select_dimension <- if(any(select_dimension %in% "date")){return(select_dimension[!select_dimension %in% "date"])} else {select_dimension}
    })

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

    observeEvent(input$scfilteradd, {
      if(sc_filter_add$filter < length(scfilter_name())){
        sc_filter_add$filter <- sc_filter_add$filter + 1
        if(sc_filter_add$filter >= 1){
          output$scfilterborder <- renderUI({
            lapply(1:sc_filter_add$filter, function(i){
              selectInput(inputId = paste0("sclist",i), label = "Select Filters", choices = scfilter_name())})
          })
          output$add_scfilter <- renderUI({
            lapply(1:sc_filter_add$filter, function(i){
              scfilter_list.func(input[[paste0("sclist",i)]])
            })
          })
        }
      }
    })

    observeEvent(input$scfilterdelete, {
      if(sc_filter_add$filter > 0){
        sc_filter_add$filter <- sc_filter_add$filter - 1
        if(sc_filter_add$filter == 0){
          output$scfilterborder <- renderUI({})
          output$add_scfilter <- renderUI({})
        } else {
          output$scfilterborder <- renderUI({
            lapply(1:sc_filter_add$filter, function(i){
              selectInput(inputId = paste0("sclist",i), label = "Select Filters", choices = scfilter_name())})
          })
          output$add_scfilter <- renderUI({
            lapply(1:sc_filter_add$filter, function(i){
              scfilter_list.func(input[[paste0("sclist",i)]])
            })
          })
        }
      }
    })

    scfilter_list.func <- function(select){
      if(any(select %in% "country")){
        tagList(
          column(5, selectInput(inputId = "opercountry", label = "Operator", choices = c("~~","==","!~","!="))),
          column(5, textInput(inputId = "exprecountry",label = "Expression"))
        )
      } else if(any(select %in% "device")) {
        tagList(
          column(5, selectInput(inputId = "operdevice", label = "Operator", choices = c("~~","==","!~","!="))),
          column(5, selectInput(inputId = "expredevice",label = "Expression", choices = c("","DESKTOP","MOBILE","TABLET")))
        )
      } else if(any(select %in% "page")) {
        tagList(
          column(5, selectInput(inputId = "operpage", label = "Operator", choices = c("~~","==","!~","!="))),
          column(5, textInput(inputId = "exprepage",label = "Expression"))
        )
      } else if(any(select %in% "query")) {
        tagList(
          column(5, selectInput(inputId = "operquery", label = "Operator", choices = c("~~","==","!~","!="))),
          column(5, textInput(inputId = "exprequery",label = "Expression"))
        )
      } else if(any(select %in% "searchAppearance")) {
        tagList(
          column(5, selectInput(inputId = "opersearch", label = "Operator", choices = c("~~","==","!~","!="))),
          column(5, selectInput(inputId = "expresearch",label = "Expression", choices = c("","AMP_BLUE_LINK","RICHCARD")))
        )
      }
    }

    # ----------------------------------------------------------------
    # Data Filter

    observeEvent(input$dtfilteradd, {
      if(dt_filter_add$filter < length(names(sc_data.df))){
        dt_filter_add$filter <- dt_filter_add$filter + 1
        output$dtfilterborder <- renderUI({
          lapply(1:dt_filter_add$filter, function(i){
            selectInput(inputId = paste0("dtlist",i), label = "Select Filters", choices = names(sc_data.df))})
        })
        output$add_dtfilter <- renderUI({
          lapply(1:dt_filter_add$filter, function(i){
            if(i != 1){
              tagList(
                column(5, selectInput(inputId = paste0("oper", i), label = "Operator", choices = c("~~","==","!~","!=",">=","<="))),
                column(5, textInput(inputId = paste0("expre", i), label = "Expression")),
                column(2, selectInput(inputId = paste0("and_or", i), label = "", choices = c("AND", "OR"), selected = "AND"))
              )
            } else {
              tagList(
                column(5, selectInput(inputId = paste0("oper", i), label = "Operator", choices = c("~~","==","!~","!=",">=","<="))),
                column(5, textInput(inputId = paste0("expre", i), label = "Expression"))
              )
            }
          })
        })
      }
    })

    observeEvent(input$dtfilterdelete, {
      if(dt_filter_add$filter > 0){
        dt_filter_add$filter <- dt_filter_add$filter - 1
        if(dt_filter_add$filter == 0){
          output$dtfilterborder <- renderUI({})
          output$add_dtfilter <- renderUI({})
        } else {
          output$dtfilterborder <- renderUI({
            lapply(1:dt_filter_add$filter, function(i){
              selectInput(inputId = paste0("dtlist",i), label = "Select Filters", choices = names(sc_data.df))})
          })
          output$add_dtfilter <- renderUI({
            lapply(1:dt_filter_add$filter, function(i){
              if(i != 1){
                tagList(
                  column(5, selectInput(inputId = paste0("oper", i), label = "Operator", choices = c("~~","==","!~","!=",">=","<="))),
                  column(5, textInput(inputId = paste0("expre", i), label = "Expression")),
                  column(2, selectInput(inputId = paste0("and_or", i), label = "", choices = c("AND", "OR"), selected = "AND"))
                )
              } else {
                tagList(
                  column(5, selectInput(inputId = paste0("oper", i), label = "Operator", choices = c("~~","==","!~","!=",">=","<="))),
                  column(5, textInput(inputId = paste0("expre", i), label = "Expression"))
                )
              }
            })
          })
        }
      }
    })

    dt_filter.func <- function(len){
      eval.func <- function(...){
        eval(parse(text = paste0("input$", ...)))
      }

      if(len == 1){
        if(eval.func("oper",len) == "~~"){
          paste0("grepl('", eval.func("expre",len), "' ,", eval.func("dtlist",len), ")")
        } else if (eval.func("oper",len) == "==") {
          paste0(eval.func("dtlist",len), " == '", eval.func("expre",len), "'")
        } else if (eval.func("oper",len) == "!~") {
          paste0("grepl('", eval.func("expre",len), "' ,", eval.func("dtlist",len), ") = F")
        } else if (eval.func("oper",len) == "!=") {
          paste0(eval.func("dtlist",len), " != '", eval.func("expre",len), "'")
        } else if (eval.func("oper",len) == ">=") {
          paste0(eval.func("dtlist",len), " >= ", eval.func("expre",len))
        } else if (eval.func("oper",len) == "<=") {
          paste0(eval.func("dtlist",len), " <= ", eval.func("expre",len))
        }
      } else {
        if(eval.func("and_or",len) == "AND"){and_or <- "&"} else {and_or <- "|"}
        if(eval.func("oper",len) == "~~"){
          paste0(" ", and_or, " grepl('", eval.func("expre",len), "' ,", eval.func("dtlist",len), ")")
        } else if (eval.func("oper",len) == "==") {
          paste0(" ", and_or, " ", eval.func("dtlist",len), " == '", eval.func("expre",len), "'")
        } else if (eval.func("oper",len) == "!~") {
          paste0(" ", and_or, " grepl('", eval.func("expre",len), "' ,", eval.func("dtlist",len), ") = F")
        } else if (eval.func("oper",len) == "!=") {
          paste0(" ", and_or, " ", eval.func("dtlist",len), " != '", eval.func("expre",len), "'")
        } else if (eval.func("oper",len) == ">=") {
          paste0(" ", and_or, " ", eval.func("dtlist",len), " >= ", eval.func("expre",len))
        } else if (eval.func("oper",len) == "<=") {
          paste0(" ", and_or, " ", eval.func("dtlist",len), " <= ", eval.func("expre",len))
        }
      }
    }

    observeEvent(input$dfstart, {
      filter.list <- paste0(lapply(1:dt_filter_add$filter, FUN = dt_filter.func), collapse = "")
      sc_data.df <<- eval(parse(text =
                                  paste0("subset(sc_data.df, ",filter.list,")")
                                  ))
      output$scdata <- renderDataTable(sc_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
    })

    observeEvent(input$scstart, {
      element_null_ck(input$scwebsite, input$scdimension, element_name = c("Web Site URL", "Dimension"), text_page = text_page, exr = {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        gar_auth("sc.httr-oauth")
        temp_err <- NULL
        sc_data.df <<- isolate({
          lapply(X = input$scwebsite,
                 FUN = my_search_analytics,
                 startDate = input$scstartdate[1],
                 endDate = input$scstartdate[2],
                 dimensions = input$scdimension,
                 dimensionFilterExp = if(input$scfilter && !is.null(scfilter_func())){scfilter_func()} else {NULL},
                 rowLimit = 5000,
                 walk_data = "byBatch") %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
        })
        removeModal()
        showModal(text_page("S&C Data 추출 완료"))
        output$scdata <- renderDataTable(sc_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
        if(!is.null(temp_err)){output$scfail <- renderText({paste0("Fail URL \n",paste(temp_err, collapse = "\n"))})} else {output$scfail <- renderText({})}
      })
    })

    output$`sc_data.xlsx` <- downloadHandler(filename = function(){''},
                                             content = function(file){write.xlsx(sc_data.df, file, row.names = FALSE)})

  }

  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
