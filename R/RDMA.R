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

  # ==============================================================================================================================
  # UI
  # ------------------------------------------------------------------------------------------------------------------------------

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
                                            tags$div(id = 'scfilter_place')
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


  # ==============================================================================================================================




  # ==============================================================================================================================
  # SERVER
  # ------------------------------------------------------------------------------------------------------------------------------

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
    filter_btn <- reactiveValues(sc_btn = 0,
                                 dt_tbn = 0)

    scfilter_name <- reactive({
      select_dimension <- input$scdimension
      select_dimension <- if(any(select_dimension %in% "date")){return(select_dimension[!select_dimension %in% "date"])} else {select_dimension}
    })

    # 인증
    observeEvent(input$scRefresh, {
      if(!file.exists("sc.httr-oauth")){
        searchConsoleR::scr_auth("sc.httr-oauth")
        website_url <- searchConsoleR::list_websites()$siteUrl
        updateSelectizeInput(session, "scwebsite", choices = website_url, options = list(maxOptions = length(website_url)))
        updateActionButton(session, inputId = "scRefresh", label = "Authorization : OK")
      }
    })

    # 인증서 제거
    observeEvent(input$scremove, {
      file.remove("sc.httr-oauth")
      updateActionButton(session, inputId = "scRefresh", label = "Authorization : NO")
    })

    # 데이터 추출 (일별 x)
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

    # SC 필터 추가
    observeEvent(input$scfilteradd, {
      filter_btn$sc_btn <- filter_btn$sc_btn + 1
      btn <- filter_btn$sc_btn
      callModule(variablesServer, btn, scfilter_list.func, scfilter_name)

      insertUI(
        selector = '#scfilter_place',
        where = "beforeEnd",
        ui = variablesUI(btn)
      )
    })

    # SC 필터 제거
    observeEvent(input$scfilterdelete, {
      removeUI(
        selector = paste0('#var', filter_btn$sc_btn)
      )
      filter_btn$sc_btn <- filter_btn$sc_btn - 1
    })

    # 데이터 추출
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

    # 데이터 추출
    output$`sc_data.xlsx` <- downloadHandler(filename = function(){''},
                                             content = function(file){write.xlsx(sc_data.df, file, row.names = FALSE)})

  }

  # ==============================================================================================================================




  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
