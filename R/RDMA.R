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
                       column(10,
                              actionButton(inputId = "scRefresh", label = paste0("Authorization : ", oauth_ck("sc.httr-oauth"))),
                              actionButton(inputId = "scremove", label = "Remove Auth")
                       )
                     ),
                     hr(),
                     wellPanel(
                       fluidRow(
                         column(3,
                                dateRangeInput(inputId = "scstartdate", label = "Date Range", start = Sys.Date() - 7, end = Sys.Date())),
                         column(7, NULL),
                         column(2,
                                shinyWidgets::materialSwitch("daily_export", "Daily Export", status = "info", inline = TRUE))
                       ),
                       fluidRow(
                         column(9,
                                selectizeInput(inputId = "scwebsite", label = "Web Site URL", choices = if(!exists("website_url")){""} else {website_url}, multiple = T)),
                         column(3,
                                selectInput(inputId = "scdimension", label = "Dimension", choices = c("date","country","device","page","query","searchAppearance"), multiple = T))
                       ),
                       # InputID 확인용
                       # verbatimTextOutput('InputID_View'),
                       # actionButton(inputId = "test_button",label = "test"),
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

    # Dimension 이 Date만 있을 땐 NA가 나오기 때문에 해당 부분 NULL로 표현하게끔 수정해야함
    scfilter_name <- reactive({
      select_dimension <- input$scdimension
      select_dimension <- if(any(select_dimension %in% "date")){
        if(length(select_dimension) == 1){
          return(NULL)
        } else {
          return(select_dimension[!select_dimension %in% "date"])
        }
      } else {
        select_dimension
      }
    })

    scfilter.func <- function(btn.num){
      vapply(X = 1:btn.num,
             FUN = function(x){
               dimension.chr <- input[[NS(x, "filterborder")]]
               operator.chr <- input[[NS(x, "operator")]]
               expression.chr <- input[[NS(x, "expression")]]
               if(dimension.chr == "country"){
                 paste("country", operator.chr, expression.chr)
               } else if(dimension.chr == "device"){
                 paste("device", operator.chr, expression.chr)
               } else if(dimension.chr == "page"){
                 paste("page", operator.chr, expression.chr)
               } else if(dimension.chr == "query"){
                 paste("query", operator.chr, expression.chr)
               } else {
                 paste("searchAppearance", operator.chr, expression.chr)
               }
             },
             FUN.VALUE = character(1))
    }

    ##### Search Console TAP -------------------------------------------------------------------------------------------------------------

    sc_data.df <- reactiveValues(sc.df = NULL,
                                 Error = NULL)
    filter_btn <- reactiveValues(sc_btn = 0,
                                 dt_tbn = 0)

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

    # SC 데이터 추출
    my_search_analytics.func <- function(siteURL, startDate, endDate, dimensions, dimensionFilterExp, rowLimit, walk_data){
      sc.df <- tryCatch({
        temp.df <- search_analytics(siteURL = siteURL,
                                    startDate = startDate,
                                    endDate = endDate,
                                    dimensions = dimensions,
                                    dimensionFilterExp = dimensionFilterExp,
                                    rowLimit = rowLimit,
                                    walk_data = walk_data) %>% mutate(url = siteURL)
        if(nrow(temp.df) == 1 && is.na(temp.df$date)){
          sc_data.df$Error <- c(sc_data.df$Error, siteURL)
          NULL
        } else {
          return(temp.df)
        }
      },
      error = function(e){
        sc_data.df$Error <- c(sc_data.df$Error, siteURL)
        NULL
      })
    }

    # SC 25000줄 씩 일일 추출
    daily_search_analytics.loop <- function(siteURL, startDate, endDate, dimensions, dimensionFilterExp, rowLimit, walk_data){
      date_range.vec <- seq(as.Date(startDate), as.Date(endDate), "days")

      lapply(X = date_range.vec,
             FUN = my_search_analytics.func,
             siteURL = siteURL,
             dimensions = dimensions,
             dimensionFilterExp = dimensionFilterExp,
             rowLimit = 25000,
             walk_data = )
    }

    # observeEvent(input$test_button, {
    #   output$InputID_View <- renderText({
    #     filter_btn$sc_btn
    #   })
    # })

    # SC 필터 추가
    observeEvent(input$scfilteradd, {
      if(filter_btn$sc_btn < length(scfilter_name())){
        filter_btn$sc_btn <- filter_btn$sc_btn + 1
        btn <- filter_btn$sc_btn

        callModule(variablesServer, btn, scfilter_name)

        insertUI(
          selector = '#scfilter_place',
          where = "beforeEnd",
          ui = variablesUI(btn)
        )
        # ========================================================================
        # InputID 확인용
        # ------------------------------------------------------------------------
        # observe({
        #   outs <- outputOptions(output)
        #   lapply(names(outs), function(name) {
        #     outputOptions(output, name, suspendWhenHidden = FALSE)
        #   })
        #   output$InputID_View <- renderPrint({outs})
        # })
        # ========================================================================

        # Dimension 수정에 따른 Operator 변경
        observeEvent(input[[NS(btn, "filterborder")]], {
          callModule(variablesServer_exp, btn, add_filter.func, input[[NS(btn, "filterborder")]])
        })
      }
    })

    # SC 필터 제거
    observeEvent(input$scfilterdelete, {
      if(filter_btn$sc_btn > 0){
        removeUI(
          selector = paste0('#var', filter_btn$sc_btn)
        )
        filter_btn$sc_btn <- filter_btn$sc_btn - 1
      }
    })

    # 데이터 추출
    observeEvent(input$scstart, {
      element_null_ck(input$scwebsite, input$scdimension, element_name = c("Web Site URL", "Dimension"), text_page = text_page, exr = {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        gar_auth("sc.httr-oauth")
        sc_data.df$Error <- NULL
        btn.num <- filter_btn$sc_btn
        tryCatch({
          sc_data.df$sc.df <- lapply(X = input$scwebsite,
                                     FUN = my_search_analytics.func,
                                     startDate = input$scstartdate[1],
                                     endDate = input$scstartdate[2],
                                     dimensions = input$scdimension,
                                     dimensionFilterExp = if(input$scfilter && btn.num != 0){scfilter.func(btn.num)} else {NULL},
                                     rowLimit = 25000,
                                     walk_data = "byBatch") %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
          print(temp_err)
          removeModal()
          showModal(text_page("S&C Data 추출 완료"))
          output$scdata <- renderDataTable(sc_data.df$sc.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
        }, error = function(e){
          print(e)
        })
        if(!is.null(sc_data.df$Error)){output$scfail <- renderText({paste0("Fail URL \n",paste(sc_data.df$Error, collapse = "\n"))})} else {output$scfail <- renderText({})}
      })
    })

    # 데이터 추출
    output$`sc_data.xlsx` <- downloadHandler(filename = function(){''},
                                             content = function(file){write.xlsx(sc_data.df$sc.df, file, row.names = FALSE)})

  }

  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
