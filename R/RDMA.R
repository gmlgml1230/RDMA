#' RDMA
#' R Data Manipulation Add in
#' @export
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import shinyWidgets
#' @import searchConsoleR
#' @import googleAuthR
#' @import doParallel
#' @import foreach
#' @import openxlsx

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
                                dateRangeInput(inputId = "scstartdate", label = "Date Range", start = Sys.Date() - 10, end = Sys.Date() - 4)),
                         column(7, NULL),
                         column(2,
                                fluidRow(
                                  shinyWidgets::materialSwitch("extract_method1", "Daily Extract", status = "info", inline = TRUE),
                                  shinyWidgets::materialSwitch("extract_method2", "Maximum Row", status = "info", inline = TRUE)
                                )
                         )
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
                                            tags$div(id = 'datafilter_place')
                                          ),
                                          actionButton(inputId = "dfstart", label = "OK"),
                                          actionButton(inputId = "dfunique", label = "Unique")
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

    dtfilter.func <- function(btn.num){
      vapply(X = 11:btn.num,
             FUN = function(x){
               dimension.chr <- input[[NS(x, "filterborder")]]
               operator.chr <- input[[NS(x, "operator")]]
               expression.chr <- input[[NS(x, "expression")]]
               and_or.chr <- input[[NS(x, "and_or")]]

               if(is.null(and_or.chr)){
                 and_or.chr <- " "
               } else {
                 if(and_or.chr == "AND"){and_or.chr <- "&"} else {and_or.chr <- "|"}
               }

               if(operator.chr == "~~"){
                 paste0(and_or.chr, " grepl('", expression.chr, "', ", dimension.chr, ") ")
               } else if(operator.chr == "=="){
                 paste0(and_or.chr, " ", dimension.chr, " == '", expression.chr, "' ")
               } else if(operator.chr == "!~"){
                 paste0(and_or.chr, " grepl('", expression.chr, "', ", dimension.chr, ") == 0 ")
               } else if(operator.chr == "!="){
                 paste0(and_or.chr, " ", dimension.chr, " != '", expression.chr, "' ")
               } else if(operator.chr == ">="){
                 paste0(and_or.chr, " ", dimension.chr, " >= ", expression.chr, " ")
               } else if(operator.chr == "<="){
                 paste0(and_or.chr, " ", dimension.chr, " <= ", expression.chr, " ")
               }
             },
             FUN.VALUE = character(1)) %>% paste(., collapse = " ")
    }

    ##### Search Console TAP -------------------------------------------------------------------------------------------------------------

    sc_data.df <- reactiveValues(sc.df = NULL,
                                 colname = NULL,
                                 Error = NULL)
    filter_btn <- reactiveValues(sc_btn = 0,
                                 dt_btn = 10)

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

    # observeEvent(input$test_button, {
    #   output$InputID_View <- renderText({
    #     c(dtfilter.func())
    #   })
    # })





    # RDMA GSC 데이터 추출 함수

    # SC 데이터 추출
    gsc_analytics.func <- function(siteURL, startDate, endDate, dimensions, dimensionFilterExp, rowLimit, walk_data){
      gar_auth("sc.httr-oauth")
      tryCatch({
        search_analytics(siteURL = siteURL,
                         startDate = startDate,
                         endDate = endDate,
                         dimensions = dimensions,
                         dimensionFilterExp = dimensionFilterExp,
                         rowLimit = rowLimit,
                         walk_data = walk_data) %>% mutate(url = siteURL)
      },
      error = function(e){
        print(e)
      })
    }

    gsc_analytics_error.func <- function(siteURL, startDate, endDate, dimensions, dimensionFilterExp, rowLimit, walk_data){
      temp <- gsc_analytics.func(siteURL, startDate, endDate, dimensions, dimensionFilterExp, rowLimit, walk_data)
      if(!is.null(nrow(temp))){
        if(nrow(temp) != 1){
          return(temp)
        } else {
          if(is.na(temp$clicks)){
            sc_data.df$Error <- c(sc_data.df$Error, siteURL)
            temp <- NULL
            return(temp)
          }
        }
      } else {
        sc_data.df$Error <- c(sc_data.df$Error, siteURL)
        temp <- NULL
        return(temp)
      }

    }

    # 데이터 추출 시 RowLimit 측정 방법
    gsc_limit_analytics.func <- function(gsc_analytics.func, siteURL, startDate, endDate, dimensions, dimensionFilterExp, walk_data){
      row_limit.num <- 5000

      # Google SearchConsole API에서 추출하고자 하는 데이터의 양이 얼마인지 이전에 제공해주질 않아 RowLimit을 정확히 판단 할 수 없다.
      # 해당 기간 및 설정에 따른 데이터양에 따라 RowLimit을 변경시켜주어야하는데 추출을해야 확인 가능하기 때문에 이와 같이 루프를 실행한다.
      repeat{
        temp <- gsc_analytics.func(siteURL, startDate, endDate, dimensions, dimensionFilterExp, row_limit.num, walk_data)
        cat(row_limit.num, " : ",nrow(temp), "\n")

        # GSC 데이터 추출 시 에러발생 확인 : Error 발생 시 nrow 사용하면 Null값 출력
        if(is.null(nrow(temp))){
          # 해당 문구가 출력되면 Rowlimit이 틀리다는 것이다. 해당 문구 이외엔 Rowlimit과 관계없는 에러
          if(grepl('numbers of columns of arguments do not match', temp)){
            row_limit.num <- row_limit.num - 5000
            temp <- gsc_analytics.func(siteURL, startDate, endDate, dimensions, dimensionFilterExp, row_limit.num, walk_data)
            return(temp)
          } else {
            if(grepl('Invalid Credentials', temp)){
              temp <- gsc_analytics.func(siteURL, startDate, endDate, dimensions, dimensionFilterExp, row_limit.num, walk_data)
              return(temp)
            } else {
              sc_data.df$Error <- c(sc_data.df$Error, siteURL)
              temp <- NULL
              return(temp)
            }
          }
        } else {
          if(nrow(temp) != 1){
            if(nrow(temp) >= row_limit.num & row_limit.num < 95000){
              row_limit.num <- row_limit.num + 5000
            } else {
              return(temp)
            }
          } else {
            # url은 해당 url이나 그 외의 값이 NA인 경우로 그것 또한 에러에 포함 시키도록한다.
            if(is.na(temp$clicks)){
              sc_data.df$Error <- c(sc_data.df$Error, siteURL)
              temp <- NULL
              return(temp)
            }
          }
        }

      }
    }

    # GSC 일별 추출이며 byBatch 기준으로 최대한 많은 데이터 추출
    daily_analytics.loop <- function(gsc_analytics.func, siteURL, startDate, endDate, dimensions, dimensionFilterExp, walk_data, min_row.log){
      date_range.vec <- seq(as.Date(startDate), as.Date(endDate), "days")

      if(min_row.log){
        temp.func <- function(siteURL, daily.date, dimensions, dimensionFilterExp, rowLimit, walk_data){
          gsc_analytics_error.func(siteURL, daily.date, daily.date, dimensions, dimensionFilterExp, rowLimit, walk_data)
        }

        lapply(X = date_range.vec,
               FUN = temp.func,
               siteURL = siteURL,
               dimensions = dimensions,
               dimensionFilterExp = dimensionFilterExp,
               rowLimit = 5000,
               walk_data = 'byBatch') %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
      } else {
        temp.func <- function(gsc_analytics.func, siteURL, daily.date, dimensions, dimensionFilterExp, walk_data){
          gsc_limit_analytics.func(gsc_analytics.func, siteURL, daily.date, daily.date, dimensions, dimensionFilterExp, walk_data)
        }

        lapply(X = date_range.vec,
               FUN = temp.func,
               gsc_analytics.func = gsc_analytics.func,
               siteURL = siteURL,
               dimensions = dimensions,
               dimensionFilterExp = dimensionFilterExp,
               walk_data = 'byBatch') %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
      }

    }






    # SC 필터 추가
    observeEvent(input$scfilteradd, {
      if(filter_btn$sc_btn < 5){
        filter_btn$sc_btn <- filter_btn$sc_btn + 1
        btn <- filter_btn$sc_btn

        callModule(variablesServer, btn, c("country","device","page","query","searchAppearance"))

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

    # Data 필터 추가
    observeEvent(input$dtfilteradd, {
      filter_btn$dt_btn <- filter_btn$dt_btn + 1
      data_btn <- filter_btn$dt_btn

      callModule(variablesServer, data_btn, sc_data.df$colname, FALSE)

      insertUI(
        selector = '#datafilter_place',
        where = "beforeEnd",
        ui = variablesUI(data_btn, FALSE)
      )

      if(data_btn >= 12){
        callModule(variablesServer_and_or, data_btn)
      }
    })

    # Data 필터 제거
    observeEvent(input$dtfilterdelete, {
      if(filter_btn$dt_btn > 10){
        removeUI(
          selector = paste0('#var_data', filter_btn$dt_btn)
        )
        filter_btn$dt_btn <- filter_btn$dt_btn - 1
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
          if(!input$extract_method1){
            if(!input$extract_method2){
              sc_data.df$sc.df <- lapply(X = input$scwebsite,
                                         FUN = gsc_analytics_error.func,
                                         startDate = input$scstartdate[1],
                                         endDate = input$scstartdate[2],
                                         dimensions = input$scdimension,
                                         dimensionFilterExp = if(input$scfilter && btn.num != 0){scfilter.func(btn.num)} else {NULL},
                                         rowLimit = 5000,
                                         walk_data = "byBatch") %>% do.call(., what = rbind) %>% replace(is.na(.), 0) %>% data.frame(.,check.names = FALSE)
            } else {
              sc_data.df$sc.df <- lapply(X = input$scwebsite,
                                         FUN = gsc_limit_analytics.func,
                                         gsc_analytics.func = gsc_analytics.func,
                                         startDate = input$scstartdate[1],
                                         endDate = input$scstartdate[2],
                                         dimensions = input$scdimension,
                                         dimensionFilterExp = if(input$scfilter && btn.num != 0){scfilter.func(btn.num)} else {NULL},
                                         walk_data = "byBatch") %>% do.call(., what = rbind) %>% replace(is.na(.), 0) %>% data.frame(.,check.names = FALSE)
            }
          } else {
            if(!input$extract_method2){
              sc_data.df$sc.df <- lapply(X = input$scwebsite,
                                         FUN = daily_analytics.loop,
                                         gsc_analytics.func = gsc_analytics.func,
                                         startDate = input$scstartdate[1],
                                         endDate = input$scstartdate[2],
                                         dimensions = input$scdimension,
                                         dimensionFilterExp = if(input$scfilter && btn.num != 0){scfilter.func(btn.num)} else {NULL},
                                         walk_data = "byBatch",
                                         min_row.log = TRUE) %>% do.call(., what = rbind) %>% replace(is.na(.), 0) %>% data.frame(.,check.names = FALSE)
            } else {
              sc_data.df$sc.df <- lapply(X = input$scwebsite,
                                         FUN = daily_analytics.loop,
                                         gsc_analytics.func = gsc_analytics.func,
                                         startDate = input$scstartdate[1],
                                         endDate = input$scstartdate[2],
                                         dimensions = input$scdimension,
                                         dimensionFilterExp = if(input$scfilter && btn.num != 0){scfilter.func(btn.num)} else {NULL},
                                         walk_data = "byBatch",
                                         min_row.log = FALSE) %>% do.call(., what = rbind) %>% replace(is.na(.), 0) %>% data.frame(.,check.names = FALSE)
            }
          }
          removeModal()
          showModal(text_page("S&C Data 추출 완료"))
          output$scdata <- renderDataTable(sc_data.df$sc.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
          sc_data.df$colname <- names(sc_data.df$sc.df)
        }, error = function(e){
          print(e)
        })
        if(!is.null(sc_data.df$Error)){output$scfail <- renderText({paste0("Fail URL \n",paste(unique(sc_data.df$Error), collapse = "\n"))})} else {output$scfail <- renderText({})}
      })
    })

    # 데이터 전처리
    observeEvent(input$dfstart, {
      tryCatch({
        sc_data.df$sc.df <- eval(parse(text = paste0("subset(sc_data.df$sc.df, ",dtfilter.func(filter_btn$dt_btn), ")")))
      }, error = function(e){
        NULL
      })
    })

    # 데이터 중복 처리
    observeEvent(input$dfunique, {
      groub.vec <- sc_data.df$colname[sc_data.df$colname %in% c("date", "country", "device", "page", "query", "countryName", "url")]
      sc_data.df$sc.df <- eval(parse(text = paste0("sc_data.df$sc.df %>% mutate(totalposition = clicks * impressions) %>% group_by(",
                                                   paste0(groub.vec, collapse = ", "), ") %>% summarise(clicks = sum(clicks), impressions = sum(impressions), ctr = clicks/impressions, position = sum(totalposition)/impressions)")))
    })

    # 데이터 추출
    output$`sc_data.xlsx` <- downloadHandler(filename = function(){''},
                                             content = function(file){write.xlsx(sc_data.df$sc.df, file, row.names = FALSE)})

  }

  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
