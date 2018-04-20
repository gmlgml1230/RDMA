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
    gadgetTitleBar("Data Preparation Tool"),

    miniTabstripPanel(

      ##### Data Preparation TAP -----------------------------------------------------------------------------------------------------------

      # miniTabPanel(title = "Data Preparation",
      #              miniContentPanel(
      #                selectInput(inputId = "sheetcolname", label = "Column Name", choices = "", multiple = T),
      #                selectInput(inputId = "functionlist", label = "Function", choices = c("rename","summarize","group_by","replace"), selected = "", selectize = FALSE, size = 4),
      #                uiOutput("others"),
      #                fileInput("datafile", label = "Data File"),
      #                verbatimTextOutput("test"),
      #                selectInput(inputId = "sheetname", label = "Sheet Name", choices = "", multiple = T),
      #                actionButton(inputId = "importdata", label = "Import Data"),
      #                selectInput(inputId = "dataset", label = "Data Set", choices = ""),
      #                # dataTableOutput("test2"),
      #                actionButton(inputId = "add", label = "Add"),
      #                actionButton(inputId = "save", label = "SAVE"),
      #                actionButton(inputId = "reset", label = "RESET"),
      #                shinyWidgets::radioGroupButtons(inputId = "showoption", label = "Show Data", choices = c("Data Table", "NO"), selected = "NO"),
      #                conditionalPanel(condition = "input.showoption == 'Data Table'", dataTableOutput("sheetdata")),
      #                aceEditor(outputId = "tempcode", value = "", height = "100px"),
      #                aceEditor(outputId = "code", value = "", height = "100px"),
      #                actionButton(inputId = "datadownload", label = "Download", icon = icon("cloud-download"))
      #              )
      # ),

      ##### Data Merge TAP -----------------------------------------------------------------------------------------------------------------

      # miniTabPanel(title = "Data Merge"),

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
                       shinyWidgets::materialSwitch("scfilter", "Filter", status="info"),
                       conditionalPanel(condition='input.scfilter==true', uiOutput("add_scfilter")),
                       actionButton(inputId = "scstart", label = "S&C Start")
                     ),
                     dataTableOutput("scdata"),
                     hr(),
                     actionButton(inputId = "scdownload", label = "Download", icon = icon("cloud-download"))
                   )
      ),

      ##### Omniture TAP -------------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Omniture",
                   miniContentPanel(
                     fluidRow(
                       column(6,
                              actionButton(inputId = "omlogin", label = "Login"),
                              actionButton(inputId = "om_update", label = "Update"))
                     ),
                     hr(),
                     wellPanel(
                       fluidRow(
                         column(3,
                                dateRangeInput(inputId = "omstartdate", label = "Date Range", start = Sys.Date() -7, end = Sys.Date()))
                       ),
                       fluidRow(
                         column(3,
                                selectizeInput(inputId = "countryname", label = "Country Name", choices = "", multiple = T)),
                         column(3,
                                selectizeInput(inputId = "metricname", label = "Metric Name", choices = "", multiple = T)),
                         column(3,
                                selectizeInput(inputId = "elementname", label = "Element Name", choices = "", multiple = T)),
                         column(3,
                                selectizeInput(inputId = "segmentname", label = "Segment Name", choices = "", multiple = T))
                       ),
                       actionButton("omstart", "Omniture Start")
                     ),
                     dataTableOutput("omdata"),
                     hr(),
                     actionButton(inputId = "omdownload", label = "Download", icon = icon("cloud-download"))
                   )
      ),

      ##### Adwords TAP --------------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Adwords",
                   miniContentPanel(
                     fluidRow(
                       column(8,
                              actionButton(inputId = "Refresh", label = paste0("Authorization : ", oauth_ck(".google.auth.RData"))),
                              actionButton(inputId = "adremove", label = "Remove Auth"))
                     ),
                     hr(),
                     wellPanel(
                       fluidRow(
                         column(4,
                                dateRangeInput(inputId = "adstartdate", label = "Date Range", start = Sys.Date() - 7, end = Sys.Date()))
                       ),
                       fluidRow(
                         column(5,
                                selectInput(inputId = "reportname", label = "Report Name", choices = RAdwords::reports(), width = "100%")),
                         column(3,
                                selectInput(inputId = "Ad_metricname", label = "Metric Name", choices = "", multiple = T)),
                         column(3,
                                textInput(inputId = "clientcustomerId", label = "Client Customer Id"))
                       ),
                       actionButton(inputId = "adstart", label = "Adwords Start")
                     ),
                     dataTableOutput("addata"),
                     hr(),
                     actionButton(inputId = "addownload", label = "Download", icon = icon("cloud-download"))
                   )
      ),

      ##### Google Analytics ---------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Google Analytics",
                   miniContentPanel(
                     fluidRow(
                       column(6,
                              actionButton(inputId = "gaRefresh", label = paste0("Authorization : ", oauth_ck("ga.httr-oauth"))),
                              actionButton(inputId = "garemove", label = "Remove Auth")
                       )
                     ),
                     hr(),
                     wellPanel(
                       fluidRow(
                         column(3,
                                dateRangeInput(inputId = "gastartdate", label = "Date Range", start = Sys.Date() - 7, end = Sys.Date()))
                       ),
                       fluidRow(
                         column(3,
                                selectizeInput(inputId = "gaid", label = "Id", choices = if(!exists("ga_id")){""} else {ga_id$viewName}, multiple = T)),
                         column(3,
                                selectizeInput(inputId = "gametric", label = "Metric", choices = if(!exists("ga_metric")){""} else {ga_metric}, multiple = T)),
                         column(3,
                                selectizeInput(inputId = "gadimension", label = "Dimension", choices = if(!exists("ga_dimension")){""} else {ga_dimension}, multiple = T)),
                         column(3,
                                selectizeInput(inputId = "gasegment", label = "Segment", choices = if(!exists("ga_segment")){""} else {ga_segment$name}, multiple = T))
                       ),
                       actionButton(inputId = "gastart", label = "G&A Start")
                     ),
                     dataTableOutput("gadata"),
                     hr(),
                     actionButton(inputId = "gadownload", label = "Download", icon = icon("cloud-download"))
                   )
      )
    )
  )

  ##### SERVER -------------------------------------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

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

    ##### Data Preparation TAP -----------------------------------------------------------------------------------------------------------

    # variable_name <- function(name){eval(parse(text=paste0("`", name,"`")))}

    # # File Upload
    # selectfile <- reactive({
    #   selectfile <- input$datafile
    #   if(is.null(selectfile)){
    #     return(NULL)
    #   }
    #   return(selectfile)
    # })
    #
    # # Funtion 사용 시 코드 작성
    # makecode <- reactive({
    #   ret <- ""
    #   temp <- ""
    #   if(!is.null(input$functionlist)){
    #     ret <-paste0(ret, input$functionlist, "(")
    #     if(input$functionlist == "group_by"){
    #       temp <- paste0(paste0("`", input$sheetcolname, "`"), collapse = ",")
    #     }
    #     ret <- paste0(ret,temp,")")
    #   }
    #   ret
    # })
    #
    # # Upload File Sheet Name
    # observe({if(!is.null(selectfile())){updateSelectInput(session, "sheetname", choices = readxl::excel_sheets(selectfile()$datapath))}})
    #
    # # Import Data Click 시
    # observeEvent(input$importdata, {
    #   for(i in input$sheetname) {
    #     assign(i, readxl::read_excel(selectfile()$datapath, sheet = i), envir = .GlobalEnv)
    #   }
    #   updateSelectInput(session, "dataset", choices = input$sheetname)
    # })
    #
    # # Data set 선택 시 datatable, Column name, code 출력
    # observe({
    #   if(input$dataset != ""){
    #     output$sheetdata <- renderDataTable({variable_name(input$dataset)})
    #     updateSelectInput(session, "sheetcolname", choices = colnames(variable_name(input$dataset)))
    #     updateAceEditor(session, "code", value = paste0("`", input$dataset, "` <- ", "`", input$dataset, "`"))
    #   }
    # })
    #
    # # Function List 선택 시 필요 UI 추가
    # output$others <- renderUI({
    #   if(!is.null(input$functionlist)){
    #     tagList(
    #       if(input$functionlist == "rename") {textInput(inputId = "renamevalue", label = "New Column Name")},
    #       if(input$functionlist == "summarize") {eval(parse(text = paste0("textInput(inputId = '", paste0("value",1:length(input$sheetcolname)) ,"', label = '", paste0("value",1:length(input$sheetcolname)), "')", collapse = "")))}
    #     )
    #   }
    # })
    #
    # observeEvent(input$renamevalue, {
    #   if(input$renamevalue != ""){
    #     if(input$functionlist == "rename"){
    #       temp <- paste0(input$functionlist, "(`", input$renamevalue, "` = `", input$sheetcolname[1], "`)")
    #       updateAceEditor(session, "tempcode", value = temp)
    #     }
    #   }
    # })
    #
    # observeEvent(input$functionlist, {updateAceEditor(session, "tempcode", value = makecode())})
    # observeEvent(input$sheetcolname, {updateAceEditor(session, "tempcode", value = makecode())})
    # observeEvent(input$reset, {updateAceEditor(session, "code", value = paste0("`", input$dataset, "` <- ", "`", input$dataset, "`"))})
    # observeEvent(input$add, {updateAceEditor(session, "code", value = paste0(input$code, " %>%\n", input$tempcode))})
    #
    # observeEvent(input$save, {
    #   # rstudioapi::insertText(text = input$code)
    #   assign(input$dataset, eval(parse(text = input$code)), envir = .GlobalEnv)
    #   output$sheetdata <- renderDataTable({variable_name(input$dataset)})
    #   updateSelectInput(session, "sheetcolname", choices = colnames(variable_name(input$dataset)))
    #   showModal(text_page("변경되었습니다."))
    # })
    #
    # observeEvent(input$datadownload, {
    #   write.csv(variable_name(input$dataset), paste0(Sys.Date(), gsub("[~!@#$%^&*()<>?_+ ]", "", input$dataset), ".csv"), row.names = F)
    #   showModal(text_page("다운로드가 완료되었습니다."))
    # })


    ##### Search Console TAP -------------------------------------------------------------------------------------------------------------

    sc_data.df <- reactiveValues()


    my_search_analytics <- function(siteURL, startDate, endDate, dimensions, rowLimit, walk_data){
      temp_df <- search_analytics(siteURL = siteURL,
                                  startDate = startDate,
                                  endDate = endDate,
                                  dimensions = dimensions,
                                  rowLimit = rowLimit,
                                  walk_data = walk_data) %>% mutate(url = siteURL)
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

    observeEvent(input$scstart, {
      element_null_ck(input$scwebsite, input$scdimension, element_name = c("Web Site URL", "Dimension"), text_page = text_page, exr = {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        gar_auth("sc.httr-oauth")
        sc_data.df <<- isolate({
          lapply(X = input$scwebsite,
                 FUN = my_search_analytics,
                 startDate = input$scstartdate[1],
                 endDate = input$scstartdate[2],
                 dimensions = input$scdimension,
                 rowLimit = 5000,
                 walk_data = "byBatch") %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
        })
        removeModal()
        showModal(text_page("S&C Data 추출 완료"))
        output$scdata <- renderDataTable(sc_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
      })
    })

    observeEvent(input$scdownload, {
      data_ck(sc_data.df, text_page, {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        write.csv(sc_data.df, paste0(format(Sys.time(), "%Y_%m_%d_%H_%M"),"_SearchConsole.csv"), row.names = F)
        removeModal()
        showModal(text_page("다운로드가 완료되었습니다."))
      })
    })


    ##### Omniture TAP -------------------------------------------------------------------------------------------------------------------

    omni_data.df <- reactiveValues()

    if(file.exists(".om.info.RData")){
      load(".om.info.RData")
      om_id <- om_info$ID
      om_pw <- om_info$PW
      updateSelectizeInput(session, "metricname", choices = om_info$om_list$metricname, options = list(maxOptions = length(om_info$om_list$metricname)))
      updateSelectizeInput(session, "elementname", choices = om_info$om_list$elementname, options = list(maxOptions = length(om_info$om_list$elementname)))
      updateSelectizeInput(session, "segmentname", choices = om_info$om_list$segmentname_name, options = list(maxOptions = length(om_info$om_list$segmentname_name)))
    } else {
      om_id <- ""
      om_pw <- ""
    }

    om_auth_page <- function(){
      modalDialog(
        textInput(inputId = "om_id", label = "ID", value = om_id),
        textInput(inputId = "om_pw", label = "Pass Word", value = om_pw),
        footer = tagList(
          actionButton(inputId = "omauthok", label = "OK"),
          modalButton("Cancel")
        )
      )
    }

    observeEvent(input$omlogin, showModal(om_auth_page()))

    observeEvent(input$omauthok, {
      removeModal()
      isolate({
        om_info <<- list("ID" = input$om_id, "PW" = input$om_pw)

        if(file.exists(".om.info.RData") == FALSE){
          save("om_info", file = ".om.info.RData")
        } else {
          if(om_id == input$om_id && om_pw == input$om_pw){
          } else {
            save("om_info", file = ".om.info.RData")
          }
        }
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        RSiteCatalyst::SCAuth(isolate({input$om_id}), isolate({input$om_pw}))
        updateSelectizeInput(session, "countryname", choices = RSiteCatalyst::GetReportSuites()$rsid)
        removeModal()
        showModal(text_page("완료 되었습니다"))
      })
    })

    observeEvent(input$om_update, {
      isolate({
        if(is.null(input$countryname)){
          showModal(text_page("국가를 선택해주세요"))
        } else if(file.exists(".om.info.RData") == FALSE){
          showModal(text_page("로그인 후 사용가능 합니다"))
        } else {
          showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
          om_list <- list(
            "metricname" = RSiteCatalyst::GetMetrics(input$countryname[1])$id,
            "elementname" = RSiteCatalyst::GetElements(input$countryname[1])$id,
            "segmentname_id" = RSiteCatalyst::GetSegments(input$countryname[1])$id,
            "segmentname_name" = RSiteCatalyst::GetSegments(input$countryname[1])$name
          )
          removeModal()
          om_info$om_list <<- om_list
          save("om_info", file = ".om.info.RData")
          updateSelectizeInput(session, "metricname", choices = om_list$metricname, options = list(maxOptions = length(om_list$metricname)))
          updateSelectizeInput(session, "elementname", choices = om_list$elementname, options = list(maxOptions = length(om_list$elementname)))
          updateSelectizeInput(session, "segmentname", choices = om_list$segmentname_name, options = list(maxOptions = length(om_list$segmentname_name)))
          showModal(text_page("완료 되었습니다"))
        }
      })
    })

    observeEvent(input$omstart, {
      element_null_ck(input$countryname, input$metricname, input$elementname, element_name = c("Country", "Metric Name", "Element Name"), text_page = text_page, exr = {
        if(is.null(input$segmentname)){segment_id.char <- ""} else {segment_id.char <- isolate({input$segmentname})}
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        omni_data.df <<- isolate({lapply(X = input$countryname,
                                         FUN = QueueTrended,
                                         date.from = input$omstartdate[1],
                                         date.to = input$omstartdate[2],
                                         metrics = input$metricname,
                                         elements = input$elementname,
                                         top = 50000,
                                         start = 0,
                                         segment.id = om_info$om_list$segmentname_id[which(input$segmentname == om_info$om_list$segmentname_name)],
                                         enqueueOnly = FALSE,
                                         max.attempts = 1000) %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
        })
        removeModal()
        showModal(text_page("옴니츄어 추출 완료"))
        output$omdata <- renderDataTable(omni_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
      })
    })

    observeEvent(input$omdownload, {
      data_ck(omni_data.df, text_page, {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        write.csv(omni_data.df, paste0(format(Sys.time(), "%Y_%m_%d_%H_%M"),"_omniture.csv"), row.names = F)
        removeModal()
        showModal(text_page("다운로드가 완료되었습니다."))
      })
    })


    ##### Adwords TAP --------------------------------------------------------------------------------------------------------------------

    credentials <- reactiveValues()
    Ad_data.df <- reactiveValues()
    Adwords_info <- reactiveValues()


    auth_page <- function(){
      modalDialog(
        textInput(inputId = "clientid", label = "Client ID"),
        textInput(inputId = "clientsecret", label = "Client Secret"),
        textInput(inputId = "developertoken", label = "Developer Token"),
        footer = tagList(actionButton(inputId = "authok", label = "OK"),
                         modalButton("Cancel"))
      )
    }

    clientToken_page <- function(){
      modalDialog(
        textInput(inputId = "clienttoken", label = "Client Token"),
        footer = tagList(actionButton(inputId = "ok", label = "OK"))
      )
    }

    observeEvent(input$Refresh, {if(!file.exists(".google.auth.RData")){showModal(auth_page())} else {}})

    observeEvent(input$authok, {
      removeModal()
      isolate({
        credentials <- getauth(input$clientid, input$clientsecret, input$developertoken)
        credentials <<- credentials
        Adwords_info <<- list("clientid" = input$clientid,
                              "clientsecret" = input$clientsecret,
                              "developertoken" = input$developertoken)
      })
      showModal(clientToken_page())
    })

    observeEvent(input$ok, {
      credentials$c.token <- as.character(isolate({input$clienttoken}))
      removeModal()
      access_token <- loadtoken(credentials)
      google_auth <<- list()
      google_auth$credentials <<- credentials
      google_auth$access <<- access_token
      google_auth$Adwords_info <<- Adwords_info
      save("google_auth",file=".google.auth.RData")
      updateActionButton(session, inputId = "Refresh", label = "Authorization : OK")
    })

    observeEvent(input$adremove, {
      file.remove(".google.auth.RData")
      updateActionButton(session, inputId = "Refresh", label = "Authorization : NO")
    })

    observe(
      updateSelectInput(session, "Ad_metricname", choices = RAdwords::metrics(as.character(input$reportname)))
    )

    observeEvent(input$adstart, {
      element_name <- c("Metric Name", "Client Customer Id")
      null_ck <- c(is.null(input$Ad_metricname), input$clientcustomerId == "")
      if(any(null_ck)){
        showModal(text_page(paste0(paste0(element_name[null_ck], collapse = " , "), " 을(를) 선택해야 합니다.")))
      } else {
        body <- isolate({
          statement(select = input$Ad_metricname,
                    report = input$reportname,
                    start = input$adstartdate[1],
                    end = input$adstartdate[2])
        })
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        isolate({
          Ad_clientcustomerId <- unlist(strsplit(input$clientcustomerId, ","))
          Ad_data.df <<- lapply(X = Ad_clientcustomerId,
                                FUN = RAdwords::getData,
                                google_auth = google_auth,
                                statement = body) %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
        })
        removeModal()
        showModal(text_page("애드워즈 추출 완료"))
        output$addata <- renderDataTable(Ad_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
      }
    })

    observeEvent(input$addownload, {
      data_ck(ga_data.df, text_page, {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        write.csv(Ad_data.df, paste0(format(Sys.time(), "%Y_%m_%d_%H_%M"),"_adwords.csv"), row.names = F)
        removeModal()
        showModal(text_page("다운로드가 완료되었습니다."))
      })
    })


    ##### Google Analytics ---------------------------------------------------------------------------------------------------------------

    ga_data.df <- reactiveValues()

    ga_auth_page <- function(){
      modalDialog(
        textInput(inputId = "gaclientid", label = "Client ID"),
        textInput(inputId = "gaclientsecret", label = "Client Secret"),
        footer = tagList(actionButton(inputId = "gaauthok", label = "OK"),
                         modalButton("Cancel"))
      )
    }

    my_google_analytics <- function(id, date_range, metrics, dimensions, ga_id){
      temp_df <- googleAnalyticsR::google_analytics(viewId = id,
                                                    date_range = date_range,
                                                    metrics = metrics,
                                                    dimensions = dimensions,
                                                    max = -1) %>% mutate(`Id Name` = ga_id$viewName[which(ga_id$viewId %in% id)])
    }

    observeEvent(input$gaRefresh, {
      if(!file.exists("ga.httr-oauth")){
        ga_id <- reactiveValues()
        showModal(ga_auth_page())
      }
    })

    observeEvent(input$gaauthok, {
      showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
      isolate({
        options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
                                                "https://www.googleapis.com/auth/analytics.readonly",
                                                "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                                                "https://www.googleapis.com/auth/analytics.edit",
                                                "https://www.googleapis.com/auth/analytics.manage.users",
                                                "https://www.googleapis.com/auth/analytics.provision"))
        options("googleAuthR.client_id" = input$gaclientid)
        options("googleAuthR.client_secret" = input$gaclientsecret)
        googleAnalyticsR::ga_auth("ga.httr-oauth")
        updateActionButton(session, inputId = "gaRefresh", label = "Authorization : OK")
        ga_id <<- googleAnalyticsR::ga_account_list()
        ga_metric <- googleAnalyticsR::allowed_metric_dim(type = "METRIC")
        ga_dimension <- googleAnalyticsR::allowed_metric_dim(type = "DIMENSION")
        ga_segment <- googleAnalyticsR::ga_segment_list()$items
        updateSelectizeInput(session, "gaid", choices = ga_id$viewName, options = list(maxOptions = length(ga_id$viewName)))
        updateSelectizeInput(session, "gametric", choices = ga_metric, options = list(maxOptions = length(ga_metric)))
        updateSelectizeInput(session, "gadimension", choices = ga_dimension, options = list(maxOptions = length(ga_dimension)))
        updateSelectizeInput(session, "gasegment", choices = ga_segment$name, options = list(maxOptions = length(ga_segment$name)))
        removeModal()
        removeModal()
        showModal(text_page("완료 되었습니다"))
      })

    })

    observeEvent(input$garemove, {
      file.remove("ga.httr-oauth")
      updateActionButton(session, inputId = "gaRefresh", label = "Authorization : NO")
    })

    observeEvent(input$gastart, {
      element_null_ck(input$gaid, input$gametric, input$gadimension, element_name = c("Id", "Metric", "Dimension"), text_page = text_page, exr = {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        isolate({
          ga_data.df <<- lapply(X = ga_id$viewId[(which(ga_id$viewName %in% input$gaid))],
                                FUN = my_google_analytics,
                                date_range = c(input$gastartdate[1], input$gastartdate[2]),
                                metrics = input$gametric,
                                dimensions = input$gadimension,
                                ga_id = ga_id) %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
        })
        removeModal()
        showModal(text_page("GA Data 추출 완료"))
        output$gadata <- renderDataTable(ga_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
      })
    })

    observeEvent(input$gadownload, {
      data_ck(ga_data.df, text_page, {
        showModal(text_page("잠시만 기다려주세요...", buffer = TRUE))
        write.csv(ga_data.df, paste0(format(Sys.time(), "%Y_%m_%d_%H_%M"),"_googleAnalytics.csv"), row.names = F)
        removeModal()
        showModal(text_page("다운로드가 완료되었습니다."))
      })

    })

  }

  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
