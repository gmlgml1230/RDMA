#' RDMA
# R Data Manipulation Add in
#' @export
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import RAdwords
#' @import RSiteCatalyst
#' @import shinyWidgets
#' @import shinyAce
#' @importFrom rstudioapi insertText
#' @importFrom readxl excel_sheets




RDMA <- function(){

  shiny.maxRequestSize = 30 * 1024 ^ 2

  context <- getActiveDocumentContext()
  text <- context$selection[[1]]$text
  defaultData <- text

  if(file.exists(".google.auth.RData")){
    Ad_auth <- "OK"
    load(".google.auth.RData")
  } else {
    Ad_auth <- "NO"
  }

  # dplyr package Check
  if(!isNamespaceLoaded("dplyr")){attachNamespace("dplyr")}

  # DataFrame Chack
  if(length(ls()) == 0){
    df_val <- NULL
  } else {
    df_val <- ls()[unlist(lapply(ls(), FUN = function(val){return(any(class(eval(parse(text = val))) %in% c("data.frame", "tibble", "tbl_df")))}))]
    # for(i in ls()){
    #   # eval(parse(text = i)) 문자를 변수로 변환 eval : 표현식을 평가, parse : 문자열을 표현식으로 변환
    #   mydata <- any(class(eval(parse(text = i))) %in% c("data.frame", "tibble", "tbl_df")) == TRUE
    # }
  }

  ##### UI -----------------------------------------------------------------------------------------------------------------------------

  ui <- miniPage(
    gadgetTitleBar("Artience Data Preparation Tool"),

    miniTabstripPanel(

      ##### Data Preparation TAP -----------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Data Preparation",
                   miniContentPanel(
                     selectInput(inputId = "sheetcolname", label = "Column Name", choices = "", size = 9, selectize = FALSE),
                     selectInput(inputId = "functionlist", label = "Function", choices = c("rename"), selected = "", selectize = FALSE, size = 4),
                     uiOutput("others"),
                     fileInput("datafile", label = "Data File"),
                     verbatimTextOutput("test"),
                     selectInput(inputId = "sheetname", label = "Sheet Name", choices = "", multiple = T),
                     actionButton(inputId = "importdata", label = "Import Data"),
                     selectInput(inputId = "dataset", label = "Data Set", choices = ""),
                     # dataTableOutput("test2"),
                     actionButton(inputId = "add", label = "Add"),
                     actionButton(inputId = "save", label = "SAVE"),
                     shinyWidgets::radioGroupButtons(inputId = "showoption", label = "Show Data", choices = c("Data Table", "NO"), selected = "NO"),
                     conditionalPanel(condition = "input.showoption == 'Data Table'", dataTableOutput("sheetdata")),
                     aceEditor(outputId = "tempcode", value = "", height = "100px"),
                     aceEditor(outputId = "code", value = "", height = "100px"),
                     actionButton(inputId = "download", label = "Download", icon = icon("cloud-download"))
                   )
      ),

      ##### Data Merge TAP -----------------------------------------------------------------------------------------------------------------

      # miniTabPanel(title = "Data Merge"),

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
                                selectInput(inputId = "countryname", label = "Country Name", choices = "", multiple = T)),
                         column(3,
                                selectInput(inputId = "metricname", label = "Metric Name", choices = "", multiple = T)),
                         column(3,
                                selectInput(inputId = "elementname", label = "Element Name", choices = "", multiple = T)),
                         column(3,
                                selectInput(inputId = "segmentname", label = "Segment Name", choices = "", multiple = T))
                       ),
                       actionButton("omstart", "Omniture Start")
                     ),
                     dataTableOutput("omdata"),
                     hr(),
                     actionButton(inputId = "omdownload", label = "Download", icon = icon("cloud-download"))
                     # downloadButton(outputId = "om_data.csv")
                   )
      ),

      ##### Adwords TAP --------------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Adwords",
                   miniContentPanel(
                     fluidRow(
                       column(4,
                              actionButton(inputId = "Refresh", label = paste0("인증서 : ", Ad_auth)))
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
                     # downloadButton(outputId = "ad_data.csv")
                   ))
    )
  )

  ##### SERVER -------------------------------------------------------------------------------------------------------------------------

  server <- function(input, output, session) {
    # Shiny에서 5MB의 제한을 잡아놓은걸 30MB로 늘린 것
    options(shiny.maxRequestSize = 30 * 1024 ^ 2)

    text_page <- function(text){
      modalDialog(
        text,
        footer = tagList(
          modalButton("Cancel")
        )
      )
    }

    ##### Data Preparation TAP -----------------------------------------------------------------------------------------------------------


    # import_data <- reactiveValues()

    selectfile <- reactive({
      selectfile <- input$datafile
      if(is.null(selectfile)){
        return(NULL)
      }
      return(selectfile)
    })

    makecode <- reactive({

      ret=""

      if(!is.null(input$functionlist)){

        ret=paste0(ret,input$functionlist,"(")

        ret=paste0(ret,")")

      }
      ret
    })

    observe({
      if(is.null(selectfile())){
      } else {
        updateSelectInput(session, "sheetname", choices = readxl::excel_sheets(selectfile()$datapath))
        # updateSelectInput(session, "sheetcolname", choices = )
      }
    })

    observeEvent(input$importdata, {
      isolate({
        for(i in input$sheetname) {
          assign(paste0(i, ".df"), readxl::read_excel(selectfile()$datapath, sheet = i), envir = .GlobalEnv)
        }
        # import_data <<- lapply(X = input$sheetname, FUN = function(temp){readxl::read_excel(selectfile()$datapath, sheet = temp)})
        # names(import_data) <<- input$sheetname
        updateSelectInput(session, "dataset", choices = input$sheetname)
      })
    })

    observe({
      if(input$dataset != ""){
        output$sheetdata <- renderDataTable({eval(parse(text=paste0("`", input$dataset, ".df", "`")))})
        updateSelectInput(session, "sheetcolname", choices = colnames(eval(parse(text=paste0("`", input$dataset, ".df", "`")))))
        updateAceEditor(session, "code", value = input$dataset)
      }
    })


    output$others <- renderUI({
      if(!is.null(input$functionlist)){
        tagList(
          if(input$functionlist == "rename") textInput(inputId = "renamevalue", label = "New Column Name")
        )
      }
    })

    observeEvent(input$renamevalue, {
      if(input$renamevalue != ""){
        if(input$functionlist == "rename"){
          temp <- paste0(input$functionlist, "(`", input$renamevalue, "` = `", input$sheetcolname, "`)")
          updateAceEditor(session, "tempcode", value = temp)
          # output$test <- renderText({temp})
        }
      }
    })

    observeEvent(input$functionlist, {updateAceEditor(session, "tempcode", value = makecode())})
    observeEvent(input$sheetcolname, {updateAceEditor(session, "tempcode", value = makecode())})

    observeEvent(input$add, {
      updateAceEditor(session, "code", value = paste0(input$code, " %>%\n", input$tempcode))
    })

    observeEvent(input$save, {
      rstudioapi::insertText(text = input$code)


    })

    # output$test <- renderDataTable({import_data[input$dataset]})

    # output$test <- renderPrint({defaultData})
    # updateSelectInput(session, "sheetname", choices = sheetname())

    ##### Omniture TAP -------------------------------------------------------------------------------------------------------------------

    omni_data.df <- reactiveValues()
    # om_id <- reactiveValues()
    # om_pw <- reactiveValues()


    if(file.exists(".om.info.RData")){
      load(".om.info.RData")
      om_id <- om_info$ID
      om_pw <- om_info$PW
      updateSelectInput(session, "metricname", choices = om_info$om_list$metricname)
      updateSelectInput(session, "elementname", choices = om_info$om_list$elementname)
      updateSelectInput(session, "segmentname", choices = om_info$om_list$segmentname_name)
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

        RSiteCatalyst::SCAuth(isolate({input$om_id}), isolate({input$om_pw}))
        updateSelectInput(session, "countryname", choices = RSiteCatalyst::GetReportSuites()$rsid)
        showModal(text_page("완료 되었습니다"))
      })
    })

    observeEvent(input$om_update, {
      isolate({
        if(input$countryname == ""){
          showModal(text_page("국가를 선택해주세요"))
        } else if(file.exists(".om.info.RData") == FALSE){
          showModal(text_page("로그인 후 사용가능 합니다"))
        } else {
          om_list <- list(
            "metricname" = RSiteCatalyst::GetMetrics(input$countryname[1])$id,
            "elementname" = RSiteCatalyst::GetElements(input$countryname[1])$id,
            "segmentname_id" = RSiteCatalyst::GetSegments(input$countryname[1])$id,
            "segmentname_name" = RSiteCatalyst::GetSegments(input$countryname[1])$name
          )
          om_info$om_list <<- om_list
          save("om_info", file = ".om.info.RData")
          updateSelectInput(session, "metricname", choices = om_list$metricname)
          updateSelectInput(session, "elementname", choices = om_list$elementname)
          updateSelectInput(session, "segmentname", choices = om_list$segmentname_name)
          showModal(text_page("완료 되었습니다"))
        }
      })
    })

    observeEvent(input$omstart, {
      if(is.null(input$segmentname)){segment_id.char <- ""} else {segment_id.char <- isolate({input$segmentname})}

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
      showModal(text_page("옴니츄어 추출 완료"))
      output$omdata <- renderDataTable(omni_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
    })

    observeEvent(input$omdownload, {
      write.csv(omni_data.df, paste0(Sys.Date(),"_omniture.csv"), row.names = F)
      showModal(text_page("다운로드가 완료되었습니다."))
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
        footer = tagList(actionButton(inputId = "authok", label = "OK"))
      )
    }

    clientToken_page <- function(){
      modalDialog(
        textInput(inputId = "clienttoken", label = "Client Token"),
        footer = tagList(actionButton(inputId = "ok", label = "OK"))
      )
    }

    observeEvent(input$Refresh, {if(Ad_auth == "NO"){showModal(auth_page())} else {}})

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

      if(TRUE){
        save("google_auth",file=".google.auth.RData")
        updateActionButton(session, inputId = "Refresh", label = "인증서 : OK")
        if (!file.exists(".gitignore")){cat(".google.auth.RData",file=".gitignore",sep="\n")}
        if (file.exists(".gitignore")){cat(".google.auth.RData",file=".gitignore",append=TRUE)}
      }
    })

    observe(
      updateSelectInput(session, "Ad_metricname", choices = RAdwords::metrics(as.character(input$reportname)))
    )
    # observeEvent(input$adenter, {
    #   updateSelectInput(session, "Ad_metricname", choices = RAdwords::metrics(as.character(isolate({input$reportname}))))
    # })

    observeEvent(input$adstart, {
      body <- isolate({
        statement(select = input$Ad_metricname,
                  report = input$reportname,
                  start = input$adstartdate[1],
                  end = input$adstartdate[2])
      })

      Ad_data.df <<- RAdwords::getData(clientCustomerId = isolate({as.character(input$clientcustomerId)}),
                                       google_auth = google_auth,
                                       statement = body)
      showModal(text_page("애드워즈 추출 완료"))
      output$addata <- renderDataTable(Ad_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
    })


    observeEvent(input$addownload, {
      write.csv(Ad_data.df, paste0(Sys.Date(),"_adwords.csv"), row.names = F)
      showModal(text_page("다운로드가 완료되었습니다."))
    })


  }

  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}

# RDMA()
