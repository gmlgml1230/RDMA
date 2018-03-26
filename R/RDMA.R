#' RDMA
#' @export
#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import RAdwords
#' @import RSiteCatalyst
#' @import WriteXLS


# rm(list=ls())

# library(shiny)
# library(miniUI)
# library(dplyr)

# omniture tap package
# library(RAdwords)
# library(RSiteCatalyst)
# library(WriteXLS)
# library(RCurl)
# library(rjson)
#
# source("~/RDMA/R/getAuth.R")
# source("~/RDMA/R/loadToken.R")


if(file.exists(".google.auth.RData")){
  Ad_auth <- "OK"
  load(".google.auth.RData")
} else {
  Ad_auth <- "NO"
}


RDMA <- function(){

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
                     fluidRow(
                       selectInput(inputId = "dataframe1", label = "Data Frame", choices = df_val, selected = "", size = 9, selectize = FALSE)
                     ),
                     fluidRow(
                       selectInput(inputId = "selectdata", label = "Select Data", choices = "", selectize = FALSE, size = 4)
                     )
                   )
      ),

      ##### Data Merge TAP -----------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Data Merge"),

      ##### Omniture TAP -------------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Omniture",
                   miniContentPanel(
                     fluidRow(
                       column(6,
                              textInput(inputId = "loginid", label = "ID")),
                       column(6,
                              textInput(inputId = "loginpass", label = "Pass Word"),
                              actionButton(inputId = "omlogin", label = "Login"))
                     ),
                     fluidRow(
                       dateRangeInput(inputId = "omstartdate", label = "Date Range :", start = Sys.Date() -7, end = Sys.Date())
                     ),
                     fluidRow(
                       column(3,
                              selectInput(inputId = "countryname", label = "Country Name", choices = "", multiple = T),
                              actionButton(inputId = "omenter", label = "Enter")),
                       column(3,
                              selectInput(inputId = "metricname", label = "Metric Name", choices = "", multiple = T)),
                       column(3,
                              selectInput(inputId = "elementname", label = "Element Name", choices = "", multiple = T)),
                       column(3,
                              selectInput(inputId = "segmentname", label = "Segment Name", choices = "", multiple = T))
                     ),
                     fluidRow(
                       actionButton("start", "Omniture Start"),
                       actionButton("stop", "Stop"),
                       downloadButton(outputId = "omdownloaddata")
                     )
                   )
      ),

      ##### Adwords TAP --------------------------------------------------------------------------------------------------------------------

      miniTabPanel(title = "Adwords",
                   miniContentPanel(
                     fluidRow(
                       column(4,
                              actionButton(inputId = "Refresh", label = paste0("인증서 : ", Ad_auth)))
                     ),
                     fluidRow(
                       column(4,
                              dateRangeInput(inputId = "adstartdate", label = "Date Range :", start = Sys.Date() - 7, end = Sys.Date()))
                     ),
                     fluidRow(
                       column(6,
                              selectInput(inputId = "reportname", label = "Report Name", choices = RAdwords::reports()))
                     ),
                     fluidRow(
                       column(4,
                              selectInput(inputId = "Ad_metricname", label = "Metric Name", choices = "", multiple = T)),
                       column(4,
                              actionButton(inputId = "adenter", label = "Enter"))
                     ),
                     fluidRow(
                       column(4,
                              textInput(inputId = "clientcustomerId", label = "Client Customer Id"))
                     ),
                     fluidRow(
                       actionButton(inputId = "Ad_get_data", label = "Adwords Start"),
                       downloadButton(outputId = "addownloaddata"),
                       verbatimTextOutput("test")
                     )
                   ))
    )
  )

  ##### SERVER -------------------------------------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    ##### Omniture TAP -------------------------------------------------------------------------------------------------------------------

    observeEvent(input$omlogin, {
      RSiteCatalyst::SCAuth(isolate({input$loginid}), isolate({input$loginpass}))
      updateSelectInput(session, "countryname", choices = RSiteCatalyst::GetReportSuites()$rsid)
    })

    observeEvent(input$omenter, {
      updateSelectInput(session, "metricname", choices = RSiteCatalyst::GetMetrics(isolate({input$countryname[1]}))$id)
      updateSelectInput(session, "elementname", choices = RSiteCatalyst::GetElements(isolate({input$countryname[1]}))$id)
      updateSelectInput(session, "segmentname", choices = RSiteCatalyst::GetSegments(isolate({input$countryname[1]}))$id)
    })


    observeEvent(input$start, {
      if(is.null(input$segmentname)){segment_id.char <- ""} else {segment_id.char <- isolate({input$segmentname})}

      omni_data.df <<- lapply(X = isolate({input$countryname}),
                              FUN = QueueTrended,
                              date.from = isolate({input$omstartdate[1]}),
                              date.to = isolate({input$omstartdate[2]}),
                              metrics = isolate({input$metricname}),
                              elements = isolate({input$elementname}),
                              top = 50000,
                              start = 0,
                              segment.id = isolate({input$segmentname}),
                              enqueueOnly = FALSE,
                              max.attempts = 1000) %>% do.call(., what = rbind)
    })

    output$omdownloaddata <- downloadHandler(filename = function(){paste0(Sys.Date(), "_omni_data.xlsx")},
                                             content = function(file){WriteXLS(data, file, row.names = TRUE)})

    ##### Adwords TAP --------------------------------------------------------------------------------------------------------------------

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

    credentials <- reactiveValues()
    Ad_data.df <- reactiveValues()
    # google_auth <- reactiveValues()
    # access_token <- reactiveValues()

    observeEvent(input$authok, {
      removeModal()
      credentials <- isolate({getauth(input$clientid, input$clientsecret, input$developertoken)})
      credentials <<- credentials
      showModal(clientToken_page())
    })

    observeEvent(input$ok, {
      credentials$c.token <- as.character(isolate({input$clienttoken}))
      removeModal()
      access_token <- loadtoken(credentials)
      google_auth <<- list()
      google_auth$credentials <<- credentials
      google_auth$access <<- access_token

      if(TRUE){
        save("google_auth",file=".google.auth.RData")
        updateActionButton(session, inputId = "Refresh", label = "인증서 : OK")
        if (!file.exists(".gitignore")){cat(".google.auth.RData",file=".gitignore",sep="\n")}
        if (file.exists(".gitignore")){cat(".google.auth.RData",file=".gitignore",append=TRUE)}

      }

    })

    observeEvent(input$adenter, {
      updateSelectInput(session, "Ad_metricname", choices = RAdwords::metrics(as.character(isolate({input$reportname}))))
    })

    observeEvent(input$Ad_get_data, {
      body <- isolate({
        statement(select = input$Ad_metricname,
                  report = input$reportname,
                  start = input$adstartdate[1],
                  end = input$adstartdate[2])

      })

      Ad_data.df <<- RAdwords::getData(clientCustomerId = isolate({as.character(input$clientcustomerId)}),
                                       google_auth = google_auth,
                                       statement = body)
      print(paste0("애드워즈 추출 완료", Sys.time()))
      output$test <- renderText(getwd())
    })

    # output$addownloaddata <- downloadHandler(filename = function(){paste0(Sys.Date(), "_adwords_data.xlsx")},
    #                                          content = function(file){WriteXLS(Ad_data.df, file, row.names = TRUE)})
    output$addownloaddata <- downloadHandler(filename = paste0(Sys.Date(), "_adwords_data.xlsx"),
                                             content = function(file){
                                               write.table(Ad_data.df, file = file, append = T, row.names = F, sep = ',',col.names=TRUE)
                                             })




  }

  viewer <- dialogViewer("RDMA", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}

# RDMA()
