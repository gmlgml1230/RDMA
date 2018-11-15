
#' get RDMA

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
                     # InputID 확인용
                     verbatimTextOutput('InputID_View'),
                     actionButton(inputId = "test_button",label = "test"),
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
