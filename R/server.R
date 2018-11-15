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
  
  sc_data.df <- reactiveValues()
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
      temp_err <- NULL
      btn.num <- filter_btn$sc_btn
      tryCatch({
        sc_data.df <<- isolate({
          lapply(X = input$scwebsite,
                 FUN = my_search_analytics,
                 startDate = input$scstartdate[1],
                 endDate = input$scstartdate[2],
                 dimensions = input$scdimension,
                 dimensionFilterExp = if(input$scfilter && btn.num != 0){scfilter.func(btn.num)} else {NULL},
                 rowLimit = 5000,
                 walk_data = "byBatch") %>% do.call(., what = rbind) %>% replace(is.na(.), 0)
        })
        removeModal()
        showModal(text_page("S&C Data 추출 완료"))
        output$scdata <- renderDataTable(sc_data.df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
      })
      if(!is.null(temp_err)){output$scfail <- renderText({paste0("Fail URL \n",paste(temp_err, collapse = "\n"))})} else {output$scfail <- renderText({})}
    })
  })
  
  # 데이터 추출
  output$`sc_data.xlsx` <- downloadHandler(filename = function(){''},
                                           content = function(file){write.xlsx(sc_data.df, file, row.names = FALSE)})
  
}
