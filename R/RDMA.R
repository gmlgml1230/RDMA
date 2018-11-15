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

  viewer <- dialogViewer("RDMA", width = 1200, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
