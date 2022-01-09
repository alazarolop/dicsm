## @knitr opts_chunks ####

knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(connection = "con")
knitr::opts_chunk$set(out.width='65%', fig.align='center')
options(knitr.kable.NA = '')




## @knitr basic packages ####

# basic
library(glue)
library(tidyverse)
library(kableExtra)

# system
library(fs)





## @knitr parallel ####
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores = 6L)





## @knitr environ ####

# .proj_envi <- function(.clue) {
#  purrr::is_character(.clue)
# 
#  if (.Platform$GUI == "RStudio") { # When using RStudio
#    .projenvi <- rstudioapi::getActiveDocumentContext()$path %>%
#      path_dir()
#  } else { # When knitting RMarkdown document
#    .projenvi <- getwd() %>%
#      dir_ls(regexp = .clue)
#  }
# 
#  .projenvi
# }

#' Directories within subprojects
#'
#' @param .subpath 
#'
#' @return
#' @export
#'
#' @examples
.proj_subdir <- function(.subpath) {
  assertthat::is.dir(.subpath)

  .subproj <- list(
    res = fs::path(.subpath, "results"),
    img = fs::path(.subpath, "images")
  )

  walk(.subproj, fs::dir_create)

  .subproj
}



#' Store key word for subprojects
#'
#' @param .char 
#'
#' @return
#' @export
#'
#' @examples
.proj_key <- function(.char) {
  
  .subproj <- list(
    key = .char
  )
  
  .subproj
}