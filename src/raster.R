# Functions for raster processing in DiCSM

# Connections ------------------------------------------------------------------

#' String of a PostGIS connection to be used with GDAL.
#'
#' @param .tb Table of the database (vector o scalar)
#' @param .sch Schema that holds the table  (vector o scalar)
#' @param .db Database that holds the schmea (vector o scalar)
#' @param .column Column name. Default is 'rast'
#' @param .mode Mode. Default is 2: Full table as a single raster. 1: Each tile as a raster.
#'
#' @return A string with a formatted PostGIS connection
#' @export
#'
#' @examples
pgiscon <- function(.tb, .sch, .db = "dicsm", .column = "rast", .mode = 2L, .where = NULL ) {
  # Checks
  assertthat::assert_that( 
    purrr::is_character(.tb),
    purrr::is_character(.sch),
    purrr::is_character(.db),
    purrr::is_scalar_character(.column),
    purrr::is_scalar_integer(.mode)
    )
  if(purrr::is_scalar_character(.sch) == TRUE) .sch <- rep(.sch, length(.tb))
  if(purrr::is_scalar_character(.db) == TRUE) .db <- rep(.db, length(.tb))
  if(is.null(.where) == FALSE) assertthat::assert_that(
    purrr::is_character(.where))

  # Method
  .pgis <- vector(mode = "character", length = 0)
  for (i in seq_along(.tb)) {
    .pgis[[i]] <- glue::glue("PG:host='localhost' port=5432 ",
                       "user='{keyring::key_list(\"psql-su\")[1,2]}' password='{keyring::key_get(\"psql-su\")}' ",
                       "dbname='{.db[[i]]}' schema='{.sch[[i]]}' table='{.tb[[i]]}' column='{.column}' mode={.mode}")
  }
  
  if(is.null(.where) == FALSE) {
    .where <- paste0("where='", .where, "'")
    .pgis <- paste(.pgis, .where) 
  }
  
  return(.pgis)
}






# Reading rasters to R --------------------------------------------------------------------

#' Reading rasters from a PostGIS database (core function)
#'
#' @param .rt_src Character vector of PostGIS connections ready to be used in GDAL (e.g from pgiscon)
#' @param .nm Vector with names of rasters
#'
#' @return List of RasterBricks
#' @export
#'
#' @examples
pgisrast <- function(.rt_src, .nm = NULL) {
  # Checks
  assertthat::assert_that(
    purrr::is_character(.rt_src),
    all(stringr::str_detect(.rt_src, "PG:"))
  )
  
  if( purrr::is_null(.nm) == FALSE ) {
    assertthat::assert_that(
      purrr::is_character(.nm),
      length(.nm) == length(.rt_src)
    )
  }
  
  # Method
  # -- Read them with GDAL
  .rt_gdal <- foreach::foreach(i = seq_along(.rt_src), .final = function(x) setNames(x, .nm) ) %dopar% {
    rgdal::readGDAL(.rt_src[[i]], silent = TRUE)
  }
  
  # -- Transform them in rasters
  # Depending on the band number of each raster, they're loaded as a RasterBrick (>1) or as a RasterLayer.
  # List elements are named based on supplied vector of raster names.
  .rasterlist <- foreach::foreach(rast = .rt_gdal, .final = function(x) setNames(x, names(.rt_gdal)) ) %dopar% {
  #  if( ncol(rast@data) > 1 ) {
      brick(rast) 
  #  } else {
  #    raster(rast)
  #  }
  }
  
  # 
  if (length(.rasterlist) == 1) {
    .rasterlist <- .rasterlist[[1]]
  }
}






#' Reading rasters from file  (core function)
#'
#' @param .rt_src Character vector of filepaths
#'
#' @return List of RasterBricks
#' @export
#'
#' @examples
.filerast <- function(.rt_src) {
  # Checks
  assertthat::assert_that(
    purrr::is_character(.rt_src),
    all(fs::is_absolute_path(.rt_src))
  )
  
  # Method
  # -- Read them from file    
  .rasterlist <- foreach::foreach(rast = .rt_src) %dopar% {
    if( file_exists(rast) ) {
      brick(rast) 
    } 
  }
  
  # -- Check whether to transform them to RasterLayer or keep them as RasterBricks
  #.nbands <- .rasterlist %>%
  #  map(~.@file@nbands) %>%
  #  flatten_dbl() %>%
  #  max()
  
  #if (.nbands != 1) {
  #  for(i in seq_along(.rasterlist)) {
  #    if(.rasterlist[[i]]@file@nbands == 1 ) {
  #      .rasterlist[[i]] <- raster(.rasterlist[[i]])
  #    }
  #  }
  #}
  
  # Name each list element like the first raster band it holds.
  #names(.rasterlist) <- .rasterlist %>%
  #  map(~.@data@names[[1]]) %>%
  #  flatten_chr()
  
  return(.rasterlist)
}






# Raster transformations --------------------------------------------------------------------

#' Rasterlist to raster tibble
#' 
#' Transform a list of rasters into a tibble with: 
#' - raster: factor variable of raster names
#' - covar: values of the raster pixels
#'
#' @param .rasterlist A list whose elements are rasters (e.g. from a foreach loop)
#'
#' @return tibble 
#' @export
#'
#' @examples
#' dems <- rast2tbl(dems)
#' 
tblrowraster <- function(.rasterlist) {
  # Checks
  assertthat::assert_that(
    is_list(.rasterlist)
  )
  
  # Methods
  # -- Set the same name for raster bands, which will be the common variable 
  for (i in seq_along(.rasterlist)) {
    names(.rasterlist[[i]]) <- "covar"
  }
  
  # -- Take the prepared list
  .tblraster <- .rasterlist %>%
    map(as.data.frame) %>% # Convert each list element: raster ~ data frame
    map(filter, !is.na(covar) ) %>% # Filter NA elements
    bind_rows(.id = "raster") %>% # Gather all data frames in a unique one
    as_tibble() %>%
    transmute(raster = as_factor(raster), covar )
  
  return(.tblraster)
}






#' Rasterlist to raster tibble (columns version)
#'
#' Transform a list of rasters into a tibble with a column for every 
#' raster.
#'
#' @param .rasterlist A list whose elements are rasters (e.g. from a foreach loop)
#'
#' @return tibble
#' @export
#'
#' @examples
#' 
tblcolraster <- function(.rasterlist) {
  # Checks
  assertthat::assert_that(
    is_list(.rasterlist)
  )
  
  # Methods
  # Methods
  .tblraster <- .rasterlist
  .tblraster <- stack(.tblraster)
  .tblraster <- raster::as.data.frame(.tblraster)
  .tblraster <- as_tibble(.tblraster)
  .tblraster <- filter_all(.tblraster, all_vars(!is.na(.)))
  
  return(.tblraster)  
}





#' Rasterlist to matrix
#'
#' Transform a list of rasters into a matrix with a column for every
#' raster in the original rasterlist. Columns are named based on rasters.
#' 
#' @param .rasterlist 
#'
#' @return matrix with values from every raster of rasterlist as column.
#' @export
#'
#' @examples
matraster <- function(.rasterlist) {
  # CHECKS
  assertthat::assert_that(
    is_list(.rasterlist)
  )
  
  # METHODS
  ## Transformation to matrix
  .matraster <- .rasterlist %>%
    tblcolraster() %>%  
    as.matrix()
    
    #map(raster::as.matrix) %>%
    #do.call(what = cbind) %>% # Get all elements on the list a _cbind_ them
    #na.omit()
  
  return(.matraster)
}







#  Raster tibbles summaries --------------------------------------------------------------------

#' Basic summary function for a raster tibble with:
#' - mean
#' - min
#' - median
#' - max
#' - stdev
#' - count
#'
#' @param .tbl A grouped tibble
#' @param .covar Attribute to analise 
#'
#' @return tibble
#' @export
#'
#' @examples
#' 
rast_basic <- function(.tbl, .covar) {
  .covar <-enquo(.covar)
  
  .tbl %>%
    summarise(
      mean = mean(!! .covar),
      min = min(!! .covar),
      median = median(!! .covar),
      max = max(!! .covar),
      stdev = sd(!! .covar),
      count = n() )
}







# Raster files --------------------------------------------------------------------

#' Getting all associated files of a SAGA raster file.
#'
#' @param .fp filepath
#' @param .ext specific extension
#'
#' @return A vector with five filepath (one for each data type) for
#' each introduced path.
#' @export
#'
#' @examples
sagafiles <- function(.fp, .nm = FALSE) {
  if ( is.character(.fp) == FALSE) {
    .fp <- as.character(.fp)
  }
  .saga <- vector(mode = "list", length = 0)
  for (i in seq_along(.fp)) {
    .saga[[i]] <- c( "mgrd" = paste0(.fp[i], ".mgrd"),
                     "prj" = paste0(.fp[i], ".prj"),
                     "sdat" = paste0(.fp[i], ".sdat"),
                     "sdat.aux.xml" = paste0(.fp[i], ".sdat.aux.xml"),
                     "sgrd" = paste0(.fp[i], ".sgrd") )
    if ( .nm == FALSE ) {
      .saga[[i]] <- unname(.saga[[i]])
    }
  }
  .saga
}




# Statistics ----------------

#' Sampling setup for CLARA clustering
#'
#' @param .mat Matrix to work with.
#' @param .p Number of pixels to drawn for every sample. 
#' @param .pmax Maximum percentage of pixels to be drawn.
#' @param .smax Maximum number of sampling.
#'
#' @return A list with two elements: 
#' * _p_, percentage to be drawn
#' * _s_ number of samplings
#'
#' @export
#'
#' @examples
#' mat = matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = FALSE)
#' sampling(mat)
#'
sampling <- function(.mat, .p = (10^4*1.75), .pmax = 0.55, .smax = 50L){
  # Checks
  assertthat::assert_that(
    is.matrix(.mat),
    is.numeric(.p),
    is.numeric(.pmax),
    is.integer(.smax),
    .pmax <= 1
  )
  
  # Method
  .p <- ifelse(nrow(.mat) > .p, round(.p/nrow(.mat), digits = 2), .pmax) 
  .p <- ifelse(.p >= .pmax, .pmax, .p)
  
  .s <- as.integer(round((1/.p)*1.15))
  .s <- ifelse(.s < .smax, .s, .smax)
  
  list("p" = .p, "s" = .s)
} 



#' Confusion Index of a fuzzy classification
#'
#' @param .res Matrix of groups probabilities or a result from ClusterR::Clara_Medoids.
#'
#' @return a vector holding Confusion Index
#' @export
#'
#' @examples
ci <- function(.res = classout$fuzzy_probs) {
  # Create FORK cluster limited to 6 cores
  #registerDoSEQ()
  #cores <- makeCluster(6L, "FORK")
  #registerDoParallel(cores)
  
  # Confusion Index
  ## Number of groups
  n <- ncol(.res)-1
  
  ## Probabilities for two main groups
  c2 <- parApply(cores, .res, 1, function(x) sort(x, partial=n)[n])
  c1 <- parApply(cores, .res, 1, max)
  
  ## Result
  ci <- 1 - (c1 - c2 )
  
  # Reset to standard cluster
  #parallel::stopCluster(cores)
  #registerDoParallel(cores = 6L)
  
  return(ci)
}






# Raster classification -----

#' SMU1 data filtered and scaled
#'
#' @param .smu1 
#'
#' @return
#' @export
#'
#' @examples
smuproc <- function(.smu1){
  smu1_proc <- vector(mode = "list")
  
  pix <- raster::getValues(.smu1)
  pix <- pix[complete.cases(pix), ] %>% 
    scale()
  
  smu1_proc[["smu1"]] <- pix 
  rm(pix)

  # Vector holding positions of filled pixels + new raster for groups
  
  smu1_proc[["vref"]] <- smu1_proc[["clt"]] <- raster(x = .smu1, layer = 1)
  smu1_proc[["vref"]] <- raster::Which(!is.na(smu1_proc$vref)) %>% 
    as.vector()
  
  # Cleaning
  gc()
  
  return(smu1_proc)
}


#' Title
#'
#' @param .res 
#' @param ... 
#' @param .vrefb 
#' @param .cltb 
#' @param .pth 
#'
#' @return
#' @export
#'
#' @examples
clusterast <- function(.res, ..., .vrefb = smu$vref, .cltb = smu$clt, .pth = "gis") {
  # CHECKS 
  assertthat::assert_that(
    purrr::is_scalar_character(.pth)
  )
  
  # METHOD 
  # Path 
  ## Name of the method
  .nm_res <- deparse(substitute(.res)) %>% 
    str_split(., pattern = "[\\$.]", simplify = TRUE) %>% 
    .[1]
  ## Extra
  .extra <- stringr::str_c(..., collapse = "-") %>% 
    paste0("-")
  ## Path to apply
  if (.pth == "gis") {
    .pth <- path(.pth, paste0("class-", .extra, .nm_res), ext = "tif")
  }
  
  # New raster
  ## Pattern
  .vref <- .vrefb
  .clt <- .cltb
  
  ## Vector with data 
  .vref[.vref == TRUE] <- .res
  .vref[.vref == 0 ] <- NA
  
  ## Filling new raster
  .clt <- setValues(x = .clt, value = .vref)
  
  ## Write to file
  writeRaster(.clt, .pth,
              format = "GTiff", overwrite = FALSE, progress = "text")
  
  return(TRUE)
}
