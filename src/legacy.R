# Functions for raster processing in DiCSM

# Raster --------------------------------------------------

#' **DEPRECATED**. Rasters to rasterlist
#' 
#' Load several raster into a list (rasterlist).
#'
#' @param .rt_src String of source: RGDAL ready connection or path to file.
#'
#' @return rasterlist
#' @export
#'
#' @examples
#' 
rast2rastlist <- function(.rt_src) {
  # Method
  .rasterlist <- filerast(.rt_src = .rt_src)
  
  return(.rasterlist)
}





#' **DEPRECATED**. Rasterlist to tibble
#'
#' @param .rasterlist  A list whose elements are rasters (e.g. from a foreach loop)
#'
#' @return tibble
#' @export
#'
#' @examples
rastlist2tbl <- function(.rasterlist) {
  # Method
  .tblraster <- tblraster(.rasterlist)
  
  return(.tblraster)
}


#' **DEPRECATED**. Rasterlist to column tibble
#'
#' @param .rasterlist  A list whose elements are rasters (e.g. from a foreach loop)
#'
#' @return tibble
#' @export
#'
#' @examples
rastlist2coltbl <- function(.rasterlist) {
  # Method
  .tblraster <- tblcolraster(.rasterlist)
  
  return(.tblraster)
}





#' **DEPRECATED** Getting the out path of tibble of bands
#'
#' @param .tbl_band tibble with bands to get out
#' @param .d date
#' @param .nm band number
#'
#' @return
#' @export
#'
#' @examples
#' 
band_out <- function(.tbl_band, .d = NULL, .nm, .type = "sdat") {
  .d <- enquo(.d)
  .type <- enquo(.type)
  
  if ( .nm < 10 ) { 
    .nm <- paste0("0", .nm)
  }
  if ( is.null(date) == FALSE ) {
    .tbl_band <- .tbl_band %>%
      filter(date == !! .d)
  }
  .tbl_band %>%
    filter(str_detect(.$band, glue('B{.nm}$') )) %>%
    select(!! .type) %>%
    flatten_chr()
}





#' Reading rasters from a PostGIS database
#'
#' @param .rt_src Character vector of PostGIS connections ready to be used in GDAL (Prepared by pgiscon)
#' @param .nm Vector with names of rasters
#'
#' @return List of Raster\* (Layers or Bricks) or a Raster\*
#' @export
#'
#' @examples
..pgisrast <- function(.rt_src, .nm = NULL) {
  # Method
  .rasterlist <- .pgisrast(.rt_src = .rt_src, .nm = .nm)
  
  # A Raster* is returned if list length is equal to 1
  if( length(.rasterlist) == 1 ) {
    .rasterlist <- .rasterlist[[1]]
  }
  return(.rasterlist)
}  






#' Reading rasters from file
#'
#' @param .rt_src Character vector of filepaths
#'
#' @return List of Raster\* (Layers or Bricks) or a Raster\*
#' @export
#'
#' @examples
..filerast <- function(.rt_src) {
  # Method
  .rasterlist <- .filerast(.rt_src = .rt_src)
  
  # A Raster* is returned if list length is equal to 1
  if( length(.rasterlist) == 1 ) {
    .rasterlist <- .rasterlist[[1]]
  }
  return(.rasterlist)
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
tblcolraster2 <- function(.rasterlist) {
  # Checks
  assertthat::assert_that(
    is_list(.rasterlist)
  )
  
  # Methods
  .tblraster <- .rasterlist %>%
    map(raster::as.data.frame) %>% # Convert each list element: raster ~ data frame
    bind_cols() %>% # Gathering DEM as different variable columns.
    as_tibble() %>%
    filter_at(1:ncol(.), all_vars( ! is.na(.) )) # Filter NA elements in every columns
  
  return(.tblraster)  
}







#' **DEPRECATED** Extract the file path of a band from a tibble of bands.
#'
#' @param .tbl_band Tibble with bands to extract
#' @param .b_num Band number
#' @param .b_var Variable name holding band numbers
#' @param .b_out Variable name of the band raster file path 
#' @param .dt Date
#' @param .dt_var Variable name holding dates
#'
#' @return Atomic vector with a file path to a raster of a band.
#' @export
#'
#' @examples
band_rast <- function(.tbl_band, .src, .b_num, .src_var = src, .b_var = band_id, .b_out = file, .dt = NULL, .dt_var = date) {
  .src <- enquo(.src)
  .src_var <- enquo(.src_var)
  .b_num <- enquo(.b_num)
  .b_var <- enquo(.b_var)
  .b_out <- enquo(.b_out)
  .dt_e <- enquo(.dt)
  .dt_var <- enquo(.dt_var)
  
  if ( is.null(.dt) == FALSE ) {
    .tbl_band <- .tbl_band %>%
      filter(!! .dt_var == !! .dt_e)
  }
  
  .tbl_band %>%
    filter(!! .src_var == !! .src, !! .b_var == !! .b_num ) %>%
    select(!! .b_out) %>%
    flatten_chr()
}






# Parallel ------------

#' **DEPRECATED** Paralleling any function which takes a matrix and 
#' gives a matrix as result (e.g. _cor_ or _cov_)
#'
#' @param mx 
#' @param nblocks 
#' @param func 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples 
#' matt <- matrix(1:72, ncol = 32, nrow = 200)
#' x <- bigmat_cor(matt, func = cor)
#' y <- bigmat_cor(matt, func = cor, method = "spearman")
#' z <- bigmat_cor(matt, func = cor, method = "kendall")

bigmat_fun <- function(mx, nblocks = NULL, func = NULL, ... ) {
  # CHECKS
  assertthat::assert_that(
    is.matrix(mx), 
    is.null(func) == FALSE,
    is.function(func)
  )
  
  if( is.null(nblocks) == FALSE )  {
    assertthat::assert_that( purrr::is_scalar_integer(nblocks) )
    if( ncol(mx) %% nblocks != 0 ) { stop("Choose different 'nblocks' so that ncol(x) %% nblocks = 0!") }
  }
  
  
  # SETTINGS
  ncolumn <- ncol(mx)
  
  # METHOD
  ## Allocation of matrix
  funmat <- matrix(ncol = ncolumn, nrow = ncolumn)
  
  ## Automatic selection of minimum number of blocks based on matrix column number.
  ## At least, block size should be longer than a pair of column
  if( is.null(nblocks) == TRUE )  {
    nblocks <- max( as.integer((ncolumn / 2L) - 1L), 1) # Minimum number of groups is 1.
    while( ncolumn %% nblocks != 0 ) {
      nblocks <- nblocks - 1L
    } 
  }
  
  ## Allocate variables to blocks
  blocks <- split(1:ncolumn, rep(1:nblocks, each = ncolumn/nblocks))
  
  ## Combinations of groups
  combs <- arrangements::combinations(1:nblocks, 2, replace = TRUE)
  
  ## Calculation
  res <- foreach( i = 1:nrow(combs) ) %dopar% {
    b1 <- blocks[[ combs[i, 1] ]]
    b2 <- blocks[[ combs[i, 2] ]]
    
    #if (verbose) cat("Block", comb[1], "with Block", comb[2], "\n")
    #flush.console()
    block_mat <- func(mx[, b1], mx[, b2], ...)
    
    block_res <- list("b1" = b1, "b2" = b2, "mat" = block_mat)
  }
  
  for( i in seq_along(res) ) {
    b1 <- res[[i]]$b1
    b2 <- res[[i]]$b2
    block_mat <- res[[i]]$mat
    
    funmat[b1, b2] <- block_mat
    funmat[b2, b1] <- t(block_mat)
  }
  
  rm(nblocks, b1, b2, block_mat)
  gc()
  return(funmat)
}
