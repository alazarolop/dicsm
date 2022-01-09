## @knitr database

# Functions ----

#' Open a connection to a PostgreSQL database using RPostgres package
#'
#' @param .db 
#'
#' @return A RPostgres open connection
#' @export
#'
#' @examples
pg_con <- function(.db) {
  assertthat::is.string(.db)
  RPostgres::dbConnect(drv = RPostgres::Postgres(),
                       dbname = .db, host = "localhost", port = 5432, 
                       user = keyring::key_list("psql-su")[1,2], password = keyring::key_get("psql-su"), 
                       bigint = "numeric")
}


#' Open a connection to a PostgreSQL database using DBI package
#'
#' @param .db 
#'
#' @return A ODBC open connection
#' @export
#'
#' @examples
pg_con_odbc <- function(.db) {
  assertthat::is.string(.db)
  DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Driver", 
                 database = .db, host = "localhost", port = 5432,
                 UID = keyring::key_list("psql-su")[1,2], PWD = keyring::key_get("psql-su"),  
                 bigint = "numeric")
}



#' String of a PostGIS connection to be used with GDAL.
#'
#' @param .db Database that holds the schmea (vector o scalar)
#'
#' @return A string with a basic formatted PostgreSQL connection
#' @export
#'
#' @examples
pg_str <- function(.db) {
  # Checks
  assertthat::assert_that( 
    purrr::is_character(.db),
    purrr::is_scalar_character(.db)
  )
  
  # Method
  glue::glue("host='localhost' port=5432",
             "dbname='{.db}'",
             "user='{keyring::key_list(\"psql-su\")[1,2]}' password='{keyring::key_get(\"psql-su\")}'",
             .sep = " ")
  
}


#' Execute SQL files into a db
#'
#' @param db 
#' @param sql file with SQL code
#'
#' @return Side effect in the db
#' @export
#'
#' @examples
pg_exec <- function(db = "dicsm", sql) {
  assertthat::assert_that( 
    purrr::is_scalar_character(db),
    purrr::is_character(sql),
    all(fs::is_absolute_path(sql))
  )
  for (i in sql) {
    glue("psql -d {db} -U {keyring::key_list('psql-su')[1,2]} -f {i}") %>% 
      system()
  }
}





# Execution ----
con <- pg_con("dicsm")
con_odbc <- pg_con_odbc("dicsm")


library(RPostgres)

