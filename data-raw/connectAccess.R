connectAccess <- function(file,
                          driver = "Microsoft Access Driver (*.mdb, *.accdb)", uid = "", pwd = "", ...) {

  file <- normalizePath(file, winslash = "\\")

  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={", driver,
                "};Dbq=", file,
                ";Uid=", uid,
                ";Pwd=", pwd,
                ";")

  tryCatch(DBI::dbConnect(drv = odbc::odbc(), .connection_string = dbString),
                  error = function(cond) {
                    if (all(stringr::str_detect(cond$message, c("IM002", "ODBC Driver Manager")))) {
                      message(cond, "\n")
                      message("IM002 and ODBC Driver Manager error generally means a 32-bit R needs to be installed or used.")
                    } else {
                      message(cond)
                    }
                  })
  # RODBC::odbcDriverConnect(con, ...)
}

extractTables <- function(con, tables, out) {

  # Pulling just the table names
  # tableNames <- RODBC::sqlTables(con, tableType = c("TABLE", "VIEW"))["TABLE_NAME"]
  tableNames <- odbc::dbListTables(conn = con)

  # Includes system tables which cannot be read, excluding them below with negate
  # tableNames <- stringr::str_subset(tableNames, "MSys", negate = T)
  if (length(tables) == 1 & all(tables %in% "check")) {
    # If no table names are specified, then simply return the names of the possible databases for the user to pic

    # RODBC::odbcClose(con)
    DBI::dbDisconnect(con)

    cat("Specify at least one table to pull from: \n")

    return(print(tableNames))
  }

  # Apply the dbReadTable to each readable table in db
  # returnedTables <- mapply(RODBC::sqlQuery,
  #                          query = paste("SELECT * FROM", tables),
  #                          MoreArgs = list(channel = con),
  #                          SIMPLIFY = F)
  returnedTables <- mapply(DBI::dbReadTable,
                       name = tables,
                       MoreArgs = list(conn = con),
                       SIMPLIFY = F)

  # names(returnedTables) <- tables

  DBI::dbDisconnect(con)
  # RODBC::odbcClose(con)

  if (rBit == "x64" & officeBit == "x32") {
    # Save the table to be read back into R
    saveRDS(returnedTables, file = file.path(out, "savedAccessTables.rds"))
  } else {
    returnedTables
  }
}

# Connecting --------------------------------------------------------------

# Pull out arguments from the command line for use in the following functions
if (!exists("Args")) {
  Args <- commandArgs(T)

  file <- Args[1]
  out <- Args[2]
  tables <- Args[3:length(Args)]

  con <- connectAccess(file)

  extractTables(con = con,
                tables = tables,
                out = out)
}
