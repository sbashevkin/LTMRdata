# Function to connect from R to Access via the ODBC drivers ---------------

architectureCheck <- function(officeBit = NULL) {

  # What architecture of R are you on?
  rBit <- ifelse((.Machine$sizeof.pointer == 4), "x32", "x64")

  # Do you have 32 bit or 64 bit office installed
  # Can attempt to read this from the registry itself;
  # if unsuccessful, the user must specify
  if (is.null(officeBit)) {
    if (rBit == "x64") {
      fp <- file.path("SOFTWARE", "Microsoft", "Office",
                      "ClickToRun", "Configuration",
                      fsep = "\\")
      subkey <- "Platform"
    } else {
      fp <- file.path("SOFTWARE", "Microsoft", "Office", "16.0", "Outlook",
                      fsep = "\\")
      subkey <- "Bitness"
    }

    officeBit <- tryCatch(readRegistry(fp)[[subkey]],
                          error = function(cond) {
                            ifelse(grepl("not found", cond$message),
                                   stop("Cannot automatically detect the architecture of your Microsoft Office. Please fill in `x32` or `x64` manually in the `officeBit` argument.", call. = F),
                                   stop(cond))
                          })
    officeBit <- ifelse((officeBit != "x64"), "x32", "x64")
  }

  # Are they the same?
  if (officeBit != rBit) {
    # First case = in 64bit R but have only 32bit office. Here, will have to use the terminal
    if (rBit == "x64" & officeBit == "x32") {
      # Check to see if a 32 bit R is installed
      if (!file.exists(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"))) {
        stop("A 32-bit R could not be found on this machine and must be installed.", call. = F)
      }
    }
  }

  check <- ifelse(rBit == officeBit, T, F)

  list(check = check,
       rBit = rBit,
       officeBit = officeBit)
}

bridgeAccess <- function(file, tables = "check",
                         script = "connectAccess.R",
                         out = tempdir()) {

  # Does the file exist? Do you need to be on your company network?
  if (!file.exists(file)) stop("Database file path not found. Did you specify right? Are you on VPN?", call. = F)

  # Do you need to specify where the connection script is?
  if (!file.exists(script)) stop("The `connectAccess.R` script cannot be found. Specify full path to the script.", call. = F)

  # First, check architecture. If ok then just source the script; if not then invoke system2
  bitCheck <- architectureCheck()

  if (isTRUE(bitCheck$check)) {
    Args <<- c(file, out, tables)

    source(file = script)

    con <- connectAccess(file)

    extractTables(con = con,
                  tables = tables,
                  out = out,
                  rBit = bitCheck$rBit,
                  officeBit = bitCheck$officeBit)
  } else {
    file <- shQuote(normalizePath(file, winslash = "\\"))
    script <- shQuote(normalizePath(script, winslash = "\\"))
    tables <- shQuote(tables)

    terminalOutput <- system2(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"),
                              args = c(script,
                                       file, out,
                                       bitCheck, tables))

    readRDS(file.path(out, "savedAccessTables.rds"))
  }
}


