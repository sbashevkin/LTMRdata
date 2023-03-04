writeRelational <- function(data, path) {

  lengthData <- ifelse(is.list(data), length(data), 1)

  pb <- txtProgressBar(min = 0, max = lengthData, style = 3, width = 50, "=")

  for (i in 1:lengthData) {
    nameData <- names(data)[i]

    write.csv(data[[i]], file.path(path, paste0(nameData, ".csv")),
              row.names = F)

    setTxtProgressBar(pb, i)
  }
  close(pb)
}
