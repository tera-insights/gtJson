setClass("data.list",
         representation(values = "list",
                       colnames = "character",
                       rownames = "character",
                       nrows = "numeric",
                       ncols = "numeric"),
         prototype(values = list(),
                   colnames = character(),
                   rownames = character(),
                   nrows = 0,
                   ncols = 0)
         )

data.list <- function(data) {
  x <- new("data.list")
  if (data$orientation == "column" || is.null(data$orientation))
    by.column = TRUE
  else
    by.column = FALSE
  has.row.names <- data$header[[1]] == "__row__"
  if (has.row.names) {
    x@rownames <- data$content[-1]
    data$header <- data$header[-1]
    x@colnames <- data$header
    data$content <- data$content[-1]
  }
  if (by.column) {
    x@nrows <- length(data$content)
    x@ncols <- length(data$content[[1]])
    x@values <- data$content
  } else {
    lapply(1:length(length(data$content)),
           function (n) {
             column = list()
             lapply(data$content,
                    function(row) {
                      current[[length(column) + 1]] <- row[[n]]
                    })
             x@values[[length(x@values) + 1]] <- column
           })
    x@nrows <- length(data$content[[1]])
    x@ncols <- length(data$content)
  }
  if (!has.row.names) {
    x@rownames <- as.character(1:x@nrows)
    x@colnames <- data$header
  }
  return(x)
}

## Recursively references across the arguments using the list structure of
## values
setMethod("[[", "data.list",
          function(x, i, j, ...) {
            args <- c(i, j, list(...))
            if (any(lapply(x, length) > 1))
              stop("Arguments must be vectors of length 1.")
            args <- lapply(args, function(k) {
              if (is.character(k)) {
                k
              } else if (is.numeric(k)) {
                if (k - as.integer(k) != 0)
                  warning("Argument ", k, " treated as an integer.")
                as.integer(k)
              } else {
                stop(k, " is neither numeric nor a character.")
              }
            })
            ret <- x
            for (k in args)
              ret <- ret[[k]]
            ret
          })

setMethod("[", c("data.list", "ANY", "ANY", "missing"),
          function(x, i, j, ..., drop = TRUE) {
            if (missing(j)) {
              if (missing(i))
                stop("Subset operation called with missing arguments.")

## setMethod("[", data.list,
##           function(x, ...) {
##             args = list(...)
##             lapply(args[1:2], function(i) {
##               if (is.character(i))
##                 i
##               else if (is.numeric(i))

## setMethod("[", "data.list",
##           function(x, i, j) {
##             if (i > x@nrows || j > x@ncols)
##               return(NULL)
##             if (nargs() == 1)
##               if (missing(x))
##                 stop("Function improperly called on non data.list object.")
##               else
##                 return(x)
##             else if (nargs() == 2)
##               if (missing(x))
##                 stop("Function improperly called on non data.list object.")
##               else if (missing(i))
##                 return(x)
##               else


            }
          }
          )
