## Bring the import from datapath JSON output to R
Build <- function(...) {
  args <- list(...)
  if (length(args) == 1) {
    result = import(args[[1]])
  } else {
    result = lapply(args, import)
    if (is.null(names(result)))
      names(result) <- as.character(1:length(result))
    else
      for (i in 1:length(result))
        if (names(result)[[i]] == "")
          names(result)[[i]] = i
  }
  result
}

## Converts a single datapath JSON output to an R structure.
import <- function(item) {
  if (!is.list(item)) {
    type <- "NULL"
  } else {
    type <- item[["__type__"]]
    if (is.null(type))
      type <- "NULL"
  }
  switch(type,
         "table" = {
           temp <- list(content = lapply(item$content, import),
                        header = item$header)
           return(data.list(temp))
         },
         "matrix" = {
           return(t(matrix(item$data, ncol = item$n_rows)))
         },
         "vector" = {
           return(item$data)
         },
         "NULL" = {
           if (is.list(item) || (is.vector(item) && !is.null(names(item))))
             return(lapply(item, import))
           else if (is.vector(item)) ## basic type check
             return(item)
         },
         stop("Unexpected type: ", type, "\nitem:", item)
       )
}
