## Ships the export from R to the front-end for visualization
Visualize <- function(...) {
  args <- list(...)
  if (length(args) == 1) {
    result = export(args[[1]])
  } else {
    result = lapply(args, export)
    names(result) <- names(args)
  }
  if (is.null(names(result)))
    names(result) <- as.character(1:length(result))
  else
    for (i in 1:length(result))
      if (names(result)[[i]] == "")
        names(result)[[i]] = i
  result <- toJSON(result)
  ## print(result)
  temp <- tempfile("", ".", ".json")
  sink(temp)
  cat(result)
  sink()
  system2("grokit-cli", args = c("view", grokit.jobid, paste0(getwd(),"/",temp)))
  quit(save = "no")
}

## Converts a single R structure to GJSON format.
## Conversion is done on the basis of type, class, and length of input.
export <- function(item) {
  if (length(item) == 1
      && (is.numeric(item) || is.character(item)
          || is.null(item) || is.logical(item))) {
    return(item)
  } else if (is.matrix(item)) {
    return(list("__type__" = "matrix",
                "data" = as.vector(item),
                "n_rows" = nrow(item),
                "n_cols" = ncol(item)))
  } else if (is.data.frame(item)) {
      ## content = as.list(item)
      ## names(content) = NULL
      ## content = c(list(row.names(item)), content)

      ## Alin: we need to separate levels since this compresses data by a large factor
      ## it also speeds up the comptation in the front end.
      content = lapply(1:ncol(item), function(i){ return(as.numeric(item[,i])); })
      ## lev = lapply(1:ncol(item), function(i){ return(levels(item[,i])); })
      types <- lapply(item, function(x) {
        switch(typeof(x),
               double = list("__type__" = "float"),
               character = list("__type__" = "string"),
               integer = {
                   if (is.factor(x))
                     ## add a fake element to levels so we can start at 0 like GrokIt
                     list("__type__" = "factor", levels = c("NA",levels(x)))
                 else
                   list("__type__" = "integer")
             }
               )
      })
      names(types) <- NULL
      return(list("__type__" = "table",
                  "orientation" = "columns",
                  "header" = names(item), ##c("__row__", names(item)),
                  "types" = types,
                  "content" = content))
  } else if ((is.list(item) && !is.data.frame(item))
             || (is.vector(item) && !is.null(names(item)))) {
    return(lapply(item, export))
  } else if (is.vector(item)) {
    return(list("__type__" = "vector",
                "data" = item,
                "n_elem" = length(item)))
  } else {
    stop("Unknown data type: ", item)
  }
}
