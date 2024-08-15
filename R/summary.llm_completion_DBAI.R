#' Summary Method for llm_completion objects
#'
#' @param x The `llm_completion` object to be summarized.
#' @param ... optional. Other arguments to be passed to the print method.
#'
#' @return invisible
#' @export
summary.llm_completion_DBAI <- function(x, ...) {
  cli::cli_text("{.strong `llm_completion`} object:")
    cli::cli_div(theme = list(span.dbai = list(color = "blue", "font-weight" = "bold")))
    attribs <- attributes(x)
    attribs <- attribs[!names(attribs) %in% c("class", "names")]
    cli::cli_h2("Attributes:")
    cli::cli_ul()
    print_raw_recursive(attribs)
    cli::cli_end()
    cli::cli_h2("Values:")
    xchar <- as.character(x)
    n <- length(xchar)
    print(xchar[1:min(20, n)])
    if(n > 20) {
      cat("\n")
      cli::cli_text("<Showing rows 1-20 of {n}>")
    }
}

print_raw_recursive <- function(raw_list) {
  cli::cli_ul()
  for (name in names(raw_list)) {
    value <- raw_list[[name]]
    if (is.list(value)) {
      cli::cli_li("{.dbai {name}:}")
      if(length(names(value)) > 0) {
        print_raw_recursive(value)
      } else {
        cli::cli_ul()
        for(i in 1:length(value)) {
          cli::cli_li("{.dbai [{i}]:}")
          print_raw_recursive(value[[i]])
        }
        cli::cli_end()
      }
    } else if (is.data.frame(value)) {
      cli::cli_li("{.dbai {name}:} (data.frame with {nrow(value)} rows and {ncol(value)} columns)")
    } else if (is.vector(value) && length(value) > 1) {
      cli::cli_li("{.dbai {name}:} {paste(value, collapse = ', ')}")
    } else {
      cli::cli_li("{.dbai {name}:} {value}")
    }
  }
  cli::cli_end()
}
