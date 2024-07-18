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
    attrs <- setdiff(names(attributes(x)), c("class", "names"))
    cli::cli_h2("Attributes:")
    cli::cli_ul()
    for (attr_name in attrs) {
      if(attr_name == "Raw") {
        cli::cli_li("{.dbai Raw:}")
        if (length(attr(x, "Raw")) > 1) {
          cli::cli_ul()
          for (i in seq_along(attr(x, "Raw"))) {
            if(is.null(names(attr(x, "Raw"))[i])) {
              cli::cli_li("{.dbai [{i}]}")
            } else {
              cli::cli_li("{.dbai {names(attr(x, 'Raw'))[i]}:}")
            }


            print_raw_recursive(attr(x, "Raw")[[i]])
          }
          cli::cli_end()
          cli::cli_end()
        } else {
          print_raw_recursive(attr(x, "Raw"))
        }
      }
      else {
        attr_value <- attr(x, attr_name)

        if (length(attr_value) > 1) {
          cli::cli_li("{.dbai {attr_name}:}")
          cli::cli_ul()
          for (i in seq_along(attr_value)) {
            cli::cli_li("{.dbai [{i}]} {attr_value[i]}")
          }
          cli::cli_end()
          cli::cli_end()
        } else {
          cli::cli_li("{.dbai {attr_name}:} {paste(attr(x, attr_name))}")
        }
      }
    }
    cli::cli_end()
    cli::cli_h2("Values:")
  xchar <- as.character(x)
  print(xchar)
}

print_raw_recursive <- function(raw_list) {
  cli::cli_ul()
  for (name in names(raw_list)) {
    value <- raw_list[[name]]

    if (is.list(value)) {
      cli::cli_li("{.dbai {name}:}")
      print_raw_recursive(value)
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
