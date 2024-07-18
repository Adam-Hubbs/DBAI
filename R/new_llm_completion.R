#' new_llm_completion
#'
#' @param x The vector to be made into a new llm_completion object
#' @param Prompt The prompt used to generate the completion
#' @param Model The model used to generate the completion
#' @param Model_Provider The provider of the model used to generate the completion
#' @param Date The date the completion was generated
#' @param Temperature The temperature used to generate the completion
#' @param Raw A sample of the raw completion data
#'
#' @return A new llm_completion object
#' @export
new_llm_completion <- function(x = character(), Call = NULL, Prompt = NULL, Model = NULL, Model_Provider = NULL, Date = NULL, Temperature = NULL, Raw = NULL) {
  if(!is.character(x)) as.character(x)

  vctrs::new_vctr(x, Call = Call, Prompt = Prompt, Model = Model, Model_Provider = Model_Provider, Date = Date, Temperature = Temperature, Raw = Raw, class = "llm_completion_DBAI", inherit_base_type = TRUE)
}


#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.llm_completion_DBAI <- function(x, ...) {
  "llm_cmpln"
}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.llm_completion_DBAI <- function(x, ...) {
  paste0("llm_completion<", attr(x, "Model"), ">")
}


#' @export
vec_ptype2.llm_completion_DBAI.llm_completion_DBAI <- function(x, y, ...) {
  #Create a new Obj with what attributes
  n.Call <- unique(c(attr(x, "Call"), attr(y, "Call")))
  n.Prompt <- unique(c(attr(x, "Prompt"), attr(y, "Prompt")))
  n.Model <- unique(c(attr(x, "Model"), attr(y, "Model")))
  n.Model_Provider <- unique(c(attr(x, "Model_Provider"), attr(y, "Model_Provider")))
  n.Date <- unique(c(attr(x, "Date"), attr(y, "Date")))
  n.Temperature <- unique(c(attr(x, "Temperature"), attr(y, "Temperature")))
  vars <- list(n.Call, n.Prompt, n.Model, n.Model_Provider, n.Date, n.Temperature)
  if(any(lapply(vars, length) > 1)) {
    n.Raw = unique(list(attr(x, "Raw"), attr(y, "Raw")))
  } else {
    n.Raw = attr(x, "Raw")
  }
  new_llm_completion(Call = n.Call, Prompt = n.Prompt, Model = n.Model, Model_Provider = n.Model_Provider, Date = n.Date, Temperature = n.Temperature, Raw = n.Raw)
}

#' @export
vec_cast.llm_completion_DBAI.llm_completion_DBAI <- function(x, to, ...) {
  x
}

### Character
#' @export
vec_ptype2.llm_completion_DBAI.character <- function(x, y, ...) x

#' @export
vec_ptype2.character.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.character  <- function(x, to, ...) new_llm_completion(x)

#' @export
vec_cast.character.llm_completion_DBAI  <- function(x, to, ...) vctrs::vec_data(x)



### Integer
#' @export
vec_ptype2.llm_completion_DBAI.integer <- function(x, y, ...) {x}

#' @export
vec_ptype2.integer.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.integer  <- function(x, to, ...) new_llm_completion(vctrs::vec_data(x))

#' @export
vec_cast.integer.llm_completion_DBAI  <- function(x, to, ...) {
  vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to an integer, first convert it to a character vector with `as.character()`.")
}



### Double
#' @export
vec_ptype2.llm_completion_DBAI.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.double  <- function(x, to, ...) new_llm_completion(vctrs::vec_data(x))

#' @export
vec_cast.double.llm_completion_DBAI  <- function(x, to, ...) {
  vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to a double, first convert it to a character vector with `as.character()`.")
}



### Logical
#' @export
vec_ptype2.llm_completion_DBAI.logical <- function(x, y, ...) x

#' @export
vec_ptype2.logical.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.logical  <- function(x, to, ...) new_llm_completion(vctrs::vec_data(x))

#' @export
vec_cast.logical.llm_completion_DBAI  <- function(x, to, ...) {
  vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to a logical, first convert it to a character vector with `as.character()`.")
}



### Factor
#' @export
vec_ptype2.llm_completion_DBAI.factor <- function(x, y, ...) {
  message("Dropping Factor while coersing to llm_completion object")
  x
}

#' @export
vec_ptype2.factor.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.factor <- function(x, to, ...) {
  new_llm_completion(as.character(x))
}

#' @export
vec_cast.factor.llm_completion_DBAI <- function(x, to, ...) {
  vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to a factor, first convert it to a character vector with `as.character()`.")
}


#Do I need this or does the dispatch for casting work well enough?
#' @export
as_llm_completion <- function(x, ...) {
  vctrs::vec_cast(x, new_llm_completion(), ...)
}
