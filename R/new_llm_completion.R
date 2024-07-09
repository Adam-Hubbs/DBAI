#' new_llm_completion
#'
#' @param x The vector to be made into a new llm_completion object
#' @param Prompt
#' @param Model
#' @param Model_Provider
#' @param Date
#' @param Temperature
#' @param Raw
#'
#' @return A new llm_completion object
#' @export
new_llm_completion <- function(x = character(), Call = NULL, Prompt = NULL, Model = NULL, Model_Provider = NULL, Date = NULL, Temperature = NULL, Raw = NULL) {
  if(!is.character(x)) as.character(x)

  new_vctr(x, Call = Call, Prompt = Prompt, Model = Model, Model_Provider = Model_Provider, Date = Date, Temperature = Temperature, Raw = Raw, class = "llm_completion_DBAI", inherit_base_type = TRUE)
}


#Adding a error vector metadata? rcrd type? Or just int with wich row the error was on. Reconfigure repair mode to use this instead? OR Reconfigure repair mode to use vector version of llm_generate and combine using mutate to only replace NA's?

#' @export
vec_ptype_abbr.llm_completion_DBAI <- function(x, ...) {
  "llm_cmpln"
}

#' @export
vec_ptype_full.llm_completion_DBAI <- function(x, ...) {
  paste0("llm_completion<", attr(x, "Model"), ">")
}

#' @export
vec_ptype2.llm_completion_DBAI.llm_completion_DBAI <- function(x, y, ...) {
  #Create a new Obj with what attributes
  n.Call = unique(c(attr(x, "Call"), attr(y, "Call")))
  n.Prompt = unique(c(attr(x, "Prompt"), attr(y, "Prompt")))
  n.Model = unique(c(attr(x, "Model"), attr(y, "Model")))
  n.Model_Provider = unique(c(attr(x, "Model_Provider"), attr(y, "Model_Provider")))
  n.Date = unique(c(attr(x, "Date"), attr(y, "Date")))
  n.Temperature = unique(c(attr(x, "Temperature"), attr(y, "Temperature")))
  n.Raw = unique(list(attr(x, "Raw"), attr(y, "Raw")))

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
vec_cast.llm_completion_DBAI.character  <- function(x, to, ...) new_llm_completion(vec_data(x))

#' @export
vec_cast.character.llm_completion_DBAI  <- function(x, to, ...) vec_data(x)

### Integer
#' @export
vec_ptype2.llm_completion_DBAI.integer <- function(x, y, ...) x

#' @export
vec_ptype2.integer.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.integer  <- function(x, to, ...) new_llm_completion(vec_data(x))

#' @export
vec_cast.integer.llm_completion_DBAI  <- function(x, to, ...) {
  stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to an integer, first convert it to a character vector with `as.character()`.")
}


### Double
#' @export
vec_ptype2.llm_completion_DBAI.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.double  <- function(x, to, ...) new_llm_completion(vec_data(x))

#' @export
vec_cast.double.llm_completion_DBAI  <- function(x, to, ...) {
  stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to a double, first convert it to a character vector with `as.character()`.")
}

### Logical
#' @export
vec_ptype2.llm_completion_DBAI.logical <- function(x, y, ...) x

#' @export
vec_ptype2.logical.llm_completion_DBAI <- function(x, y, ...) y

#' @export
vec_cast.llm_completion_DBAI.logical  <- function(x, to, ...) new_llm_completion(vec_data(x))

#' @export
vec_cast.logical.llm_completion_DBAI  <- function(x, to, ...) {
  stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to a logical, first convert it to a character vector with `as.character()`.")
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
  stop_incompatible_cast(x, to, x_arg = "", to_arg = "", message = "If you want to convert a llm_completion object to a factor, first convert it to a character vector with `as.character()`.")
}


re <- vec_c(new_llm_completion("Hello", Model = "GPT"), factor("Hey"))


### Factor
### Throw error if attempt to cast factor to llm_completion
#Implement coersion using ptype


vec_ptype_show(new_llm_completion("Hello"), double(), character(), integer(), logical(), factor())

vctrs:::stop_lossy_cast
vec_ptype_show
stop_incompatible_type
stop_incompatible_cast
vctrs:::stop_incompatible
vctrs:::stop_vctrs
rlang:::abort
rlang:::cnd_message_info
rlang:::error_cnd
vctrs:::cnd_type_message

ord <- factor(c("low", "medium", "high", "low", "high"))
ord

cha <- c("low", "medium", "flarb")

newc <- c(ord, cha)
newc
str(newc)
typeof(newc)
attributes(newc)

newc2 <- c(cha, ord)
newc2
str(newc2)
typeof(newc2)
attributes(newc2)

newc3 <- c(cha, as.character(ord))
newc3
str(newc3)
typeof(newc3)
attributes(newc3)

newc4 <- c(as.factor(cha), ord)
newc4
str(newc4)
typeof(newc4)
attributes(newc4)
as.factor
as.character.factor
factor

#Character and Factor -> character
#Call as.factor on the character vector





#' @export
print.llm_completion_DBAI <- function(x, ...) {
  xchar <- as.character(x)
  print(xchar, ...)
}

#' @export
as_llm_completion <- function(x, ...) {
  vec_cast(x, new_llm_completion(), ...)
}
