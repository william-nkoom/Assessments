# load essential packages

pacman::p_load(devtools,
               roxygen2,
               tidyverse,
               dplyr,
               rio
               )

#' Function to generate dummy variables for categorical variables

#' This function generates dummy variables where levels of the categories are
#' encoded 0 or 1.
#' For example, for a dichotomic variable like gender, males may be encoded 0
#' and females encoded 1.
#' For a categorical variable of more than 2 levels, all levels are encoded 0,
#' where the label for that level is false, and encoded 1 if the the label
#' is true for an observation.
#' For example educational level with say high school, bachelor and masters
#' as the levels, a person with masters may be encoded 1 but 0 high school
#' and bachelor in the dataset. Where an observation is is encoded 0 for
#' both high school and bachelor, it is implied that the person has masters,
#' in which case this scenario can consider masters as the reference point.

# Detailed description of parameters

#' @aliases generate_dummies
#' @param data data frame containing the variables to be encoded
#' @param variables character vector
#' specifying levels of the categorical variables
#' @param categories codes 1 or 0, to reflect the codes of the levels
#' of categories
#' @param labels optional list of labels for the categories of each variable
#' @param reference reference level indicating  where all other levels of the
#' categorical data are encoded 0
#' @param keep optional logical vector indicating whether to retain
#' the original character variables in the output data frame
#' Default is `True`for all variables
#' @return data frame with new categorical variables added
#' specify the codes assigned to  each level of the categorical variable.
#'  Each new column is named as `variable category`, where `variable`
#'  is the name of the original categorical variable
#' @export
#' @importFrom dplyr any_of
#' @importFrom magrittr %>%
generate_dummies <- function(data, variables, reference_level = NULL) {
  # Add a temporary id column for merging data sets later
  if (!"function_temp_id" %in% names(data)) {
    data$function_temp_id <- seq(1, nrow(data))
  } else {
    stop("There is already a variable called function_temp_id in the data set.
         Please rename it.")
  }
  # check reference_level if provided has the same length as variables
  if (!is.null(reference_level) && length(reference_level) != length(variables))
    {
    stop("The length of 'reference_level' must match the length of 'variables'
         if provided.")
  }
  # Loop over each variable
  for (i in seq_along(variables)) {
    var_name <- variables[i]
    # Check if the variable exists in the data set
    if (!var_name %in% names(data)) {
      stop(paste("Variable", var_name, "not found in the data set."))
    }

    # check if variable is categorical
    if (!is.factor(var) && is.character(var)) {
      stop(paste(var_name, "is not a category variable."))
    }

    # check if reference level is a category in the specified variable
    if (!(reference_level %in% levels(data[[var_name]]))) {
      stop(paste("Reference level", reference_level, "not in", var_name))
    }

    # Create dummy variables for each level, excluding the reference level
    # if provided
    levels_to_include <- if (!is.null(reference_level)) {
      levels(data[[var_name]])[levels(data[[var_name]]) != reference_level[i]]
    } else {
      levels(data[[var_name]])
    }
    # Generate dummy variables
    dummies <- model.matrix(~ function_temp_id + data[[var_name]] - 1,
                            data = data)
    colnames(dummies)[-1] <- paste(var_name, levels(data[[var_name]]),
                                   sep = "_")
    # Remove reference level column if specified
    if (!is.null(reference_level)) {
      dummies <- dummies[, !colnames(dummies) %in%
                           paste(var_name,reference_level[i], sep = "_")]
    }
    # Add dummies to the data frame
    data <- merge(data, dummies, by = "function_temp_id")
  }
  # Remove temporary id column
  data <- data %>% select(-function_temp_id)
  # Return the modified data set with dummy variables
  return(data)
}

# Example use of the function

# set random seed for reproducibility
set.seed(123)
# generate toy data set
toy_data <- data.frame(
  id = seq(1, 15),
  gender = factor(sample(c("M", "F"), 15, replace = TRUE)),
  age = factor(sample(c("20-35", "36-49", "50-65", ">66"), 15, replace = TRUE))
)
# display the data
toy_data
generate_dummies(data = toy_data, variables = "age", reference_level = "20-35")
#' export(generate_dummies)

# test 1 produced error


# test 2 produced error


