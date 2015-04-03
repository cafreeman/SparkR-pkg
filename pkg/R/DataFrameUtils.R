############################## DF Utility Functions ##################################
# A set of utility functions for using DataFrames with MLLib                         #
# Includes an overloaded model.matrix method for DataFrames, as well as supporting   #
# functions for manipulating DataFrames                                              #
######################################################################################

setGeneric("model.matrix",function(object, vars) { standardGeneric("model.matrix") })

setMethod("model.matrix",
          signature(object = "DataFrame", vars = "list"),
          function (object, vars) {
            # Identify all string fields in the formula and pull them out of the df
            stringVars <- lapply(Filter(function(x) x[[2]] == "string", dtypes(select(object, vars))),
                                 function(i) i[[1]])
            # Store non-categorical field names for creating the final dataframe
            otherVars <- setdiff(vars, stringVars)
            # Distinct values in each categorical field
            varLevels <- lapply(stringVars, function(var) {
              list(fieldName = var, levels = getLevels(object, var))
            })
            # Full list of column names for the resulting df
            allNames <- union(otherVars, unlist(lapply(varLevels, function(x) x$levels)))
            # Binary field creators for each categorical
            catDummies <- unlist(lapply(varLevels, function(level) {
              explodeField(level$fieldName, level$levels)  
            }), recursive = FALSE)
            explodedDF <- selectExpr(object, union(otherVars, catDummies))
            dfOut <- castAll(explodedDF, "double")
          })

getLevels <- function(df, field) {
  # Get distinct values for a single field
  levels <- sort(collect(distinct(select(df, field)))[[1]])
  # Drop the last field value (for k-1)
  levels[2:length(levels)]
}

# Explode categorical fields into one binary measure per distinct value
explodeField <- function(fieldName, levels) {
  lapply(levels, function(level) {
    paste("IF (", safeName(fieldName), " = \"", level, "\", 1, 0) AS ", safeName(level), sep = "")
  })
}

# Cast all fields of a DataFrame to a given data type
castAll <- function(df, type) {
  exprs <- lapply(names(df), function(name) {
    paste("CAST(", safeName(name), " AS ", type, ") AS ", safeName(name), sep = "")
  })
  selectExpr(df, exprs)
}

# Wrap a field name with backticks to ensure the SQL parser will interpret correctly
safeName <- function(name) {
  paste("`", name, "`", sep = "")
}
