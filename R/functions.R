df_to_schema <- function(
    df,
    name = deparse(substitute(df)),
    categorical_threshold = 10
) {
  schema <- c(paste("Table:", name), "Columns:")
  
  column_info <- lapply(names(df), function(column) {
    # Map R classes to SQL-like types
    sql_type <- if (is.integer(df[[column]])) {
      "INTEGER"
    } else if (is.numeric(df[[column]])) {
      "FLOAT"
    } else if (is.logical(df[[column]])) {
      "BOOLEAN"
    } else if (inherits(df[[column]], "POSIXt")) {
      "DATETIME"
    } else {
      "TEXT"
    }
    
    info <- paste0("- ", column, " (", sql_type, ")")
    
    # For TEXT columns, check if they're categorical
    if (sql_type == "TEXT") {
      unique_values <- length(unique(df[[column]]))
      if (unique_values <= categorical_threshold) {
        categories <- unique(df[[column]])
        categories_str <- paste0("'", categories, "'", collapse = ", ")
        info <- c(info, paste0("  Categorical values: ", categories_str))
      }
    } else if (sql_type %in% c("INTEGER", "FLOAT", "DATETIME")) {
      rng <- range(df[[column]], na.rm = TRUE)
      if (all(is.na(rng))) {
        info <- c(info, "  Range: NULL to NULL")
      } else {
        info <- c(info, paste0("  Range: ", rng[1], " to ", rng[2]))
      }
    }
    return(info)
  })
  
  schema <- c(schema, unlist(column_info))
  return(paste(schema, collapse = "\n"))
}
