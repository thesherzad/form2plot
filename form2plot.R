# Purpose: Get request, pull source dataset, generate R code (using AI), call
# tool (using AI) to plot it. Follow template and formats

get_form_rows <- function(form){
  form_cols <- length(form)
  # form_rows <- nrow(form)
  form_nms <- names(form)
  
  chat <- purrr::map_chr(1:form_cols, function(col){
    glue::glue(form_nms[col], ": ", unlist(form[1, form_nms[col]]), "\n")
  }) |> invisible()
  
  glue::glue_collapse(chat, sep = "\n")
}

form_to_chat <- function(form){
  stopifnot(is.data.frame(form))
  stopifnot(nrow(form) > 0)
  
  form_row <- purrr::map_chr(1:nrow(form), function(r){
    get_form_rows(form |> dplyr::slice(r))
  }) |> invisible()
  
  glue::glue_collapse(form_row, sep = "\n\n")
}

# save the source data temporarily for querying
# save_to_duckdb <- function(conn, df, df_nm){
#   duckdb::duckdb_register(conn, df_nm, df, experimental = FALSE)
# }

# create data description from the given data by user
get_prompt <- function(df_list){
  # Read the prompt file
  prompt_content <- readLines("assets/prompt.md", warn = FALSE)
  prompt_text <- paste(prompt_content, collapse = "\n")
  
  # DuckDB schema
  schema <- purrr::map2(df_list, names(df_list), function(df, nm){
    # save_to_duckdb(conn, df, nm)
    paste0(df_to_schema(df, name = nm), "\n\n")
  }) |> invisible()
  
  whisker::whisker.render(
    prompt_text,
    list(
      schema = schema
    )
  )
}


# read in ADaM datasets
df_list <- list(
  ADSL = pharmaverseadam::adsl,
  ADLB = pharmaverseadam::adlb,
  ADVS = pharmaverseadam::advs,
  ADAE = pharmaverseadam::adae
)

# step 1; prepare system prompt -------------------------------------------
## this step includes informing LLM to follow the template
source("R/functions.R")
sys_prompt <- get_prompt(df_list)

# define system prompt for better response
library(ellmer)

llm <- chat_openai(
  system_prompt = sys_prompt
)

# step 2; register tools --------------------------------------------------
save_request <- function(file_name, rcode){
  rstudioapi::documentNew(text = rcode)
}

get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}

get_r_version <- function(){
  R.version.string
}

llm$register_tool(tool(
  get_current_time,
  "Gets the current time in the given time zone.",
  tz = type_string(
    "The time zone to get the current time in. Defaults to `\"UTC\"`.",
    required = FALSE
  )
))

llm$register_tool(tool(
  get_r_version,
  "Gets the installed R version."
))

llm$register_tool(tool(
  save_request,
  "Saves the R code for every plot separately into a new document.",
  file_name = type_string(
    "A file name. This name should be derived from the graph ID, title, group variable (if any).",
    required = FALSE
  ),
  rcode = type_string(
    "R code to generate the graphs(s) for a single request.",
    required = FALSE
  )
))

# step 3; read and prep request form --------------------------------------
form <- readxl::read_excel("assets/Graph_Request_Form.xlsx") #|> dplyr::slice(4)
graph_instruction <- form_to_chat(form)
graph_instruction

# send request / get response
llm$chat(
  glue::glue(
    "Create graph for below request(s): \n",
    graph_instruction
  )
)

