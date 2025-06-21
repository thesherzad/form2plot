# Automate creating graphs using LLM
Inform LLM about your context => Get graph request form => Generate R code using LLM to create graphs

This project uses `ellmer` R package to interact with LLM. Please follow its documentation for setting up an API Key, if you haven't already.

I used `df_to_schema` function from `querychat` package to create a good context for LLM about datasets.
