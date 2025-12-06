library(quarto)
library(tidyverse)
library(glue)


template_path = "final_template.qmd" 
output_dir    = "reports/neighborhoods" 


data_path = "D:/MSCA/PA446/final/Chicago_Education_Analysis_446Final/data/finaldata"
finaldata = read.csv(data_path)


community_list = finaldata %>%
  pull(community_area) %>%
  unique() %>%
  str_to_upper() %>%
  sort() %>%



render_report = function(community) {
  
  message(paste("Processing:", community, "..."))
  safe_name = str_replace_all(community, "[^a-zA-Z0-9]", "_")
  file_name = glue("Report_{safe_name}.html")
  
  tryCatch({
    quarto_render(
      input = template_path,
      execute_params = list(community_name = community),
      
      output_file = file_name,
      output_dir = output_dir,
      quiet = TRUE 
    )
    message(paste("success:", file_name))
    
  }, error = function(e) {
    message(paste("Error generating report for:", community))
    message(e)
  })
}

walk(community_list, render_report)
