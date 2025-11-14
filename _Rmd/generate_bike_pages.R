# Define the path to your template file
template_file <- "template.qmd"

# Define the output directory for the new QMD files
output_dir <- "generated_reports"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Read the template content
template_content <- readLines(template_file)

# Define the data for each report (e.g., a list of lists or a data frame)
reports_data <- list(
  list(report_title = "Sales Report Q1", topic_description = "quarterly sales performance", data_source = "Q1 sales data"),
  list(report_title = "Marketing Campaign Analysis", topic_description = "effectiveness of recent marketing campaigns", data_source = "marketing analytics"),
  list(report_title = "Product Feedback Summary", topic_description = "customer feedback for product X", data_source = "customer survey data")
)

# Loop through the data and create a QMD file for each report
for (i in seq_along(reports_data)) {
  current_data <- reports_data[[i]]
  
  # Replace placeholders in the template content
  new_content <- template_content
  new_content <- gsub("\\{\\{report_title\\}\\}", current_data$report_title, new_content)
  new_content <- gsub("\\{\\{topic_description\\}\\}", current_data$topic_description, new_content)
  new_content <- gsub("\\{\\{data_source\\}\\}", current_data$data_source, new_content)
  
  # Define the output filename
  output_filename <- file.path(output_dir, paste0(gsub(" ", "_", tolower(current_data$report_title)), ".qmd"))
  
  # Write the new QMD content to a file
  writeLines(new_content, output_filename)
  
  message(paste("Created:", output_filename))
}
