library(tidyverse)

ethnologue <- read_tsv("/Users/skirgard/Nextcloud/Hedvigs_academia/2023/socities_strangers_paper/Complexity_rewrite/drive-download-20231030T163221Z-001/Table_of_Languages.tsv", show_col_types = F) %>% 
  dplyr::select(iso_639_3_code = ISO_639, Is_written_ethnologue = "Is_Written")

linguameta <- read_tsv("https://raw.githubusercontent.com/google-research/url-nlp/refs/heads/main/linguameta/linguameta.tsv", show_col_types = F) %>% 
#  mutate(writing_systems = str_split(writing_systems, pattern = ",")) %>% 
#  unnest(writing_systems) %>% 
#  mutate(writing_systems = trimws(writing_systems)) %>% 
  mutate(Is_written_linguameta = ifelse(is.na(writing_systems), F, T)) %>% 
  dplyr::select(iso_639_3_code, Is_written_linguameta, writing_systems )


linguameta$writing_systems %>% unique()
linguameta$Is_written_linguameta %>% table()

joined <- full_join(ethnologue, linguameta)

joined %>% 
  mutate(diff = ifelse(Is_written_linguameta == Is_written_ethnologue, "same", "diff")) %>% View()

2939 / (4376 + 2939)




library(jsonlite)

# Set the directory containing your JSON files
json_dir <- "../../../google-research/url-nlp/linguameta/data/"  
# List all JSON files in the directory
json_files <- list.files(json_dir, pattern = "\\.json$", full.names = TRUE)

# Function to read each JSON file safely
read_json_as_df <- function(file) {
  result <- tryCatch({
    df <- fromJSON(file, flatten = TRUE) |> as.data.frame()
    df$file_name <- basename(file)  # Optional: add file name as a column
    df
  }, error = function(e) {
    warning(paste("Failed to read", file, ":", e$message))
    NULL
  })
  return(result)
}

# Read all JSONs into a list of data frames
all_data <- lapply(json_files, read_json_as_df)

# Remove any NULL results from failed parses
all_data <- Filter(Negate(is.null), all_data)

# Combine using bind_rows (handles different columns)
combined_df <- bind_rows(all_data)
