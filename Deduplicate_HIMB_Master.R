library(readr)
library(dplyr)
library(stringr)
library(stringi)

INPUT_FILE <- "HIMB_Master.csv"

OUTPUT_FILE <- "HIMB_Master_deduplicated.csv"
REMOVED_FILE <- "HIMB_Master_duplicates_removed.csv"
REVIEW_FILE <- "HIMB_Master_duplicate_groups_for_review.csv"

# Read the master dataset
master <- read_csv(
  INPUT_FILE,
  show_col_types = FALSE,
  na = c("", "NA")
)

original_columns <- names(master)

# Confirm that the required columns exist
required_columns <- c(
  "year",
  "authors",
  "first_author_last",
  "title",
  "notes",
  "type"
)

missing_columns <- setdiff(required_columns, names(master))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Normalize text so capitalization, punctuation, accent marks, and
# extra spaces do not prevent duplicate titles from matching.
normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

master_prepared <- master |>
  mutate(
    original_row = row_number(),
    
    normalized_title = normalize_text(title),
    
    # Extract a four-digit year while preserving the original year column
    normalized_year = str_extract(
      ifelse(is.na(year), "", as.character(year)),
      "\\d{4}"
    ),
    
    # Count the populated fields in each row.
    # This helps retain the more complete version of a duplicate.
    completeness_score = rowSums(
      across(
        all_of(original_columns),
        ~ !is.na(.x) & str_squish(as.character(.x)) != ""
      )
    ),
    
    authors_length = nchar(ifelse(is.na(authors), "", authors)),
    notes_length = nchar(ifelse(is.na(notes), "", notes)),
    title_length = nchar(ifelse(is.na(title), "", title)),
    
    # Only use title + year when both are present.
    # Records without a usable year are kept as separate records.
    duplicate_key = if_else(
      normalized_title != "" & !is.na(normalized_year),
      paste(normalized_year, normalized_title, sep = " || "),
      paste0("UNMATCHED || ", original_row)
    )
  )

# Rank records within each duplicate group.
#
# Preference is given to:
# 1. The record with the most populated fields
# 2. The record with the most detailed notes
# 3. The record with the fullest author list
# 4. The record with the fullest title
# 5. The record appearing first in the original dataset
ranked_records <- master_prepared |>
  group_by(duplicate_key) |>
  arrange(
    desc(completeness_score),
    desc(notes_length),
    desc(authors_length),
    desc(title_length),
    original_row,
    .by_group = TRUE
  ) |>
  mutate(
    duplicate_group_size = n(),
    keep_record = row_number() == 1
  ) |>
  ungroup()

# Record which original row was retained for each group
kept_rows <- ranked_records |>
  filter(keep_record) |>
  transmute(
    duplicate_key,
    kept_original_row = original_row
  )

# Final deduplicated master dataset
master_deduplicated <- ranked_records |>
  filter(keep_record) |>
  arrange(original_row) |>
  select(all_of(original_columns))

# Records removed from the deduplicated dataset
duplicates_removed <- ranked_records |>
  filter(!keep_record) |>
  left_join(
    kept_rows,
    by = "duplicate_key"
  ) |>
  arrange(duplicate_key, original_row) |>
  select(
    original_row,
    kept_original_row,
    duplicate_group_size,
    all_of(original_columns),
    normalized_title,
    normalized_year
  )

# Every record belonging to a duplicate group, including the retained record
duplicate_groups_for_review <- ranked_records |>
  filter(
    duplicate_group_size > 1,
    !str_starts(duplicate_key, "UNMATCHED")
  ) |>
  left_join(
    kept_rows,
    by = "duplicate_key"
  ) |>
  arrange(duplicate_key, desc(keep_record), original_row) |>
  select(
    original_row,
    kept_original_row,
    keep_record,
    duplicate_group_size,
    all_of(original_columns),
    normalized_title,
    normalized_year
  )

# Safety check
stopifnot(
  nrow(master_deduplicated) + nrow(duplicates_removed) ==
    nrow(master)
)

# Save the results without overwriting the original master file
write_csv(
  master_deduplicated,
  OUTPUT_FILE,
  na = ""
)

write_csv(
  duplicates_removed,
  REMOVED_FILE,
  na = ""
)

write_csv(
  duplicate_groups_for_review,
  REVIEW_FILE,
  na = ""
)

message("Deduplication finished.")
message("Original records: ", nrow(master))
message("Deduplicated records: ", nrow(master_deduplicated))
message("Records removed: ", nrow(duplicates_removed))
message("Saved deduplicated master to: ", OUTPUT_FILE)
message("Saved removed records to: ", REMOVED_FILE)
message("Saved duplicate review file to: ", REVIEW_FILE)