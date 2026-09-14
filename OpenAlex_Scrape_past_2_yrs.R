# HIMB publications from OpenAlex: recent update
#
# Searches the current and previous calendar years for HIMB-related
# publications. It saves:
#
# 1. The recent search results
# 2. An updated OpenAlex dataset combining the recent results with the
#    existing full historical OpenAlex dataset
#
# The script does not directly overwrite the master HIMB dataset.

readRenviron("~/.Renviron")

# Confirm that an OpenAlex API key is available.
# The actual key remains in .Renviron and is not stored in this script.
if (!nzchar(Sys.getenv("OPENALEX_API_KEY"))) {
  stop(
    paste(
      "No OpenAlex API key was found.",
      "Add OPENALEX_API_KEY to your .Renviron file and restart R."
    )
  )
}

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(tibble)

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------

UHM_INST_ID <- "I117965899"

OPENALEX_EMAIL <- Sys.getenv(
  "OPENALEX_EMAIL",
  unset = ""
)

CURRENT_YEAR <- as.integer(
  format(Sys.Date(), "%Y")
)

START_YEAR <- CURRENT_YEAR - 1L
END_DATE <- as.character(Sys.Date())

DATE_FILTER <- paste0(
  "from_publication_date:",
  START_YEAR,
  "-01-01,to_publication_date:",
  END_DATE
)

RECENT_OUTPUT_FILE <- "HIMB_OpenAlex_recent_2_years.csv"

EXISTING_OPENALEX_FILE <- "HIMB_OpenAlex_publications.csv"

UPDATED_OPENALEX_FILE <- "HIMB_OpenAlex_publications_updated.csv"

# ---------------------------------------------------------------------------
# HIMB search terms
# ---------------------------------------------------------------------------

himb_search_terms <- c(
  "Hawaii Institute of Marine Biology",
  "Hawai‘i Institute of Marine Biology",
  "Hawai'i Institute of Marine Biology",
  "HIMB",
  "Moku o Lo'e",
  "Moku o Loe",
  "Moku o Loʻe",
  "Kaneohe Bay",
  "Kāne‘ohe Bay",
  "Kane'ohe Bay",
  "Kane‘ohe Bay",
  "Kāne'ohe Bay",
  "Hawaii Marine Laboratory",
  "Hawai‘i Marine Laboratory"
)

# ---------------------------------------------------------------------------
# Text-processing functions
# ---------------------------------------------------------------------------

normalize_text <- function(x) {
  x <- ifelse(
    is.na(x),
    "",
    x
  )
  
  stringi::stri_trans_general(
    x,
    "Latin-ASCII"
  ) |>
    str_to_lower() |>
    str_squish()
}

# Normalize the search dictionary so capitalization and Hawaiian
# punctuation variants do not interfere with matching.

himb_detect_terms <- unique(
  normalize_text(
    c(
      "Hawaii Institute of Marine Biology",
      "HIMB",
      "Moku o Loe",
      "Hawaii Marine Laboratory",
      "Kaneohe Bay"
    )
  )
)

safe_chr <- function(x, default = "") {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    default
  } else {
    as.character(x[[1]])
  }
}

safe_num <- function(x, default = NA_real_) {
  value <- suppressWarnings(
    as.numeric(
      safe_chr(
        x,
        NA_character_
      )
    )
  )
  
  ifelse(
    length(value) == 0 || is.na(value),
    default,
    value
  )
}

openalex_id <- function(w) {
  str_remove(
    safe_chr(w$id),
    "^https://openalex.org/"
  )
}

reconstruct_abstract <- function(index) {
  if (
    is.null(index) ||
    length(index) == 0
  ) {
    return("")
  }
  
  positions <- lapply(
    index,
    function(x) {
      suppressWarnings(
        as.integer(
          unlist(x)
        )
      )
    }
  )
  
  all_positions <- unlist(
    positions,
    use.names = FALSE
  )
  
  all_positions <- all_positions[
    !is.na(all_positions)
  ]
  
  if (length(all_positions) == 0) {
    return("")
  }
  
  words <- rep(
    "",
    max(all_positions) + 1L
  )
  
  for (term in names(positions)) {
    pos <- positions[[term]]
    pos <- pos[!is.na(pos)]
    
    if (length(pos) > 0) {
      words[pos + 1L] <- term
    }
  }
  
  str_squish(
    paste(
      words,
      collapse = " "
    )
  )
}

# ---------------------------------------------------------------------------
# OpenAlex API functions
# ---------------------------------------------------------------------------

oa_get <- function(query, max_attempts = 8) {
  if (nzchar(OPENALEX_EMAIL)) {
    query$mailto <- OPENALEX_EMAIL
  }
  
  api_key <- Sys.getenv(
    "OPENALEX_API_KEY",
    unset = ""
  )
  
  if (nzchar(api_key)) {
    query$api_key <- api_key
  }
  
  for (attempt in seq_len(max_attempts)) {
    response <- GET(
      "https://api.openalex.org/works",
      query = query,
      timeout(60)
    )
    
    if (status_code(response) < 400) {
      return(
        fromJSON(
          content(
            response,
            "text",
            encoding = "UTF-8"
          ),
          simplifyVector = FALSE
        )
      )
    }
    
    if (attempt == max_attempts) {
      stop_for_status(response)
    }
    
    Sys.sleep(
      min(
        60,
        2^attempt
      )
    )
  }
}

fetch_openalex <- function(search = NULL, filter = NULL) {
  cursor <- "*"
  output <- list()
  
  repeat {
    query <- list(
      per_page = 100,
      cursor = cursor
    )
    
    if (!is.null(search)) {
      query$search <- search
    }
    
    if (!is.null(filter)) {
      query$filter <- filter
    }
    
    page <- oa_get(query)
    
    output <- append(
      output,
      page$results
    )
    
    cursor <- page$meta$next_cursor
    
    if (
      is.null(cursor) ||
      !nzchar(cursor)
    ) {
      break
    }
    
    Sys.sleep(0.1)
  }
  
  output
}

# ---------------------------------------------------------------------------
# HIMB matching functions
# ---------------------------------------------------------------------------

contains_term <- function(
    text,
    terms = himb_detect_terms
) {
  text <- normalize_text(text)
  
  terms[
    vapply(
      terms,
      function(term) {
        str_detect(
          text,
          fixed(term)
        )
      },
      logical(1)
    )
  ]
}

get_match_details <- function(w) {
  hits <- list()
  
  hits$title <- contains_term(
    safe_chr(w$title)
  )
  
  hits$abstract <- contains_term(
    reconstruct_abstract(
      w$abstract_inverted_index
    )
  )
  
  hits$venue <- contains_term(
    paste(
      safe_chr(
        w$primary_location$source$display_name
      ),
      safe_chr(
        w$biblio$journal_title
      )
    )
  )
  
  raw_affiliations <- character()
  institutions <- character()
  
  if (!is.null(w$authorships)) {
    raw_affiliations <- unlist(
      map(
        w$authorships,
        "raw_affiliation_strings"
      ),
      use.names = FALSE
    )
    
    institutions <- unlist(
      map(
        w$authorships,
        function(a) {
          if (is.null(a$institutions)) {
            return(character())
          }
          
          map_chr(
            a$institutions,
            ~ safe_chr(.x$display_name)
          )
        }
      ),
      use.names = FALSE
    )
  }
  
  hits$raw_affiliation <- contains_term(
    paste(
      raw_affiliations,
      collapse = " "
    )
  )
  
  hits$institution <- contains_term(
    paste(
      institutions,
      collapse = " "
    )
  )
  
  matched <- names(hits)[
    lengths(hits) > 0
  ]
  
  list(
    is_himb_related = length(matched) > 0,
    matched_fields = paste(
      matched,
      collapse = "; "
    ),
    matched_terms = paste(
      unique(
        unlist(
          hits,
          use.names = FALSE
        )
      ),
      collapse = "; "
    )
  )
}

# ---------------------------------------------------------------------------
# Citation and output-formatting functions
# ---------------------------------------------------------------------------

# OpenAlex supplies display names rather than reliably separated given and
# family names. This is therefore a best-effort conversion.

apa_person <- function(display_name) {
  name <- str_squish(display_name)
  
  if (!nzchar(name)) {
    return("")
  }
  
  parts <- str_split(
    name,
    "\\s+"
  )[[1]]
  
  if (length(parts) == 1) {
    return(parts)
  }
  
  family <- parts[length(parts)]
  
  initials <- paste0(
    str_sub(
      parts[-length(parts)],
      1,
      1
    ),
    ".",
    collapse = " "
  )
  
  paste0(
    family,
    ", ",
    initials
  )
}

apa_authors <- function(names) {
  names <- names[nzchar(names)]
  
  formatted <- map_chr(
    names,
    apa_person
  )
  
  n <- length(formatted)
  
  if (n == 0) {
    return("")
  }
  
  if (n == 1) {
    return(formatted)
  }
  
  if (n <= 20) {
    return(
      paste0(
        paste(
          formatted[-n],
          collapse = ", "
        ),
        ", & ",
        formatted[n]
      )
    )
  }
  
  paste0(
    paste(
      formatted[1:19],
      collapse = ", "
    ),
    ", ... ",
    formatted[n]
  )
}

page_range <- function(w) {
  first <- safe_chr(
    w$biblio$first_page
  )
  
  last <- safe_chr(
    w$biblio$last_page
  )
  
  if (!nzchar(first)) {
    return("")
  }
  
  if (
    nzchar(last) &&
    last != first
  ) {
    paste0(
      first,
      "–",
      last
    )
  } else {
    first
  }
}

build_apa_citation <- function(
    authors,
    year,
    title,
    journal,
    volume,
    issue,
    pages,
    doi,
    type
) {
  has_authors <- nzchar(authors)
  
  creator <- ifelse(
    has_authors,
    authors,
    title
  )
  
  date <- ifelse(
    is.na(year),
    "(n.d.).",
    paste0(
      "(",
      year,
      ")."
    )
  )
  
  # APA moves the title into the author position when no author is available.
  
  title_piece <- ifelse(
    has_authors && nzchar(title),
    paste0(
      title,
      "."
    ),
    ""
  )
  
  doi_url <- ifelse(
    nzchar(doi),
    paste0(
      "https://doi.org/",
      doi
    ),
    ""
  )
  
  if (
    type %in% c(
      "article",
      "review",
      "editorial",
      "letter",
      "erratum"
    ) &&
    nzchar(journal)
  ) {
    vol_issue <- if (nzchar(volume)) {
      paste0(
        volume,
        ifelse(
          nzchar(issue),
          paste0(
            "(",
            issue,
            ")"
          ),
          ""
        )
      )
    } else {
      ""
    }
    
    source <- paste(
      c(
        journal,
        vol_issue,
        pages
      )[
        nzchar(
          c(
            journal,
            vol_issue,
            pages
          )
        )
      ],
      collapse = ", "
    )
    
    return(
      str_squish(
        paste(
          creator,
          date,
          title_piece,
          paste0(
            source,
            "."
          ),
          doi_url
        )
      )
    )
  }
  
  str_squish(
    paste(
      creator,
      date,
      title_piece,
      doi_url
    )
  )
}

works_to_df <- function(works, details) {
  rows <- map2(
    works,
    details,
    function(w, match) {
      author_names <- if (is.null(w$authorships)) {
        character()
      } else {
        map_chr(
          w$authorships,
          ~ safe_chr(.x$author$display_name)
        )
      }
      
      doi <- str_remove(
        safe_chr(w$doi),
        "^https?://(dx\\.)?doi\\.org/"
      )
      
      journal <- safe_chr(
        w$primary_location$source$display_name
      )
      
      volume <- safe_chr(
        w$biblio$volume
      )
      
      issue <- safe_chr(
        w$biblio$issue
      )
      
      pages <- page_range(w)
      year <- safe_num(w$publication_year)
      type <- safe_chr(w$type, "other")
      authors_apa <- apa_authors(author_names)
      
      tibble(
        openalex_id = openalex_id(w),
        title = safe_chr(w$title),
        year = year,
        authors = paste(
          author_names,
          collapse = "; "
        ),
        authors_apa = authors_apa,
        journal = journal,
        volume = volume,
        issue = issue,
        pages = pages,
        doi = doi,
        type = type,
        matched_fields = match$matched_fields,
        matched_terms = match$matched_terms,
        confidence = case_when(
          str_detect(
            match$matched_fields,
            "raw_affiliation|institution"
          ) ~ "high",
          
          str_detect(
            match$matched_fields,
            "abstract"
          ) &
            str_detect(
              match$matched_fields,
              "title|venue"
            ) ~ "moderate",
          
          TRUE ~ "lower"
        ),
        apa_citation = build_apa_citation(
          authors_apa,
          year,
          safe_chr(w$title),
          journal,
          volume,
          issue,
          pages,
          doi,
          type
        ),
        source = "OpenAlex",
        retrieved_date = as.character(
          Sys.Date()
        )
      )
    }
  )
  
  bind_rows(rows) |>
    arrange(
      desc(year),
      title
    )
}

# ---------------------------------------------------------------------------
# Download recent candidate works
# ---------------------------------------------------------------------------

message(
  "Downloading UH Manoa works from ",
  START_YEAR,
  " through ",
  END_DATE,
  "..."
)

uhm_filter <- paste0(
  "institutions.id:",
  UHM_INST_ID,
  ",",
  DATE_FILTER
)

uhm_works <- fetch_openalex(
  filter = uhm_filter
)

message(
  "Downloaded ",
  length(uhm_works),
  " recent UH Manoa works."
)

# ---------------------------------------------------------------------------
# Run recent global HIMB searches
# ---------------------------------------------------------------------------

message(
  "Running recent HIMB-related metadata searches..."
)

search_works <- himb_search_terms |>
  map(
    \(term) {
      message(
        "Searching OpenAlex for: ",
        term
      )
      
      fetch_openalex(
        search = term,
        filter = DATE_FILTER
      )
    }
  ) |>
  flatten()

message(
  "Downloaded ",
  length(search_works),
  " records from the targeted searches before deduplication."
)

# ---------------------------------------------------------------------------
# Combine and deduplicate candidate records
# ---------------------------------------------------------------------------

candidates <- c(
  uhm_works,
  search_works
)

ids <- map_chr(
  candidates,
  openalex_id
)

candidates <- candidates[
  !duplicated(ids) &
    nzchar(ids)
]

message(
  "Checking ",
  length(candidates),
  " unique candidates for HIMB-related terms..."
)

# ---------------------------------------------------------------------------
# Identify HIMB-related records
# ---------------------------------------------------------------------------

match_details <- map(
  candidates,
  get_match_details
)

keep <- map_lgl(
  match_details,
  "is_himb_related"
)

himb_works <- candidates[keep]
himb_match_details <- match_details[keep]

himb_publications <- works_to_df(
  himb_works,
  himb_match_details
)

# ---------------------------------------------------------------------------
# Save the recent two-year results
# ---------------------------------------------------------------------------

write.csv(
  himb_publications,
  RECENT_OUTPUT_FILE,
  row.names = FALSE,
  na = ""
)

saveRDS(
  himb_publications,
  str_replace(
    RECENT_OUTPUT_FILE,
    "\\.csv$",
    ".rds"
  )
)

message(
  "Saved ",
  nrow(himb_publications),
  " recent HIMB-related publications to ",
  RECENT_OUTPUT_FILE,
  "."
)

# ---------------------------------------------------------------------------
# Combine recent results with the existing full OpenAlex dataset
# ---------------------------------------------------------------------------

if (file.exists(EXISTING_OPENALEX_FILE)) {
  message(
    "Combining the recent results with ",
    EXISTING_OPENALEX_FILE,
    "..."
  )
  
  existing_openalex <- read.csv(
    EXISTING_OPENALEX_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  existing_ids <- unique(
    existing_openalex$openalex_id[
      nzchar(existing_openalex$openalex_id)
    ]
  )
  
  new_record_count <- sum(
    !himb_publications$openalex_id %in% existing_ids
  )
  
  updated_openalex <- bind_rows(
    existing_openalex,
    himb_publications
  ) |>
    mutate(
      openalex_id = as.character(openalex_id),
      retrieved_date = as.character(retrieved_date)
    ) |>
    arrange(
      openalex_id,
      desc(retrieved_date)
    ) |>
    distinct(
      openalex_id,
      .keep_all = TRUE
    ) |>
    arrange(
      desc(year),
      title
    )
  
  write.csv(
    updated_openalex,
    UPDATED_OPENALEX_FILE,
    row.names = FALSE,
    na = ""
  )
  
  saveRDS(
    updated_openalex,
    str_replace(
      UPDATED_OPENALEX_FILE,
      "\\.csv$",
      ".rds"
    )
  )
  
  message(
    "Finished: found ",
    new_record_count,
    " OpenAlex records not present in the previous dataset."
  )
  
  message(
    "Saved ",
    nrow(updated_openalex),
    " total OpenAlex publications to ",
    UPDATED_OPENALEX_FILE,
    "."
  )
} else {
  warning(
    paste(
      EXISTING_OPENALEX_FILE,
      "was not found.",
      "The recent-results files were created,",
      "but an updated combined OpenAlex dataset was not created."
    )
  )
}