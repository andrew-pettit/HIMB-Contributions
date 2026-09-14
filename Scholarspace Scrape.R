library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)

# Add all ScholarSpace collection UUIDs you want to search
collection_uuids <- c(
  "c719524e-b287-4dc3-aa0a-cce1ca823086", # Marine Biology MS
  "2e7f0b3b-04e3-44cd-ba0a-a831e96383d7", # Marine Biology PhD
  "c245d566-0be1-4d85-9e9c-9ab03749b42f", #NREM MS
  "61310095-f2c0-43cb-8d70-3f0542a05315", #NREM PhD
  "e4ea04ff-1153-433f-b342-21b36b5c6f9d", #Microbio Marine Bio MS
  "499c42e6-8aa4-4598-aaf2-8217a5af0467", #Microbio Marine PhD MS
  "6102f99f-860c-4f43-82ec-369d05fc62c1", #OCN MS
  "50f514da-006b-44fb-9737-dfc5ded4b172", #OCN PhD
  "6187cee6-b9e1-4ba2-a515-d065c2689005", #EECB Marine Bio MS
  "080bac03-4e20-4e0a-83ca-c6a086a9ef50", #Microbio MS
  "ee1375ad-67fd-485a-a384-929082c6dd6c", #Microbio PhD
  "0ce9acee-b246-4c72-b5d5-b50c9326fd97", #OCN Marine Bio MS
  "ddc96e3a-01d4-45d3-82bf-c964107f5c47", #OCN Marine Bio PhD
  "dde03732-0362-4405-b5f4-1fc39f21104b", #OCN Resource Eng. MS
  "d22df59f-03eb-40ae-b799-ccc41f0396a2", ##OCN Resource Eng. PhD
  "79273454-3fe6-4d2a-a37c-1e02a1d5c34a", # MS Zoology
  "f67ea182-d057-463b-8958-4ee152dc6802", #PhD Zoology
  "ee8fbf67-e383-43e1-a277-22581c553e6e", #MS Zoology EECB
  "a80d39d3-b232-4a57-9d87-c24a8266b7bd", #PhD Zoology EECB 
  "ec552969-a91f-44ef-a7c3-345e54d63b08", #MS Zoology Marine Bio
  "ca24ee24-031d-44f7-a480-822a97faf8b9", #PhD Zoology Marine Bio 
  "bff9fd96-9326-4b22-9792-47e5ddead574", #Entomology MS
  "15819b70-0ed4-4711-8673-cc64ab6bb187", #Entomology PhD
  "ef4ad09c-ba13-483e-8673-2850f7d00d56", #Botanical Science PhD
  "11c35460-e181-4bde-98af-aa82dcf77493", #Botanical Science EECB MS 
  "05ef071d-e001-4f39-9bfb-d94aea91933b", #Botanical Science EECB PhD
  "7d2b21cd-fd38-4338-9514-aeaadb35f820", #Botanical Science MS 
  "a77dead9-b6c4-426a-89ee-31bb914ab016", #Botanical Science PhD
  "943160d7-577e-4390-bd10-da1d3bcd4edd", #Botany MS 
  "582b4b95-8fc0-41ec-917e-b07c6e825997", #Botany PhD
  "99313c21-bb8f-4c85-b78e-a718aa15ed64", #Botany EECB MS 
  "b8b4cf0c-ac6d-4a7e-8f21-5cad2154be6f" #Botany EECB PhD
  
)

terms <- c(
  '"Hawaii Institute of Marine Biology"',
  "HIMB",
  '"Coconut Island"',
  '"Moku o Loʻe"',
  '"Moku o Loe"',
  '"Kaneohe Bay"',
  '"Kāne‘ohe Bay"', 
  "Hawaii Marine Laboratory", 
  "Hawai‘i Marine Laboratory"
)

get_meta <- function(item, field) {
  vals <- item$metadata[[field]]
  if (is.null(vals)) return(NA_character_)
  paste(vapply(vals, function(x) x$value, character(1)), collapse = "; ")
}

get_first_author_last <- function(authors) {
  if (is.na(authors) || authors == "") return(NA_character_)
  
  first_author <- str_split(authors, ";")[[1]][1]
  
  if (str_detect(first_author, ",")) {
    last_name <- str_split(first_author, ",")[[1]][1]
  } else {
    last_name <- tail(str_split(first_author, "\\s+")[[1]], 1)
  }
  
  str_trim(last_name)
}

search_term_page <- function(collection_uuid, term, page = 0, size = 100) {
  
  url <- "https://scholarspace.manoa.hawaii.edu/server/api/discover/search/objects"
  
  resp <- request(url) |>
    req_url_query(
      scope = collection_uuid,
      query = term,
      page = page,
      size = size
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
  
  objects <- resp$`_embedded`$searchResult$`_embedded`$objects
  
  if (is.null(objects) || length(objects) == 0) {
    return(tibble())
  }
  
  map_dfr(objects, function(obj) {
    
    item <- obj$`_embedded`$indexableObject
    
    authors <- get_meta(item, "dc.contributor.author")
    
    raw_year <- get_meta(item, "dc.date.issued")
    
    year <- stringr::str_extract(raw_year, "\\d{4}") |>
      as.numeric()
    
    tibble(
      collection_uuid = collection_uuid,
      item_uuid = item$uuid,
      authors = authors,
      first_author_last_name = get_first_author_last(authors),
      year = year,
      date_issued = raw_year,
      title = get_meta(item, "dc.title"),
      degree = get_meta(item, "dc.description.degree"),
      department = get_meta(item, "thesis.degree.discipline"),
      abstract = get_meta(item, "dc.description.abstract"),
      url = get_meta(item, "dc.identifier.uri"),
      himb_match_evidence = term
    )
  })
}

search_term_all_pages <- function(collection_uuid, term, size = 100) {
  
  page <- 0
  all_results <- list()
  
  repeat {
    cat("Collection:", collection_uuid, "| Searching:", term, "| Page:", page, "\n")
    
    result <- search_term_page(collection_uuid, term, page, size)
    
    if (nrow(result) == 0) break
    
    all_results[[length(all_results) + 1]] <- result
    page <- page + 1
  }
  
  bind_rows(all_results)
}

all_results <- map_dfr(collection_uuids, function(collection_uuid) {
  map_dfr(terms, function(term) {
    search_term_all_pages(collection_uuid, term)
  })
})

probable_himb <- all_results %>%
  group_by(item_uuid) %>%
  summarise(
    collection_uuids = paste(unique(collection_uuid), collapse = "; "),
    authors = first(na.omit(authors)),
    first_author_last_name = first(na.omit(first_author_last_name)),
    year = first(na.omit(year)),
    title = first(na.omit(title)),
    degree = first(na.omit(degree)),
    department = first(na.omit(department)),
    abstract = first(na.omit(abstract)),
    url = first(na.omit(url)),
    himb_match_evidence = paste(unique(himb_match_evidence), collapse = "; "),
    .groups = "drop"
  )

write_csv(probable_himb, "probable_himb_theses_multiple_collections.csv")

cat(
  "\nDone!\n",
  nrow(probable_himb),
  "unique probable HIMB-affiliated theses/dissertations found.\n"
  
)
 