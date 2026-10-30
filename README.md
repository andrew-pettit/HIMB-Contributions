# HIMB Publications and Contributions

This repository compiles publications and other research outputs associated with the Hawaiʻi Institute of Marine Biology (HIMB). It brings together historical HIMB Library records with publications identified through OpenAlex and ScholarSpace.

## Final publication list

**`HIMB_Master_CURRENT_YEAR.csv` is the final compiled publication list (e.g., HIMB_Master_2026.csv).** Use the most recent dated file to browse the bibliography or analyze publication years, authors, and record types.

## Data compilation and curation methods

- Reviewed and transcribed historical HIMB Contributions, technical reports, theses, and other relevant records from the HIMB library.
- Identified relevant theses across UH departments through ScholarSpace.
- Conducted a full historical OpenAlex search using HIMB-related keywords and publications associated with UH Mānoa.
- Combined records from these sources into `HIMB_Master.csv`.
- Identified and reviewed duplicates across the combined records.
- Saved the final list as `HIMB_Master_deduplicated.csv`.

**There is no need to repeat the archive research, ScholarSpace collection, or full historical OpenAlex scrape for routine updates.**

## How to update the publication list 

For routine updates, run `OpenAlex_Scrape_past_2_yrs.R` to identify recent publications. This searches OpenAlex for publications from January 1 of the previous calendar year through the date of execution. It saves the results to `HIMB_Master_OpenAlex_updated.csv` without overwriting the original master file. If more than two calendar years have elapsed since the last update, adjust START_YEAR in the script to cover the intervening years.

Review the newly added publications in `HIMB_OpenAlex_master_review.csv`. Confirm that new records are relevant to HIMB and make any necessary corrections. After reviewing the updated dataset, save it as `HIMB_Master.csv` for subsequent updates and deduplication.

OpenAlex may miss some theses and other repository records. For a more thorough update, run `Scholarspace Scrape.R`. Review the results and add any relevant records that are missing to `HIMB_Master.csv`.

After adding records, run `Deduplicate_HIMB_Master.R` to identify duplicates using normalized title and year. Review the duplicate-group and removed-record outputs, including whether removed records contain useful citation details. The resulting `HIMB_Master_deduplicated.csv` is the final publication list.

After reviewing the deduplication outputs and preserving any useful details from removed records, save a copy of the finalized `HIMB_Master_deduplicated.csv` as `HIMB_Master.csv`, replacing the previous
working master. This ensures that the next update starts from the latest curated dataset. Rename the remaining HIMB_Master_deduplicated.csv to HIMB_Master_CURRENT_YEAR.csv, replacing CURRENT_YEAR with the year of the update (e.g., HIMB_Master_2026.csv), to retain a dated copy of the final publication list.


## Repository files
- **`HIMB_Master_CURRENT_YEAR.csv`** — Dated final publication list (e.g., `HIMB_Master_2026.csv`).
- **`HIMB_Master_deduplicated.csv`** — Output of the deduplication script; reviewed and renamed to the dated final publication list.
- **`HIMB_Master.csv`** — Latest curated working master used as the input for future updates and deduplication.
- **`Deduplicate_HIMB_Master.R`** — Script that identifies and removes duplicates from `HIMB_Master.csv`.
- **`HIMB_Master_duplicates_removed.csv`** — Records removed during deduplication.
- **`HIMB_Master_duplicate_groups_for_review.csv`** — Duplicate groups saved for review.
- **`HIMB_OpenAlex_publications.csv`** — Results of the completed historical OpenAlex search.
- **`HIMB_OpenAlex_recent_2_years.csv`** — Results of the recent two-year OpenAlex search.
- **`HIMB_OpenAlex_publications_updated.csv`** — Combined historical and recent OpenAlex records; an intermediate output, not the final master list.
- **`HIMB_OpenAlex_master_review.csv`** — Log of added candidates, filled citations, uncertain matches, and alternative citations for review.
- **`HIMB_Master_OpenAlex_updated.csv`** — Proposed updated master containing recent OpenAlex candidates and citation backfills; requires review.
- **`OpenAlex_Scrape.R`** — Script used for the completed full historical OpenAlex search. NOTE: This script will take hours to run and may exhaust your daily OpenAlex usage. 
- **`OpenAlex_Scrape_past_2_yrs.R`** — Script to rerun for ongoing publication updates. NOTE: You will need to create an account and use your personal OpenAlex API key. 
- **`Scholarspace Scrape.R`** — Script used to collect ScholarSpace theses and dissertations.
- **`probable_himb_theses_multiple_collections.csv`** — Candidate HIMB theses identified across multiple collections.
- **`HIMB Contribution List.Rproj`** — RStudio project file.
- **`.gitignore`** — Specifies local files and settings excluded from Git tracking.

## Fields in the final dataset

Each row in `HIMB_Master_CURRENT_YEAR.csv` represents one publication or other research output.

- **`year`** — Publication or document year.
- **`authors`** — Author names.
- **`first_author_last`** — Last name of the first author.
- **`title`** — Publication or document title.
- **`notes`** — Additional details that vary by record, including journal or report information and, for some theses, the degree type (MS or PhD) and degree-granting institution.
- **`type`** — Publication or document category.
- **`url_doi`** — URL or DOI, when available.
- **`source`** — Where the record was obtained.
- **`apa_citation`** — Plain-text, best-effort APA citation, when available. Citations may be incomplete or require formatting corrections.

## Coverage and limitations

OpenAlex and ScholarSpace may not contain every historical publication, technical report, or thesis. Works that used HIMB facilities may be missed if their indexed metadata does not mention HIMB, an associated affiliation, or a relevant location. Archive records may also have incomplete or uncertain citation details.

Automated deduplication uses normalized title and year and may miss duplicates with different titles or missing years. It retains one record per matching group rather than combining information across records.

Verify individual citations against the original publication, DOI record, or archive document before citing them in a manuscript.
