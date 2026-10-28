# HIMB Publications and Contributions

This repository compiles publications and other research outputs associated with the Hawaiʻi Institute of Marine Biology (HIMB). It brings together historical HIMB Library records with publications identified through OpenAlex and ScholarSpace.

## Final publication list

**`HIMB_Master_deduplicated.csv` is the final compiled publication list.** Use this file to browse the bibliography or analyze publication years, authors, and record types.

This inventory is a work in progress. Some historical citations and HIMB associations may still require verification against original publications or archive records.

## What has already been done

The initial compilation and historical research have been completed:

- Reviewed and transcribed historical HIMB Contributions and other relevant archive records.
- Collected potential HIMB theses from multiple collections.
- Collected relevant publications from ScholarSpace.
- Ran the full historical OpenAlex search.
- Combined records from these sources into `HIMB_Master.csv`.
- Identified and reviewed duplicates across the combined records.
- Saved the final list as `HIMB_Master_deduplicated.csv`.

**There is no need to repeat the archive research, ScholarSpace collection, or full historical OpenAlex scrape for routine updates.**

## How to update the publication list

To add newly published works, rerun **`OpenAlex_Scrape_past_2_yrs.R`**. This is the script intended for ongoing updates; it searches the recent two-year period so newly published or recently indexed works can be added to the master dataset.

After an update, check the new records for relevance to HIMB and for overlap with publications already in the final list. Keep `HIMB_Master_deduplicated.csv` as the publication list used for analysis and sharing.

## Repository files

- **`HIMB_Master_deduplicated.csv`** — Final compiled publication list after duplicate removal.
- **`HIMB_Master.csv`** — Combined master dataset assembled from the original sources.
- **`Deduplicate_HIMB_Master.R`** — Script used during the initial compilation to identify and remove duplicates.
- **`HIMB_Master_duplicates_removed.csv`** — Records removed during deduplication.
- **`HIMB_Master_duplicate_groups_for_review.csv`** — Duplicate groups saved for review.
- **`HIMB_OpenAlex_publications.csv`** — Results of the completed historical OpenAlex search.
- **`HIMB_OpenAlex_recent_2_years.csv`** — Results of the recent two-year OpenAlex search.
- **`HIMB_OpenAlex_publications_updated.csv`** — Updated OpenAlex publication dataset.
- **`OpenAlex_Scrape.R`** — Script used for the completed full historical OpenAlex search.
- **`OpenAlex_Scrape_past_2_yrs.R`** — Script to rerun for ongoing publication updates.
- **`Scholarspace Scrape.R`** — Script used to collect ScholarSpace records during the initial compilation.
- **`probable_himb_theses_multiple_collections.csv`** — Candidate HIMB theses identified across multiple collections.
- **`HIMB Contribution List.Rproj`** — RStudio project file.
- **`.gitignore`** — Specifies local files and settings excluded from Git tracking.

## Fields in the final dataset

Each row in `HIMB_Master_deduplicated.csv` represents one publication or other research output.

- **`index`** — Row number from the master dataset. This is not a permanent publication ID.
- **`year`** — Publication or document year.
- **`authors`** — Author names.
- **`first_author_last`** — Last name of the first author.
- **`title`** — Publication or document title.
- **`notes`** — Journal information, citation details, report notes, or other context.
- **`type`** — Publication or document category.
- **`url_doi`** — URL or DOI, when available.
- **`citation`** — Formatted citation for the publication or document.
- **`Source`** — Dataset or archive from which the record was obtained.

Blank fields indicate information that has not been recorded or verified. The `notes` field contains different kinds of information depending on the source. Check citations against the original works before using them in a manuscript.

## Coverage and limitations

OpenAlex and ScholarSpace may not contain every historical publication, technical report, or thesis. A work that used HIMB facilities may lack an indexed HIMB affiliation, while a work about a nearby location may have no institutional connection to HIMB. Archive records can also have incomplete or uncertain citation details.

Verify individual citations against the original publication, DOI record, or archive document before citing them in a manuscript.
