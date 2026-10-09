# Internal constants of the package (not exported). They are evaluated once,
# when the package is built.

# Column types of the Retraction Watch CSV file, used by
# load_retraction_data(). Dates are kept as text and parsed during cleaning.
RETRACTION_DATA_TYPES <- cols(
  "Record ID" = col_integer(),
  "Title" = col_character(),
  "Subject" = col_character(),
  "Institution" = col_character(),
  "Journal" = col_character(),
  "Publisher" = col_character(),
  "Country" = col_character(),
  "Author" = col_character(),
  "URLS" = col_character(),
  "ArticleType" = col_character(),
  "RetractionDate" = col_character(),
  "RetractionDOI" = col_character(),
  "RetractionPubMedID" = col_character(),
  "OriginalPaperDate" = col_character(),
  "OriginalPaperDOI" = col_character(),
  "OriginalPaperPubMedID" = col_character(),
  "RetractionNature" = col_character(),
  "Reason" = col_character(),
  "Paywalled" = col_character(),
  "Notes" = col_character()
)


# Column types of the retraction reason groupings CSV file, used by
# load_retraction_data(). The `Count` column is not used, but is included for
# completeness.
RETRACTION_REASON_GROUPINGS_TYPES <- cols(
  Group = col_character(),
  Reason = col_character(),
  Count = col_integer()
)