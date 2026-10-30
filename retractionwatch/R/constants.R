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

# Country classes, ordered by precedence: a paper takes the first class that
# any of its countries falls in (see add_paper_country_class())
COUNTRY_CLASSES <- c("United Kingdom", "Other OECD", "Rest of World")

# Values of the Retraction Watch `Country` column that do not name a country.
# They are treated as missing (see split_by_country())
UNKNOWN_COUNTRIES <- c("Unknown", "U")

# SCImago country names recoded to the names used by Retraction Watch, so
# both sources can be joined by `Country`. Used by build_sjr_country_counts()
SJR_COUNTRY_RECODES <- c(
  "Russian Federation" = "Russia",
  "Viet Nam" = "Vietnam",
  "Macao" = "Macau",
  "Democratic Republic Congo" = "Democratic Republic of the Congo",
  "Congo" = "Republic of the Congo (Congo-Brazzaville)",
  "Brunei Darussalam" = "Brunei (Brunei Darussalam)",
  "Myanmar" = "Myanmar (formerly Burma)",
  "Bosnia and Herzegovina" = "Bosnia & Herzegovina",
  "Syrian Arab Republic" = "Syria",
  "Trinidad and Tobago" = "Trinidad & Tobago",
  "Eswatini" = "Eswatini (formerly Swaziland)",
  "Macedonia" = "North Macedonia (formerly Macedonia)",
  "Côte d’Ivoire" = "Cote d'Ivoire",
  "Reunion" = "Réunion Island",
  "Timor-Leste" = "East Timor",
  "Saint Vincent and the Grenadines" = "Saint Vincent & the Grenadines",
  "Saint Kitts and Nevis" = "St. Kitts & Nevis"
)
