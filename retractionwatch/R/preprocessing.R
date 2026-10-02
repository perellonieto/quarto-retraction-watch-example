
#' Clean the raw Retraction Watch data
#'
#' Cleans reasons and dates, adds one indicator column per reason group,
#' retraction delay categories, OECD/UK flags, an `international` flag and a
#' `Mass_Retraction` factor (IEEE 2009-2011 and Hindawi mass retractions).
#'
#' @param retraction_data Raw Retraction Watch data.
#' @param oecd_countries Data frame with a `Name` column of OECD countries.
#' @param retraction_reason_conversion Data frame with `Reason` and `Group`
#'   columns mapping each reason to a reason group.
#' @param long_df_by_countries If `TRUE`, papers with several countries are
#'   split into one row per country.
#' @return The cleaned data frame.
#' @export
clean_data <- function(retraction_data, 
                       oecd_countries,
                       retraction_reason_conversion,
                       long_df_by_countries=T
){
  
  
  # Cleaning reasons for retraction
  retraction_data$Reason <- gsub('+', '', retraction_data$Reason, fixed=T)
  # Remove trailing colon
  retraction_data$Reason <- gsub(';$', ';', retraction_data$Reason)
  # Unique all countries
  all_country_names <- unique(unlist(strsplit(retraction_data$Country, ';')))
  
  
  # Number of countries involved in each article
  retraction_data$number_of_countries <- lapply(retraction_data$Country, function(x){
    x <- strsplit(x, ';')
    return(length(unlist(x)))
  }) %>% unlist()
  
  # Cleaning Dates 
  retraction_data$OriginalPaperDate <- gsub(' 0:00',
                                            '', 
                                            retraction_data$OriginalPaperDate)
  retraction_data$OriginalPaperDate <- mdy(retraction_data$OriginalPaperDate)
  
  retraction_data$RetractionDate <- gsub(' 0:00',
                                         '', 
                                         retraction_data$RetractionDate)
  retraction_data$RetractionDate <- mdy(retraction_data$RetractionDate)
  
  retraction_data <- retraction_data %>% 
    mutate(retraction_year = format(RetractionDate, "%Y"),
           publication_year = format(OriginalPaperDate, "%Y"))
  
  
  grouped_reasons <- sapply(unique(retraction_reason_conversion$Group), function(x) {
    # All the items which belong to this group
    strs_to_check <- unique(retraction_reason_conversion$Reason[retraction_reason_conversion$Group == x])
    
    res <- lapply(retraction_data$Reason, function(y){
      any(unlist(strsplit(y,';')) %in% strs_to_check)
      
    })
    return(unlist(res))
  }, simplify=F) %>% tibble::as_tibble() %>% mutate_all(as.numeric)
  
  retraction_data <- bind_cols(retraction_data,grouped_reasons)
  
  retraction_data$retracted_within_year <- case_when(
    (retraction_data$RetractionDate - retraction_data$OriginalPaperDate)/365.25 <=1 ~ 'Within 1 year',
    (retraction_data$RetractionDate - retraction_data$OriginalPaperDate)/365.25 >1~ 'After 1 year')
  
  retraction_data$retracted_within_two_years <- case_when(
    (retraction_data$RetractionDate - retraction_data$OriginalPaperDate)/365.25 <=2 ~ 'Within 2 years',
    (retraction_data$RetractionDate - retraction_data$OriginalPaperDate)/365.25 >2~ 'After 2 years')
  
  
  # Reformatting Data for one 
  # country per row. Please note
  # this will mean duplicate retractions
  # for papers with more than country 
  # per paper
  
  if(long_df_by_countries==T){
    retraction_data$paper_index <- 1:nrow(retraction_data)
    retraction_data <- retraction_data %>%
      # create row ID:
      mutate(row = row_number()) %>%
      # separate rows on " /":
      separate_rows(Country, sep = ';') 
    
    retraction_data$oecd <- case_when(
      retraction_data$Country %in% oecd_countries$Name ~ 'OECD',
      TRUE ~ 'Non-OECD'
    )
    retraction_data$UK <- case_when(
      retraction_data$Country =="United Kingdom" ~ 'UK',
      TRUE ~ 'Non-UK'
    )
  }
  
  retraction_data <- retraction_data |>
    group_by(`Record ID`) |>
    mutate(international = if_any(Country, ~n_distinct(.) > 1)) %>%
    ungroup()
  
  retraction_data$Mass_Retraction <- case_when(
    retraction_data$Publisher=='IEEE: Institute of Electrical and Electronics Engineers'&
      (retraction_data$publication_year %in% c(2009,2010,2011) | retraction_data$retraction_year %in% c(2009,2010,2011)) ~ 'IEEE Mass Retraction',
    retraction_data$Publisher=='Hindawi'&
      (retraction_data$retraction_year %in% c(2022,2023,2014)) ~ 'Hindawi Mass Retraction',
    TRUE ~ 'Other')
  retraction_data$Mass_Retraction <- factor(retraction_data$Mass_Retraction,
                                            levels = c('IEEE Mass Retraction','Hindawi Mass Retraction','Other'))
  
  # retraction_data$hindawi_retr <- case_when(
  #   retraction_data$Publisher=='Hindawi'&
  #     (retraction_data$retraction_year %in% c(2022,2023,2014)) ~ 'Hindawi',
  #   TRUE ~ 'Other')
  
  retraction_data$publication_year <- as.numeric(retraction_data$publication_year)
  retraction_data$retraction_year <- as.numeric(retraction_data$retraction_year)
  retraction_data$RetractionID = retraction_data["Record ID"]
  return(retraction_data)
  
}

#' Classify countries into UK, other OECD and rest of world
#'
#' @param country Character vector of country names.
#' @param oecd_names Character vector of OECD country names.
#' @return Character vector with values `"United Kingdom"`, `"Other OECD"`
#'   or `"Rest of World"`.
#' @export
classify_country <- function(country, oecd_names) {
  case_when(
    country == "United Kingdom" ~ "United Kingdom",
    country %in% oecd_names ~ "Other OECD",
    TRUE ~ "Rest of World"
  )
}

#' Add a `country_class` column based on the `Country` column
#'
#' @param data Data frame with a `Country` column.
#' @param oecd_countries Data frame with a `Name` column of OECD countries.
#' @return `data` with a `country_class` column (see [classify_country()]).
#' @export
add_country_class <- function(data, oecd_countries) {
  data %>%
    mutate(country_class = classify_country(.data$Country, oecd_countries$Name))
}
