#' get_selected_variants_ch
#'    Create dataframe and plot of selected SARS-CoV-2 variants
#'
#' @param metafile - a meta data file from GISAID
#' @param selected_variants - a vector of Pango lineage strings.
#' @param host - character, String indicating host species.
#' @param country - character, country of interest. If NULL, all countries are included.
#' @param title - character, optional plot title
#'
#' @return - a dataframe
#'    containing the columns
#'    `Collection date`, `Pango lineage`, Count
#' 
get_selected_variants_ch <- function(metafile,
                                     selected_variants,
                                     host = 'Human', 
                                     country = 'USA',
                                     title = NULL,
                                     start_date = as.Date("2020-01-01")) {
  require(tidyverse)

  # callback function for read_tsv_chunked
  # filter chunk by Collection.date Pango lineage, and variants
  filter_chunk <- function(df, pos) {
    df %>% filter(Host == {{ host }}) %>%
      select(`Collection date`, Location, `Pango lineage`) %>%
      mutate(`Collection date` = as.Date(`Collection date`, format = '%Y-%m-%d')) %>%
      filter(`Collection date` >= {{ start_date }}) %>%
      filter(`Pango lineage` %in% {{ selected_variants }})
  }
  
  df_BA <- read_tsv_chunked('data/metadata.tsv', DataFrameCallback$new(filter_chunk), show_col_types = FALSE)

  # extract country from location
  df_BA <- df_BA %>%
    separate(Location, c("Region", "Country", "State"), sep = "\\s*/\\s*", extra = "drop", fill = "right")

  # filter by country
  if(! is.null(country)) {
    df_BA <- df_BA %>%
      filter(Country == {{ country }})
  }

  # accumulate counts
  df_BA <- df_BA %>%
    select(`Collection date`, `Pango lineage`) %>%
    group_by(`Collection date`, `Pango lineage`) %>%
    count() %>%
    rename(Count = n) %>%
    drop_na() %>%
    arrange(`Collection date`)
  
  p <- ggplot(df_BA) +
    geom_bar(aes(x = `Collection date`, y = Count, color = `Pango lineage`, fill = `Pango lineage`), stat = 'identity')
  if(! is.null(title))
    p <- p + ggtitle(title)
  print(p)
  
  return(df_BA)
}