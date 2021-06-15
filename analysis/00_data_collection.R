# Online Data Collection to put in Raw Data Directory

# 1. World Excess Mortality Database
df_exc <- read.csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")
df_exc$iso3c <- countrycode::countrycode(
  df_exc$country_name, "country.name.en", "iso3c",
  custom_match = c("Kosovo" = "XKX")
)

# Here cp_path is a function from within this package (i.e. in R directory)
saveRDS(df_exc, cp_path("data/raw/world_excess_mortality.rds"))

# 2. Ourworldindata
owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
saveRDS(owid, cp_path(paste0("data/raw/owid_", Sys.Date(), ".rds")))

# 3. worldometers
worldometers_data <- function(date) {

  date <- as.Date(date, "%Y-%m-%d")
  if (is.na(date)) {
    stop("Date must be provided in ISO format (i.e., YYYY-MM-DD)")
  }

  get_country_data <- function(link, iso3c, name) {

    url <- paste0("https://www.worldometers.info/coronavirus/country/", link)
    html <- xml2::read_html(url)
    scrs <- rvest::html_nodes(html, "script")
    hcs <- grep("Highcharts.chart", unlist(lapply(scrs, as.character)))

    text <- scrs[hcs]

    # 2021 fix func
    date_func <- function(dates_d){

      jans <- grep("Jan", dates_d)

      if (length(jans) == 0) {
        dates_d <- as.Date(paste(dates_d, "2020"),  "%b %d %Y")
      } else if (all(diff(jans)==1) && as.Date(date) < as.Date("2021-01-01")) {
        dates_d <- as.Date(paste(dates_d, "2020"),  "%b %d %Y")
      } else {
        jan_diffs <- diff(jans)
        new_years <- jans[which(jan_diffs != 1) + 1]
        if(length(new_years) == 0) {
          new_years <- jans[1]
        }
        dates_d_l <- vector("list", length(new_years)+1)
        years <- seq(2020, 2020 + length(new_years), 1)
        for (i in seq_along(dates_d_l)) {

          # starts
          if(i == 1) {
            i_1 <- 1
          } else {
            i_1 <- new_years[i-1]
          }

          # ends
          if(i == length(dates_d_l)) {
            i_end <- length(dates_d)
          } else {
            i_end <- new_years[i]-1
          }

          dates_d_l[[i]] <- as.character(as.Date(paste(dates_d[seq(i_1, i_end)], years[i]),  "%b %d %Y"))

        }
        dates_d <- unlist(dates_d_l)
      }

      return(dates_d)

    }

    death_dat <- text[grep("coronavirus-deaths-linear", text)]
    if(length(death_dat) == 0) {

      dates_d <- NA
      deaths <- NA

    } else {

      txt <- xml2::xml_text(death_dat)
      spl <- strsplit(txt, "\n")[[1]]

      dates_d <- spl[grep("categories", spl)][1]
      dates_d <- strsplit(dates_d, "\"|,")[[1]][which(nchar(strsplit(dates_d, "\"|,")[[1]])==6)]
      dates_d <- date_func(dates_d)
      deaths <- spl[grep("data", spl)[1]]
      deaths <- tail(head(strsplit(deaths, ",|\\[|\\]")[[1]],-1),-1)
      deaths <- suppressWarnings(as.numeric(deaths))
      deaths <- c(0,diff(deaths))

    }
    cases_dat <- text[grep("graph-cases-daily", text)]
    txt <- xml2::xml_text(cases_dat)
    spl <- strsplit(txt, "\n")[[1]]

    dates_c <- spl[grep("categories", spl)]
    dates_c <- strsplit(dates_c, "\"|,")[[1]][which(nchar(strsplit(dates_c, "\"|,")[[1]])==6)]
    dates_c <- date_func(dates_c)

    cases <- spl[grep("data", spl)[1]]
    cases <- tail(head(strsplit(cases, ",|\\[|\\]")[[1]],-1),-1)
    cases <- suppressWarnings(as.numeric(cases))

    df <- data.frame("dateRep" = dates_c, "cases" = cases)
    df$deaths <- deaths[match(df$dateRep, dates_d)]
    df$deaths[is.na(df$deaths)] <- 0

    df$countryterritoryCode <- iso3c
    df$Region <- name
    df <- df[order(df$dateRep, decreasing = TRUE),]

    return(df)

  }

  # country names from worldometers
  wo <- "https://www.worldometers.info/coronavirus/#countries"
  wo <- xml2::read_html(wo) %>% rvest::html_nodes(".mt_a")

  # create country names and links
  countries <- xml2::xml_text(wo)
  links <- gsub("country/|/","",rvest::html_attr(wo, "href"))
  iso3cs <- countrycode::countrycode(countries, "country.name.en", "iso3c",
                                     custom_match = c("CAR" = "CAF"))

  # df of args to run
  df_args <- data.frame(countries = countries, links = links, iso3cs = iso3cs)
  df_args <- unique(df_args)
  df_args <- na.omit(df_args)

  # loop over data needs
  dats <- lapply(seq_along(df_args$countries), function(i) {
    get_country_data(df_args$links[i], df_args$iso3cs[i], df_args$countries[i])
  })
  df <- do.call(rbind, dats)

  # and fill in leading NAs
  df$cases[is.na(df$cases)] <- 0
  df$deaths[is.na(df$deaths)] <- 0

  # AND handling their peculiar negative deaths based on comparison against ECDC and manually cleaning :)

  # FRA
  # -217 deaths day
  df$deaths[df$dateRep == as.Date("2020-05-20") & df$countryterritoryCode == "FRA"] <- 125
  df$deaths[df$dateRep == as.Date("2020-05-19") & df$countryterritoryCode == "FRA"] <- 186
  df$deaths[df$dateRep == as.Date("2020-05-18") & df$countryterritoryCode == "FRA"] <- 68
  df$deaths[df$dateRep == as.Date("2020-05-17") & df$countryterritoryCode == "FRA"] <- 88
  df$deaths[df$dateRep == as.Date("2020-05-16") & df$countryterritoryCode == "FRA"] <- 130

  # CYP
  # -2 deaths day
  df$deaths[df$dateRep == as.Date("2020-04-05") & df$countryterritoryCode == "CYP"] <- 0
  df$deaths[df$dateRep == as.Date("2020-04-04") & df$countryterritoryCode == "CYP"] <- 0
  df$deaths[df$dateRep == as.Date("2020-04-03") & df$countryterritoryCode == "CYP"] <- 0

  # CZE
  # 2 x -1 deaths day
  df$deaths[df$dateRep == as.Date("2020-06-14") & df$countryterritoryCode == "CZE"] <- 0
  df$deaths[df$dateRep == as.Date("2020-06-15") & df$countryterritoryCode == "CZE"] <- 0
  df$deaths[df$dateRep == as.Date("2020-05-19") & df$countryterritoryCode == "CZE"] <- 0
  df$deaths[df$dateRep == as.Date("2020-05-18") & df$countryterritoryCode == "CZE"] <- 1

  # FIN
  # -1 deaths day
  df$deaths[df$dateRep == as.Date("2020-04-07") & df$countryterritoryCode == "FIN"] <- 0
  df$deaths[df$dateRep == as.Date("2020-04-08") & df$countryterritoryCode == "FIN"] <- 2

  # IRL
  # 2 x -deaths day
  df$deaths[df$dateRep == as.Date("2020-06-01") & df$countryterritoryCode == "IRL"] <- 0
  df$deaths[df$dateRep == as.Date("2020-05-31") & df$countryterritoryCode == "IRL"] <- 1
  df$deaths[df$dateRep == as.Date("2020-05-26") & df$countryterritoryCode == "IRL"] <- 0
  df$deaths[df$dateRep == as.Date("2020-05-25") & df$countryterritoryCode == "IRL"] <- 2
  df$deaths[df$dateRep == as.Date("2020-11-24") & df$countryterritoryCode == "IRL"] <- 0
  df$deaths[df$dateRep == as.Date("2020-11-23") & df$countryterritoryCode == "IRL"] <- 0

  # LUX
  # -2 deaths day
  df$deaths[df$dateRep == as.Date("2020-04-15") & df$countryterritoryCode == "LUX"] <- 0
  df$deaths[df$dateRep == as.Date("2020-04-14") & df$countryterritoryCode == "LUX"] <- 1

  # COG
  # few off days
  df$deaths[df$dateRep == as.Date("2020-09-10") & df$countryterritoryCode == "COG"] <- 0
  df$deaths[df$dateRep == as.Date("2020-09-09") & df$countryterritoryCode == "COG"] <- 0
  df$deaths[df$dateRep == as.Date("2020-09-08") & df$countryterritoryCode == "COG"] <- 1
  df$deaths[df$dateRep == as.Date("2020-09-04") & df$countryterritoryCode == "COG"] <- 0
  df$deaths[df$dateRep == as.Date("2020-09-03") & df$countryterritoryCode == "COG"] <- 4

  # worldometers is a day ahead of ECDC - so to keep it all aligned
  df$dateRep <- as.Date(df$dateRep) - 1
  wo <- df
  names(wo) <- c("date", "cases", "deaths", "iso3c", "country_name")

  return(wo)

}

worldometers <- worldometers_data(Sys.Date())
saveRDS(worldometers, cp_path(paste0("data/raw/worldometers_", Sys.Date(), ".rds")))
