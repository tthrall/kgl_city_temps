#####
###
#     city_temperatures.R
#
#       Download daily temperatures from 1995-2020
#       at 300+ cities.  Provide summaries and examples.
#
#       Primary source:
#         Daily Temperature of Major Cities | Kaggle.com
#
#       Derived from:
#         Professor J. Kelly Kissock at U. Dayton (2004)
#         See: https
#         ecommons.udayton.edu/cgi/viewcontent.cgi
#         ?article=1140&context=mee_fac_pub
#
###
#####

##
#  get_kgl_city_temps()
#
#  Background: A zipped file has been downloaded into "data > retain".
#    The file contains city temperatures from kaggle https
#    www.kaggle.com/datasets/sudalairajkumar/
#    daily-temperature-of-major-cities
#
#  Data processing by this function:
#
#     decompress downloaded zipped file
#     read decompressed file
#     recode missing values
#     construct date
#     remove duplicates
#     save as zipped RDS file
#
#  By-product: large uncompressed files in "data > tmp"
#
#  Return value: kgl_city_temps as tibble
##
get_kgl_city_temps <- function(
    read_dnld_file = FALSE # <lgl> read initial download? (else RDS)
) {
  # GOOD: Kaggle download is read and cleaned successfully in memory.
  # TODO: Compress the cleaned file, so that it can be expanded and read.
  assertthat::assert_that(
    read_dnld_file == FALSE,
    msg = "kgl_city_temps.zip has been compressed external to this function"
  )
  
  # read and save Kaggle download file as needed
  if (read_dnld_file) {
    # compressed Kaggle file -> temporary CSV file
    here::here(
      "data", "compressed", "kaggle-city-temperatures.zip") |>
      zip::unzip(
        files = "city_temperature.csv",
        exdir = here::here("data", "tmp"))
    
    # read CSV file
    kgl_city_temps <- readr::read_csv(
      file = here::here(
        "data", "tmp", "city_temperature.csv"
      ),
      col_types = cols(
        Month = col_integer(),
        Day   = col_integer(),
        Year  = col_integer()
      )) |>
      # temp: recode -99 temperatures to NA
      dplyr::mutate(temp = dplyr::na_if(AvgTemperature, -99)) |>
      dplyr::select(- AvgTemperature) |>
      # construct date
      dplyr::mutate(
        d_chr = Day |> stringr::str_pad(
          side = "left", pad = "0", width = 2, use_width = TRUE),
        m_chr = Month |> stringr::str_pad(
          side = "left", pad = "0", width = 2, use_width = TRUE),
        ymd_chr = paste0(Year, m_chr, d_chr),
        dt = lubridate::ymd(ymd_chr)
      ) |>
      dplyr::select(Region:Year, dt, temp)
    
    # extract one copy of duplicated data
    Wash_DC <- kgl_city_temps |>
      dplyr::filter(
        State == "District of Columbia",
        City  == "Washington") |>
      # take only the first of 2 copies of the data
      slice_head(n = 9265)
    
    # replace duplicated data with extracted data
    kgl_city_temps <- kgl_city_temps |>
      dplyr::filter(
        City != "Washington",
        City != "Washington DC"
      ) |>
      dplyr::bind_rows(Wash_DC)
    
    # save as temporary TSV file
    kgl_city_temps |>
      readr::write_tsv(file = here::here(
        "data", "tmp", "kgl_city_temps.txt"
      ))
    
    # compress TSV file and retain
    zip::zip(
      files = here::here(
        "data", "tmp", "kgl_city_temps.txt"),
      zipfile = here::here(
        "data", "compressed", "kgl_city_temps.zip")
    )
    
  } else {
    # compressed TSV file -> temporary TSV file
    here::here(
      "data", "compressed", "kgl_city_temps.zip") |>
      zip::unzip(
        files = "kgl_city_temps.txt",
        exdir = here::here("data", "tmp")
      )
    
    # read TSV file
    kgl_city_temps <- readr::read_tsv(here(
      "data", "tmp", "kgl_city_temps.txt"
    ))
  }
  # NB: delete large tmp files before committing to GitHub
  return(kgl_city_temps)
}

##
#  get_kgl_city_smy()
#
#  summarize dates and temps for each city
#
#  return: kgl_city_smy as tibble
##
get_kgl_city_smy <- function(
    x = kgl_city_temps # <tbl> includes vars City, dt, temp
) {
  kgl_city_smy <- x |>
    group_by(Region, Country, State, City) |>
    summarize(
      n_days = n(),
      n_miss = is.na(temp) |> sum() |> as.integer(),
      dt_min = min(dt, na.rm = TRUE),
      dt_max = max(dt, na.rm = TRUE),
      t_avg  = mean(temp, na.rm = TRUE),
      t_sd   = sd(temp, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  return(kgl_city_smy)
}

##
#  get_kgl_temps_smy()
#
#  (min, median, max) of
#  (n_days, t_avg, t_sd)
#  across cities
#
#  return: kgl_city_smy as tibble
##
get_kgl_temps_smy <- function(
    x = kgl_city_smy # <tbl> includes vars n_days, t_avg, t_sd
) {
  kgl_temps_smy_long <- x |>
    summarize(
      min_n_days = min(n_days),
      mid_n_days = median(n_days),
      max_n_days = max(n_days),

      min_t_avg = min(t_avg),
      mid_t_avg = median(t_avg),
      max_t_avg = max(t_avg),

      min_t_sd = min(t_sd),
      mid_t_sd = median(t_sd),
      max_t_sd = max(t_sd)
    )

  kgl_temps_vec <-
    (kgl_temps_smy_long |> as.matrix()) [1, ] |>
    as.vector()

  kgl_temps_smy <- tibble::tibble(
    n_days = kgl_temps_vec [1:3] |> as.integer(),
    t_avg  = kgl_temps_vec [4:6],
    t_sd   = kgl_temps_vec [7:9]
  ) |>
    mutate(stat = c("min", "median", "max")) |>
    select(stat, everything())
  return(kgl_temps_smy)
}

##
#  get_city_slices
#
#  identify cities having (min, max) values
#
#  return: city_slices as tibble
##
get_city_slices <- function(
    x = kgl_city_smy # <tbl> includes vars n_days, t_avg, t_sd
) {
  days_min_cty <- x |> dplyr::slice_min(n_days) |>
    dplyr::mutate(slice = "days_min")
  days_max_cty <- x |> dplyr::slice_max(n_days) |>
    dplyr::mutate(slice = "days_max")

  t_avg_min_cty <- x |> dplyr::slice_min(t_avg) |>
    dplyr::mutate(slice = "t_avg_min")
  t_avg_max_cty <- x |> dplyr::slice_max(t_avg) |>
    dplyr::mutate(slice = "t_avg_max")

  t_sd_min_cty <- x |> dplyr::slice_min(t_sd) |>
    dplyr::mutate(slice = "t_sd_min")
  t_sd_max_cty <- x |> dplyr::slice_max(t_sd) |>
    dplyr::mutate(slice = "t_sd_max")

  city_slices <- days_min_cty |>
    dplyr::bind_rows(
      days_max_cty,

      t_avg_min_cty,
      t_avg_max_cty,

      t_sd_min_cty,
      t_sd_max_cty
    ) |>
    dplyr::select(slice, everything())
  return(city_slices)
}

##
#  impute_temp()
#
#  replace missing temperatures with moving average
#
#  caveat:
#    assumes city name uniquely identifies city within tibble x
#
#  return: city_t_imp as tibble
##
impute_temp <- function(
    city_names,          # <chr> selected City name(s)
    x = kgl_city_temps,  # <tbl> includes vars City, dt, temp
    k = 3L,              # <int> half-width of moving average
    weighting = "linear" # <chr> argument passed to imputeTS::na.ma()
) {
  assertthat::assert_that(
    all(city_names %in% x$City)
  )

  selected_city_temps <- x |>
    dplyr::filter(City %in% city_names)

  city_t_imp <- selected_city_temps |>
    dplyr::group_by(City) |>
    mutate(
      t_imp = temp |> imputeTS::na_ma(
        k = k, weighting = weighting)
    ) |>
    dplyr::ungroup()

  return(city_t_imp)
}


##
#  EOF
##
