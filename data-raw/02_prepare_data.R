library(dplyr)
library(ensurer)
library(janitor)
library(geobr)
library(purrr)
library(stringr)
library(tidyr)

###############################################################################
# PROCESS THE INVENTORY DATA
#------------------------------------------------------------------------------
# The data is composed of plots. Each plot has 2 files: a SHP and a CSV. The
# SHP contains the perimeter (a square) while the CSV contais data about each
# tree.
#------------------------------------------------------------------------------
# NOTE:
# - The trees' coordinates are given using UTM without zone; the zone is
#   available in each SHP.
# - Some CSV files have weird UTM coordinates which required special
#   processing.
###############################################################################


#---- Setup ----

base_dir <- "~/Documents/data/sustainable_landscapes_brazil/Forest_Inventory_Brazil_2007"
out_sits_csv <- "/home/alber/Documents/github/forestmeasurebramz/data-raw/time_series_4_sits.csv"



#---- Utilitary functions ----

#' Get the CRS of a file with vector geographic information.
#' @param file_path A path to a file.
#' @return          A CRS object.
get_crs <- function(file_path) {
    file_path %>%
        sf::read_sf() %>%
        sf::st_crs() %>%
        return()
}

#' Check that coordinate columns exist and that the aren't NAs.
#' @param x A tibble.
#' @param lon A character. Name of the longitude column.
#' @param lat A character. Name of the latitude column.
#' @return    A tibble.
drop_na_coords <- function(x, lon, lat) {
    stopifnot("Columns not found!" = all(c(lon, lat) %in% colnames(x)))
    x %>%
        tidyr::drop_na(tidyselect::any_of(c(lon, lat))) %>%
    return()
}

#' Clean tibble's coordinates and drop those with NA.
#' @param x      A tibble.
#' @return       A tibble.
clean_coordinates <- function(x) {
    # TODO: Is it always 6 or 7 numbers before the decimal point?
    x %>%
        dplyr::mutate(
            utm_easting = as.character(utm_easting),
            utm_e = dplyr::if_else(is.na(as.numeric(utm_easting)),
                                   gsub("\\.", "", utm_easting),
                                   NA_character_),
            utm_e = dplyr::if_else(!is.na(utm_e),
                                   gsub('^([0-9]{6})([0-9]+)$', '\\1\\.\\2',
                                        utm_e),
                                   utm_e),
            utm_easting = dplyr::if_else(is.na(utm_e), utm_easting, utm_e),
            utm_easting = as.numeric(utm_easting),
            utm_northing = as.character(utm_northing),
            utm_n = dplyr::if_else(is.na(as.numeric(utm_northing)),
                                   gsub("\\.", "", utm_northing),
                                   NA_character_),
            utm_n = dplyr::if_else(!is.na(utm_n),
                                   gsub('^([0-9]{7})([0-9]+)$', '\\1\\.\\2',
                                        utm_n),
                                   utm_e),
            utm_northing = dplyr::if_else(is.na(utm_n), utm_northing, utm_n),
            umt_northing = as.numeric(utm_northing)
        ) %>%
        dplyr::select(-utm_e, -utm_n) %>%
        tidyr::drop_na(utm_easting, utm_northing) %>%
        return()
}

#' Cast a tibble to sf.
#' @param x      A tibble.
#' @param my_crs A CRS object.
#' @param lon    A character. Name of the longitude column in x.
#' @param lat    A character. Name of the latitude column in x.
#' @return       An sf object.
cast_to_sf <- function(x, my_crs, lon = "utm_easting", lat = "utm_northing") {
    stopifnot(inherits(my_crs, what = "crs"))
    x %>%
        sf::st_as_sf(coords = c(lon, lat),
                     crs = my_crs) %>%
        sf::st_zm(drop = TRUE, what = "ZM") %>%
        return()
}

#' Select columns from a tibble.
#' @param dat_tb    A tibble.
#' @param col_names A character. Names of the columns to select.
#' @return          A tibble.
select_columns <- function(data_tb, col_names) {
    data_tb %>%
        dplyr::select(tidyselect::all_of(col_names)) %>%
        return()
}

#' Count the number of samples that fall outside the given plots.
#' @param samples A sf object of point geometry.
#' @param plots   A sf object of polygon geometry.
#' @return        An integer.
samples_outside_plots <- function(samples, plots) {
    samples_inside_plot <-
        samples %>%
        sf::st_transform(sf::st_crs(plots)) %>%
        sf::st_intersects(plots, sparse = FALSE) %>%
        rowSums() %>%
        sum()
    return(nrow(samples) - samples_inside_plot)
}

#' Switch the X and Y columns in a tibble.
#' @param x      A tibble.
#' @param invert A logical. Invert columns?
#' @return       A tibble.
invert_axis <- function(x, invert) {
    # TODO: Convert column names into parameters.
    if (!invert)
        return(x)
    x %>%
        dplyr::rename(
            utm_easting = utm_northing,
            utm_northing = utm_easting
        ) %>%
        return()
}

#' Test if the given samples fall inside the Amazon.
#' @param samples A sf object of point geometry.
#' @param amazon  A sf object of polygon geometry.
#' @return        An logical
samples_in_amazon <- function(samples, amazon) {
    res <-
        samples %>%
        sf::st_transform(sf::st_crs(amazon)) %>%
        sf::st_intersects(amazon, sparse = FALSE) %>%
        rowSums()
    return(res != 0)
}


#---- Process the plot files (SHPs) ----

# NOTE: The plot files provide the CRS for the UTM coordiantes in the
#       inventory files.
plot_files <-
    base_dir %>%
    file.path("data") %>%
    ensurer::ensure_that(dir.exists(.),
                         err_desc = "Missing directory!") %>%
    list.files(pattern = "*.shp$",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(plot_path = value) %>%
    dplyr::mutate(
        plot_name = tools::file_path_sans_ext(basename(plot_path))
    ) %>%
    tidyr::separate(col = plot_name,
                    into = c("site", "subsite", "start", "end",
                             "inventory", "plot"),
                    remove = FALSE) %>%
    dplyr::mutate(end = stringr::str_match(end, pattern = "[0-9]{4}")[,1],
                  start = as.numeric(start),
                  end = as.numeric(end)) %>%
    dplyr::select(-inventory, -plot) %>%
    dplyr::mutate(plot_crs = purrr::map(plot_path, get_crs),
                  plot_sf  = purrr::map(plot_path, sf::read_sf),
                  plot_sf  = purrr::map(plot_sf, sf::st_zm,
                                        drop = TRUE, what = "ZM"))



#---- Process the inventory files (CSVs) ----

inventory_files <-
    base_dir %>%
    file.path("data") %>%
    ensurer::ensure_that(dir.exists(.),
                         err_desc = "Missing directory!") %>%
    list.files(pattern = "*.csv$",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(
        inventory_name = tools::file_path_sans_ext(basename(file_path))
    ) %>%
    tidyr::separate(col = inventory_name,
                    into = c("site", "subsite", "start", "end", "inventory"),
                    remove = FALSE) %>%
    dplyr::mutate(end = stringr::str_match(end, pattern = "[0-9]{4}")[,1],
                  start = as.numeric(start),
                  end = as.numeric(end)) %>%
    dplyr::select(-inventory) %>%
    dplyr::mutate(csv = purrr::map(file_path, readr::read_csv,
                  col_types = readr::cols(.default = readr::col_character())),
                  csv = purrr::map(csv, janitor::clean_names)) %>%
    dplyr::left_join(plot_files, by = c("site", "subsite", "start", "end"))



#---- Prepare a CSV files for retrieving time series ----

# Get the common column names in the CSVs.
common_names  <-
    inventory_files %>%
    dplyr::mutate(cnames = purrr::map(csv, colnames)) %>%
    dplyr::pull(cnames) %>%
    Reduce(intersect, .)

# Cast the data to sf.

data_tb <-
    inventory_files %>%
    dplyr::mutate(csv = purrr::map(csv, select_columns,
                                   col_names = common_names),
                  csv = purrr::map(csv, clean_coordinates)) %>%
    # NOTE: Fix inverted coordinate axis.
    dplyr::mutate(inverted_axis = dplyr::if_else(
        basename(file_path) == "DUC_A01_2016_Inventory.csv", TRUE, FALSE)) %>%
    dplyr::mutate(csv = purrr::map2(csv, inverted_axis, invert_axis)) %>%
    # NOTE: Fix CRS mismatch between CSV and Shapefile.
    dplyr::mutate(
        plot_crs_updated = dplyr::if_else(
            basename(file_path) == "DUC_A01_2016_Inventory.csv",
            list(
               sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
            ),
            plot_crs)
    ) %>%
    dplyr::mutate(csv_sf = purrr::map2(csv, plot_crs_updated, cast_to_sf))

# Report how many points fall outside the plots.
print("NOTE: number of samples falling outside buffered plots!")
data_tb %>%
    dplyr::mutate(plot_sf_buffer = purrr::map(plot_sf, sf::st_buffer,
                                              dist = 10),
                  samples_outside= purrr::map2_int(csv_sf, plot_sf_buffer,
                                                   samples_outside_plots)) %>%
    dplyr::filter(samples_outside > 0) %>%
    dplyr::mutate(file_name = basename(file_path)) %>%
    dplyr::select(file_name, samples_outside) %>%
    dplyr::arrange(dplyr::desc(samples_outside))

# Build a tibble formatted according to sits package.
data_tb <-
    data_tb %>%
    dplyr::mutate(csv_sf = purrr::map(csv_sf, sf::st_transform,
                                      crs = 4326)) %>%
    dplyr::mutate(csv_sf = purrr::map2(csv_sf, file_path,
        function(csv_sf, file_path) {
            csv_sf %>%
                dplyr::mutate(file_name = basename(file_path)) %>%
                return()
        }))

data_sf <- do.call(rbind, data_tb$csv_sf)

sits_tb <-
    data_sf %>%
    dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                  latitude  = sf::st_coordinates(.)[,2],
                  start_date = "2001-01-01",
                  end_date   = "2023-01-01",
                  label = common_name,
                  cube = NA,
                  time_series = NA,
                  in_amazon = samples_in_amazon(., geobr::read_amazon())) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(id = stringr::str_c(longitude, latitude, sep = ";")) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::filter(in_amazon) %>%
    dplyr::select(longitude, latitude, start_date,
                  end_date, label, cube, time_series)

# Save data.
sits_tb %>%
    saveRDS(file = paste0(tools::file_path_sans_ext(out_sits_csv), ".rds"))

sits_tb %>%
    readr::write_csv(file = out_sits_csv)

