# ERA5 monthly via the Copernicus Climate Data Store (CDS) ----------------------------
# Verified 2026-11-18: ecmwfr >= 2.0 uses the new CDS API with a Personal Access Token
# (PAT). One-time setup:
#   1. Create a free ECMWF account at https://cds.climate.copernicus.eu and copy your PAT
#      from your profile page.
#   2. ecmwfr::wf_set_key(key = "<PAT>")      # stored in the system keyring, not in the repo
#   3. Accept the dataset licence once on the dataset's web page ("Download" tab).

#' Request ERA5 / ERA5-Land monthly means for a bounding box
#'
#' Builds a CDS request for monthly averaged reanalysis and downloads a NetCDF via
#' [ecmwfr::wf_request()]. Cached with the access date like every other fetcher.
#'
#' @param var CDS variable name(s), e.g. `"2m_temperature"`, `"total_precipitation"`,
#'   `"volumetric_soil_water_layer_1"`.
#' @param bbox Bounding box as `c(xmin, ymin, xmax, ymax)` in degrees (lon/lat, the
#'   `sf`/`terra` convention). Converted internally to the CDS order N, W, S, E.
#' @param years Integer vector of years.
#' @param months Integer vector of months (default all).
#' @param dataset `"reanalysis-era5-single-levels-monthly-means"` (0.25°, default) or
#'   `"reanalysis-era5-land-monthly-means"` (0.1°, land only).
#' @param product_type Default `"monthly_averaged_reanalysis"`.
#' @param read Return a `terra::SpatRaster` instead of the file path (needs `terra`).
#' @param dry_run Return the request list without sending it (useful to inspect/edit).
#' @inheritParams cached_fetch
#' @return File path (or SpatRaster if `read = TRUE`; or the request if `dry_run = TRUE`).
#' @section Pitfalls:
#' * Units: temperature in K (subtract 273.15); `total_precipitation` monthly means are in
#'   m/day (multiply by days in month × 1000 for mm/month).
#' * Large requests queue for a long time: keep bbox and years small for a 4-hour case,
#'   or aggregate to country level once and cache.
#' * Grid cells are not area-weighted: weight by cos(latitude) or crop area (MapSPAM).
#' @examples
#' get_era5_monthly("2m_temperature", bbox = c(102, 8, 110, 24), years = 2020:2024,
#'                  dry_run = TRUE)   # Vietnam
#' @export
get_era5_monthly <- function(var, bbox, years, months = 1:12,
                             dataset = "reanalysis-era5-single-levels-monthly-means",
                             product_type = "monthly_averaged_reanalysis",
                             read = FALSE, dry_run = FALSE, dir = NULL, refresh = FALSE) {
  stopifnot(length(bbox) == 4, bbox[1] < bbox[3], bbox[2] < bbox[4])
  area <- c(bbox[4], bbox[1], bbox[2], bbox[3])  # N, W, S, E
  key <- paste0("era5_", sub("reanalysis-", "", dataset), "_",
                substr(rlang::hash(list(var, bbox, years, months, product_type)), 1, 10))
  request <- list(
    dataset_short_name = dataset,
    product_type = product_type,
    variable = as.list(var),
    year = as.list(as.character(years)),
    month = as.list(sprintf("%02d", months)),
    time = "00:00",
    area = area,
    data_format = "netcdf",
    download_format = "unarchived",
    target = NULL
  )
  if (dry_run) {
    request$target <- basename(.cache_path(key, "nc", ac_data_dir(dir)))
    return(request)
  }
  .need("ecmwfr", "for get_era5_monthly()")
  path <- cached_fetch(key, "nc", function(p) {
    request$target <- basename(p)
    ok <- tryCatch(ecmwfr::wf_get_key(), error = function(e) NULL)
    if (is.null(ok)) stop("No CDS key found. Run ecmwfr::wf_set_key(key = '<your PAT>') once.",
                          call. = FALSE)
    ecmwfr::wf_request(request = request, transfer = TRUE, path = dirname(p),
                       time_out = 3 * 3600, verbose = FALSE)
  }, source = paste0("Copernicus CDS ", dataset, ": ", paste(var, collapse = ", ")),
  url = paste0("https://cds.climate.copernicus.eu/datasets/", dataset),
  dir = dir, refresh = refresh)
  if (read) {
    .need("terra", "to read NetCDF files")
    return(terra::rast(path))
  }
  path
}
