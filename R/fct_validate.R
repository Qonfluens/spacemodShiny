# validation pour le module data
validate_data <- function(input, shared_global) {
  !is.null(shared_global$sf_ROI) &&
    !is.null(shared_global$raster_ground)
}
