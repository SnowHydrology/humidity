#' Dew point temperature conversion
#'
#' Convert air temperature in degrees Celsius and relative humidity in percent
#' to dew point temperature in degrees Celsius
#'
#' From \url{http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.html}
#'
#' Based on Alduchov and Eskridge(1996): "Improved Magnus form approximation of 
#' saturation vapor pressure." Journal of Applied Meteorology
#' @param TAIR The air temperature in degrees Celsius
#' @param RH The relative humidity in percent
#' @return The dew point temperature in degrees Celsius
#' @examples 
#' tdew <- dewpoint(5, 80);
#' @export
dewpoint <- function(TAIR, RH){
    243.04 * (log(RH / 100) + ((17.625 * TAIR) / (243.04 + TAIR))) /
    (17.625 - log(RH /100) - ((17.625 * TAIR) / (243.04 + TAIR)))
}


#' Wet bulb temperature conversion
#'
#' Convert air temperature in degrees Celsius and relative humidity as percent
#' to wet bulb temperature in degrees Celsius
#'
#' From Stull (2011): "Wet-bulb temperature from relative humidity and 
#' air temperature." Journal of Applied Meteorology and Climatology
#' @param TAIR The air temperature in degrees Celsius
#' @param RH The relative humidity in percent
#' @return The wet bulb temperature in degrees Celsius
#' @examples 
#' twet <- wetbulb(5, 80);
#' @export
wetbulb <- function(TAIR, RH){
	TAIR * atan(0.151977 *( (RH + 8.313659) ^ 0.5)) +
      atan(TAIR + RH) -
      atan(RH - 1.676331) +
      ((0.00391838 * (RH ^ 1.5)) * atan(0.023101 * RH)) -
      4.86035
} 

#' Relative humidity conversion
#'
#' Convert air temperature in degrees Celsius and dew point temperature in 
#' degrees Celsius to relative humidity in percent
#'
#' From \url{http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.html}
#'
#' Based on Alduchov and Eskridge(1996): "Improved Magnus form approximation of 
#' saturation vapor pressure." Journal of Applied Meteorology
#' @param TAIR The air temperature in degrees Celsius
#' @param TDEW The dew point temperature in degrees Celsius
#' @return The relative humidity in percent
#' @examples 
#' rh <- relhum(5, 4);
#' @export
relhum <- function(TAIR, TDEW){
	100 * (exp((17.625 * TDEW) / (243.04 + TDEW)) / exp((17.625 * TAIR)/(243.04 + TAIR))) 
}