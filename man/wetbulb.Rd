% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humidity.R
\name{wetbulb}
\alias{wetbulb}
\title{Wet bulb temperature conversion}
\usage{
wetbulb(TAIR, RH)
}
\arguments{
\item{TAIR}{The air temperature in degrees Celsius}

\item{RH}{The relative humidity in percent}
}
\value{
The wet bulb temperature in degrees Celsius
}
\description{
Convert air temperature in degrees Celsius and relative humidity as percent
to wet bulb temperature in degrees Celsius
}
\details{
From Stull (2011): "Wet-bulb temperature from relative humidity and 
air temperature." Journal of Applied Meteorology and Climatology
}
\examples{
twet <- wetbulb(5, 80);
}
