% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host opens a remaining goat door.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{...}{A numeric value}
}
\value{
The function returns a length 1 numeric vector
indicating the position of the door they chose.
}
\description{
\code{open_goat_door()} generates the host's selection
of a  remaining goat door and opens it
for the Contestant to see.
}
\details{
If the contestant selected the car on their first
door selection, the host randomly selects one of
the two remaining goat doors. If the contestant selected
a goat door for their initial selection, the host
selects the remaining unopened goat door.
}
\examples{
`open_goat_door ()`
[1]

`open_goat_door ()`
[2]

`open_goat_door ()`
[3]

}
