% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_database_export.R
\name{as_tibble_list}
\alias{as_tibble_list}
\alias{as_tibble_list.star_database}
\title{Generate a list of tibbles with fact and dimension tables}
\usage{
as_tibble_list(db)

\method{as_tibble_list}{star_database}(db)
}
\arguments{
\item{db}{A \code{star_database} object.}
}
\value{
A list of \code{tibble}
}
\description{
To port databases to other work environments it is useful to be able to
export them as a list of tibbles, as this function does.
}
\examples{

db1 <- star_database(mrs_cause_schema, ft_num) |>
  snake_case()
tl1 <- db1 |>
  as_tibble_list()

db2 <- star_database(mrs_age_schema, ft_age) |>
  snake_case()

ct <- constellation("MRS", db1, db2)
tl <- ct |>
  as_tibble_list()

}
\seealso{
\code{\link{star_database}}

Other star database exportation functions: 
\code{\link{as_csv_files}()},
\code{\link{as_dm_class}()},
\code{\link{as_multistar}()},
\code{\link{as_rdb}()},
\code{\link{as_single_tibble_list}()},
\code{\link{as_xlsx_file}()},
\code{\link{draw_tables}()}
}
\concept{star database exportation functions}
