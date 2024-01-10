
## ----------------------------------------------------------------------------------------------------------

#' `star_schema` S3 class
#'
#' An empty `star_schema` object is created in which definition of facts
#' and dimensions can be added.
#'
#' To get a star database (a `star_database` object) we need a flat table
#' and a `star_schema` object. The definition of facts and dimensions in
#' the `star_schema` object is made from the flat table columns.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' s <- star_schema()
#'
#' @export
star_schema <- function() {
  structure(list(facts = NULL, dimensions = NULL), class = "star_schema")
}


#' Define facts in a `star_schema` object.
#'
#' Facts are part of a `star_schema` object. They can be defined directly
#' as a `fact_schema` object or giving the name and a set of measures
#' that can be empty (does not have explicit measures).
#'
#' Associated with each measurement there is an aggregation function that can be
#' SUM, MAX or MIN. AVG is not considered among the possible aggregation
#' functions: The reason is that calculating AVG by considering subsets of
#' data does not necessarily yield the AVG of the total data.
#'
#' An additional measurement corresponding to the COUNT of aggregated rows is
#' added which, together with SUM, allows us to obtain the mean if needed.
#'
#' @param schema A `star_schema` object.
#' @param facts A `fact_schema` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names, each one for its
#'   corresponding measure. If none is indicated, the default is SUM. Additionally
#'   they can be MAX or MIN.
#' @param nrow_agg A string, name of a new measure that represents the COUNT
#'   of rows aggregated for each resulting row.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' s <- star_schema() |>
#'   define_facts(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "Other Deaths"
#'     )
#'   )
#'
#' s <- star_schema()
#' f <- fact_schema(
#'   name = "mrs_cause",
#'   measures = c(
#'     "Pneumonia and Influenza Deaths",
#'     "Other Deaths"
#'   )
#' )
#' s <- s |>
#'   define_facts(f)
#' @export
define_facts <- function(schema, facts, name, measures, agg_functions, nrow_agg) UseMethod("define_facts")

#' @rdname define_facts
#'
#' @export
define_facts.star_schema <-
  function(schema,
           facts = NULL,
           name = NULL,
           measures = NULL,
           agg_functions = NULL,
           nrow_agg = NULL) {
    if (!is.null(facts)) {
      stopifnot("Schema does not include fact_schema object." = methods::is(facts, "fact_schema"))
      stopifnot("If a fact_schema has been defined, the rest of the fields cannot be defined." = is.null(name) &
                  is.null(measures) &
                  is.null(agg_functions) & is.null(nrow_agg))
    } else {
      facts <- fact_schema(
        name = name,
        measures = measures,
        agg_functions = agg_functions,
        nrow_agg = nrow_agg
      )
    }
    f <- list(facts)
    names(f) <- snakecase::to_snake_case(facts$name)
    structure(list(facts = f, dimensions = schema$dimensions), class = "star_schema")
  }

#' Define dimension in a `star_schema` object.
#'
#' Dimensions are part of a `star_schema` object. They can be defined directly
#' as a `dimension_schema` object or giving the name and a set of attributes.
#'
#' @param schema A `star_schema` object.
#' @param dimension A `dimension_schema` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#' @param scd_nk A vector of attribute names, scd natural key.
#' @param scd_t0 A vector of attribute names, scd T0 attributes.
#' @param scd_t1 A vector of attribute names, scd T1 attributes.
#' @param scd_t2 A vector of attribute names, scd T2 attributes.
#' @param scd_t3 A vector of attribute names, scd T3 attributes.
#' @param scd_t6 A vector of attribute names, scd T6 attributes.
#' @param is_when A boolean, is when dimension.
#' @param ... When dimension configuration parameters.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' s <- star_schema() |>
#'   define_dimension(
#'     name = "when",
#'     attributes = c(
#'       "Week Ending Date",
#'       "WEEK",
#'       "Year"
#'     )
#'   )
#'
#' s <- star_schema()
#' d <- dimension_schema(
#'   name = "when",
#'   attributes = c(
#'     "Week Ending Date",
#'     "WEEK",
#'     "Year"
#'   )
#' )
#' s <- s |>
#'   define_dimension(d)
#' @export
define_dimension <-
  function(schema,
           dimension,
           name,
           attributes,
           scd_nk,
           scd_t0,
           scd_t1,
           scd_t2,
           scd_t3,
           scd_t6,
           is_when,
           ...)
    UseMethod("define_dimension")

#' @rdname define_dimension
#'
#' @export
define_dimension.star_schema <-
  function(schema,
           dimension = NULL,
           name = NULL,
           attributes = NULL,
           scd_nk = NULL,
           scd_t0 = NULL,
           scd_t1 = NULL,
           scd_t2 = NULL,
           scd_t3 = NULL,
           scd_t6 = NULL,
           is_when = FALSE,
           ...) {
    if (!is.null(dimension)) {
      stopifnot(
        "Schema does not include dimension_schema object." = methods::is(dimension, "dimension_schema")
      )
      stopifnot(
        "If a dimension_schema has been defined, the rest of the fields cannot be defined." = is.null(name) &
          is.null(attributes) &
          is.null(scd_nk) &
          is.null(scd_t0) &
          is.null(scd_t1) &
          is.null(scd_t2) &
          is.null(scd_t3) &
          is.null(scd_t6)
      )
    } else {
      dimension <-
        dimension_schema(
          name = name,
          attributes = attributes,
          scd_nk = scd_nk,
          scd_t0 = scd_t0,
          scd_t1 = scd_t1,
          scd_t2 = scd_t2,
          scd_t3 = scd_t3,
          scd_t6 = scd_t6,
          is_when = is_when,
          ...
        )
    }
    if (is.null(schema$dimensions)) {
      d <- list(dimension)
      names(d) <- snakecase::to_snake_case(dimension$name)
    } else {
      stopifnot(
        "The schema already contains a dimension of the same name." = !(
          snakecase::to_snake_case(dimension$name) %in% names(schema$dimensions)
        )
      )
      d <- schema$dimensions
      n <- names(d)
      d[[length(d) + 1]] <- dimension
      names(d) <- c(n, snakecase::to_snake_case(dimension$name))
    }
    structure(list(facts = schema$facts, dimensions = d), class = "star_schema")
  }

# generic
get_measure_names_schema <- function(schema) UseMethod("get_measure_names_schema")

get_attribute_names_schema <- function(schema) UseMethod("get_attribute_names_schema")


#' Get measure names
#'
#' Get the names of the measures defined in the fact schema.
#'
#' @param schema A `star_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_measure_names_schema.star_schema <- function(schema) {
  names <- NULL
  for (fact in schema$facts) {
    names <- c(names, get_measure_names_schema(fact))
  }
  unique(names)
}


#' Get attribute names
#'
#' Get the attribute names.
#'
#' @param schema A `dimension_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_attribute_names_schema.star_schema <- function(schema) {
    names <- NULL
    for (dimension in schema$dimensions) {
      names <- c(names, get_attribute_names_schema(dimension))
    }
    unique(names)
  }

## ----------------------------------------------------------------------------------------------------------

#' `dimension_schema` S3 class
#'
#' A `dimension_schema` object is created, we have to define its name and the
#' set of attributes that make it up.
#'
#' A `dimension_schema` object is part of a `star_schema` object, defines
#' a dimension of the star schema.
#'
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#' @param scd_nk A vector of attribute names, scd natural key.
#' @param scd_t0 A vector of attribute names, scd T0 attributes.
#' @param scd_t1 A vector of attribute names, scd T1 attributes.
#' @param scd_t2 A vector of attribute names, scd T2 attributes.
#' @param scd_t3 A vector of attribute names, scd T3 attributes.
#' @param scd_t6 A vector of attribute names, scd T6 attributes.
#' @param is_when A boolean, is when dimension.
#' @param ... When dimension configuration parameters.
#'
#' @return A `dimension_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' d <- dimension_schema(
#'   name = "when",
#'   attributes = c(
#'     "Week Ending Date",
#'     "WEEK",
#'     "Year"
#'   )
#' )
#'
#' @export
dimension_schema <- function(name = NULL,
                             attributes = NULL,
                             scd_nk = NULL,
                             scd_t0 = NULL,
                             scd_t1 = NULL,
                             scd_t2 = NULL,
                             scd_t3 = NULL,
                             scd_t6 = NULL,
                             is_when = FALSE,
                             ...) {
  stopifnot("Missing dimension name." = !is.null(name))
  if (!(
    length(attributes) +
    length(scd_nk) +
    length(scd_t0) +
    length(scd_t1) +
    length(scd_t2) +
    length(scd_t3) +
    length(scd_t6) > 0
  )) {
    stop(sprintf("Missing the dimension '%s' attributes.", name))
  }
  if (!(length(c(
    attributes,
    scd_nk,
    scd_t0,
    scd_t1,
    scd_t2,
    scd_t3,
    scd_t6
  )) == length(unique(
    c(attributes,
      scd_nk,
      scd_t0,
      scd_t1,
      scd_t2,
      scd_t3,
      scd_t6)
  )))) {
    stop(sprintf("There are repeated attributes in the '%s' dimension.", name))
  }
  if (length(attributes) > 0) {
    stopifnot("Generic attributes and scd components cannot be defined at the same time." = length(c(
      scd_nk, scd_t0, scd_t1, scd_t2, scd_t3, scd_t6
    )) == 0)
  }
  if (is_when) {
    dots <- list(...)
    w <- when::when()
  }
  if (length(attributes) > 0) {
    res <-   structure(
      list(
        name = name,
        attributes = attributes),
      class = "dimension_schema"
    )
  } else {
    res <-   structure(
      list(
        name = name,
        scd_nk = scd_nk,
        scd_t0 = scd_t0,
        scd_t1 = scd_t1,
        scd_t2 = scd_t2,
        scd_t3 = scd_t3,
        scd_t6 = scd_t6
      ),
      class = "dimension_schema"
    )
  }
  res
}


#' Get attribute names
#'
#' Get the attribute names.
#'
#' @param schema A `dimension_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_attribute_names_schema.dimension_schema <- function(schema) {
  if (length(schema$attributes) > 0) {
    res <- schema$attributes
  } else {
    res <-
      c(
        schema$scd_nk,
        schema$scd_t0,
        schema$scd_t1,
        schema$scd_t2,
        schema$scd_t3,
        schema$scd_t6
      )
  }
  res
}


#' Is a scd dimension
#'
#' @param schema A `dimension_schema` object.
#'
#' @return A boolean.
#'
#' @keywords internal
is_scd <- function(schema) {
  res <- !(
    is.null(schema$scd_nk) &
      is.null(schema$scd_t0) &
      is.null(schema$scd_t1) &
      is.null(schema$scd_t2) &
      is.null(schema$scd_t3) &
      is.null(schema$scd_t6)
  )
  res
}

## ----------------------------------------------------------------------------------------------------------

#' `fact_schema` S3 class
#'
#' A `fact_schema` object is created, the essential data is a name and
#' a set of measures that can be empty (does not have explicit measures).
#' It is part of a `star_schema` object, defines the facts of the star schema.
#'
#' Associated with each measure there is an aggregation function that can be
#' SUM, MAX or MIN. AVG is not considered among the possible aggregation
#' functions: The reason is that calculating AVG by considering subsets of
#' data does not necessarily yield the AVG of the total data.
#'
#' An additional measure corresponding to the COUNT of aggregated rows is added
#' which, together with SUM, allows us to obtain the AVG if needed.
#'
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names, each one for its
#'   corresponding measure. If none is indicated, the default is SUM. Additionally
#'   they can be MAX or MIN.
#' @param nrow_agg A string, name of a new measure that represents the COUNT
#'   of rows aggregated for each resulting row.
#'
#' @return A `fact_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f <- fact_schema(
#'   name = "mrs_cause",
#'   measures = c(
#'     "Pneumonia and Influenza Deaths",
#'     "Other Deaths"
#'   )
#' )
#'
#' f <- fact_schema(
#'   name = "mrs_cause",
#'   measures = c(
#'     "Pneumonia and Influenza Deaths",
#'     "Other Deaths"
#'   ),
#'   agg_functions = c(
#'     "MAX",
#'     "SUM"
#'   ),
#'   nrow_agg = "Nrow"
#' )
#'
#' @export
fact_schema <- function(name = NULL,
                        measures = NULL,
                        agg_functions = NULL,
                        nrow_agg = NULL) {
  stopifnot("Missing fact name." = !is.null(name))
  if (!is.null(agg_functions)) {
    stopifnot("Each measure must have an aggregation function." = length(measures) == length(agg_functions))
    for (agg_function in agg_functions) {
      if (!(agg_function %in% c("SUM", "MAX", "MIN"))) {
        stop(sprintf("'%s' is not one of the allowed aggregation functions (SUM, MAX and MIN).", agg_function))
      }
    }
  }
  stopifnot("There are repeated measures in the facts." = length(c(measures, nrow_agg)) == length(unique(c(measures, nrow_agg))))
  structure(
    list(
      name = name,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    ),
    class = "fact_schema"
  )
}

# generic
get_fact_name <- function(schema) UseMethod("get_fact_name")
get_agg_functions <- function(schema) UseMethod("get_agg_functions")
get_nrow_agg <- function(schema) UseMethod("get_nrow_agg")



#' Get fact name
#'
#' @param schema A `fact_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_fact_name.fact_schema <- function(schema) {
  schema$name
}


#' Get measure names
#'
#' Get the names of the measures defined in the fact schema.
#'
#' @param schema A `fact_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_measure_names_schema.fact_schema <- function(schema) {
  schema$measures
}

#' Get aggregate functions
#'
#' @param schema A `fact_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_agg_functions.fact_schema <- function(schema) {
  schema$agg_functions
}


#' Get number of rows aggregate column
#'
#' @param schema A `fact_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_nrow_agg.fact_schema <- function(schema) {
  schema$nrow_agg
}
