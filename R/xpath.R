#' XPath node-set intersection
#'
#' Finds XPath nodes that are between two node-sets of the same kind using the
#' Kayessian method for node-set intersection.
#'
#' @details
#' The Kayessian XPath expression is:
#'
#' `$ns1[count(.|$ns2) = count($ns2)]`
#'
#' Where `$ns1` and `$ns2` are the node-set 1 and node-set 2 respectively.
#'
#' If `end` is NULL, the next sibling node of the same type of the starting
#' node-set will be used.
#'
#' @param start a string containing the XPath element used as starting point
#' @param end a string containing the XPath element used as end point.
#'
#' @returns a string representing the resulting XPath expression
#'
#' @examples
#' start <- "//table/tr"
#' end <- "//table/tr/td"
#' xpath_intersect_nodesets(start, end)
#'
#' @export
xpath_intersect_nodesets <- function(start, end = NULL) {
  if (is.null(end)) {
    element <- stringr::str_extract(start, "^[A-Za-z0-9]+")
    end <- glue::glue("{start}/following-sibling::{element}[1]")
  }

  ns1 <- glue::glue("//{start}/following-sibling::node()")
  ns2 <- glue::glue("//{end}/preceding-sibling::node()")

  glue::glue("{ns1}[count(.|{ns2}) = count({ns2})]")
}

#' Exclude XPath attributes from a node-set
#'
#' Filters out nodes that have certain attributes from a node-set.
#'
#' @param attrs character vector of XPath attributes to exclude
#'
#' @returns XPath expression to filter out nodes with specified attributes
#'
#' @examples
#' xpath_exclude_attrs(c("class", "id"))
#'
#' @export
xpath_exclude_attrs <- function(attrs) {
  filter_attrs(attrs = attrs, negate = TRUE)
}

#' Include XPath attributes from a node-set
#'
#' Creates an XPath expression to filter nodes that have the specified
#' attributes.
#'
#' @param attrs a character vector of XPath attributes to include
#'
#' @returns a string representing the resulting XPath expression#'
#'
#' @examples
#' # Filter out `class` and `id` attributes from a node-set
#' xpath_include_attrs(c("class", "id"))
#'
#' @export
xpath_include_attrs <- function(attrs) {
  filter_attrs(attrs = attrs, negate = FALSE)
}

#' Build a filter for XPath attributes
#'
#' Creates an XPath expression to filter nodes based on the presence or
#' absence of certain attributes.
#'
#' @param attrs a character vector of XPath attributes.
#'
#' @param negate a logical value indicating whether to filter out nodes
#'        that contain or exclude the specified attributes. If `TRUE`,
#'        the expression will exclude nodes with the specified attributes.
#'        If `FALSE`, the expression will only return nodes with the
#'        specified attributes.
#'
#' @returns a string representing the resulting XPath expression.
#'
#' @noRd
filter_attrs <- function(attrs, negate) {

  str <- stringr::str_c("contains(., '", attrs, "')", collapse = " and ")

  if (negate)
    str <- stringr::str_c("not(", str, ")")

  return(str)
}

#' Get index for a certain XPath node
#'
#' Creates XPath expression that returns the index of a specific node within
#' a node-set.
#'
#' @param root A string containing a full XPath that serves as the root for the
#'        `nodeset`.
#'
#' @param nodeset A string vector containing a XPath nodeset. The index will be
#'        calculated based on the order of the nodes in this nodeset.
#'
#' @returns A string representing the XPath expression for the index.
#'
#' @examples
#' root <- "//table/tr"
#' nodeset <- "//table/tr/td"
#' xpath_get_index(root, nodeset)
#'
#' @export
xpath_get_index <- function(root, nodeset) {

  nodeset <- glue::glue("/{nodeset}")

  glue::glue("count({root}{nodeset}/preceding-sibling::*)")
}

#' Creates an XPath expression that checks if a node contains any of the
#' specified tags.
#'
#' @param tags A character vector of tags to check for.
#'
#' @return A string representing the XPath expression.
#'
#' @examples
#' xpath_contains(c("class", "id"))
#'
#' @export
xpath_contains <- function(tags) {
  glue::glue_collapse(glue::glue('contains(., "{tags}")'), sep = " or ")
}
