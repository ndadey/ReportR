#' Ordering key for grade labels
#'
#' Converts common grade labels (PK, K, numeric grades, etc.) into a numeric
#' key used for sorting.
#'
#' @param g A vector of grade labels (character, integer, or factor).
#'
#' @return A numeric vector. Unknown labels map to `Inf` so they sort last.
#' @export
#' @examples
#' grade_key(c("PK", "K", "3", "Grade 5", "8"))
grade_key <- function(g) {
  g <- toupper(trimws(as.character(g)))

  g[g %in% c("PK", "PREK", "PRE-K")]        <- "-1"
  g[g %in% c("K", "KG", "KINDERGARTEN")]     <- "0"

  # Pull leading number (handles "3", "03", "Grade 3", "3rd", etc.)
  num <- suppressWarnings(
    as.numeric(sub("^.*?(-?\\d+).*?$", "\\1", g))
  )
  num[is.na(num)] <- Inf
  num
}
