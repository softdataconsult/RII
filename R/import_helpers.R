# Internal helper shared by read_rii_excel(), read_rii_csv() and
# read_rii_spss(): once each has turned its source file into a plain data
# frame `d`, this optionally recodes text Likert labels, then does the
# common "raw vs freq" dispatch into rii_table()/rii_table_freq(). Not
# exported.
.rii_from_dataframe <- function(d, layout, items, item_col, scale_max,
                                 scale_min, reverse_items, weights,
                                 recode_text = FALSE) {
  if (recode_text && layout == "raw") {
    d <- recode_likert(d)
  }

  if (layout == "raw") {
    if (is.null(items)) {
      items <- names(d)[vapply(d, is.numeric, logical(1))]
      if (length(items) == 0) {
        stop("No numeric columns found to treat as Likert items. ",
             "Pass 'items' explicitly (or items = \"auto\" to have ",
             "detect_likert_items() pick them).", call. = FALSE)
      }
    }
    return(rii_table(d, items = items, scale_max = scale_max,
                      scale_min = scale_min, reverse_items = reverse_items))
  }

  # layout == "freq"
  if (is.null(item_col)) {
    is_num <- vapply(d, is.numeric, logical(1))
    if (all(is_num)) {
      stop("Could not find a non-numeric item-label column; pass ",
           "'item_col' explicitly.", call. = FALSE)
    }
    item_col <- names(d)[!is_num][1]
  }
  rii_table_freq(d, weights = weights, item_col = item_col,
                 reverse_items = reverse_items)
}
