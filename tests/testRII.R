library(RII)

## --- rii_raw ---------------------------------------------------------
scores <- c(5, 5, 5, 5, 5)                # everyone gives the max score
r <- rii_raw(scores, scale_max = 5)
stopifnot(abs(r["rii"] - 1) < 1e-8)
stopifnot(r["n"] == 5)

scores0 <- c(1, 1, 1, 1, 1)               # everyone gives the min score
stopifnot(abs(rii_raw(scores0, scale_max = 5)["rii"] - 0.2) < 1e-8)

## rii_raw and mean/scale_max should agree
set.seed(1)
x <- sample(1:5, 40, replace = TRUE)
r2 <- rii_raw(x, scale_max = 5)
stopifnot(abs(r2["rii"] - mean(x) / 5) < 1e-8)

## out-of-range scores should error
res <- try(rii_raw(c(0, 2, 3)), silent = TRUE)
stopifnot(inherits(res, "try-error"))

## --- rii_freq ----------------------------------------------------------
## rii_freq on an expanded vector should match rii_raw on the raw scores
freq <- c(2, 3, 10, 20, 15)               # counts for scores 1..5
expanded <- rep(1:5, freq)
rf <- rii_freq(freq)
rr <- rii_raw(expanded)
stopifnot(abs(rf["rii"] - rr["rii"]) < 1e-8)
stopifnot(abs(rf["mean"] - rr["mean"]) < 1e-8)
stopifnot(abs(rf["sd"] - rr["sd"]) < 1e-6)

## --- classify_rii --------------------------------------------------------
bands <- classify_rii(c(0.95, 0.75, 0.5, 0.3, 0.1))
stopifnot(as.character(bands) ==
            c("High", "High-Medium", "Medium", "Medium-Low", "Low"))

## --- rii_table -----------------------------------------------------------
data(building_collapse_nigeria, package = "RII")
items <- building_collapse_nigeria[, 5:14]
tbl <- rii_table(items)
stopifnot(inherits(tbl, "rii_table"))
stopifnot(nrow(tbl) == ncol(items))
stopifnot(all(diff(tbl$RII) <= 0))         # sorted descending
stopifnot(tbl$Rank[1] == 1)
stopifnot(all(tbl$RII >= 0 & tbl$RII <= 1))

## rii_table should match manual rii_raw on the same column
manual <- rii_raw(items$PoorSupervision)
row <- tbl[tbl$Item == "PoorSupervision", ]
stopifnot(abs(row$RII - round(manual["rii"], 3)) < 1e-8)

## --- rii_table_freq should agree with rii_table on the same data --------
freq_tbl <- do.call(rbind, lapply(items, function(col) {
  table(factor(col, levels = 1:5))
}))
freq_tbl <- as.data.frame(freq_tbl)
names(freq_tbl) <- paste0("n", 1:5)
tbl2 <- rii_table_freq(freq_tbl)
m1 <- setNames(tbl$RII, tbl$Item)
m2 <- setNames(tbl2$RII, tbl2$Item)
m2 <- m2[names(m1)]
stopifnot(all(abs(m1 - m2) < 1e-6))

## --- plot.rii_table should run without error -----------------------------
tmp <- tempfile(fileext = ".png")
grDevices::png(tmp, width = 800, height = 600)
plot(tbl)
grDevices::dev.off()
stopifnot(file.exists(tmp))

## --- read_rii_excel: raw layout -------------------------------------------
raw_path <- system.file("extdata", "rii_raw_example.xlsx", package = "RII")
stopifnot(nzchar(raw_path))
tbl_raw_xl <- read_rii_excel(
  raw_path, layout = "raw",
  items = c("SubstandardMaterials", "PoorSupervision",
            "InadequateSoilInvestigation", "PoorWorkmanship")
)
stopifnot(inherits(tbl_raw_xl, "rii_table"))
stopifnot(nrow(tbl_raw_xl) == 4)
stopifnot(all(tbl_raw_xl$RII >= 0 & tbl_raw_xl$RII <= 1))

## should match rii_table() run directly on the same first-40-row subset
manual_xl <- rii_table(items[1:40, c("SubstandardMaterials", "PoorSupervision",
                                      "InadequateSoilInvestigation",
                                      "PoorWorkmanship")])
m1 <- setNames(tbl_raw_xl$RII, tbl_raw_xl$Item)
m2 <- setNames(manual_xl$RII, manual_xl$Item)
stopifnot(all(abs(m1 - m2[names(m1)]) < 1e-8))

## --- read_rii_excel: freq layout -------------------------------------------
freq_path <- system.file("extdata", "rii_freq_example.xlsx", package = "RII")
stopifnot(nzchar(freq_path))
tbl_freq_xl <- read_rii_excel(freq_path, layout = "freq", item_col = "Factor")
stopifnot(inherits(tbl_freq_xl, "rii_table"))
stopifnot(nrow(tbl_freq_xl) == 10)
stopifnot(all(tbl_freq_xl$RII >= 0 & tbl_freq_xl$RII <= 1))

## --- generalized scale range: 0-based scale -------------------------------
x5 <- c(5, 4, 5, 3, 5)                    # scored 1-5
x0 <- x5 - 1                              # the same responses, scored 0-4
r5 <- rii_raw(x5, scale_min = 1, scale_max = 5)
r0 <- rii_raw(x0, scale_min = 0, scale_max = 4)
## RII is computed directly from the formula on the 0-based scale (the
## value differs from the 1-based version, since the sum of weights shifts
## by n, but it must match a manual calculation of the same formula)
stopifnot(abs(r0["rii"] - sum(x0) / (4 * length(x0))) < 1e-8)

## rii_freq with 0-based weights should match rii_raw on the 0-based scores
freq0 <- as.numeric(table(factor(x0, levels = 0:4)))
rf0 <- rii_freq(freq0, weights = 0:4)
stopifnot(abs(rf0["rii"] - r0["rii"]) < 1e-8)

## a 3-point scale
x3 <- c(1, 2, 3, 3, 2, 1, 3)
r3 <- rii_raw(x3, scale_min = 1, scale_max = 3)
stopifnot(abs(r3["rii"] - mean(x3) / 3) < 1e-8)

## out-of-range scores for the *stated* scale should still error
res0 <- try(rii_raw(c(1, 2, 6), scale_min = 1, scale_max = 5), silent = TRUE)
stopifnot(inherits(res0, "try-error"))

## --- reverse_score / reverse-scoring -----------------------------------
stopifnot(identical(reverse_score(c(1, 2, 3, 4, 5)), c(5, 4, 3, 2, 1)))
stopifnot(identical(reverse_score(c(0, 1, 2, 3, 4), scale_min = 0, scale_max = 4),
                     c(4, 3, 2, 1, 0)))

## rii_raw(reverse = TRUE) should match manually reverse-scoring first
x <- c(1, 1, 2, 5, 4, 3, 2, 1)
manual_rev <- rii_raw(reverse_score(x, 1, 5))
auto_rev <- rii_raw(x, reverse = TRUE)
stopifnot(abs(manual_rev["rii"] - auto_rev["rii"]) < 1e-8)

## rii_freq(reverse = TRUE) should match rii_raw on reverse-scored expansion
freq <- c(2, 3, 10, 20, 15)
expanded <- rep(1:5, freq)
rf_rev <- rii_freq(freq, reverse = TRUE)
rr_rev <- rii_raw(reverse_score(expanded, 1, 5))
stopifnot(abs(rf_rev["rii"] - rr_rev["rii"]) < 1e-8)

## rii_table with reverse_items and a 0-based scale
items0 <- items - 1   # reuse the building_collapse_nigeria items, shifted
tbl_rev <- rii_table(items0, scale_min = 0, scale_max = 4,
                      reverse_items = "Overloading")
stopifnot(inherits(tbl_rev, "rii_table"))
row_normal <- rii_raw(items0$Overloading, scale_min = 0, scale_max = 4)
row_rev    <- rii_raw(items0$Overloading, scale_min = 0, scale_max = 4, reverse = TRUE)
tbl_row <- tbl_rev[tbl_rev$Item == "Overloading", ]
stopifnot(abs(tbl_row$RII - round(row_rev["rii"], 3)) < 1e-8)
stopifnot(abs(tbl_row$RII - round(row_normal["rii"], 3)) > 1e-6)  # actually changed

## rii_table_freq with reverse_items
freq_tbl2 <- freq_tbl
tbl_freq_rev <- rii_table_freq(freq_tbl2, reverse_items = "PoorSupervision")
base_row <- tbl2[tbl2$Item == "PoorSupervision", ]
rev_row  <- tbl_freq_rev[tbl_freq_rev$Item == "PoorSupervision", ]
stopifnot(abs(base_row$RII - rev_row$RII) > 1e-6)

## --- second bundled dataset: 4-point scale, non-construction domain ------
data(csa_adoption_barriers_nigeria, package = "RII")
csa_items <- csa_adoption_barriers_nigeria[, 6:15]
csa_tbl <- rii_table(csa_items, scale_max = 4)
stopifnot(inherits(csa_tbl, "rii_table"))
stopifnot(nrow(csa_tbl) == ncol(csa_items))
stopifnot(all(diff(csa_tbl$RII) <= 0))
stopifnot(all(csa_tbl$RII >= 0 & csa_tbl$RII <= 1))
## every raw score must actually respect the stated 1-4 range
stopifnot(all(sapply(csa_items, function(x) all(x >= 1 & x <= 4))))
## RII should match a manual computation for one item
manual_csa <- rii_raw(csa_items$HighCostOfImprovedInputs, scale_max = 4)
row_csa <- csa_tbl[csa_tbl$Item == "HighCostOfImprovedInputs", ]
stopifnot(abs(row_csa$RII - round(manual_csa["rii"], 3)) < 1e-8)

## --- write_rii_excel: round-trips and file gets created -------------------
out_path <- tempfile(fileext = ".xlsx")
write_rii_excel(tbl, out_path)
stopifnot(file.exists(out_path))
back <- openxlsx::read.xlsx(out_path, startRow = 3)
stopifnot(all(back$RII == tbl$RII))
stopifnot(all(back$Item == tbl$Item))
stopifnot(all(back$Band == tbl$Band))
unlink(out_path)

## write_rii_excel with include_chart = FALSE should still work
out_path2 <- tempfile(fileext = ".xlsx")
write_rii_excel(tbl, out_path2, include_chart = FALSE)
stopifnot(file.exists(out_path2))
unlink(out_path2)

## --- detect_likert_items / rii_table(items = "auto") ----------------------
set.seed(42)
n_resp <- 50
raw_survey <- data.frame(
  `S/N`      = 1:n_resp,
  GENDER     = sample(1:2, n_resp, replace = TRUE),
  AGE        = sample(1:4, n_resp, replace = TRUE),
  MARITAL    = sample(1:3, n_resp, replace = TRUE),
  `YR IN SER`= sample(1:6, n_resp, replace = TRUE),
  `EMP STA`  = rep(1, n_resp),                    # constant -> no variation
  OJ1        = sample(1:5, n_resp, replace = TRUE),
  OJ2        = sample(1:5, n_resp, replace = TRUE),
  PJ6        = sample(1:5, n_resp, replace = TRUE),
  JS1i       = sample(1:5, n_resp, replace = TRUE),
  stray      = c(4, rep(NA, n_resp - 1)),          # 1 real value, rest NA
  check.names = FALSE
)

scan <- detect_likert_items(raw_survey)
stopifnot(inherits(scan, "likert_item_scan"))
detected <- scan$Column[scan$Suggested]
stopifnot(setequal(detected, c("OJ1", "OJ2", "PJ6", "JS1i")))
stopifnot(!("S/N" %in% detected), !("GENDER" %in% detected),
          !("AGE" %in% detected), !("MARITAL" %in% detected),
          !("YR IN SER" %in% detected), !("EMP STA" %in% detected),
          !("stray" %in% detected))

tbl_auto <- suppressMessages(rii_table(raw_survey, items = "auto"))
stopifnot(inherits(tbl_auto, "rii_table"))
stopifnot(setequal(tbl_auto$Item, c("OJ1", "OJ2", "PJ6", "JS1i")))
## should match manually specifying the same items
tbl_manual <- rii_table(raw_survey, items = c("OJ1", "OJ2", "PJ6", "JS1i"))
stopifnot(all(sort(tbl_auto$RII) == sort(tbl_manual$RII)))

## items = "auto" should error clearly if nothing looks like an item
no_items_df <- data.frame(`S/N` = 1:10, GENDER = sample(1:2, 10, TRUE))
res_auto_empty <- try(rii_table(no_items_df, items = "auto"), silent = TRUE)
stopifnot(inherits(res_auto_empty, "try-error"))

## --- read_rii_csv: raw layout -------------------------------------------
csv_path <- tempfile(fileext = ".csv")
write.csv(building_collapse_nigeria, csv_path, row.names = FALSE)
tbl_csv <- read_rii_csv(csv_path, layout = "raw",
                         items = c("SubstandardMaterials", "PoorSupervision"))
stopifnot(inherits(tbl_csv, "rii_table"))
stopifnot(nrow(tbl_csv) == 2)
manual_csv <- rii_table(building_collapse_nigeria,
                         items = c("SubstandardMaterials", "PoorSupervision"))
m1 <- setNames(tbl_csv$RII, tbl_csv$Item)
m2 <- setNames(manual_csv$RII, manual_csv$Item)
stopifnot(all(abs(m1[names(m2)] - m2) < 1e-8))
unlink(csv_path)

## read_rii_csv with items = "auto" on a demographic+item mix
csv_auto_path <- tempfile(fileext = ".csv")
write.csv(raw_survey, csv_auto_path, row.names = FALSE)
tbl_csv_auto <- suppressMessages(read_rii_csv(csv_auto_path, layout = "raw", items = "auto"))
stopifnot(setequal(tbl_csv_auto$Item, c("OJ1", "OJ2", "PJ6", "JS1i")))
unlink(csv_auto_path)

## --- read_rii_spss: raw layout, with SPSS-labelled values -----------------
if (requireNamespace("haven", quietly = TRUE)) {
  sav_path <- tempfile(fileext = ".sav")
  d_lab <- raw_survey
  names(d_lab) <- make.names(names(d_lab))   # valid SPSS variable names
  d_lab$GENDER <- haven::labelled(d_lab$GENDER, c(Male = 1, Female = 2))
  d_lab$OJ1 <- haven::labelled(d_lab$OJ1, c(SD = 1, D = 2, N = 3, A = 4, SA = 5))
  haven::write_sav(d_lab, sav_path)

  tbl_spss <- suppressMessages(read_rii_spss(sav_path, layout = "raw", items = "auto"))
  stopifnot(inherits(tbl_spss, "rii_table"))
  stopifnot(setequal(tbl_spss$Item, c("OJ1", "OJ2", "PJ6", "JS1i")))
  ## labelled values should round-trip to the same numbers as the unlabelled data
  tbl_unlab <- rii_table(raw_survey, items = c("OJ1", "OJ2", "PJ6", "JS1i"))
  m_spss <- setNames(tbl_spss$RII, tbl_spss$Item)
  m_unlab <- setNames(tbl_unlab$RII, tbl_unlab$Item)
  stopifnot(all(abs(m_spss[names(m_unlab)] - m_unlab) < 1e-8))
  unlink(sav_path)
}

## --- recode_likert / detect_text_likert_items ------------------------------
text_survey <- data.frame(
  RespID = 1:20,
  Gender = sample(c("Male", "Female"), 20, replace = TRUE),
  Comments = sample(c("good", "fine", "could improve", "no comment"), 20, replace = TRUE),
  Q1 = sample(c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
              20, replace = TRUE),
  Q2 = sample(c("strongly disagree", " Disagree ", "NEUTRAL", "Agree", "Strongly Agree"),
              20, replace = TRUE),   # mixed case/whitespace, should still match
  stringsAsFactors = FALSE
)

scan_text <- detect_text_likert_items(text_survey)
stopifnot(inherits(scan_text, "likert_text_scan"))
stopifnot(setequal(scan_text$Column[scan_text$Suggested], c("Q1", "Q2")))
stopifnot(all(scan_text$Matched_Scale[scan_text$Column %in% c("Q1","Q2")] == "agreement5"))

## explicit cols + named built-in mapping
d_recoded <- suppressMessages(recode_likert(text_survey, cols = c("Q1", "Q2"),
                                             mapping = "agreement5"))
stopifnot(is.numeric(d_recoded$Q1), is.numeric(d_recoded$Q2))
stopifnot(all(d_recoded$Q1 >= 1 & d_recoded$Q1 <= 5))
## case/whitespace variants in Q2 must map to the same values as clean Q1 labels
stopifnot(all(d_recoded$Q2 >= 1 & d_recoded$Q2 <= 5))
stopifnot(identical(attr(d_recoded, "rii_recoded_items"), c("Q1", "Q2")))

## auto cols (cols = NULL) should detect the same two columns
d_auto <- suppressMessages(recode_likert(text_survey))
stopifnot(setequal(attr(d_auto, "rii_recoded_items"), c("Q1", "Q2")))

## rii_table(items = "auto") should pick up the recoded columns via the attribute
tbl_text <- suppressMessages(rii_table(d_auto, items = "auto"))
stopifnot(setequal(tbl_text$Item, c("Q1", "Q2")))

## unmapped value should error by default, and convert to NA with allow_na=TRUE
bad_survey <- data.frame(Q1 = c("Strongly agree", "Somewhat agree", "Disagree"))
res_bad <- try(recode_likert(bad_survey, cols = "Q1", mapping = "agreement5"), silent = TRUE)
stopifnot(inherits(res_bad, "try-error"))
d_na <- suppressWarnings(recode_likert(bad_survey, cols = "Q1", mapping = "agreement5",
                                        allow_na = TRUE))
stopifnot(sum(is.na(d_na$Q1)) == 1)

## custom mapping (not a built-in name)
custom_survey <- data.frame(Q1 = c("no", "maybe", "yes", "yes"))
d_custom <- suppressMessages(recode_likert(custom_survey, cols = "Q1",
                                            mapping = c("no" = 1, "maybe" = 2, "yes" = 3)))
stopifnot(identical(d_custom$Q1, c(1, 2, 3, 3)))

## --- read_rii_csv with recode_text = TRUE (one-line text-Likert workflow) -
csv_text_path <- tempfile(fileext = ".csv")
write.csv(text_survey, csv_text_path, row.names = FALSE)
tbl_one_line <- suppressMessages(
  read_rii_csv(csv_text_path, layout = "raw", items = "auto", recode_text = TRUE)
)
stopifnot(inherits(tbl_one_line, "rii_table"))
stopifnot(setequal(tbl_one_line$Item, c("Q1", "Q2")))
unlink(csv_text_path)

## --- detect_likert_items with a "Q1_DescriptiveName" naming convention ----
## (item_pattern must match a code PREFIX, not require an exact match,
## since real column names often have descriptive text after the code)
q_style_survey <- data.frame(
  RespID = 1:25, Timestamp = Sys.time() + 1:25,
  Gender = sample(1:2, 25, TRUE), Gender_txt = sample(c("Male","Female"), 25, TRUE),
  AgeGroup = sample(1:4, 25, TRUE), Level = sample(1:4, 25, TRUE),
  CGPA = round(runif(25, 2, 5), 2), TimeSpent = round(runif(25, 1, 8), 1),
  Q1_NotifyDuringLecture = sample(1:5, 25, TRUE),
  Q2_ReducesStudyTime    = sample(1:5, 25, TRUE),
  Q3_StaysUpLate         = sample(1:5, 25, TRUE),
  Q4_AcademicUse         = sample(1:5, 25, TRUE),
  Q5_DelayedAssignments  = sample(1:5, 25, TRUE)
)
scan_q <- detect_likert_items(q_style_survey)
detected_q <- scan_q$Column[scan_q$Suggested]
stopifnot(setequal(detected_q, c("Q1_NotifyDuringLecture", "Q2_ReducesStudyTime",
                                  "Q3_StaysUpLate", "Q4_AcademicUse",
                                  "Q5_DelayedAssignments")))
stopifnot(!any(c("RespID", "Gender", "AgeGroup", "Level", "CGPA", "TimeSpent") %in% detected_q))
tbl_q <- suppressMessages(rii_table(q_style_survey, items = "auto"))
stopifnot(nrow(tbl_q) == 5)

## --- items as a numeric column range (e.g. one section/construct) --------
multi_construct <- data.frame(
  RespID = 1:40,
  Section_A_Q1 = sample(1:5, 40, TRUE), Section_A_Q2 = sample(1:5, 40, TRUE),
  Section_A_Q3 = sample(1:5, 40, TRUE),
  Section_B_Q1 = sample(1:5, 40, TRUE), Section_B_Q2 = sample(1:5, 40, TRUE)
)
tbl_range <- rii_table(multi_construct, items = 2:4)
stopifnot(setequal(tbl_range$Item, c("Section_A_Q1", "Section_A_Q2", "Section_A_Q3")))
## should match specifying the same columns by name
tbl_range_named <- rii_table(multi_construct,
                              items = c("Section_A_Q1", "Section_A_Q2", "Section_A_Q3"))
stopifnot(all(sort(tbl_range$RII) == sort(tbl_range_named$RII)))

## a separate rii_table() call per construct/section is the way to get
## independent rankings for each section of a longer questionnaire
tbl_section_a <- rii_table(multi_construct, items = 2:4)
tbl_section_b <- rii_table(multi_construct, items = 5:6)
stopifnot(nrow(tbl_section_a) == 3, nrow(tbl_section_b) == 2)
stopifnot(!any(tbl_section_b$Item %in% tbl_section_a$Item))

## out-of-range / non-integer numeric items should error clearly
res_bad_range <- try(rii_table(multi_construct, items = 1:100), silent = TRUE)
stopifnot(inherits(res_bad_range, "try-error"))
res_bad_range2 <- try(rii_table(multi_construct, items = c(1.5, 2)), silent = TRUE)
stopifnot(inherits(res_bad_range2, "try-error"))

cat("All RII package tests passed.\n")
