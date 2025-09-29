# Question 5: Standalone Version
# Dictionary-based sentiment analysis

# Load only what we need
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)

# Clear workspace
rm(list = ls())
gc()

csv_path <- "/Users/zhaoshuti/Desktop/TAT/problem-set-01-2025-ShutiZhao/midterm_candidates_labeled_all_May05.csv"
stopifnot(file.exists(csv_path))

# Read header to get column names
hdr <- names(read.csv(csv_path, nrows = 0, check.names = FALSE))

# We only need these two columns
want_cols <- c("text","party_clean")
stopifnot(all(want_cols %in% hdr))
col_idx <- match(want_cols, hdr)

# Helper to read a small block
read_block <- function(skip, n) {
  blk <- try(
    read.csv(csv_path,
             header = FALSE,
             skip = skip + 1,
             nrows = n,
             check.names = FALSE,
             col.names = hdr,
             stringsAsFactors = FALSE),
    silent = TRUE
  )
  if (inherits(blk, "try-error")) return(NULL)
  if (!nrow(blk)) return(NULL)
  blk[want_cols]
}

# Stream the file in small blocks
need_per <- 5
skip <- 0
step <- 1000

dem_rows <- list()
rep_rows <- list()

repeat {
  blk <- read_block(skip, step)
  if (is.null(blk)) break

  # Minimal cleaning
  blk$text <- tolower(trimws(as.character(blk$text)))
  ok <- !is.na(blk$text) & nzchar(blk$text)
  blk <- blk[ok, , drop = FALSE]

  # Detect parties
  is_dem <- grepl("dem", tolower(blk$party_clean))
  is_rep <- grepl("rep|gop", tolower(blk$party_clean))

  if (sum(is_dem) && length(dem_rows) < need_per)
    dem_rows[[length(dem_rows) + 1]] <- blk[is_dem, , drop = FALSE]
  if (sum(is_rep) && length(rep_rows) < need_per)
    rep_rows[[length(rep_rows) + 1]] <- blk[is_rep, , drop = FALSE]

  got_dem <- sum(vapply(dem_rows, nrow, 0L))
  got_rep <- sum(vapply(rep_rows, nrow, 0L))
  if (got_dem >= need_per && got_rep >= need_per) break

  skip <- skip + step
  if ((skip / step) %% 2 == 0) gc()
}

dem <- if (length(dem_rows)) do.call(rbind, dem_rows) else data.frame(text=character(),party_clean=character())
rep <- if (length(rep_rows)) do.call(rbind, rep_rows) else data.frame(text=character(),party_clean=character())

# Take first N of each
dem <- head(dem, need_per)
rep <- head(rep, need_per)

stopifnot(nrow(dem) + nrow(rep) > 0)
sdat <- rbind(dem, rep)
party2 <- ifelse(grepl("dem", tolower(sdat$party_clean)), "Democrat", "Republican")

# Tiny hand dictionary
POS <- c("good","great","proud","honor","support","win","wins","victory","thank","thanks",
         "happy","congrats","congratulations","progress","safe","safety","jobs","opportunity",
         "affordable","strong","leadership","secure","freedom")
NEG <- c("bad","fail","failure","crisis","crime","corrupt","corruption","wrong","broken",
         "inflation","taxes","lie","lies","liar","attack","attacks","illegal","ban","banned",
         "danger","unsafe","costly","chaos","problem","problems")

# Ultra-light tokenizer + scorer
clean_tokens <- function(s) {
  s <- gsub("https?://\\S+|www\\S+", " ", s, perl = TRUE)
  s <- gsub("@\\w+|#\\w+|&amp;|&gt;|&lt;", " ", s, perl = TRUE)
  s <- gsub("[^a-z]+", " ", s, perl = TRUE)
  strsplit(trimws(s), " +", fixed = FALSE)[[1]]
}

score_one <- function(s) {
  w <- clean_tokens(s)
  if (!length(w)) return("Neutral")
  p <- sum(w %in% POS); n <- sum(w %in% NEG)
  if (p > n) "Positive" else if (n > p) "Negative" else "Neutral"
}

sentiment <- vapply(sdat$text, score_one, character(1L))
res <- data.frame(party2 = party2, sentiment = sentiment, stringsAsFactors = FALSE)

cat("Overall sentiment (first", need_per, "per party found via streaming):\n")
print(table(res$sentiment))

cat("\nSentiment by party:\n")
print(table(res$party2, res$sentiment))

# Optional peek
utils::head(cbind(text_preview = substr(sdat$text, 1, 80), res), 6)

# ===== NEW CHUNKS FROM YOUR QMD FILE =====

# Attach labels to each document
labeled_q5 <- data.frame(
  party = party2,
  text  = sdat$text,
  sentiment = sentiment,
  stringsAsFactors = FALSE
)

# Peek
cat("\nLabeled documents:\n")
utils::head(labeled_q5, 8)

# Save a tiny labeled sample for your repo / Quarto
out_csv <- "q5_labeled_sample.csv"
write.csv(labeled_q5, out_csv, row.names = FALSE)
cat("Saved labeled sample to:", normalizePath(out_csv), "\n")

# Overall class prevalence
overall_tab <- table(labeled_q5$sentiment)

# By-party prevalence
by_party_tab <- table(labeled_q5$party, labeled_q5$sentiment)

cat("\nOverall sentiment distribution:\n")
print(overall_tab)

cat("\nSentiment by party:\n")
print(by_party_tab)

# ---- Base R plots (no extra packages) ----
# Overall
png("q5_sentiment_overall.png", width = 900, height = 560)
par(mar = c(5,5,3,1))
barplot(overall_tab,
        col = "grey70",
        main = "Q5: Sentiment prevalence (overall)",
        ylab = "Number of posts",
        xlab = "Sentiment")
box()
dev.off()

# By party (stacked bars)
png("q5_sentiment_by_party.png", width = 1000, height = 560)
par(mar = c(5,5,3,1))
barplot(by_party_tab,
        beside = FALSE, # stacked
        col = c("grey60","grey85"),
        main = "Q5: Sentiment prevalence by party (stacked)",
        ylab = "Number of posts",
        xlab = "Sentiment")
legend("topright", legend = rownames(by_party_tab),
       fill = c("grey60","grey85"), bty = "n")
box()
dev.off()

cat("Saved plots: q5_sentiment_overall.png, q5_sentiment_by_party.png\n")

# Summary of results
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("QUESTION 5 COMPLETE RESULTS SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("Sample size:", nrow(labeled_q5), "documents\n")
cat("Parties:", paste(unique(labeled_q5$party), collapse = ", "), "\n")
cat("Sentiments:", paste(unique(labeled_q5$sentiment), collapse = ", "), "\n")
cat("Files created:\n")
cat("  - q5_labeled_sample.csv (labeled data)\n")
cat("  - q5_sentiment_overall.png (overall sentiment plot)\n")
cat("  - q5_sentiment_by_party.png (sentiment by party plot)\n")
