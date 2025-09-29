#!/usr/bin/env Rscript
# Question 6.2 Standalone Analysis
# Qualitative document analysis with top features, TF-IDF, and keywords-in-context

# Load required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readr, dplyr, stringr, quanteda, quanteda.textstats, tibble)

cat("Question 6.2: Qualitative Document Analysis\n")
cat("==========================================\n\n")

# Check if input file exists
if (!file.exists("q5_labeled_sample.csv")) {
  stop("Error: q5_labeled_sample.csv not found. Please run question5_standalone.R first.")
}

# Load the labeled data
cat("Loading labeled data...\n")
lab <- readr::read_csv("q5_labeled_sample.csv", show_col_types = FALSE)
cat("Loaded", nrow(lab), "documents\n")
cat("Columns:", paste(names(lab), collapse = ", "), "\n\n")

# Verify sentiment column exists
if (!"sentiment" %in% names(lab)) {
  stop("Error: 'sentiment' column not found in the data")
}

# Build a compact document-feature matrix
cat("Building document-feature matrix...\n")
corp <- quanteda::corpus(lab, text_field = "text")
toks <- quanteda::tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) |>
  quanteda::tokens_tolower() |>
  quanteda::tokens_remove(quanteda::stopwords("en"))

dfm_all <- quanteda::dfm(toks) |>
  quanteda::dfm_trim(min_termfreq = 2)  # drop singletons to reduce noise

cat("DFM created with", nfeat(dfm_all), "features and", ndoc(dfm_all), "documents\n\n")

# 1) Top features overall
cat("TOP FEATURES OVERALL:\n")
cat("====================\n")
tf_overall <- quanteda.textstats::textstat_frequency(dfm_all, n = 20)
print(tf_overall)
cat("\n")

# 2) Top features by sentiment class
cat("TOP FEATURES BY SENTIMENT:\n")
cat("==========================\n")
tf_by_sent <- quanteda.textstats::textstat_frequency(
  dfm_all, groups = lab$sentiment, n = 20
)
print(tf_by_sent)
cat("\n")

# 3) High TF-IDF terms (discriminative tokens) by sentiment class
cat("HIGH TF-IDF TERMS BY SENTIMENT:\n")
cat("===============================\n")
dfm_grouped <- quanteda::dfm_group(dfm_all, groups = lab$sentiment)
dfm_tfidf   <- quanteda::dfm_tfidf(dfm_grouped)

tfidf_top <- lapply(rownames(dfm_tfidf), function(g) {
  tibble::tibble(
    sentiment = g,
    term  = quanteda::featnames(dfm_tfidf),
    tfidf = as.numeric(dfm_tfidf[g, ])
  ) |>
    dplyr::arrange(dplyr::desc(tfidf)) |>
    dplyr::slice_head(n = 15)
})
tfidf_top <- dplyr::bind_rows(tfidf_top)
print(tfidf_top)
cat("\n")

# 4) Keywords-in-Context for politically-salient terms
cat("KEYWORDS-IN-CONTEXT ANALYSIS:\n")
cat("=============================\n")
kwic_terms <- c("crime","inflation","vote","support","thank","border","abortion")
kwic_list <- lapply(kwic_terms, function(w) {
  out <- try(quanteda::kwic(toks, pattern = w, window = 6), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out) || nrow(out)==0) return(NULL)
  out$keyword <- w
  out
})
kwic_tbl <- dplyr::bind_rows(Filter(Negate(is.null), kwic_list))

if (!is.null(kwic_tbl) && nrow(kwic_tbl) > 0) {
  print(kwic_tbl)
} else {
  cat("No keywords-in-context found for the specified terms.\n")
}
cat("\n")

# 5) Save results to CSV files
cat("Saving results to CSV files...\n")
readr::write_csv(tf_overall, "q6_2_top_features_overall.csv")
readr::write_csv(tf_by_sent, "q6_2_top_features_by_sentiment.csv")
readr::write_csv(tfidf_top, "q6_2_tfidf_top_by_sentiment.csv")
if (!is.null(kwic_tbl) && nrow(kwic_tbl) > 0) {
  readr::write_csv(kwic_tbl, "q6_2_kwic_examples.csv")
  cat("Saved: q6_2_kwic_examples.csv\n")
}

cat("Saved: q6_2_top_features_overall.csv\n")
cat("Saved: q6_2_top_features_by_sentiment.csv\n")
cat("Saved: q6_2_tfidf_top_by_sentiment.csv\n\n")

# 6) Summary and recommendations
cat("ANALYSIS SUMMARY:\n")
cat("================\n")
cat("• Total documents analyzed:", nrow(lab), "\n")
cat("• Sentiment distribution:\n")
print(table(lab$sentiment))
cat("\n")
cat("• Vocabulary size after trimming:", nfeat(dfm_all), "\n")
cat("• Most frequent terms overall:", paste(head(tf_overall$feature, 5), collapse = ", "), "\n\n")

cat("DICTIONARY RECOMMENDATIONS:\n")
cat("===========================\n")
cat("Based on the qualitative analysis, consider adding these terms to your sentiment dictionary:\n\n")

# Extract top terms by sentiment for recommendations
pos_terms <- tf_by_sent %>% 
  filter(group == "Positive") %>% 
  slice_head(n = 5) %>% 
  pull(feature)
neg_terms <- tf_by_sent %>% 
  filter(group == "Negative") %>% 
  slice_head(n = 5) %>% 
  pull(feature)

cat("Positive sentiment terms:", paste(pos_terms, collapse = ", "), "\n")
cat("Negative sentiment terms:", paste(neg_terms, collapse = ", "), "\n\n")

cat("Additional recommendations:\n")
cat("• Add context-specific terms: 'fraud', 'devastating', 'blessed'\n")
cat("• Include campaign terms: 'volunteer', 'team', 'anniversary'\n")
cat("• Consider negation handling: 'isn't', 'won't', 'can't'\n")
cat("• Add domain-specific terms: 'hurricane', 'disaster', 'relief'\n")
cat("• Include political action terms: 'fight', 'win', 'defeat'\n\n")

cat("Analysis complete! Check the generated CSV files for detailed results.\n")
