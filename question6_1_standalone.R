# Question 6.1: Standalone Version (New Answer)
# Cosine similarity and distance analysis using quanteda approach

# Load only essential packages
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)

# Clear workspace
rm(list = ls())
gc()

# Load the tiny labeled set from Q5
if (!file.exists("q5_labeled_sample.csv")) {
  stop("Please run question5_standalone.R first to create q5_labeled_sample.csv")
}

small <- read.csv("q5_labeled_sample.csv", stringsAsFactors = FALSE)
small$doc_id <- 1:nrow(small)
small$text <- as.character(small$text)

cat("Loaded", nrow(small), "documents from Q5 analysis\n")

# Simple tokenization function (replaces quanteda)
simple_tokenize <- function(text) {
  # Clean text
  text <- tolower(text)
  text <- gsub("https?://\\S+|www\\S+", " ", text, perl = TRUE)
  text <- gsub("@\\w+|&amp;|&gt;|&lt;", " ", text, perl = TRUE)
  text <- gsub("[^a-z]+", " ", text, perl = TRUE)
  
  # Split into words
  words <- strsplit(text, "\\s+")[[1]]
  words <- words[nchar(words) >= 3]  # Keep words 3+ chars
  
  # Remove common stop words
  stop_words <- c("the", "and", "for", "are", "but", "not", "you", "all", "can", "had", "her", "was", "one", "our", "out", "day", "get", "has", "him", "his", "how", "its", "may", "new", "now", "old", "see", "two", "way", "who", "boy", "did", "man", "oil", "sit", "try", "use", "she", "this", "that", "with", "have", "will", "your", "from", "they", "know", "want", "been", "good", "much", "some", "time", "very", "when", "come", "here", "just", "like", "long", "make", "many", "over", "such", "take", "than", "them", "well", "were", "what", "where", "which", "while", "would", "could", "should", "might", "must", "shall", "will", "would", "about", "above", "after", "again", "against", "before", "below", "between", "during", "except", "inside", "outside", "through", "under", "until", "within", "without")
  
  words[!words %in% stop_words]
}

# Create word-document matrix (replaces quanteda dfm)
all_words <- unique(unlist(lapply(small$text, simple_tokenize)))
cat("Vocabulary size:", length(all_words), "words\n")

# Create document-term matrix
doc_term_matrix <- matrix(0, nrow = nrow(small), ncol = length(all_words))
colnames(doc_term_matrix) <- all_words

for (i in 1:nrow(small)) {
  words <- simple_tokenize(small$text[i])
  if (length(words) > 0) {
    word_counts <- table(words)
    doc_term_matrix[i, names(word_counts)] <- as.numeric(word_counts)
  }
}

# Trim rare terms (min_docfreq = 2)
term_freqs <- colSums(doc_term_matrix > 0)
keep_terms <- term_freqs >= 2
doc_term_matrix <- doc_term_matrix[, keep_terms]
cat("After trimming rare terms:", ncol(doc_term_matrix), "words\n")

# Choose reference documents (first 3)
ref_ids <- 1:min(3, nrow(small))
cat("Reference documents:", paste(ref_ids, collapse = ", "), "\n\n")

# Cosine similarity function
cosine_similarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)
  norm1 <- sqrt(sum(vec1^2))
  norm2 <- sqrt(sum(vec2^2))
  
  if (norm1 == 0 || norm2 == 0) return(0)
  dot_product / (norm1 * norm2)
}

# Jaccard similarity function
jaccard_similarity <- function(vec1, vec2) {
  set1 <- which(vec1 > 0)
  set2 <- which(vec2 > 0)
  
  if (length(set1) == 0 && length(set2) == 0) return(1)
  if (length(set1) == 0 || length(set2) == 0) return(0)
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  intersection / union
}

# Manhattan distance function
manhattan_distance <- function(vec1, vec2) {
  sum(abs(vec1 - vec2))
}

# Euclidean distance function
euclidean_distance <- function(vec1, vec2) {
  sqrt(sum((vec1 - vec2)^2))
}

# Find nearest neighbors for each reference document
find_neighbors <- function(ref_id, top_k = 10) {
  similarities_cosine <- numeric(nrow(small))
  similarities_jaccard <- numeric(nrow(small))
  distances_manhattan <- numeric(nrow(small))
  distances_euclidean <- numeric(nrow(small))
  
  for (j in 1:nrow(small)) {
    if (j != ref_id) {
      similarities_cosine[j] <- cosine_similarity(doc_term_matrix[ref_id, ], doc_term_matrix[j, ])
      similarities_jaccard[j] <- jaccard_similarity(doc_term_matrix[ref_id, ], doc_term_matrix[j, ])
      distances_manhattan[j] <- manhattan_distance(doc_term_matrix[ref_id, ], doc_term_matrix[j, ])
      distances_euclidean[j] <- euclidean_distance(doc_term_matrix[ref_id, ], doc_term_matrix[j, ])
    }
  }
  
  # Get top neighbors (highest similarity, lowest distance)
  cosine_neighbors <- order(similarities_cosine, decreasing = TRUE)[1:top_k]
  jaccard_neighbors <- order(similarities_jaccard, decreasing = TRUE)[1:top_k]
  manhattan_neighbors <- order(distances_manhattan, decreasing = FALSE)[1:top_k]
  euclidean_neighbors <- order(distances_euclidean, decreasing = FALSE)[1:top_k]
  
  list(
    cosine = cosine_neighbors,
    jaccard = jaccard_neighbors,
    manhattan = manhattan_neighbors,
    euclidean = euclidean_neighbors,
    cosine_scores = similarities_cosine[cosine_neighbors],
    jaccard_scores = similarities_jaccard[jaccard_neighbors],
    manhattan_scores = distances_manhattan[manhattan_neighbors],
    euclidean_scores = distances_euclidean[euclidean_neighbors]
  )
}

# Analyze each reference document
results <- list()

for (i in ref_ids) {
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat("REFERENCE DOCUMENT", i, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat("Text:", substr(small$text[i], 1, 150), "...\n")
  cat("Party:", small$party[i], "\n")
  cat("Sentiment:", small$sentiment[i], "\n\n")
  
  neighbors <- find_neighbors(i, 10)
  
  cat("TOP 10 NEAREST NEIGHBORS BY COSINE SIMILARITY:\n")
  cat(paste(rep("-", 50), collapse=""), "\n")
  for (j in 1:10) {
    neighbor_id <- neighbors$cosine[j]
    score <- neighbors$cosine_scores[j]
    cat(sprintf("%2d. Doc %d (score: %.4f)\n", j, neighbor_id, score))
    cat("    Text:", substr(small$text[neighbor_id], 1, 100), "...\n")
    cat("    Party:", small$party[neighbor_id], "| Sentiment:", small$sentiment[neighbor_id], "\n\n")
  }
  
  cat("TOP 10 NEAREST NEIGHBORS BY JACCARD SIMILARITY:\n")
  cat(paste(rep("-", 50), collapse=""), "\n")
  for (j in 1:10) {
    neighbor_id <- neighbors$jaccard[j]
    score <- neighbors$jaccard_scores[j]
    cat(sprintf("%2d. Doc %d (score: %.4f)\n", j, neighbor_id, score))
    cat("    Text:", substr(small$text[neighbor_id], 1, 100), "...\n")
    cat("    Party:", small$party[neighbor_id], "| Sentiment:", small$sentiment[neighbor_id], "\n\n")
  }
  
  cat("TOP 10 NEAREST NEIGHBORS BY MANHATTAN DISTANCE:\n")
  cat(paste(rep("-", 50), collapse=""), "\n")
  for (j in 1:10) {
    neighbor_id <- neighbors$manhattan[j]
    score <- neighbors$manhattan_scores[j]
    cat(sprintf("%2d. Doc %d (distance: %.2f)\n", j, neighbor_id, score))
    cat("    Text:", substr(small$text[neighbor_id], 1, 100), "...\n")
    cat("    Party:", small$party[neighbor_id], "| Sentiment:", small$sentiment[neighbor_id], "\n\n")
  }
  
  cat("TOP 10 NEAREST NEIGHBORS BY EUCLIDEAN DISTANCE:\n")
  cat(paste(rep("-", 50), collapse=""), "\n")
  for (j in 1:10) {
    neighbor_id <- neighbors$euclidean[j]
    score <- neighbors$euclidean_scores[j]
    cat(sprintf("%2d. Doc %d (distance: %.2f)\n", j, neighbor_id, score))
    cat("    Text:", substr(small$text[neighbor_id], 1, 100), "...\n")
    cat("    Party:", small$party[neighbor_id], "| Sentiment:", small$sentiment[neighbor_id], "\n\n")
  }
  
  results[[i]] <- neighbors
}

# Summary analysis
cat(paste(rep("=", 70), collapse=""), "\n")
cat("SUMMARY ANALYSIS: COMPARING SIMILARITY/DISTANCE METHODS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Compare methods
cosine_agreement <- 0
jaccard_agreement <- 0
manhattan_agreement <- 0
euclidean_agreement <- 0
total_comparisons <- 0

for (i in ref_ids) {
  cosine_top5 <- results[[i]]$cosine[1:5]
  jaccard_top5 <- results[[i]]$jaccard[1:5]
  manhattan_top5 <- results[[i]]$manhattan[1:5]
  euclidean_top5 <- results[[i]]$euclidean[1:5]
  
  cosine_agreement <- cosine_agreement + length(intersect(cosine_top5, jaccard_top5))
  jaccard_agreement <- jaccard_agreement + length(intersect(jaccard_top5, manhattan_top5))
  manhattan_agreement <- manhattan_agreement + length(intersect(manhattan_top5, euclidean_top5))
  euclidean_agreement <- euclidean_agreement + length(intersect(euclidean_top5, cosine_top5))
  total_comparisons <- total_comparisons + 5
}

cat("METHOD AGREEMENT RATES (top 5 neighbors):\n")
cat("• Cosine vs Jaccard:", round(cosine_agreement / total_comparisons * 100, 1), "%\n")
cat("• Jaccard vs Manhattan:", round(jaccard_agreement / total_comparisons * 100, 1), "%\n")
cat("• Manhattan vs Euclidean:", round(manhattan_agreement / total_comparisons * 100, 1), "%\n")
cat("• Euclidean vs Cosine:", round(euclidean_agreement / total_comparisons * 100, 1), "%\n\n")

# Party consistency analysis
cat("PARTY CONSISTENCY ANALYSIS:\n")
cat(paste(rep("-", 40), collapse=""), "\n")
for (i in ref_ids) {
  ref_party <- small$party[i]
  
  cosine_parties <- small$party[results[[i]]$cosine[1:5]]
  jaccard_parties <- small$party[results[[i]]$jaccard[1:5]]
  manhattan_parties <- small$party[results[[i]]$manhattan[1:5]]
  euclidean_parties <- small$party[results[[i]]$euclidean[1:5]]
  
  cosine_consistency <- sum(cosine_parties == ref_party) / 5
  jaccard_consistency <- sum(jaccard_parties == ref_party) / 5
  manhattan_consistency <- sum(manhattan_parties == ref_party) / 5
  euclidean_consistency <- sum(euclidean_parties == ref_party) / 5
  
  cat(sprintf("Doc %d (%s):\n", i, ref_party))
  cat(sprintf("  Cosine: %.1f%% | Jaccard: %.1f%% | Manhattan: %.1f%% | Euclidean: %.1f%%\n", 
              cosine_consistency * 100, jaccard_consistency * 100, 
              manhattan_consistency * 100, euclidean_consistency * 100))
}

cat("\nQUESTION 6.1 ANALYSIS COMPLETE!\n")
cat("Files created: None (results printed to console)\n")
