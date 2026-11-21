# Project Title: An NLP-Based Analysis of Risk Disclosures Across Industries (2021–2025)

# Submitted by: Swetha Rani Keshava Ravi
# Subject: DSCI 784 Project Capstone

# 1. SETUP & LIBRARIES

pkgs <- c("tidyverse", "rvest", "xml2", "tidytext", "textdata",
          "topicmodels", "wordcloud", "RColorBrewer", "SnowballC", "Matrix", "cluster", "mclust")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

options(width = 150, dplyr.width = Inf, tibble.width = Inf)
set.seed(784)

local_dir  <- "C:/Users/sweth/Downloads/DSCI_784_Corporate_Risk_Disclosures/01_Raw_Data"
output_dir <- file.path(dirname(local_dir), "02_Analysis_Results")
text_dir   <- file.path(output_dir, "Extracted_Text")
chart_dir  <- file.path(output_dir, "Charts")
table_dir  <- file.path(output_dir, "Tables")

for (d in c(output_dir, text_dir, chart_dir, table_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

sink(file.path(output_dir, "Console_Output_Item1A.txt"), split = TRUE)
options(error = function() { while (sink.number() > 0) sink() })

save_table <- function(x, name) write.csv(x, file.path(table_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
save_chart <- function(p, name, w, h) { print(p); ggsave(file.path(chart_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300) }

# 2. DEFINE THE FILINGS 

filings <- expand_grid(
  ticker = c("JPM", "BAC", "GS", "AAPL", "MSFT", "JNJ"),
  year = 2021:2025
) %>% mutate(
  sector = case_when(
    ticker %in% c("JPM", "BAC", "GS") ~ "Banking",
    ticker %in% c("AAPL", "MSFT") ~ "Technology",
    ticker == "JNJ" ~ "Pharmaceuticals"
  ),
  doc = paste0(ticker, "_", year, "_10K")
) %>% select(doc, ticker, sector, year)

# 3. EXTRACT ITEM 1A 

read_item1a <- function(doc_name) {
  cat("Extracting Item 1A from:", doc_name, "\n")
  file_path <- file.path(local_dir, paste0(doc_name, ".html"))
  if(!file.exists(file_path)) return(NA_character_)
  
  file_size <- file.info(file_path)$size
  raw_html <- readBin(file_path, what = "raw", n = file_size)
  html_string <- iconv(rawToChar(raw_html), from = "UTF-8", to = "UTF-8", sub = "")
  
  page <- read_html(html_string, encoding = "UTF-8")
  xml_remove(xml_find_all(page, "//script | //style"))
  xml_remove(xml_find_all(page, "//*[contains(translate(@style,' ',''),'display:none')]"))
  
  full_text <- str_squish(html_text2(xml_find_first(page, "//body")))
  
  start_pattern <- "\\bITEM\\s*1A\\.?\\s*RISK\\s+FACTORS\\b"
  end_pattern <- "\\bITEM\\s*1B\\.?\\s*UNRESOLVED\\s+STAFF\\s+COMMENTS\\b"
  
  start_hits <- gregexpr(start_pattern, full_text, ignore.case = TRUE, perl = TRUE)[[1]]
  end_hits <- gregexpr(end_pattern, full_text, ignore.case = TRUE, perl = TRUE)[[1]]
  
  if (start_hits[1] == -1 || end_hits[1] == -1) return(NA_character_)
  
  final_end <- max(end_hits)
  valid_starts <- start_hits[start_hits < final_end]
  if (length(valid_starts) == 0) return(NA_character_)
  
  start_position <- max(valid_starts)
  valid_ends <- end_hits[end_hits > start_position]
  if (length(valid_ends) == 0) return(NA_character_)
  
  end_position <- min(valid_ends)
  return(str_squish(substr(full_text, start_position, end_position - 1)))
}

corpus <- filings %>% mutate(text = map_chr(doc, read_item1a))
if (any(is.na(corpus$text)) || any(nchar(corpus$text) == 0)) {
  stop("One or more filings failed Item 1A extraction. Verify headings match specifications.")
}

walk2(corpus$doc, corpus$text, function(name, txt) writeLines(txt, file.path(text_dir, paste0(name, "_Item1A.txt")), useBytes = TRUE))

# 4. TEXT PREPARATION & DESCRIPTIVE STATISTICS

tokens_raw <- corpus %>%
  mutate(text = str_replace_all(str_to_lower(text), "[^a-z\\s]", " ")) %>%
  unnest_tokens(word, text, token = "words", to_lower = FALSE)

custom_stopwords <- tibble(word = c(
  "company", "companies", "business", "businesses", "including", "include", "included", "includes",
  "may", "might", "could", "would", "should", "also", "one", "two", "three", "new", "year", "years",
  "million", "billion", "item", "form", "annual", "report", "risk", "risks", "factor", "factors",
  "section", "information", "certain", "financial", "statements", "fiscal", "inc", "corporation",
  "limited", "subsidiary", "subsidiaries", "jpmorgan", "chase", "merrill", "lynch", "bofa", "goldman",
  "sachs", "apple", "microsoft", "johnson", "jnj", "aapl", "msft"
))

tokens <- tokens_raw %>%
  filter(str_detect(word, "^[a-z]{3,}$") | word == "ai") %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stopwords, by = "word")

# Calculate descriptive statistics
doc_stats <- tokens_raw %>%
  dplyr::count(doc, name = "raw_tokens") %>%
  left_join(
    tokens %>%
      group_by(doc) %>%
      summarise(
        prepared_tokens = n(),
        unique_terms = n_distinct(word),
        .groups = "drop"
      ),
    by = "doc"
  ) %>%
  left_join(filings, by = "doc") %>%
  arrange(ticker, year)

# Document term-counts
counts   <- tokens %>% dplyr::count(doc, word, name = "n")
term_df  <- counts %>% dplyr::count(word, name = "df")
N        <- n_distinct(counts$doc)
keep     <- term_df %>% filter(df >= 3, df <= 0.9 * N)
counts_k <- counts %>% inner_join(keep, by = "word")

# 5. CORPORATE PLOTS

# Chart 1. Prepared tokens in each filing
p1 <- ggplot(doc_stats, aes(factor(year), prepared_tokens, fill = ticker)) +
  geom_col(show.legend = FALSE) + facet_wrap(~ ticker, nrow = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Chart 1. Prepared Tokens per Filing", x = "Fiscal year", y = "Tokens") +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
save_chart(p1, "Chart_01_Prepared_Tokens", 12, 6)

# Chart 2. Fifteen most frequent terms
top_terms <- tokens %>% dplyr::count(word, sort = TRUE)
p2 <- top_terms %>% slice_head(n = 15) %>%
  ggplot(aes(n, reorder(word, n))) + geom_col(fill = "#102A43") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Chart 2. Top 15 Terms", x = "Frequency", y = NULL) + theme_minimal(base_size = 11)
save_chart(p2, "Chart_02_Top_15_Terms", 10, 7)

# Chart 3. Word Cloud

set.seed(784)
wordcloud(words = top_terms$word,freq = top_terms$n,max.words = 120,random.order = FALSE,colors = brewer.pal(8, "Dark2"))
title(
  main = "Chart 3. Word Cloud"
)
png(filename = file.path(chart_dir, "Chart_03_Word_Cloud.png"),width = 1600,height = 1200,res = 150)
set.seed(784)
wordcloud(words = top_terms$word,freq = top_terms$n,max.words = 120,random.order = FALSE,colors = brewer.pal(8, "Dark2"))
title(
  main = "Chart 3. Word Cloud"
)
dev.off()


# Chart 4. Most distinctive terms by company (TF-IDF)
tfidf <- counts_k %>% mutate(tf = 1 + log(n), idf = log((1 + N) / (1 + df)) + 1, w = tf * idf) %>%
  group_by(doc) %>% mutate(w = w / sqrt(sum(w^2))) %>% ungroup()
M <- tfidf %>% cast_sparse(doc, word, w) %>% as.matrix()
M <- M[filings$doc, ]
S <- M %*% t(M)
hc <- hclust(dist(M), method = "ward.D2")
company_terms <- tfidf %>% left_join(filings, by = "doc") %>%
  group_by(ticker, word) %>% summarise(w_sum = sum(w), .groups = "drop") %>%
  left_join(filings %>% dplyr::count(ticker, name = "n_docs"), by = "ticker") %>%
  mutate(mean_w = w_sum / n_docs) %>% group_by(ticker) %>% slice_max(mean_w, n = 8, with_ties = FALSE) %>% ungroup()
p4 <- ggplot(company_terms, aes(mean_w, reorder_within(word, mean_w, ticker), fill = ticker)) +
  geom_col(show.legend = FALSE) + scale_y_reordered() + facet_wrap(~ ticker, scales = "free_y") +
  labs(title = "Chart 4. Distinctive Terms by Company", x = "Mean TF-IDF", y = NULL) +
  theme_minimal(base_size = 10)
save_chart(p4, "Chart_04_Distinctive_Terms", 12, 8)

# Chart 5. Dendrogram (Prints to RStudio window and saves folder device)
plot(hc,main = "Chart 5. Hierarchical Clustering",xlab = "SEC Filings",sub = "",cex = 0.65)
png(filename = file.path(chart_dir, "Chart_05_Dendrogram.png"),width = 1600,height = 1200,res = 150)
plot(hc,main = "Chart 5. Hierarchical Clustering",xlab = "SEC Filings",sub = "",cex = 0.6)
dev.off()

# Chart 6. Document Map MDS
xy <- cmdscale(as.dist(1 - S), k = 2) %>% as_tibble(.name_repair = ~ c("x", "y")) %>% bind_cols(filings)
p6 <- ggplot(xy, aes(x, y, colour = ticker, group = ticker)) +
  geom_path(alpha = 0.5) + geom_point(size = 2) + geom_text(aes(label = year), vjust = -0.9, size = 2.8, show.legend = FALSE) +
  labs(title = "Chart 6. Document Map", x = NULL, y = NULL, colour = NULL) +
  theme_minimal(base_size = 11) + theme(axis.text = element_blank(), panel.grid = element_blank())
save_chart(p6, "Chart_06_Document_Map", 11, 8)

# Chart 7. Language Drift over Time
drift <- filings %>% group_by(ticker) %>%
  mutate(base = doc[year == min(year)],
         cos_to_2021 = S[cbind(match(doc, rownames(S)), match(base, rownames(S)))]) %>% ungroup()

p7 <- ggplot(drift, aes(year, cos_to_2021, colour = ticker)) + geom_line() + geom_point() +
  labs(title = "Chart 7. Language Drift Since 2021",
       x = "Fiscal year", y = "Cosine similarity to 2021", colour = NULL) + theme_minimal(base_size = 11)
save_chart(p7, "Chart_07_Language_Drift", 11, 7)

# Chart 8. Top terms in each LDA topic
dtm <- counts_k %>% cast_dtm(doc, word, n)
lda <- LDA(dtm, k = 4, control = list(seed = 784))
lda_terms <- tidy(lda, matrix = "beta") %>% group_by(topic) %>% slice_max(beta, n = 10, with_ties = FALSE) %>% ungroup()
p8 <- ggplot(lda_terms, aes(beta, reorder_within(term, beta, topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) + scale_y_reordered() + facet_wrap(~ topic, scales = "free_y") +
  labs(title = "Chart 8. LDA Topic Terms", x = "Probability in topic", y = NULL) + theme_minimal(base_size = 10)
save_chart(p8, "Chart_08_LDA_Topics", 12, 8)

# Chart 9. Heatmap share of topics
gamma_raw <- tidy(lda, matrix = "gamma")
doc_ids <- rownames(dtm)
gamma <- gamma_raw %>%
  mutate(
    doc = if (all(as.character(document) %in% doc_ids)) {
      as.character(document)
    } else {
      doc_ids[as.integer(as.character(document))]
    }
  ) %>%
  left_join(filings, by = "doc")
if (any(is.na(gamma$ticker))) {
  stop("Some LDA documents could not be matched to the original filings.")
}
topic_mix <- gamma %>%
  group_by(ticker, topic) %>%
  summarise(share = mean(gamma), .groups = "drop")
p9 <- ggplot(topic_mix, aes(factor(topic), ticker, fill = share)) +
  geom_tile() + geom_text(aes(label = round(share, 2)), color = "brown", fontface = "bold") +
  scale_fill_gradient(low = "#F5F7FA", high = "#102A43") +
  labs(title = "Chart 9. Topic Share by Company", x = "Topic", y = NULL, fill = "Share") +
  theme_minimal(base_size = 11)
save_chart(p9, "Chart_09_Topic_Heatmap", 10, 6)

# Chart 10. Sentiment Word Rates

lex <- get_sentiments("loughran") %>% filter(sentiment %in% c("negative", "positive", "uncertainty", "litigious", "constraining"))
totals <- tokens %>% dplyr::count(doc, name = "total")
sent <- tokens %>% inner_join(lex, by = "word", relationship = "many-to-many") %>%
  dplyr::count(doc, sentiment) %>% left_join(totals, by = "doc") %>%
  mutate(rate = 1000 * n / total) %>% left_join(filings, by = "doc")
p10 <- ggplot(sent, aes(year, rate, colour = ticker)) + geom_line() + geom_point(size = 1.5) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(title = "Chart 10. Sentiment Rates", x = "Fiscal year", y = "Rate", colour = NULL) +
  theme_minimal(base_size = 10)
save_chart(p10, "Chart_10_Sentiment", 12, 8)

# Chart 11. Use of selected terms over time

trend_terms <- c("covid", "inflation", "climate", "cybersecurity", "ai")
trend <- expand_grid(doc = filings$doc, word = trend_terms) %>%
  left_join(tokens %>% filter(word %in% trend_terms) %>% dplyr::count(doc, word), by = c("doc", "word")) %>%
  mutate(n = replace_na(n, 0)) %>% left_join(totals, by = "doc") %>% left_join(filings, by = "doc") %>%
  mutate(rate = 1000 * n / total)
p11 <- ggplot(trend, aes(year, rate, colour = ticker)) + geom_line() + geom_point(size = 1.5) +
  facet_wrap(~ word, scales = "free_y") +
  labs(title = "Chart 11. Term Trends", x = "Fiscal year", y = "Rate", colour = NULL) +
  theme_minimal(base_size = 10)
save_chart(p11, "Chart_11_Term_Trends", 12, 8)

# Chart 12. Lexical dispersion plot

disp_doc   <- "MSFT_2025_10K"
disp_terms <- c("ai", "cloud", "cybersecurity", "competition", "customers", "revenue")
p12 <- tokens %>% filter(doc == disp_doc) %>% mutate(pos = row_number() / n()) %>%
  filter(word %in% disp_terms) %>%
  ggplot(aes(pos, factor(word, levels = rev(disp_terms)))) +
  geom_point(shape = "|", size = 6, colour = "#102A43") +
  labs(title = paste("Chart 12. Lexical Dispersion", disp_doc),
       x = "Relative structural position in Item 1A (0 = start, 1 = end)", y = NULL) + theme_minimal(base_size = 11)
save_chart(p12, "Chart_12_Lexical_Dispersion", 11, 6)

# 6. EXPORT ALL DATA TABLES 

tables_to_save <- list(
  "03_Top_15_Risk_Terms"       = top_terms %>% slice_head(n = 15),
  "04_Distinctive_Risk_Terms"  = company_terms %>% group_by(ticker) %>% summarise(top_terms = paste(word, collapse=", ")),
  "05_Language_Drift_Metrics"  = drift %>% select(ticker, year, cos_to_2021) %>% pivot_wider(names_from = year, values_from = cos_to_2021),
  "06_LDA_Topic_Definitions"   = lda_terms %>% group_by(topic) %>% summarise(top_terms = paste(term, collapse=", ")),
  "07_Topic_Mix_By_Company"    = topic_mix %>% pivot_wider(names_from = topic, names_prefix = "topic_", values_from = share),
  "08_Sentiment_By_Company"    = sent %>% group_by(ticker, sentiment) %>% summarise(rate = mean(rate), .groups="drop") %>% pivot_wider(names_from = sentiment, values_from = rate)
)
walk2(tables_to_save, names(tables_to_save), save_table)
cat("ITEM 1A RISK DISCLOSURES NLP PIPELINE COMPLETE!\n")
while (sink.number() > 0) sink()
