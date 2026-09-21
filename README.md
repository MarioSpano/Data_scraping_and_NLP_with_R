# Data_scraping_and_NLP_with_R
This repository contains a beginner-friendly R project designed to explore the fundamentals of Natural Language Processing (NLP) and text analytics. Using movie discussions scraped from Reddit and imported Letterboxd reviews.

This code serves as a practical sandbox for learning how to process, analyze, and visualize raw unstructured text data.
The code is an end-to-end text mining pipeline, from data collection to advanced clustering, without relying on black-box sentiment APIs.

Data Sources
The project analyzes audience reception and discussions surrounding recent films (e.g., Dune: Part Two, Spiderman: Across the Spiderverse, No Other Land) using two distinct data sources:

Reddit (Dynamic Extraction): Uses the RedditExtractoR package to live-scrape thread titles, original posts, and comments based on specific keyword queries.
Letterboxd (Static Import): Processes external CSV datasets containing user reviews and movie metadata.


The NLP 
The code break down human language into quantifiable data in few steps:

Text Preprocessing: Cleans the raw data by removing URLs, HTML tags, punctuation, emojis, and converting text to lowercase using the textclean package.

POS Tagging & Collocations: Uses udpipe to perform Parts-Of-Speech tagging (identifying nouns, verbs, etc.) and detects statistically significant multi-word expressions (like "spider_man" or "part_two") to treat them as single tokens.

Lemmatization: Reduces words to their base dictionary form (e.g., "movies" becomes "movie") and filters out grammatical noise (stopwords).

Lexicometrics: Calculates vocabulary richness metrics for each movie, including Corpus Size (N), Vocabulary Size (V), Hapax (words appearing only once), Type-Token Ratio (TTR), and the Guiraud index.

Descriptive Analytics: Generates term-document matrices (TDM) to extract high-frequency terms, visualizing the results through bar charts and word clouds.

Hierarchical Clustering: Measures the cosine distance between word frequencies to group co-occurring terms into distinct semantic clusters, visualized via dendrograms and comparison clouds. The optimal number of clusters is guided by Silhouette score analysis.

Metadata sectionm:
In order to run the second half of the script, you need to download two CSV files (TableMovies.csv and CommentTable_Final.csv) containing Letterboxd data, as well as a formatted Excel file (Review_file.xlsx) to resolve specific multi-word corrections.
