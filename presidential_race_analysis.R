# Data Analysis: 2020 Presidential Election Fraud Analysis via Benford's Law
# Author: Dillan Marroquin
# Date: 05/01/2024 (Originally conducted on 08/11/2021)

# Load libraries
library(tidyverse)
library(scales)

# Set working directory
setwd("C:/Users/di11m/Downloads/Coursera/Data Analysis/RStudio")

# Load election data for Washoe and NV
washoe <- read.csv("Election_Results_Washoe_CLEANED.csv")
nv <- read.csv("Election_Results_NV_CLEANED.csv")

# Define candidate names
candidates <- c("BIDEN, JOSEPH R.", "BLANKENSHIP, DON", "JORGENSEN, JO", "None Of These Candidates", "TRUMP, DONALD J.")

# Function to filter and sum votes per candidate
sum_votes <- function(data, candidate_name){
  candidate_data <- filter(data, Candidate == candidate_name)
  return(sum(candidate_data$Votes))
}

# Calculate total votes per candidate: Washoe and NV
biden_votes_w <- sum_votes(washoe, candidates[1])
biden_votes_n <- sum_votes(nv, candidates[1])
blank_votes_w <- sum_votes(washoe, candidates[2])
blank_votes_n <- sum_votes(nv, candidates[2])
jorg_votes_w <- sum_votes(washoe, candidates[3])
jorg_votes_n <- sum_votes(nv, candidates[3])
none_votes_w <- sum_votes(washoe, candidates[4])
none_votes_n <- sum_votes(nv, candidates[4])
trump_votes_w <- sum_votes(washoe, candidates[5])
trump_votes_n <- sum_votes(nv, candidates[5])

# Total votes per candidate: Washoe and NV
total_votes_w <- sum(biden_votes_w, blank_votes_w, jorg_votes_w, none_votes_w, trump_votes_w)
total_votes_n <- sum(biden_votes_n, blank_votes_n, jorg_votes_n, none_votes_n, trump_votes_n)

# Total votes by candidate data frame: Washoe and NV
votes_by_candidate_w <- data.frame(
  Candidate = candidates,
  Votes = c(biden_votes_w, blank_votes_w, jorg_votes_w, none_votes_w, trump_votes_w)
)
votes_by_candidate_n <- data.frame(
  Candidate = candidates,
  Votes = c(biden_votes_n, blank_votes_n, jorg_votes_n, none_votes_n, trump_votes_n)
)

# Percentage of votes per candidate: Washoe and NV
percents_w <- votes_by_candidate_w$Votes / total_votes_w
percents_n <- votes_by_candidate_n$Votes / total_votes_n

# Percentage of votes per candidate data frame: Washoe and NV
percent_total_w <- data.frame(
  Candidate = candidates,
  Percent = percents_w * 100
)
percent_total_n <- data.frame(
  Candidate = candidates,
  Percent = percents_n * 100
)

# Graph generator function
generate_graph <- function(candidate_names, votes, title){
  # Data frame for graph
  graph_data <- data.frame(
    Candidate = candidate_names,
    Votes = votes
  )
  # Generate graph
  graph <- ggplot(graph_data, aes(x = Candidate, y = Votes, fill = Candidate)) +
    geom_col() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma_format()) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_manual(values = c(
      "BIDEN, JOSEPH R." = "blue",
      "BLANKENSHIP, DON" = "purple",
      "JORGENSEN, JO" = "yellow",
      "None Of These Candidates" = "black",
      "TRUMP, DONALD J." = "red")) +
    labs(
      title = title,
      x = "Candidate",
      y = "Votes"
    )
  return(graph)
}

# Graph of election results: Washoe and NV
election_graph_w <- generate_graph(
  candidate_names = candidates,
  votes = c(biden_votes_w, blank_votes_w, jorg_votes_w, none_votes_w, trump_votes_w),
  title = "2020 Presidential Election Results: Washoe County"
  )

election_graph_n <- generate_graph(
  candidate_names = candidates,
  votes = c(biden_votes_n, blank_votes_n, jorg_votes_n, none_votes_n, trump_votes_n),
  title = "2020 Presidential Election Results: Nevada"
)

# Benford's Law distribution
bens_law <- function(d) log10(1 + 1 / d)  #Formula
digits <- 1:9
bens_data <- data.frame(Digits = digits, Percentage = bens_law(digits))

bens_graph <-
  ggplot(bens_data,
         aes(x = Digits, y = Percentage)) +
  geom_line(color = "red") +
  geom_point(color = "red", size = 3) + 
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  labs(title = "Benford's Law",
       x = "Leading Digit",
       y = "Proportion of Occurrence")

# Leading Digits: Washoe
ld_occur_w <- sapply(1:9, function(x) sum(washoe$LeadDigit == x))
ld_percent_w <- ld_occur_w / sum(ld_occur_w)

ld_data_w <- data.frame(Digits = digits, Percentage = ld_percent_w)

ld_graph_w <-
  ggplot(ld_data_w,
         aes(x = Digits, y = Percentage)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  labs(title = "Leading Digit of Total Votes\nin Washoe Presidential Election",
       x = "Leading Digit",
       y = "Proportion of Occurrence")


# Leading Digits: NV
ld_occur_n <- sapply(1:9, function(x) sum(washoe$LeadDigit == x))
ld_percent_n <- ld_occur_n / sum(ld_occur_n)

ld_data_n <- data.frame(Digits = digits, Percentage = ld_percent_w)

ld_graph_n <-
  ggplot(ld_data_n,
         aes(x = Digits, y = Percentage)) +
  geom_line(color = "orange") +
  geom_point(color = "orange", size = 3) +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  labs(title = "Leading Digit of Total Votes\nin Nevada Presidential Election",
       x = "Leading Digit",
       y = "Proportion of Occurrence")

# Benford's Law comparison: Washoe
table_wb <- data.frame(Digits = digits,
                      Percentage = c(bens_law(digits), ld_percent_w),
                      Focus = c(rep("Benford", 9), rep("Washoe", 9)))

graph_wb <-
  ggplot(table_wb,
         aes(x = Digits, y = Percentage, group = Focus)) + 
  geom_line(aes(color = Focus)) +
  geom_point(aes(color = Focus)) +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Leading Digits of Votes in Washoe v. Benford",
       x = "Leading Digit",
       y = "Proportion of Occurrence")


# Benford's Law comparison: NV
table_nb <- data.frame(Digits = digits,
                      Percentage = c(bens_law(digits), ld_percent),
                      Focus = c(rep("Benford", 9), rep("Nevada", 9)))

graph_nb <-
  ggplot(table_nb,
         aes(x = Digits, y = Percentage, group = Focus)) + 
  geom_line(aes(color = Focus)) +
  geom_point(aes(color = Focus)) +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  scale_color_manual(values=c("red", "orange")) +
  labs(title = "Leading Digits of Votes in Nevada v. Benford",
       x = "Leading Digit",
       y = "Proportion of Occurrence")

# Benford's Law comparison: NV, Washoe, Benford
table_all <- data.frame(Digits = digits, 
                       Percentage = c(bens_law(digits), ld_percent_w, ld_percent_n), 
                       Focus = c(rep("Benford", 9), rep("Washoe", 9), rep("Nevada", 9)))

graph_all <-
  ggplot(table_all,
         aes(x = Digits, y = Percentage, group = Focus)) + 
  geom_line(aes(color = Focus)) +
  geom_point(aes(color = Focus)) +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  scale_color_manual(values = c("red", "orange", "blue")) +
  labs(title = "Washoe and Nevada v. Benford",
       x = "Leading Digit",
       y = "Proportion of Occurrence")