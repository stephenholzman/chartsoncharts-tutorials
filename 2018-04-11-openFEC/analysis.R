#Access data from the openFEC api using R wrapper from robinspollak.

library(devtools)
install_github("robinspollak/R.openFEC")

#Load libraries needed to run this script.

library(R.openFEC)
library(httr)
library(tidyverse)

#signup for api key at https://api.open.fec.gov/developers/. Save a one line file called "data.gov.key" in the root project folder, that one line assigning the key to a variable like the next line:
# api_key <- "KEYSTRINGFROMFEC"

source("data.gov.key")

#Set params for the election we are interested in.

election_of_interest <- list(
  state = "VA",
  office_abb = "H",
  office_full = "house",
  district = "05",
  cycle = "2018",
  candidate_status = c("C") #Tested options are C for current candidate, P for prior, 
)

#Find current candidates so we can get IDs necessary for more detailed financial info and filter on current status.
#If there are more candidates than 20 currently in the race, you'll need to do this several times changing the page parameter or increase results per page.
candidates_running <- candidates_search(api_key,
                                        query_params = list(
                                          state = election_of_interest$state,
                                          office = election_of_interest$office,
                                          district = election_of_interest$district,
                                          cycle = election_of_interest$cycle,
                                          page="1")
)


candidates_running_df <- data.frame(
  name = map(candidates_running[["content"]][["results"]], function(x) x$name) %>% unlist(),
  candidate_id = map(candidates_running[["content"]][["results"]], function(x) x$candidate_id) %>% unlist(),
  party = map(candidates_running[["content"]][["results"]], function(x) x$party) %>% unlist(),
  candidate_status = map(candidates_running[["content"]][["results"]], function(x) x$candidate_status) %>% unlist()
) %>%
  separate(name, c('last_name', 'first_name'), sep = ', ') %>%
  filter(candidate_status %in% election_of_interest$candidate_status)

financial_summary <- get_election_financial_summary(api_key,
                                                    query_params = list(
                                                      state = election_of_interest$state,
                                                      office = election_of_interest$office_full,
                                                      district = election_of_interest$district,
                                                      cycle = election_of_interest$cycle)
)

financial_summary_df <- data.frame(
  name = map(financial_summary[["content"]][["results"]], function(x) x$candidate_name) %>% unlist(),
  candidate_id = map(financial_summary[["content"]][["results"]], function(x) x$candidate_id) %>% unlist(),
  total_receipts = map(financial_summary[["content"]][["results"]], function(x) x$total_receipts) %>% unlist(),
  total_disbursements = map(financial_summary[["content"]][["results"]], function(x) x$total_disbursements) %>% unlist(),
  cash_on_hand_end_period = map(financial_summary[["content"]][["results"]], function(x) x$cash_on_hand_end_period) %>% unlist()
) %>%
  left_join(candidates_running_df, by = c("candidate_id"="candidate_id")) %>%
  filter(candidate_status %in% election_of_interest$candidate_status) %>%
  select(last_name,candidate_id,party,total_receipts,cash_on_hand_end_period,total_disbursements) %>%
  gather(type,amount,total_receipts:total_disbursements) %>%
  mutate(type = str_replace_all(type,"_"," ") %>% str_to_title,
         last_name = str_to_title(last_name))



period_ending <- as.factor(map(financial_summary[["content"]][["results"]], function(x) x$coverage_end_date) %>% unlist())

if(length(levels(period_ending)) != 1){
  stop("There are different period_endings for the candidates returned.")
}else{
  period_ending <- substr(levels(period_ending)[1],1,10)
}

candidate_order <- financial_summary_df %>%
  filter(type == "Total Receipts") %>%
  arrange(amount)

financial_summary_df$last_name <- factor(financial_summary_df$last_name, levels = candidate_order$last_name)

colors <- c("DEM" = "#2c4d82", "REP" = "#8e1b1b", "IND" = "#a3a3a3", "GRE" = "#1c561d", "LIB" = "#afac3d")

output_path <- file.path("output",election_of_interest$cycle,election_of_interest$state,election_of_interest$office_full,election_of_interest$district)

dir.create(output_path,recursive = TRUE)

filename <- paste0(Sys.Date(),"-total-receipts-",election_of_interest$cycle,"-",election_of_interest$state,"-",election_of_interest$office_full,"-",election_of_interest$district,".png")

author <- "@StephenHolz"

p1 <- financial_summary_df %>%
  ggplot() +
  geom_bar(aes(x = last_name, y = amount, fill = party), stat = "identity", alpha = .95) +
  geom_label(aes(x = last_name, y = amount, label = scales::dollar(amount)),hjust = -0.1, label.size = 0, label.padding = unit(0.1,"line")) +
  facet_wrap(~type) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar,limits = c(0,max(financial_summary_df$amount)*2)) +
  scale_fill_manual(values = colors) +
  labs(
    title = paste0(election_of_interest$cycle," ",election_of_interest$state,election_of_interest$district," ",str_to_title(election_of_interest$office_full)," Race Financial Summary"),
    subtitle = paste0("Period ending ",period_ending),
    x = "",
    y = "",
    caption = paste0("Source: Federal Election Commmission, data retreived on ",Sys.Date(), " by ",author),
    fill = "Party"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "#7c7c7c",size = .4),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(.8,"cm"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 10)
  )

ggsave(filename = filename, plot = p1, path = output_path, width = 12, height = 6, unit = "in")
