setwd("C:/Users/molly/OneDrive/Documents/5465/CSCI 5465 Final Project")
library(readxl)
event_listing <- read_excel("Event Listing.xlsx", 
                            sheet = "AE Management")
#View(event_listing)
library(readxl)
subj_listing <- read_excel("Event Listing.xlsx", 
                            sheet = "Subject Listing")
#View(subj_listing)
#install.packages("ggplot2")
library(ggplot2)
total_enrolled <-  colSums(subj_listing[,"Number_enrolled_subjects"])

#Graph how many events each subject reported
average_AE_per_subj <- nrow(event_listing)/total_enrolled
ID <- ggplot(event_listing, aes(x = Subject_ID))
ID2 <- (ID + geom_bar(color = 'black', fill = '#87a3bf'))
avg_AE_subj <- ID2 + geom_hline(yintercept = average_AE_per_subj, color = 'red')
print(avg_AE_subj+xlab('Subject ID') + ylab('Number of events') + ggtitle('Number of events reported per subject'))

#Graph how many events reported per country
#install.packages('plyr')
library(plyr)
count_countries <- count(event_listing, Site)
event_rate_country <- count_countries[,"n"]/subj_listing[,"Number_enrolled_subjects"]
colnames(event_rate_country)[colnames(event_rate_country)=="n"] <- "event_rate"
count_countries <-cbind(count_countries,event_rate_country)

Country_plot <- ggplot() + 
  #country
  geom_bar(data = event_listing, aes(x = Site), color = 'black', fill = '#87a3bf') +
  #event rate
  geom_point(data = count_countries, aes(x = Site, y = event_rate), color = 'red')
print(Country_plot + xlab('Country') + ylab('Number of events reported') + ggtitle('Number of events reported per country'))

#Graph how many events reported per PT
count_codes <- na.omit(count(event_listing, Adverse_Event_MedDRA_Term))
top20_PTs <- count_codes[order(-count_codes$n),][1:20,]

PT <- ggplot(top20_PTs, aes(x = reorder(Adverse_Event_MedDRA_Term, -n), y = n))
PT_plot <- (PT+geom_bar(stat = "identity", color = 'black', fill = '#87a3bf'))
print(PT_plot + xlab('Preferred Term') + ylab('Number of events') + ggtitle('Top 20 events reported per MedDRA Preferred Term') + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#Graph procedure related AEs by PT
#install.packages("tidyverse")
library(tidyverse)
proc_related <- event_listing%>%filter(Assessment_of_Relationship_to_Procedure == 'Related')
proc_related2 <- as.data.frame(proc_related)
count_proc_codes <- na.omit(count(proc_related2, Adverse_Event_MedDRA_Term))

proc_related_plot <- ggplot() +
  geom_bar(data = count_proc_codes, aes(x = reorder(Adverse_Event_MedDRA_Term, -n), y = n), stat = 'identity', color = 'black', fill = '#87a3bf')
print(proc_related_plot + xlab('Preferred Term') + ylab('Number of events') + ggtitle('Procedure related events reported per MedDRA Preferred Term') + theme(axis.text.x = element_text(angle = 90, hjust = 1)))

#Graph device related AEs by PT
dev_related <- event_listing%>%filter(Assessment_of_Relationship_to_Investigational_Device == 'Related')
dev_related2 <- as.data.frame(dev_related)
count_dev_codes <- na.omit(count(dev_related2, Adverse_Event_MedDRA_Term))

dev_related_plot <- ggplot() +
  geom_bar(data = count_dev_codes, aes(x = reorder(Adverse_Event_MedDRA_Term, -n), y = n), stat = 'identity', color = 'black', fill = '#87a3bf')
print(dev_related_plot + xlab('Preferred Term') + ylab('Number of events') + ggtitle('Device related events reported per MedDRA Preferred Term') + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
