#####
# Data wrangling of KZN 26 cellphone, phonebook, and acquaintance records
# Data source: Court record - Thereza Botha, directly sourced from Vodacom & MTN through the court
# Data includes calls from 1 - 4 Oct 2006, Contact lists, Testimonial evidence
#
# Copyright - Annie Kok - anniekok3@gmail.com
#####

rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

# 1. Calls data entry and cleaning

## 1.1 Read in all the data, stored in xlsx files converted from pdfs ####

files <- list.files('../Datasets/Extracted_from_1_Oct/')
for(f in files) assign(str_remove(str_remove(f, '.xlsx'), '\\_'), read_xlsx(paste0('../Datasets/Extracted_from_1_Oct/', f)))

## 1.2 Read in all known cell numbers ####

numbs <- read_excel('Metadata_files/All_phone_numbers.xlsx', sheet = 'Sheet1', col_types = 'text')
numbs$ID <- str_remove_all(numbs$ID, '\\_')

## 1.3 Merge all call records into single raw edgelist ####

# NB!!! Only Vodacom includes SMS's. Will include for now, then filter out
# NB!!! Seems like only Vodacom includes missed calls as well
# NB!!! Including the location for now, but will not use in PhD, format differs between V & MTN

# Use the first one to set up template to which to rbind
# This one was Vodacom, which does not split between sender/receiver
edgelist <- Acc1 %>% 
  rename(call_duration = `Call Duration`, call_type = `Call Type`) %>% 
  mutate(sender = case_when(call_type == 'MOC' | call_type == 'MOSMS' ~ 'Acc1', 
                            call_type == 'MTC' | call_type == 'MTSMS' ~ as.character(`Other Party`),
                            call_type == 'CF' ~ as.character(`Third Party`)),
         sender_location = ifelse(call_type == 'MOC' | call_type == 'MOSMS', `Cell Name`, NA),
         receiver = ifelse(call_type == 'MOC' | call_type == 'MOSMS', `Other Party`, 'Acc1'),
         receiver_location = ifelse(call_type == 'MOC' | call_type == 'MOSMS', NA, `Cell Name`),
         call_type = case_when(call_type == 'MTSMS' | call_type == 'MOSMS' ~ 'sms',
                               call_type == 'MTC' | call_type == 'MOC' ~ 'call',
                               call_type == 'CF' ~ 'missed_call'),
         datetime = dmy_hms(`Call date`)) %>% 
  select(datetime, sender, receiver, sender_location, receiver_location, call_duration, call_type)

acc_ass_dfs <- ls(pattern = '^Acc|^Ass') # names of dataframes read in
df_list <- mget(acc_ass_dfs)             # make into list

# Vodacom contains an IMSI column, MTN does not
for(df_name in names(df_list)[-1]){
  df <- df_list[[df_name]]
  
  # Check whether Vodacom or MTN
  if ('IMSI' %in% colnames(df)) { #Vodacom
    next_one <- df %>% 
      rename(call_duration = `Call Duration`, call_type = `Call Type`) %>% 
      mutate(sender = case_when(call_type == 'MOC' | call_type == 'MOSMS' ~ df_name, 
                                call_type == 'MTC' | call_type == 'MTSMS' ~ as.character(`Other Party`),
                                call_type == 'CF' ~ as.character(`Third Party`)),
             receiver = ifelse(call_type == 'MOC' | call_type == 'MOSMS', `Other Party`, df_name),
             sender_location = ifelse(call_type == 'MOC' | call_type == 'MOSMS', `Cell Name`, NA),
             receiver_location = ifelse(call_type == 'MOC' | call_type == 'MOSMS', NA, `Cell Name`),
             call_type = case_when(call_type == 'MTSMS' | call_type == 'MOSMS' ~ 'sms',
                                   call_type == 'MTC' | call_type == 'MOC' ~ 'call',
                                   call_type == 'CF' ~ 'missed_call'),
             datetime = dmy_hms(`Call date`)) %>% 
      select(datetime, sender, receiver, sender_location, receiver_location, call_duration, call_type)
    
    edgelist <- rbind(edgelist, next_one)
  } else{ #MTN
    #Time is messy
    df$Time <- str_sub(paste0('00000', as.character(df$Time)), start = -6) #add zeroes for times that had leading zeroes
    
    next_one <- df %>% 
      rename(call_duration = `Actual Duration`) %>% 
      mutate(sender = ifelse(`Call Direction` == 'O', df_name, Calling),
             receiver = ifelse(`Call Direction` == 'I', df_name, Called),
             sender_location = ifelse(`Call Direction` == 'O', `Site Location`, NA),
             receiver_location = ifelse(`Call Direction` == 'I', `Site Location`, NA),
             call_type = 'call',
             Time = format(as.POSIXct(as.character(Time), format = '%H%M%S'), '%H:%M:%S'),
             datetime = mdy(Date) + hms(Time)) %>% 
      select(datetime, sender, receiver, sender_location, receiver_location, call_duration, call_type)
    
    edgelist <- rbind(edgelist, next_one)
  }
}

## 1.4 Extra cleaning ####

edgelist <- edgelist %>% 
  mutate(sender = ifelse(str_detect(sender, '\\.'), str_sub(sender, end = 5), sender),
         receiver = ifelse(str_detect(receiver, '\\.'), str_sub(receiver, end = 5), receiver)) %>%  # Merge those with 2 cell numbers on different network into one
  mutate(sender = str_sub(sender, -9, -1),
         receiver = str_sub(receiver, -9, -1)) %>%  # Remove +27 prefixes
  filter(!is.na(sender) & !is.na(receiver), # Remove blanks
         nchar(receiver) != 3, # Remove service numbers
         nchar(receiver) != 6) %>% # Remove 2 random 6-digit numbers dialled by Acc26
  arrange(datetime)

# Replace all other numbers with IDs where matched...
# Just do for-loops again...

numbs[is.na(numbs)] <- '0'
for(nr in numbs$Cell1){
  edgelist$sender[edgelist$sender == nr] <- numbs$ID[numbs$Cell1 == nr]
  edgelist$receiver[edgelist$receiver == nr] <- numbs$ID[numbs$Cell1 == nr]
}
for(nr in numbs$Cell2){
  edgelist$sender[edgelist$sender == nr] <- numbs$ID[numbs$Cell2 == nr]
  edgelist$receiver[edgelist$receiver == nr] <- numbs$ID[numbs$Cell2 == nr]
}
for(nr in numbs$Cell3){
  edgelist$sender[edgelist$sender == nr] <- numbs$ID[numbs$Cell3 == nr]
  edgelist$receiver[edgelist$receiver == nr] <- numbs$ID[numbs$Cell3 == nr]
}

wtf <- edgelist[edgelist$sender == edgelist$receiver,]
# Contact between two different numbers linked to Acc2?

# Remove duplicates. This is messy and tricky... 
# The same calls from different sources sometimes have a small time discrepancy
# Also, we have sender's location from one source and receiver's from another, so need to combine
# I'm going to identify duplicates, combine them, remove both originals, and add the combined

edgelist <- edgelist %>% 
  distinct() %>%      # Ass5 has a bunch of duplicate entries
  mutate(ind = 1:n()) # index for removing duplicates

time_diff <- c(999, difftime(edgelist$datetime[-1], edgelist$datetime[-nrow(edgelist)], 
                             units = 'secs'))
dupl <- edgelist[(time_diff <= 2 | lead(time_diff <= 2)) & #2 seconds error threshold for the same calls
                   (edgelist$sender == lead(edgelist$sender) | edgelist$sender == lag(edgelist$sender)) & 
                   (edgelist$receiver == lead(edgelist$receiver) | edgelist$receiver == lag(edgelist$receiver)), ] 

# Een random fokker...
time_diff2 <- c(999, difftime(dupl$datetime[-1], dupl$datetime[-nrow(dupl)], units = 'secs'))
dupl <- dupl[(time_diff2 <= 2 | lead(time_diff2 <= 2)), ]
dupl <- dupl[-nrow(dupl), ] #lagged NA from diff

# These are the duplicate rows to remove
rm_ind <- dupl$ind

# Now combine them together
firsts <- dupl[seq(1, (nrow(dupl) - 1), 2), ] %>% select(-ind)
seconds <- dupl[seq(2, nrow(dupl), 2), ] %>% select(-ind)

comb <- firsts
comb$call_type <- ifelse((firsts$call_type == 'missed_call' | seconds$call_type == 'missed_call'), 'missed_call', 'call')
comb$sender_location[is.na(comb$sender_location)] <- seconds$sender_location[is.na(comb$sender_location)]
comb$receiver_location[is.na(comb$receiver_location)] <- seconds$receiver_location[is.na(comb$receiver_location)]

# Remove the duplicates
edgelist <- filter(edgelist, !ind %in% rm_ind) %>% select(-ind)

# Add the combined duplicates
edgelist <- rbind(edgelist, comb) %>% arrange(datetime)

# Last thing...Replace unknown numbers with unknown IDs

all_nrs <- c(edgelist$sender, edgelist$receiver)
unknowns <- unique(all_nrs[nchar(all_nrs) == 9])
unknowns <- as.data.frame(cbind(cell = unknowns, id = paste0('Unk', 1:length(unknowns))))
for(nr in unknowns$cell) {
  edgelist$sender[edgelist$sender == nr] <- unknowns$id[unknowns$cell == nr]
  edgelist$receiver[edgelist$receiver == nr] <- unknowns$id[unknowns$cell == nr]
}

write.csv(unknowns, file = 'unknowns.csv') #list to check numbers/names in phonebooks

## 1.5 Add events ####

# For future research (crime script analysis)

# 2. Phone book data entry ####

## 2.1 Load data extracted from the judgment showing acc contacts saved on the handsets of the accused. 
hofcontactslinks <- read.csv('../Datasets/Edgelists/court_contact_list.csv', header = T, as.is = T) %>% 
  mutate(source = str_remove(source, '\\_'),
         target = str_remove(target, '\\_'))

## 2.2 Load data extracted from accused cellphone phonebooks

# Read in all phonebooks
p <- '../Datasets/Phonebooks/All_Phonebooks.xlsx' 
phonecontactlinks <- cbind(read_xlsx(path = p), source = excel_sheets(p)[1])
for(s in 2:length(excel_sheets(p))) phonecontactlinks <- rbind(phonecontactlinks, cbind(read_xlsx(path = p, sheet = s), source = excel_sheets(p)[s]))

phonecontactlinks <- select(phonecontactlinks, -Name) %>%  # Don't need name
  mutate(Cell = str_sub(Cell, -9, -1)) %>% 
  filter(nchar(Cell) == 9) %>% # Only 9-digit cell numbers
  rename(cell = Cell)

# Replace phonebook numbers with known names
for(nr in numbs$Cell1) phonecontactlinks$cell[phonecontactlinks$cell == nr] <- numbs$ID[numbs$Cell1 == nr]
for(nr in numbs$Cell2) phonecontactlinks$cell[phonecontactlinks$cell == nr] <- numbs$ID[numbs$Cell2 == nr]
for(nr in numbs$Cell3) phonecontactlinks$cell[phonecontactlinks$cell == nr] <- numbs$ID[numbs$Cell3 == nr]

# Replace unknown phonebook numbers with already labeled unknowns from calls
phonecontactlinks <- left_join(phonecontactlinks, unknowns, 'cell') %>% 
  mutate(cell = ifelse(is.na(id), cell, id)) %>% #
  select(-id)

# Replace the rest with new unknown ids
phonebook_unknowns <- unique(phonecontactlinks$cell[nchar(phonecontactlinks$cell) == 9])
phonebook_unknowns <- as.data.frame(cbind(cell = phonebook_unknowns, id = paste0('Ph_Unk', 1:length(phonebook_unknowns))))
phonecontactlinks <- left_join(phonecontactlinks, phonebook_unknowns, 'cell') %>% 
  mutate(cell = ifelse(is.na(id), cell, id)) %>% #
  rename(contact = cell) %>% 
  select(source, contact, -id) %>% 
  distinct()

# 3. Testimonial evidence data entry ####

## 3.1. Load compiled data extracted from judgment based on acc testimony about their acquaintances before the robbery
acq <- read.csv('../Datasets/Edited_Acquaintances/acq_before_edge.csv', header = T) %>%
  mutate(source = str_remove(source, '\\_'),
         target = str_remove(target, '\\_'))

# 4. Nodes and Node attributes ####

nodes <- data.frame(Id = c(unique(c(edgelist$sender, edgelist$receiver)), 'Acc10', 'SAPSHandler', 'Noted2')) # Acc10 is omitted because not in edgelist (no nr)

# Add additional attributes
attr_list <- read_xlsx('Metadata_files/Link_matrices.xlsx', 4) %>% mutate(Id = str_remove(Id, '\\_'))

# Just individuals from call records
all_nodes <- nodes %>% 
  arrange(Id) %>% 
  mutate(Type = substr(Id, 1, 3)) %>% 
  left_join(attr_list, 'Id')  

# Now all of these plus phonebook individuals
ph_all_nodes <- select(all_nodes, Id, Type) %>% 
  rbind(select(phonebook_unknowns, Id = id) %>% 
          mutate(Type = 'Ph_Unk')) %>% 
  left_join(attr_list, 'Id')   %>% 
  arrange(Type)

# Rather add more descriptive Types for plotting purposes later
all_nodes <- all_nodes %>% 
  mutate(Type = case_when(
    Type == 'Acc' ~ 'Accused',
    Type == 'Ass' ~ 'Associate',
    Type == 'Not' ~ 'Noted Person',
    Type == 'SAP' ~ 'SAPS Handler',
    Type == 'Sus' ~ 'Suspected Associate',
    Type == 'Tax' ~ 'Taxi Employee',
    Type == 'Unk' ~ 'Unknown')
  )

ph_all_nodes <- ph_all_nodes %>% 
  mutate(Type = case_when(
    Type == 'Acc' ~ 'Accused',
    Type == 'Ass' ~ 'Associate',
    Type == 'Not' ~ 'Noted Person',
    Type == 'Ph_Unk' ~ 'Phonebook Contact',
    Type == 'SAP' ~ 'SAPS Handler',
    Type == 'Sus' ~ 'Suspected Associate',
    Type == 'Tax' ~ 'Taxi Employee',
    Type == 'Unk' ~ 'Unknown')
  )

# 5. Remove unnecessary objects ####

rm(list=ls()[! ls() %in% c('all_nodes','edgelist', 'hofcontactslinks', 'numbs', 'acq', 'phonecontactlinks', 'ph_all_nodes')])



