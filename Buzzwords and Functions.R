##########
#BUZZWORDS
##########

#HOW MUCH/TOTAL AMOUNT? - SUM()

#TO WHO - GROUP_BY() / FILTER()
#ex. filter to Prince George's County vendors and using str_detect() and grouping by types of donors

# ex. how much state grant money went to stem cell research projects each fiscal year 
      #SUM()                             #FILTER                         #GROUP_BY()

#ex.find records where the zip code is 
     #filter()

#FIND ALL VARIATIONS - filtered manually in the dataframe to find key words and then use str_detect() and uses operators AND (&) OR (|) symbols to find all variations.

#PER YEAR / LIST OF EACH - GROUP_BY()
#ex. produce a list of grantees

#UNIQUE ROWS / ONLY ONE ROW PER CATEGORY - DISTINCT()

#IDENTIFY DUPLICATES / DUPLICATED - DUPLICATED()

#"WHEN YOU NEED TO MANUALLY CREATE GROUPS FOR YOURSELF BECAUSE THEY'RE NOT CLEAN ENOUGH FOR YOU TO USE OR THEY DON'T EXIST" - MUTATE() AND THEN CASE_WHEN()
#assigns a value based on conditions that you explicitly state for each case. 

#ex. clean the results so that you had one record per grantee

##############
#LAB RUN DOWNS
##############

library(tidyverse)
library(lubridate)
library(readr)
library(janitor)
library(refinr)


umd_courses <- read_rds("umd_courses.rds")

# Turn off scientific notation
options(scipen=999)

glimpse(umd_courses)
head(umd_courses)
summary(umd_courses)
colnames(umd_courses)
nrow(theatre_seats_15)

#R Comparison Operators (Used to COMPARE TWO VALUES)
#  ==  Equal to e.g., person == "Stacy"       !=  NOT Equal to
#  >   Greater than                           <   Less than
#  >=  Greater than or equal to               <=  Less than or equal to

#R Logical Operators (Used to COMBINE CONDITIONAL STATEMENTS)
#  &  "AND"     |  "OR"    !  "NOT"



#PL.1
umd_courses |>
  group_by(term) |>  
  summarise(
    count_classes = n(),           #count 
    total_seats = sum(seats),      #sum/total 
    mean_seats = mean(seats),      #mean (average)
    median_seats = median(seats),  #median (middle number)
    min_seats = min(seats),        #minimum
    max_seats = max(seats),        #maximum
  ) |>
  arrange(desc(total_seats))       #Ordering output

#PL.1
umd_courses |>
  group_by(term, department) |>    #grouping by multiple terms (more specific groups when more terms)
  summarise(
    count_classes = n()
  ) |>
  arrange(term, department)

#L.1
#is.na() indicates which elements are missing.
#  !is.na() indicates elements are NOT missing
umd_courses |>
  filter(!is.na(instructors)) |>               #filter(!is.na) this gets rid of rows that do not have a value for instructors. without the ! it would return rows that are empty. Double negative thinks about NA case first                                        
  filter(instructors!="Instructor: TBA")



#PL.2
#Using filter() to find specific records
#When it comes to filters, OR is additive (can ONLY ADD ROWS); AND is restrictive (can ONLY REMOVE ROWS).
journalism_courses <- umd_courses |> filter(department == "Journalism")   #String/Character filter
courses_2023 <- umd_courses |> filter(term > 202300)                      #Numerical Filter (with > operator)

theatre_seats_15 <- umd_courses |> filter(department == "Theatre") |> filter(seats >= 15)  #Filtering based on MULTIPLE CONDITIONS
and_theatre_seats_15 <- umd_courses |> filter(department == "Theatre" & seats >= 15)       #SAME FILTER but with & (AND) OPERATOR

and_theatre_seats_15 <- umd_courses |> filter(department == "Theatre" | seats >= 15)       #Filter with | (AND/OR) OPERATOR

#Filter on PARTIAL MATCH:
umd_courses |>
  filter(str_detect(title, "Shakespeare"))
#NOTE: `str_like` which uses the wildcard search operator % to do partial matching

#Filter with both string and numerical using & (AND) operator
filtered_courses <- umd_courses |> 
  filter(str_detect(title, "Climate") & seats >= 1) |> 
  group_by(department)

#Using select() to see only certain columns
selected_journalism_courses <- journalism_courses |> select(id, title)

#Using lubridate to work with dates
#Turning a character date into a real date
maryland_expenses <- maryland_expenses |> mutate(expenditure_date=mdy(expenditure_date))

maryland_expenses |>
  mutate(month = floor_date(expenditure_date, "month")) |>
  group_by(month) |>
  summarise(total_amount = sum(amount)) |>
  arrange(desc(total_amount))

#Using mutate() to add and update columns
general_22 <- general_22 |>
  mutate(
    total_votes = cox + moore + lashar + wallace + write_ins,    #New column of total_votes by adding variables
    pct_moore = moore/total_votes                                #New column of pct_moore by creating a ratio by dividing (moore/total_votes)
  )

standardized_maryland_expenses <- maryland_expenses |>
  mutate(
    payee_upper = str_to_upper(payee_name)              #CAPITALIZING String
  )

#Using case_when() to conditionally populate columns
maryland_expenses_with_state <- maryland_expenses |>
  mutate(
    state = case_when(
      str_detect(address, " Maryland ") ~ "MD",         #When the address variable partial matches " Maryland " THEN state = "MD"
      str_detect(address, " California ") ~ "CA",       #etc.
      str_detect(address, " Washington ") ~ "WA",
      str_detect(address, " Louisiana ") ~ "LA",
      str_detect(address, " Florida ") ~ "FL",
      str_detect(address, " North Carolina ") ~ "NC",
      str_detect(address, " Massachusetts ") ~ "MA",
      str_detect(address, " West Virginia ") ~ "WV",
      str_detect(address, " Virginia ") ~ "VA",
      .default = NA                                    #The default value. Here, if address is none of the specified states above, state == NA (NULL)
    )
  )

#PL.3
#Detect wrong spatial data
payments |>
  group_by(`Vendor Zip`) |>
  filter(str_length(`Vendor Zip`) < 5) |> 
  summarise(
    count=n()
  ) |>
  arrange(desc(count))

#Looking at categories
md_grants_loans |> 
  group_by(`Fiscal Year`, Category) |> 
  summarize(count = n()) |> 
  arrange(`Fiscal Year`)

# cleaning function
cleaned_md_grants_loans <- md_grants_loans |>
  clean_names() |> 
  rename(source = grantor) |>  #Rename Columns
  mutate(source = str_to_upper(source), grantee = str_to_upper(grantee), description = str_to_upper(description)) |>   #Uppercase Function
  distinct() |>  #Remove duplicate rows
  mutate(zip5 = str_sub(zip_code, start=1L, end=5L)) |> #Clean up ZIP code  (start=1L and end=5L LIMITS THE ZIP TO THE FIRST 5 NUMBERS)
  mutate(zip5 = case_when(
    zip5 == "Vario" ~ NA,     #Setting some weird ZIP entries to NA (NULL/BLANK)
    zip5 == "UB7 O" ~ NA,
    zip5 == "UB7 " ~ NA,
    .default = zip5))   

#Clean up zip5 field more with case_when()


#L.3




#PL.4







#Check for duplicate rows
cleaned_md_grants_loans |>
  get_dupes()


#filter for entries that do not match with raw and clean data

filter(Grantee != grantee_clean) |> 
  arrang(desc(count))     #this shows us which entries changed the most

#this is the same thing as saying 

mutate( matches = case_when(
  Grantee != grantee_clean ~ "does not match",
  .default = "matches")) |> 
  filter(matches == "does not match")


#L.4

#QUESTION TO ASK: when we're clustering, do we change the name to something else if we know or are pretty sure it means something else (e.g., Face Book to Facebook or Micheal"s Pizza even though the other option i s without an apostrophe)?



################
#NOTES ON TRENDS
################

#INCREASE/ DECREASES OVER TIME
#PEAKS (SPIKES) 
#DIPS
#             FOR BOTH YOU CAN ASK HOW MANY PERIODS?
#STAGNATING?
#IF NUMBERS SPIKE/DIP, DO THEY RECOVER TO PRE-SPIKE/DIP? WHEN?
#CYCLICAL
