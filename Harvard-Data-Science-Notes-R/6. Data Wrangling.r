###########################
# IMPORTING SPREADSHEETS #
##########################

# It is important to know your working directory
getwd()   # Find wd
setwd()   # Change wd
# All file reading functions look to the wd

# Get file from a downloaded package example
path <- system.file("extdata", package="dslabs")
list.files(path)

# When moving file, file.path() works across OS and is preferred to
# paste function
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# Copy file to your working directory
file.copy(fullpath, getwd())

# Check it was copied
file.exists(filename)


# READR AND READXL FUNCTIONS FROM TIDYVERSE
# ReadR is a tidyverse library that contains functions for reading
# data into R
# read_table ~ white space delimited values (.txt)
# read_csv ~ comma delimited values (.csv)
# read_csv2 ~ semicolon delimited values (.csv)
# read_tsv ~ tab delimited values (.tsv)
# read_delim ~ reads general text file format, define delimiter (.txt)

# ReadXL provides functions for reading Excel files into R
# read_excel ~ auto detect the format (.xls, .xlsx)
# read_xls ~ original format (.xls)
# read_xlsc ~ new format (.xlsx)

# excel_sheets gives the name of the sheets within an excel file
# and can be passed to the sheet argument in the functions above

# We can use read_lines to see what is in a file
read_lines("murders.csv", n_max=3)

# Now we know what's in the file we are ready to load the data
dat <- read_csv(filename)   # or
dat <- read_csv(fullpath)
# We get a message telling us what data types R assigned
# The loaded contents are a tibble
head(dat)


# IMPORTING DATA USING STANDARD R FUNCTIONS
# R has inbuilt functions such as
# read.table
# read.csv
# read.delim

# There are a couple of important differences to remember
dat2 <- read.csv(filename)
class(dat2)           # Imports as a dataframe not a tibble
class(dat2$region)    # Characters are imported as factors

# This can be avoided by setting the stringsAsFactors arg to false
dat3 <- read.csv(filename, stringsAsFactors = FALSE)
class(dat3$region)


# DOWNLOADING FILES FROM THE INTERNET
# We can download from the internet directly if we have a URL
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
# You can also download the file to store as a local copy
download.file(url, "murders.csv")   # Filename here is new filename

# When downloading from the internet there are 2 functions
# that are useful
# tempdir() creates a directory is very likely to be unique
# tempfile() creates a character string that is very like to be unique
tmp_filename <- tempfile()        # Creates unique string
download.file(url, tmp_filename)  # Downloads to the string
dat <- read_csv(tmp_filename)     # Reads data in
file.remove(tmp_filename)         # Deletes temp file
head(dat)


# ASSESSMENT QUESTION
library(tidyverse)
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url, col_names=FALSE)
nrow(dat)
ncol(dat)


#############################
# TIDYING DATA - TIDY DATA #
############################

# Tidy data definition
# Each row represents one observation and the columns represent
# the different variables that we have data on for those observations

# Original gapminder data file (not tidied)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, '1960':'1967')

# Difference between wide_data and tidy_data format
# Wide format:  Each row contains several observations
#               One of the variables (year) is stored in the header

# In order to use the tidyverse, we need to wrange this data
# into a tidy format

# The tidyr package includes several functions that are useful
# for tidying data

# GATHER FUNCTION
# 3rd arg - specifies columns to gather, default is all columns
# 1st arg - sets the name of the column that will hold the variable
#             that are currently kept in the wide column name
# 2nd arg - sets the name of the column that will hold the values
#             in the column cells
new_tidy_data <- wide_data %>%
                  gather(year, fertility, '1960':'2015')

# As we took all the columns except country it is quicker to 
# define what columns shouldn't be gathered, like this:
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

# Check the classes of the new columns
class(new_tidy_data$year)       # integer in tidy

# We can add another argument to the gather function
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert=TRUE)
class(new_tidy_data$year)       # integer in tidy

# The spread function is the inverse of gather and converts data
# from a tidy format into a wide format
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, '1960':'1969')


########################
# SEPARATE AND UNITE #
#######################

# The following data is more complex and is not tidy or optimal
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
# See that the column names have names for 2 variables in them
# life_expectancy and fertility
# It doesn't make sense to use year as a column name as it spans
# multiple columns. We will call the column key.
# gather all columns except country (the default of this function)
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# This is a common problem so tidyr has a function for this
# separate on underscores (underscore is default so can be left out)
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))
# We also see that we get warnings because the "_" is also contained
# with the expected column name (life_expectancy) and has been
# caught and truncated in the separate()

# We can catch this with an extra column and tell separate how to
# fill in the gaps
# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# However this is a better way where we can merge the extra column
# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_",
                          extra = "merge")

# separate then spread to get tidy
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"),
                    fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"),
                    fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)
  # NA comes from the NA we filled the column with earlier


# ASSESSMENT EXERCISES
library(tidyverse)
library(dslabs)
head(co2)

# Q9
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
# Q10
co2_tidy <- gather(co2_wide,month,co2,-year)

# Q11
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# Q12
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat_tidy <- spread(dat, gender, admitted)

# Q13
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))



#####################
# COMBINING TABLES #
####################

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2    # slice takes rows by row number - above omits rows 4 and 6

# experiment with different joins
left_join(tab1, tab2)       # Join without piping data
tab1 %>% left_join(tab2)    # NAs in tab2 where missing
tab1 %>% right_join(tab2)   # NAs in tab1 where missing
inner_join(tab1, tab2)      # Only where join matches both tables
full_join(tab1, tab2)       # Puts NA in both tables where missing
semi_join(tab1, tab2)       # Only tab1 data where join IN tab2
anti_join(tab1, tab2)       # Only tab1 data where join NOT IN tab2


############
# BINDING #
###########

# Unlike the join functions, the binding functions do not try to
# match by a variable, but rather just combine datasets.
# Objects must match structure wise otherwise we get an error
bind_cols(a = 1:3, b = 4:6)

# bind_cols() binds two objects by making them columns in a tibble.
# The R-base function cbind() binds columns but makes a data frame
# or matrix instead.
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

# The bind_rows() function is similar but binds rows instead of
# columns. The R-base function rbind() binds rows but makes 
# a data frame or matrix instead.
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)


##################
# SET OPERATORS #
#################

# When applied to vectors, as their name suggests they are quite
# useful - union, intersect, etc..

# However, when using the tidyverse, or more specifically dplyr
# then these functions can also be applied to data frames

# You can take intersections of vectors using intersect().
# This returns the elements common to both sets.
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- 1:5
tab2 <- 3:7
intersect(tab1, tab2)

# You can take the union of vectors using union().
# This returns the elements that are in either set.
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- 1:5
tab2 <- 3:7
union(tab1, tab2)

# The set difference between a first and second argument can be
# obtained with setdiff(). Note that this function is not symmetric.
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- 1:5
tab2 <- 3:7
setdiff(tab1, tab2)

# The function set_equal() tells us if two sets are the same,
# regardless of the order of elements.
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)


# EXERCISES
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
# Check the Master data frame
Master %>% as_tibble()

# Q5
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

# Q6
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

# Q7
Awards <- AwardsPlayers %>% filter(yearID == 2016) %>%
  select(playerID) %>% distinct()

setdiff(top_names$playerID, Awards$playerID)



#################
# WEB SCRAPING #
################

# Web scraping is extracting data from a website.

# The rvest web harvesting package includes functions to
# extract nodes of an HTML document: html_nodes() extracts
# all nodes of different types, and html_node() extracts 
# the first node.

# html_table() converts an HTML table to a data frame.

# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)
# We will revisit this example once we learn how to remove
# commas and other text from the values


##################
# USING CSS SELECTORS #
#################

# To demonstrate this we will try to extract the recipe 
# name, total preparation time, and list of ingredients
# from this guacamole recipe. Looking at the code for 
# this page, it seems that the task is impossibly complex.
# However, selector gadgets actually make this possible.
# SelectorGadget is piece of software that allows you to 
# interactively determine what CSS selector you need to 
# extract specific components from the webpage.
# If you plan on scraping data other than tables, we
# highly recommend you install it. A Chrome extension is
# available which permits you to turn on the gadget 
# highlighting parts of the page as you click through,
# showing the necessary selector to extract those segments.

# You can see how complex the selectors are. In any case we are now 
# ready to extract what we want and create a list:
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

# You can see how complex the selectors are. In any case we are now ready
# to extract what we want and create a list:
guacamole <- list(recipe, prep_time, ingredients)
guacamole

# Since recipe pages from this website follow this general layout, we 
# can use this code to create a function that extracts this information:
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

# and then use it on any of their webpages:
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

# There are several other powerful tools provided by rvest. For example, 
# the functions html_form(), set_values(), and submit_form() permit you
# to query a webpage from R. This is a more advanced topic not covered
# here.


# EXERCISES - MAYBE THESE WILL WORK

# Locate and load web page
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

# Find the tablse you want with the tables node function - html_nodes()
nodes <- html_nodes(h, "table")
class(nodes)    # Returns tables in XML nodeset format

# View the text within the nodes
html_text(nodes[[8]])

# If the node content is table then we use html_table to create a dataframe
html_table(nodes[[8]])


# Q1
# Convert the first 4 nodes and inspect them to find tables of team payroll
df1 <- html_table(nodes[[1]], fill=TRUE)
df2 <- html_table(nodes[[2]])
df3 <- html_table(nodes[[3]])
df4 <- html_table(nodes[[4]])
# In a rather unexpected twise, this is the answer
sapply(nodes[1:4], html_table, fill=TRUE)    # 2, 3, 4 give tables with payroll info


# Q2 
# Inspect the last 3 nodes
html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])
# Caught out by results format


# Q3
# Remove "No." from tab_1
# Makes first row headers
# Full join and see how many records
tab_1 <- html_table(nodes[[12]], header=TRUE) %>%
          select(Team, Payroll, Average)
tab_2 <- html_table(nodes[[21]], header=TRUE)
full_join(tab_1, tab_2, by=c("Team"))
# 58 rows


# Q4
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
length(tab)


# Q5
# Analyse first 7 html tables using fill=true.
# First table with 9 columns, first = date(s) conducted
x <- sapply(tab[1:length(tab)],html_table, fill=TRUE)
y<- sapply(x, names)
y



###################
# STRING PARSING #
##################

# The most common tasks in string processing include:
# extracting numbers from strings
# removing unwanted characters from text
# finding and replacing characters
# extracting specific parts of strings
# converting free form text to more uniform formats
# splitting strings into multiple values
# The stringr package in the tidyverse contains string processing functions
#   that follow a similar naming format (str_functionname) and are
#   compatible with the pipe.


# DEFINING STRINGS: SINGLE AND DOUBLE QUOTES & ESCAPING
# Define a string by surrounding text with either single quotes
# or double quotes
s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

# To include a single quote inside a string, use double quotes on the outside
# To include a double quote inside a string, use single quotes on the outside
a <- "5'"      # 5 foot
a <- '10"'     # 10 inches
# The cat() function displays a string as it is represented inside R.
# To include a double quote inside of a string surrounded by double quotes
cat(a)

# use the backslash (\) to escape the double quote
# Escape a single quote to include it inside of a string defined by single
# quotes
# s <- '5'10"'    # error
# s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)
# We will see additional uses of the escape later.


# STRINGR PACKAGE
# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes - they are character, not numeric
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

# The main types of string processing tasks are
# detecting, locating, extracting and replacing elements of strings.

# The stringr package from the tidyverse includes a variety of string
# processing functions that begin with str_ and take the string as the
# first argument, which makes them compatible with the pipe.

# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])     # Cannot coerce to numberic
library(tidyverse)    # includes stringr

# We can find columns with commas (for example) with this code:
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
# We then use str_replace_all to remove them
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# Using parse_number() it becomes more efficient
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

# We can parse numbers for our table using this
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head


raw <- c("May", "$117,921", "15,437")
# Remove $ and comma
raw %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)

# You can use the parse_number command to remove all non-numeric characters. 
raw %>% mutate_at(2:3, parse_number)


# MORE ADVANCED STRING PARSING

# In the raw heights data, many students did not report their height 
# as the number of inches as requested. There are many entries with real
# height information but in the wrong format, which we can extract with
# string processing

# When there are both text and numeric entries in a column, the column 
# will be a character vector. Converting this column to numeric will 
# result in NAs for some entries

# To correct problematic entries, look for patterns that are shared 
# across large numbers of entries, then define rules that identify those
# patterns and use these rules to write string processing tasks
# Use suppressWarnings() to hide warning messages for a function

# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


################################
# REGEX - REGULAR EXPRESSIONS #
################################
# A regular expression (regex) is a way to describe a specific pattern
# of characters of text. A set of rules has been designed to do this
# specifically and efficiently.

# stringr functions can take a regex as a pattern.
# str_detect() indicates whether a pattern is present in a string.

# The main difference between a regex and a regular string is that a
# regex can include special characters.

# The | symbol inside a regex means "or".

# Use '\\d' to represent digits. The backlash is used to distinguish it
# from the character 'd'. In R, you must use two backslashes for digits
# in regular expressions; in some other languages, you will only use one
# backslash for regex special characters.

# str_view() highlights the first occurrence of a pattern, and the
# str_view_all() function highlights all occurrences of the pattern.

# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
str_view(s, pattern)
# highlight all instances of a pattern
str_view_all(s, pattern)

# Using the backslash escape to find digits d
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "Six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)


################################################
# CHARACTER CLASSES, ANCHORS AND QUANITIFIERS #
###############################################

# Generally check for two types of error
# Failing to match
# Incorrectly matching

# Character classes are used to define a series of characters than can be
# matched. We define character classes with the square brackets [ ]
str_view(s, "[56]") # Looks for 5 or 6
# We can do this with range too, using brackets or \\d
str_view(s, "[5-6]") # Looks for 5 or 6
str_view(s, "\\d") # Looks for all digits
str_detect(s, "\\d") # Looks for all digits
# In Regex EVERYTHING is a character, there are no numbers
# [0-9], [a-z], [A-Z], or [a-zA-Z]

# ANCHORS let us define patterns that must start or end at specific places
# for example finding an exact digit
# The two most common anchors are ^ and $ which represent the beginning
# and end of a string respectively
# ^\\d$ = start of string, followed by 1 digit, followed by end of string
str_view(s, "^\\d$") # Looks for strings with only 1 digit

# Example detecting inches in height, these can have 2 characters - 11 inches
# This is done by QUANTIFIERS and is achieved using curly brackets { }
# along with the possible number of times that the previous entry repeats
# The pattern for detecting 1 or 2 digits is "\\d{1,2}"
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# To construct a pattern looking for feet and inches
pattern <- "^[4-7]'\\d{1,2}\"$"     # escape the " for inches
yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"", "I am 5'11\"", "3' 3\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)
# NOTE this allows inches > 12. We will learn to how to restrict later


# SEARCH AND REPLACE WITH REGEX
# str_replace() replaces the first instance of the detected pattern with
# a specified string.

# Spaces are characters and R does not ignore them. Spaces are specified
# by the special character \\s.

# Additional quantifiers include *, + and ?. * means 0 or more instances
# of the previous character. ? means 0 or 1 instances. + means 1 or more
# instances.

# Before removing characters from strings with functions like str_replace()
# and str_replace_all(), consider whether that replacement would have
# unintended effects - "6 1" would be become 61 instead of 73 (6ft1)

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()


# GROUPINGS WITHIN PATTERNS
# Groups are defined using parentheses

# Once we define groups, we can use the function str_match() to extract
# the values these groups define. str_extract() extracts only strings
# that match a pattern, not the values defined by groups

# You can refer to the ith group with \\i.
# For example, refer to the value in the second group with \\2.
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)     # Makes columns of grouping results
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head


# TESTING AND IMPROVING
# Wrangling with regular expressions is often an iterative process of 
# testing the approach, looking for problematic entries, and improving 
# the patterns

# Use the pipe to connect stringr functions

# It may not be worth writing code to correct every unique problem in the
# data, but string processing techniques are flexible enough for most needs
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind    # Returns NOT of results
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

# show problems
converted[!index]

# COMPLEX PATTERN MATCHING EXAMPLE
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
    str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
    str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)


#####################
# STRING PARSING 3 #
####################

# SEPARATE WITH REDEX
# The extract() function behaves similarly to the
# separate() function but allows extraction of groups from
# regular expressions

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")


# Four clear patterns of entries have arisen along with some
# other minor problems:
  
# Many students measuring exactly 5 or 6 feet did not enter
# any inches. For example, 6' - our pattern requires that
# inches be included

# Some students measuring exactly 5 or 6 feet entered just that
# number

# Some of the inches were entered with decimal points. For
# example 5'7.5''. Our pattern only looks for two digits

# Some entires have spaces at the end, for example 5 ' 9

# Some entries are in meters and some of these use European
# decimals: 1.6, 1,7

# Two students added cm

# One student spelled out the numbers: Five foot eight inches

# It is not necessarily clear that it is worth writing code
# to handle all these cases since they might be rare enough
# However, some give us an opportunity to learn some more
# regex techniques so we will build a fix


# Case 1
# If we add a '0 to, for example, convert all 6 to 6'0, then 
# our pattern will match. This can be done using groups using
# the following code:
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
# The pattern says it has to start (^), be followed with a
# digit between 4 and 7, and then end there ($). The 
# parenthesis defines the group that we pass as \\1 to the
# replace regex

# Cases 2 and 4
# We can adapt this code slightly to handle case 2 as well
# which covers the entry 5'. Note that the 5' is left untouched
# by the code above. This is because the extra ' makes the
# pattern not match since we have to end with a 5 or 6. To
# handle case 2, we want to permit the 5 or 6 to be followed
# by no or one symbol for feet. So we can simply add '{0,1} 
# after the ' to do this. We can also use the none or once 
# special character ?. As we saw previously, this is different 
# from * which is none or more. We now see that this code also
# handles the fourth case as well:
str_replace(s, "^([56])'?$", "\\1'0")
# Note that here we only permit 5 and 6 but not 4 and 7. This
# is because heights of exactly 5 and exactly 6 feet tall are
# quite common, so we assume those that typed 5 or 6 really
# meant either 60 or 72 inches. However, heights of exactly
# 4 or exactly 7 feet tall are so rare that, although we
# accept 84 as a valid entry, we assume that a 7 was entered
# in error.

# Case 3
# We can use quantifiers to deal with  case 3. These entries
# are not matched because the inches include decimals and 
# our pattern does not permit this. We need allow the second
# group to include decimals and not just digits. This means 
# we must permit zero or one period . followed by zero or more
# digits. So we will use both ? and *. Also remember that for
# this particular case, the period needs to be escaped since
# it is a special character (it means any character except a
# line break).

# So we can adapt our pattern, 
# currently ^[4-7]\\s*'\\s*\\d{1,2}$, to permit a decimal at 
# the end:
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# Case 5
# Meters using commas, we can approach similarly to how we
# converted the x.y to x'y. A difference is that we require
# that the first digit is 1 or 2:
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
# We will later check if the entries are meters using their
# numeric values.

# Trimming
# In general, spaces at the start or end of the string are
# uninformative. These can be particularly deceptive because 
# sometimes they can be hard to see:
s <- "Hi "
cat(s)
identical(s, "Hi")
# This is a general enough problem that there is a function
# dedicated to removing them: str_trim.
str_trim("5 ' 9 ")
# To upper and to lower case
# One of the entries writes out numbers as words: Five foot 
# eight inches. Although not efficient, we could add 12 extra
# str_replace to convert zero to 0, one to 1, and so on. To
# avoid having to write two separate operations for Zero and
# zero, One and one, etc., we can use the str_to_lower() 
# function to make all words lower case first:
s <- c("Five feet eight inches")
str_to_lower(s)

# Putting it into a function
# We are now ready to define a procedure that handles converting 
# all the problematic cases. We can now put all this together
# into a function that takes a string vector and tries to
# convert as many strings as possible to a single format.
# Below is a function that puts together the previous code 
# replacements:
convert_format <- function(s){
    s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
  }

# We can also write a function that converts words to numbers:
words_to_numbers <- function(s){
    str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
  }

# Now we can see which problematic entries remain:
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


# LET'S PUT IT ALL TOGETHER
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
    mutate(original = height, 
    height = words_to_numbers(height) %>% convert_format()) %>%
    extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
    mutate_at(c("height", "feet", "inches"), as.numeric) %>%
    mutate(guess = 12*feet + inches) %>%
    mutate(height = case_when(
      !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
      !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
      !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
      !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
      TRUE ~ as.numeric(NA))) %>%
    select(-guess)

# We can check all the entries we converted using the 
# following code:
new_heights %>%
    filter(not_inches(original)) %>%
    select(original, height) %>% 
    arrange(height) %>%
    View()     # Shows data grid

# Let's take a look at the shortest students in our dataset 
# using the following code:
new_heights %>% arrange(height) %>% head(n=7)

# We see heights of 53, 54, and 55. In the original heights
# column, we also have 51 and 52. These short heights are
# very rare and it is likely that the students actually meant 
# 5'1, 5'2, 5'3, 5'4, and 5'5. But because we are not completely
# sure, we will leave them as reported.


# STRING SPLITTING
# The function str_split() splits a string into a character 
# vector on a delimiter (such as a comma, space or underscore)
# By default, str_split() generates a list with one element for
# each original string. Use the function argument 
# simplify=TRUE to have str_split() return a matrix instead.

# The map() function from the purrr package applies the same
# function to each element of a list. To extract the ith entry 
# of each element x, use map(x, i).

# map() always returns a list. Use map_chr() to return a
# character vector and map_int() to return an integer.
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]  # Makes first row header

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# map_chr() returns characters, map_int() returns integers
# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)


##########################
# EXTRACTING FROM A PDF #
#########################

# One of the datasets provided in dslabs shows scientific funding rates
# by gender in the Netherlands:
library(dslabs)
data("research_funding_rates")
research_funding_rates 

# The data come from a paper published in the prestigious journal PNAS
# However, the data are not provided in a spreadsheet; they are in a
# table in a PDF document. We could extract the numbers by hand, but 
# this could lead to human error. Instead we can try to wrangle the
# data using R

# Downloading the data
# We start by downloading the PDF document then importing it into R
# using the following code:
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# If we examine the object text we notice that it is a character vector
# with an entry for each page. So we keep the page we want using the
# following code:
  
raw_data_research_funding_rates <- txt[2]
# The steps above can actually be skipped because we include the raw
# data in the dslabs package as well:
  
data("raw_data_research_funding_rates")
# Looking at the download. Examining this object,
raw_data_research_funding_rates %>% head

# we see that it is a long string. Each line on the page, including the
# table rows, is separated by the symbol for newline: \n.

# We can therefore can create a list with the lines of the text as
# elements:
tab <- str_split(raw_data_research_funding_rates, "\n")

# Because we start off with just one element in the string, we end up
# with a list with just one entry:
tab <- tab[[1]]
# By examining this object,
tab %>% head
# we see that the information for the column names is the third and
# fourth entires:
the_names_1 <- tab[3]
the_names_2 <- tab[4]
# In the table, the column information is spread across two lines. We
# want to create one vector with one name for each column. We can do
# this using some of the functions we have just learned.

# Extracting the table data. Let's start with the first line:
the_names_1
# We want to remove the leading space and everything following the comma
# We can use regex for the latter. Then we can obtain the elements by
# splitting using the space. We want to split only when there are 2 or
# more spaces to avoid splitting success rate. So we use the 
# regex \\s{2,} as follows:
the_names_1 <- the_names_1 %>%
    str_trim() %>%
    str_replace_all(",\\s.", "") %>%
    str_split("\\s{2,}", simplify = TRUE)
the_names_1

# Now let's look at the second line:
the_names_2
# Here we want to trim the leading space and then split by space as we
# did for the first line:
the_names_2 <- the_names_2 %>%
    str_trim() %>%
    str_split("\\s+", simplify = TRUE)
the_names_2

# Now we can join these to generate one name for each column:
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
    str_to_lower() %>%
    str_replace_all("\\s", "_")
the_names

# Now we are ready to get the actual data. By examining the tab object, 
# we notice that the information is in lines 6 through 14. We can use
# str_split() again to achieve our goal:
new_research_funding_rates <- tab[6:14] %>%
    str_trim %>%
    str_split("\\s{2,}", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(the_names) %>%
    mutate_at(-1, parse_number)
    new_research_funding_rates %>% head()

# We can see that the objects are identical:
identical(research_funding_rates, new_research_funding_rates)



########################################
# RECODING - A KIND OF FIND & REPLACE #
#######################################

# Change long factor names with the recode() function from the tidyverse 

# Other similar functions include recode_factor() and fct_recoder() in 
# the forcats package in the tidyverse. The same result could be obtained
# using the case_when() function, but recode() is more efficient to write
# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
    filter(region=="Caribbean") %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()

# display long country names
gapminder %>% 
    filter(region=="Caribbean") %>%
    filter(str_length(country) >= 12) %>%
    distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
    mutate(country = recode(country, 
                        'Antigua and Barbuda'="Barbuda",
                        'Dominican Republic' = "DR",
                        'St. Vincent and the Grenadines' = "St. Vincent",
                        'Trinidad and Tobago' = "Trinidad")) %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()


# EXERCISES
# Add a third column for decimals
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)
extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")

# Split the vector into 3 columns
day <- c("Monday", "Tuesday")
str_split(schedule, ", | and ")
str_split(schedule, ",\\s|\\sand\\s")

# Turn the vector into a tidy table
schedule <- c("Mandy, Chris, Laura", "Jim, Mary, Joseph")

# Recode column data
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))


# Q5 onwards
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

# Remove % and convert to decimal probability
polls <- polls %>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
polls <- polls[-1]
view(polls)
as.numeric(str_replace(polls$remain, "%", ""))/100

# Function to remove NAs with zero
str_replace("N/A", 0)


####################
# DATES AND TIMES #
###################

# In computers a reference date is selected as the base date, or 'Epoch'
# which is then used to reference all over dates and therefore enable conversion
# of dates to numbers.  Computer languages usually use Jan 1st 1970

# The tidyverse includes functionality for dealing with dates through the
# lubridate package
library(lubridate)

# Some useful tricks
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

# extract month label
month(dates, label = TRUE)

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

# Times
Sys.time()            # current system time
now()                 # current time in your time zone
now("GMT")            # current time in GMT
now() %>% hour()      # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# See all available timezones using this function
OlsonNames()

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)


################
# TEXT MINING #
###############

# The tidytext package helps us convert free form text into a tidy table

# Use unnest_tokens() to extract individual words and other meaningful
# chunks of text

# Sentiment analysis assigns emotions or a positive/negative score to
# tokens. You can extract sentiments using get_sentiments(). Common
# lexicons for sentiment analysis are "bing", "afinn", "nrc" and
# "loughran"

# Recommended book on text mining; https://www.tidytextmining.com/

# Analysis of Donald Trump's tweets
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# In general, we can extract data directly from Twitter using the
# rtweet package. However, in this case, a group has already compiled
# data for us and made it available at
# http://www.trumptwitterarchive.com.

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

# For convenience the tweets are also in the dslabs library
library(dslabs)
data("trump_tweets")

# This is data frame with information about the tweet:
head(trump_tweets)

# The variables that are included are:
names(trump_tweets)

# The help file ?trump_tweets provides details on what each variable
# represents. The tweets are represented by the text variable:
trump_tweets %>% select(text) %>% head

# and the source variable tells us the device that was used to compose 
# and upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))

# We can use extract to remove the Twitter for part of the source and 
# filter out retweets.
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

# We are interested in what happened during the campaign, so for the analysis
# here we will focus on what was tweeted between the day Trump announced his
# campaign and election day. So we define the following table:
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# We can now use data visualization to explore the possibility that two 
# different groups were tweeting from these devices. For each tweet, we
# will extract the hour, in the east coast (EST), it was tweeted then 
# compute the proportion of tweets tweeted at each hour for each device.
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous() +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# We notice a big peak for the Android in early hours of the morning, 
# between 6 and 8 AM. There seems to be a clear different in these patterns
# We will therefore assume that two different entities are using these two
# devices. Now we will study how their tweets differ. To do this we introduce
# the tidytext package.

# Text as data
# The tidytext package helps us convert free from text into a tidy table. 
# Having the data in this format greatly facilitates data visualization and 
# applying statistical techniques.
install.packages("tidytext")
library(tidytext)
# The main function needed to achieve this is unnest_tokens(). A token 
# refers to the units that we are considering to be a data point. The most
# common tokens will be words, but they can also be single characters, 
# ngrams, sentences, lines or a pattern defined by a regex. The functions 
# will take a vector of strings and extract the tokens so that each one
# gets a row in the new table. Here is a simple example:
example <- data_frame(line = c(1, 2, 3, 4),
              text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Now let's look at a quick example with a tweet number 3008:
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# Note that the function tries to convert tokens into words and strips
# characters important to twitter such as # and @. A token in twitter is
# not the same as in regular English. For this reason, instead of using the
# default token, words, we define a regex that captures twitter character. 
# The pattern appears complex but all we are defining is a pattern that
# starts with @, # or neither and is followed by any combination of letters 
# or digits:
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# We can now use the unnest_tokens() function with the regex option and 
# appropriately extract the hashtags and mentions:
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# Another minor adjustment we want to make is remove the links to pictures:
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# Now we are ready to extract the words for all our tweets.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

# And we can now answer questions such as "what are the most commonly used
# words?"
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

# It is not surprising that these are the top words. The top words are not
# informative. The tidytext package has database of these commonly used
# words, referred to as stop words, in text mining:
stop_words
# If we filter out rows representing stop words with 
# filter(!word %in% stop_words$word):
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

# We end up with a much more informative set of top 10 tweeted words:
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# Some exploration of the resulting words (not show here) reveals a couple
# of unwanted characteristics in our tokens. First, some of our tokens are
# just numbers (years for example). We want to remove these and we can find
# them using the regex ^\d+$. Second, some of our tokens come from a quote
# and they start with '. We want to remove the ' when it's at the start of
# a word, so we will use str_replace(). We add these two lines to the code
# above to generate our final table:
tweet_words <- campaign_tweets %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word &
             !str_detect(word, "^\\d+$")) %>%
    mutate(word = str_replace(word, "^'", ""))

# Now that we have all our words in a table, along with information about 
# what device was used to compose the tweet they came from, we can start 
# exploring which words are more common when comparing Android to iPhone.

# For each word we want to know if it is more likely to come from an
# Android tweet or an iPhone tweet. We previously introduced the odds 
# ratio, a summary statistic useful for quantifying these differences
# For each device and a given word, let's call it y, we compute the odds
# or the ratio between the proportion of words that are y and not y and
# compute the ratio of those odds. Here we will have many proportions that 
# are 0 so we use the 0.5 correction.
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

# Given that several of these words are overall low frequency words we can
# impose a filter based on the total frequency like this:
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

# We already see somewhat of a pattern in the types of words that are being
# tweeted more in one device versus the other. However, we are not
# interested in specific words but rather in the tone. Vaziri's assertion
# is that the Android tweets are more hyperbolic. So how can we check this
# with data? Hyperbolic is a hard sentiment to extract from words as it
# relies on interpreting phrases. However, words can be associated to more
# basic sentiment such as as anger, fear, joy and surprise. In the next
# section we demonstrate basic sentiment analysis.
  
# Sentiment Analysis
# In sentiment analysis we assign a word to one or more "sentiment".
# Although this approach will miss context dependent sentiments, such as 
# sarcasm, when performed on large numbers of words, summaries can provide 
# insights.
  
# The first step in sentiment analysis is to assign a sentiment to each
# word. The tidytext package includes several maps or lexicons in the object
# sentiments:
    
# sentiments 
# There are several lexicons in the tidytext package that give different sentiments
# For example, the bing lexicon divides words into positive and negative
# We can see this using the tidytext function get_sentiments():
get_sentiments("bing")

# The AFINN lexicon assigns a score between -5 and 5, with -5 the most
# negative and 5 the most positive.
install.packages("textdata")
get_sentiments("afinn")

# The loughran and nrc lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# To start learning about how these lexicons were developed, read this 
# help file: ?sentiments.

# For the analysis here we are interested in exploring the different
# sentiments of each tweet, so we will use the nrc lexicon:
nrc <- get_sentiments("nrc") %>%
select(word, sentiment)

# We can combine the words and sentiments using inner_join(), which will 
# only keep words associated with a sentiment. Here are 10 random words
# extracted from the tweets:
tweet_words %>% inner_join(nrc, by = "word") %>% 
select(source, word, sentiment) %>% sample_n(10)

# Now we are ready to perform a quantitative analysis comparing Android and
# iPhone by comparing the sentiments of the tweets posted from each device.
# Here we could perform a tweet by tweet analysis, assigning a sentiment to
# each tweet. However, this somewhat complex since each tweet will have
# several sentiments attached to it, one for each word appearing in the
# lexicon. For illustrative purposes, we will perform a much simpler 
# analysis: we will count and compare the frequencies of each sentiment
# appears for each device.
sentiment_counts <- tweet_words %>%
    left_join(nrc, by = "word") %>%
    count(source, sentiment) %>%
    spread(source, n) %>%
    mutate(sentiment = replace_na(sentiment, replace = "none"))
  sentiment_counts

# Because more words were used on the Android than on the phone:
tweet_words %>% group_by(source) %>% summarize(n = n())

# for each sentiment we can compute the odds of being in the device: 
# proportion of words with sentiment versus proportion of words without and 
# then compute the odds ratio comparing the two devices:
sentiment_counts %>%
    mutate(Android = Android / (sum(Android) - Android) , 
           iPhone = iPhone / (sum(iPhone) - iPhone), 
           or = Android/iPhone) %>%
    arrange(desc(or))

# So we do see some difference and the order is interesting: the largest
# three sentiments are disgust, anger, and negative! But are they
# statistically significant? How does this compare if we are just assigning
# sentiments at random?
    
# To answer that question we can compute, for each sentiment, an odds ratio
# and confidence interval. We will add the two values we need to form a
# two-by-two table and the odds ratio:
library(broom)
log_or <- sentiment_counts %>%
    mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
            se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
            conf.low = log_or - qnorm(0.975)*se,
            conf.high = log_or + qnorm(0.975)*se) %>%
    arrange(desc(log_or))
  
# log_or
# A graphical visualization shows some sentiments that are clearly
# overrepresented:
log_or %>%
    mutate(sentiment = reorder(sentiment, log_or),) %>%
    ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
    geom_errorbar() +
    geom_point(aes(sentiment, log_or)) +
    ylab("Log odds ratio for association between Android and sentiment") +
    coord_flip() 

#  We see that the disgust, anger, negative sadness and fear sentiments are
# associated with the Android in a way that is hard to explain by chance
# alone. Words not associated to a sentiment were strongly associated with 
# the iPhone source, which is in agreement with the original claim about 
# hyperbolic tweets.
  
# If we are interested in exploring which specific words are driving these
# differences, we can back to our android_iphone_or object:
android_iphone_or %>% inner_join(nrc) %>%
    filter(sentiment == "disgust" & Android + iPhone > 10) %>%
    arrange(desc(or))

# We can make a graph:
android_iphone_or %>% inner_join(nrc, by = "word") %>%
    mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
    mutate(log_or = log(or)) %>%
    filter(Android + iPhone > 10 & abs(log_or)>1) %>%
    mutate(word = reorder(word, log_or)) %>%
    ggplot(aes(word, log_or, fill = log_or < 0)) +
    facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
    geom_bar(stat="identity", show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# TEXT MINING EXERCISES
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

# Polls starting in April
data(brexit_polls) 
brexit_polls %>% filter(month(startdate) == 4) %>% nrow
# Use round_date
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")
# Determine how many polls ended on each day (max wins)
table(weekdays(brexit_polls$enddate))

data(movielens)
head(movielens, n=3)
# Convert seconds since epoch to date
install.packages("anytime")
library(anytime)
# Most popular year
dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews
# Most popular hour of day
reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews


######################################
# THE GUTENBURGR PROJECT ASSESSMENT #
#####################################
library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
options(digits = 3)
# View books and metadata
gutenberg_metadata

# Find title "Pride and Prejudice"
gutenberg_metadata %>% filter(str_detect(title, "Pride and Prejudice"))
# Find the English language version
gutenberg_works(title=="Pride and Prejudice", languages="en")
# Download the book and do a word count with tidytext
book <- gutenberg_download(1342)
words <- data_frame(book)
words <- words %>% unnest_tokens(word, text) %>%
          filter(!is.na(word))
nrow(words)

# Remove stop words and do a word count
# filter(!word %in% stop_words$word):
words <- words %>% filter(!word %in% stop_words$word)
nrow(words)

# Remove any tokens that have a digit
words <- words %>% filter(!str_detect(word, "\\d"))

# Find most frequent words
frequent <- words %>% count(word) %>%
      top_n(10, n) %>% mutate(word = reorder(word, n)) %>%
      arrange(desc(n))

fq_gt_100 <- words %>% count(word) %>%
      filter(n>100) %>% mutate(word = reorder(word, n)) %>%
      arrange(desc(n)) %>% nrow


# Define the afinn lexicon
afinn <- get_sentiments("afinn") %>% select(word, value)
# Join onto words
words %>% inner_join(afinn, by = "word") %>%
  select(word, value) %>%
        # Positive words
        filter(value > 0) %>%  nrow
        # Value of 4
        #filter(value == 4) %>%  nrow




############################################################
# ASSESSMENT PROJECT - HURRICANE MARIA, PUERTO RICO, 2017 #
###########################################################

# Import PDF
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

# Load pdftools and extract the data
install.packages("pdftools")
library(pdftools)
txt <- pdf_text(fn)
length(txt)   # Appearances can be deceptive, 1 per page

# Extract page 9 and split using \n as the new line character
string <- txt[9]
pattern <- "\n"
x <- str_split(string, pattern)
class(x)
length(x)

# Define s as first entry of x
s <- x[[1]]   # Double square brackets
class(s)
length(s)

# Trim white space
s <- str_trim(s, side=c("both"))
s[[1]]

# Find the header row matching 2015 and get the index
header_index <- str_which(s, "2015")[1]
header_index

# Save the contents of the header row
header <- s[[header_index]]
# Split the row
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]

# Get tail index
tail_index <- str_which(s, "Total")[1]
# How many rows have a single numnber
n <- str_count(s, "\\d+")
sum(n==1)

# Remove entries from header_index and before and tail_index onwards
# Also remove entries where n = 1
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

# Remove all text that is not a digit or a space
s <- str_remove_all(s, "[^\\d\\s]")

# Convert s into a matrix containing just day and death count
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
as.numeric(s[,2]) %>% mean        # Mean deaths Sep 2016
as.numeric(s[,3]) %>% mean        # Mean deaths Sep 2017
as.numeric(s[1:19,4]) %>% mean    # Mean deaths before hurricane
as.numeric(s[20:30,4]) %>% mean        # Mean deaths after hurricane

# With answers and column headers - convert to table
tab <- s %>% as_data_frame() %>% setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2018"[20:30])

# Change tab to a tidy format
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))

# Make a plot of deaths v day, exclude 2018 as no data
# use colour to denote year, add a vertical line at day 20
# to denote the day that the hurricane hit
tab %>% filter(year != 2018) %>%
    ggplot(aes(day, deaths, color=year)) +
      geom_line() +
      geom_vline(xintercept = 20) +
      geom_point()

