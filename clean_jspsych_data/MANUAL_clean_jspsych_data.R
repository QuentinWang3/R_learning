# This is the MANUAL of function "clean_jspsych_data.R"
# By simply importing the function from the local file, users can use the function to clean jsPsych experiment result data files.
#
# Here is the description of parameters.
#' @param input_path 
#' The path to the file, default is NULL, and a selection window will open automatically for users to select the folder that has all the raw data.
#' 
#' @param phases_to_keep 
#' Because the raw data in jsPsych is composed of many rows that are redundant, only the rows containing these characters will be kept.
#' 
#' @param file_prefix_length 
#' For files named as "01MyName", the first two digits will be recorded in the dataframe as "pID" which means participants' ID.
#' 
#' @param ifws_pid_threshold 
#' Since my own jsPsych experiment has two versions for same group of participants, participants 1~15 are experiment type 1, and 16~30 are type 2, the number 15 is used to record this difference.
#' 
#' @param resp_col The name of column that records keyboard response.
#' @param answ_col The name of column that records correct answer of each trial.
#
# Here is a sample of how to use it.


library(rstudioapi)
library(tidyverse)


# import function via clean_jspsych_data.R file
source(file.choose())

# use function to clean the raw data (should be put in a folder) and the function returns a cleaned dataframe
clean_data <- clean_jspsych_data(input_path = NULL,
                   # by setting this to NULL, a folder selection window will open
                   phases_to_keep = c("word", "sentence"),
                   file_prefix_length = 2,
                   ifws_pid_threshold = 15,
                   resp_col = "response",
                   answ_col = "correct_ans" )

# Compare the data before and after, you can see the difference.

str(clean_data)

view(clean_data) 

