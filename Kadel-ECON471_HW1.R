## ECON 471 R assignment 1
library(dplyr)
# Question 1: Create a data frame with the following column 
# vector a) Student ID, b) Names c) Grades d) Gender and 
# e) Math Score. Have 8 observations or rows (you can assign
# any values or names):
ID <- 1:8
Names <- c("Ram","Radha","Hari","Megha","Neha","Bhim"
           ,"Bella","Arjun")
Grades <- c(44,54,63,64,40,72,58,50)
Gender <- c("male","female","male","female","female",
            "male","female","male")
Math <- c(60,58,71,43,49,75,49,43)
stu_dat <- data.frame(ID, Names, Grades, Gender, Math)

## Question 2:Calculate the mean and standard deviation
# of grades by gender (by using %>% command).
set_2 <- stu_dat %>% select(c("Grades","Gender")) %>% 
  group_by(Gender) %>% 
    summarize(avg = mean(Grades), std_dev = sd(Grades), 
              no. = n())

# Alternative w/o using Piping:
# set_2 <- stu_dat[,c("Grades","Gender")]
# summarize(group_by(set_2, Gender), avg = mean(Grades), 
#           std_dev = sd(Grades), no. = n())

## Question 3: Create another data frame with a) and b) 
# as above (same ID and Names as in Q1) but now with one 
# additional column but with only 6 observations. Have 
# student's nationality in this new column. Join these two 
# data frames using a) inner_join and b) left_join command.
nations_dat <- stu_dat[1:6, c("ID","Names")]
# adding new variable to the data frame:
nations_dat$Nationality <- c("Nepal","US","Canada",
  "Sri Lanka","India","Nepal")
# both the merging methods seem to give the same result:
injoin <- inner_join(nations_dat, stu_dat)
ljoin  <- left_join(nations_dat, stu_dat)

## Question 4: Add one more column to your first data 
# frame. In this column have the average grade that 
# includes math grades. To calculate this new average 
# assume the original grade is calculated out of 5
# courses, excluding math.
stu_dat$GPA <- (stu_dat$Grades*5 + stu_dat$Math)/6

## Question 5: Select only Student ID, Grades, and Math 
# from the first data frame and convert it to a long 
# format and then back to wide format.
set_5 <- select(.data = stu_dat, c("ID","Grades","Math"))
# the data frame is a wide data frame. tidyr library is 
# needed for the conversion:
long <- gather(set_5, Attribute, Value, -ID)
# convert it back to wide:
wide <- spread(long, Attribute, Value)
# check if wide and set_5 are the same, as expected:
identical(wide,set_5)
