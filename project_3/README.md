# Urban Ministries Project 3
## Amber Young

## Urban Ministries
Urban Ministries, established in 1983, offers many programs to help support homeless and low-income people in the Durham community. These programs include:

* A community shelter, which can accomodate over 140 people a night
* A community cafe, which offers breakfast, lunch, and dinner daily to the homeless
* A food pantry, that provides people with groceries, and
* A clothing closet, that supplies individuals and families with clothing each month.

All together this organization helps about 6,000 people per year with over 4,000 volunteers, and they rely heavily on donations of food and clothing from the community.

## The Data
The data, provided by Urban Ministries, focuses on their emergency shelter program, and can be found here:

https://github.com/biodatascience/datasci611/tree/gh-pages/data/project2_2019

## Purpose of this Project
The purpose of this project is to provide UM with summary statistics and plots describing the characteristics of people who have used their emergency shelter. 
Hopefully, with this information, UM can target its services to better serve its clients.

## Useful Variables
Some of the most interesting outcome variables for this project include: Entry Date, Exit Date, and Reason For Leaving.
From the dates we can calculate the length of stay for each person. 
The "Reason for Leaving" variable includes information on whether a client completed the program set out for them at UM.

The data also includes characteristics of shelter clients, such as:
disability status, age, gender, race, ethnicity, veteran status, etc.
These will be very useful in calculating summary statistics about what type of people are clients at UM.

## Instructions to recreate analysis
The report is already included here in the results folder.
To recreate this analysis, in any linux environment (such as the VCL), run the following:


git clone https://github.com/datasci611/bios611-projects-fall-2019-ayoung31.git

cd bios611-projects-fall-2019-ayoung31/project_3

make results/report.html
