# The Urban Ministries Project
### Conducted by: Amber Young

## What is Urban Ministries?
Urban Ministries, established in 1983, offers many programs to help support homeless and low-income people in the Durham community. These programs include:

* A community shelter, which can accomodate over 140 people a night
* A community cafe, which offers breakfast, lunch, and dinner daily to the homeless
* A food pantry, that provides people with groceries, and
* A clothing closet, that supplies individuals and families with clothing each month.

All together this organization helps about 6,000 people per year with over 4,000 volunteers, and they rely heavily on donations of food and clothing from the community.

## What questions will this project address?
Some challenges an organization like this faces is knowing "how much" and "when".
Having answers to these two questions, can help them know when to collect donations, when to hold food/clothing drives, and even when they need to schedules volunteers.
Therefore, this project seeks to help Urban Ministries understand the trends in their food and clothing distribution by answering questions such as:

* How much food and clothing do they need in supply to make it through a week?
* When can they expect to serve larger families who naturally need more resources?

## What information do we have?
The dataset provided by Urban Ministries includes over 20 years of information on the food pantry and clothing closet.
Specifically, we have entries for every family or individual that has ever sought assistance from this organization.
We know the size of each family, how many pounds of food they collected in a visit, and the number of clothing items they obtained. 
Additionally, we have a date for each entry, from which we can obtain information such as the day of the week.
These variables can be very helpful in answering some of the project's main questions.

## How will this data be used to accomplish project goals?
Total pounds of food, Number of clothing items, and Proportion of large families will be predicted using a linear model by day of the week.
The predicted values as well as their confidence intervals will be plotted to understand trends.
They will also be presented as a table to give urban ministries a tool when preparing for a given day of the week.
All of this will be displayed in a shiny dashboard that allows the user to choose which item they are interested in predicting (food, clothes, or large families).