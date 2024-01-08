---
title: "Data_Visualization"
author: "Ezhilan Wilson (22121128) & Athul S (22121011)"
output: word_document
---

## Analyze the dataset provided with the case. Draw insights and provide recommendations and an action plan.

Data is collected through surveys and the frequencies were plotted in order to draw the inferences from data by analysing inputs to make recommendation to make the attrition rate reduced during the first year of employment.

```{r include=FALSE}

# Loading the required Liabrary
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(googlesheets4)
library(treemapify)

gs4_auth()

data = read_sheet("https://docs.google.com/spreadsheets/d/1WmzF7XBjyKre1vdOsqScpWg0517ZadJQRKTU9yE7JuA/edit#gid=1226588633", sheet = "Student Spreadsheet")

```

**I. EARLY ATTRITION IN VARIOUS DEPARTMENTS**

```{r}

x = table(data$`Department of Employee`)
x = as.data.frame(x)
colnames(x) = c("Department", "No.of Employees")
x

ggplot(x, aes(x = `No.of Employees`, y = Department)) +
  geom_bar(stat = "identity") +
    labs(title = "EARLY ATTRITION IN VARIOUS DEPARTMENTS", x = "No. of Employees", y = "Department") +
  theme(legend.position = "bottom")
```

There were various departments of the power plant and we can see that the production department has the highest number of new hire resignations. Projects and Engineer with the rate of 8%, Sales and promotion with 7% and administration with 7%.

**II. PERCENTAGE OF RESIGNATIONS FROM VARIOUS SOURCES OF HIRE**

```{r}

a = table(data$`Source of Hire`)
a = as.data.frame(a)
colnames(a) = c("Source of Hire", "Percentage of Resignation")
a$`Percentage of Resignation` = percent(a$`Percentage of Resignation`, accuracy = NULL, scale = 0.1, trim = TRUE)
a

ggplot(a, aes(x = "", y = `Percentage of Resignation`, fill = `Source of Hire`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Resignations by Source of Hire",
       fill = "Source of Hire") +
  theme_void() +
  theme(legend.position = "right")

```

The source with the highest new hire turnover was campus recruitment (74%), other job portals (16%),, agency hiring (4%), referrals (4%) and LinkedIn (2%).

**III. ALMA MATER OF RESIGNING EMPLOYEES**

```{r}

alma = table(data$`Alma Mater`)
alma = as.data.frame(alma)
colnames(alma) = c("Alma Mater", "No of Resignation")
alma

ggplot(alma, aes(area = `Percentage of Resignation`, fill = `Alma Mater`, label = `Alma Mater`)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Alma Mater of Resignation ", fill = "Alma Mater") +
  theme_minimal()

```

41% of the IIT graduates resigned in their first year of employment, 25% were DTU graduates, 13% were NIT graduates, 11% were NSIT graduates, 6% were IIIT graduates and 4% from IIEST graduates. There is no first-year attrition rate observed in graduates from other universities.

**IV. iTRAINING EXPENSES INCURRED FOR EMPLOYEES WHO RESIGNED IN THE FIRST YEAR OF EMPLOYMENT**

```{r}

ggplot(data, aes(x = `Training Expenses (US$)`)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "#69b3a2") +
  scale_x_continuous(breaks = seq(0, 600, by = 50)) +
  labs(title = "Training Expenses Incurred", x = "Training Expenses (US$)", y = "No of Employees")
```

Attrition was higher when the training expenses were the least. Interestingly, attrition was even higher among candidates where \$200--\$300 was invested in their training, as this investment raised the candidate's demand among competitors. However, when the training expenses were beyond \$300, the attrition rate drastically lowered, depicting the development of employee loyalty.

**V. ABSENTEEISM AMONG EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}

absentism = table(cut(data$`Absenteeism Rate (%)`, seq(0, 60, 10)))
absentism = as.data.frame(absentism)
colnames(absentism) = c("Absentism", "Frequency")
absentism

ggplot(absentism, aes(x = Absentism, y = Frequency)) +
  geom_bar(stat = "identity", color = "black", fill = "blue") +
  labs(title = "Absenteeism among employee", x = "Absenteeism", y = "Frequency") +
  theme_classic()

```

According to the analysis we can conclude that the absenteeism rate is more than 40-60%.

**VI. TRAINING EFFICIENCY OF EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}

mean_rating <- mean(data$`Training Efficiency Rating (%)`)
sd_rating <- sd(data$`Training Efficiency Rating (%)`)
x <- seq(mean_rating - 2 * sd_rating, mean_rating + 2 * sd_rating, length = 100)
y <- dnorm(x, mean_rating, sd_rating)
bellcurve_data <- data.frame(x = x, y = y)

ggplot(bellcurve_data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Training Efficiency rating of employee", x = "Training Efficiency Rating (%)", y = "Density") +
  theme_classic()

```

Employee training efficiency, observed by the candidate's direct manager, is based on the evaluation of employee performance after each training session. The graph demonstrates that training efficiency does not seem to have any direct relationship with turnover levels among employees with less than one year of tenure .

**VII. EMPLOYEES' LENGTH OF SERVICE BEFORE RESIGNING WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
los = table(data$`Average Length of Service (Months)`)
los = as.data.frame(los)
colnames(los) = c("Month","Frequency")
los

ggplot(los, aes(x = Month, y = Frequency)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Length of Service", x = "Months", y = "No. of Employees")

```

It means the employees duration with the organization before resigning.40.8% people did after nine months of services.27% for six to nine months. Fewer resigned in the initial three months of employment.

**VIII. TIME UNTIL THE NEXT PROMOTION FOR EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}

ggplot(data, aes(x = `Time until Promotion (Months)`)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#69b3a2") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  labs(title = "Time until Promotion (Months)", x = "Training Expenses (US$)", y = "Frequency")

```

Time left for the employees to reach their next promotion. Among the employees who resigned in the first year of employment, those whose next promotion was 9-12 months away had the highest attrition rate of 48%.

**IX. REASONS CITED FOR RESIGNATION BY EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
reasonforres = table(data$`Reason for Resigning`)
reasonforres = as.data.frame(reasonforres)
colnames(reasonforres) = c("Reasons", "No. of Employees")
reasonforres

ggplot(reasonforres, aes(x = `No. of Employees`, y = Reasons)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Reasons for Resigning", x = "No. of Employees", y = "Reasons")

```

21% of resigning employees stated that they resigned due to the unfavourable job location, 19% stated that they left because they found that the work unchallenging, 16% stated that they left for a better opportunity in the industry and 14% reported that they need to go for further studies.

**X. DISTANCE BETWEEN HOME AND WORKPLACE OF EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
distance = table(data$`Distance between Home and Workplace (km)`)
distance = as.data.frame(distance)
colnames(distance) = c("Distance", "No. of Employee")
distance

ggplot(distance, aes(x = Distance, y = `No. of Employee`)) +
  geom_point() +
  labs(title = "Distance between Home and Workplace", x = "Distance (km)", y = "No. of Employees")

```

It was observed that those who resides farther from the plant intended to resign the most as compared with the people nearby.

**XI. PERFORMANCE RATING OF EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
performance = table(data$`Performance Rating (%)`)
performance = as.data.frame(performance)
colnames(performance) = c("Performance Rating", "No of Employee")
performance

ggplot(performance, aes(x = `Performance Rating`, y = `No of Employee`)) +
  geom_bar(stat = "identity", color = "black", fill = "cyan") +
  labs(title = "Performance Rating", x = "Performance Rating (%)", y = "No. of Employees")

```

98% had satisfactory ratings and 40% had excellent performance ratings indicated that high performers are resigning from their jobs within one year.

**XII. TRAINING COMPLETION AMONG EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
trainingcomp = table(cut(data$`Training Completion (%)`,seq(0,100,20)))
trainingcomp = as.data.frame(trainingcomp)
colnames(trainingcomp) = c("Training Completion", "No of Employee")
trainingcomp

ggplot(trainingcomp, aes(x = `Training Completion`, y = `No of Employee`, fill = `Training Completion`)) +
  geom_bar(stat = "identity") +
  labs(title = "Training Completion Percentage", x = "Training Completion (%)", y = "No. of Employees") +
  theme(legend.position = "bottom")

```

It shows that employees who completed the highest percentages of training also made up the highest percentage of empoyees who resigned within the first year of employment .

**XIII. POWER PLANT LOCATION OF EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
powerplantlocation = table(data$`Power Plant Location`)
powerplantlocation = as.data.frame(powerplantlocation)
colnames(powerplantlocation) = c("Powerplant Location", "No. of Employee")
powerplantlocation

# Create a pie chart using ggplot2
ggplot(powerplantlocation, aes(x="", y=`No. of Employee`, fill=`Powerplant Location`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(fill="Power Plant Location", title="Power Plant Locations")

```

Employees resign from remote locations than from plants that are located close to urban centres. Plant in Kawai, Rajasthan, is the most remote, located 300 km from an urban area (see case Exhibit 1), and has the highest new-hire turnover rate, while the plant in Mundra, Gujarat, located just 45 km from an urban locality, has the lowest rate.

**XIV. AGE OF EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
ggplot(data, aes(x = `Age`)) + 
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(breaks = seq(0, 25, by = 1)) +
  labs(title = "Age Histogram", x = "Age of Employee", y = "No. of Employee")

```

It can be observed that the younger employees tend to resign slightly more than older employees

**XV. RATING OF NON-ENGAGEMENT WITH EXTRAMURAL ACTIVITIES AMONG EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
nonengagement = table(cut(data$`Non-engagement Rating (%)`, seq(0,100,10)))
nonengagement = as.data.frame(nonengagement)
colnames(nonengagement) = c("Non-engement rating", "No. of Employees")
nonengagement

ggplot(data = nonengagement, aes(x = `Non-engement rating`, y = `No. of Employees`)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Non-engagement Ratings", x = "Non-engagement rating", y = "No. of Employees")+
  theme_classic()

```

Of the employees who resigned within the first year of employment, 46 per cent rarely engaged in extramural activities (with a non-engagement rating of 80 to 100 per cent), whereas only 3 per cent of employees who resigned were highly engaged in extramural activities (with a non-engagement rating of between 0 and 20 per cent).

**XVI. PREVIOUS PLACE OF RESIDENCE OF EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
location = table(data$`Previous Place of Residence`)
location = as.data.frame(location)
colnames(location) = c("Location", "No of Employees")
location

# Create bar chart
ggplot(location, aes(x = `No of Employees`, y = Location, fill = Location)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Number of Employees", y = "Previous Place of Residence", 
       title = "Number of Employees by Previous Place of Residence")

```

Candidates whose previous place of residence was Delhi accounted for 22 per cent of first-year attrition. Similar trends are observed for employees whose previous place of residence was Punjab, Kerala, or Maharashtra. Employees whose previous place of residence was Bihar, Jharkhand, or Chhattisgarh are found to resign the least, most likely because they are acquainted with manufacturing environments.

**XVII. DEPARTMENT OF RESIGNING EMPLOYEES**

```{r}
department = table(data$`Department of Employee`)
department = as.data.frame(department)
colnames(department) = c("Department", "No. of Employees")
department

ggplot(department, aes(x=`No. of Employees`, y=Department)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title = "Number of Employees by Department",
       x = "Number of Employees", y = "Department") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

**XVIII. REASONS CITED FOR RESIGNATION BY PRODUCTION DEPARTMENT EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}

reason = table(data$`Reason for Resigning`)
reason = as.data.frame(reason)
colnames(reason) = c("Reason for resiging", "No. of Employee")
reason

# Create a bar chart using ggplot2
ggplot(reason, aes(x = `Reason for resiging`, y = `No. of Employee`, fill = `Reason for resiging`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Reason for Resigning", y ="No. of Employees" , title = "Reasons for Resigning")

```

**XIX. ALMA MATER OF PRODUCTION DEPARTMENT EMPLOYEES WHO RESIGNED WITHIN THE FIRST YEAR OF EMPLOYMENT**

```{r}
almapr = table(data$`Alma Mater`,data$`Department of Employee`)
almapr = as.data.frame(almapr)
colnames(almapr) = c("Alma Mater", "Department of Employee", "Count")
almapr_prod <- subset(almapr, `Department of Employee` == "Production")
almapr_prod

ggplot(almapr_prod, aes(x = `Alma Mater`, y = `Count`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Alma Mater of Production Department Employees",
       x = "Alma Mater",
       y = "Count")

```
