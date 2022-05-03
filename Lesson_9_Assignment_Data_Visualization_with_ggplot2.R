# Lesson 9 Assignment - Data Visualization with ggplot2

# Your assignment is to write the commands instructed in the comments below. To run your
# commands, simply hit Ctrl+Enter (command+return on a MAC) when the cursor is on that 
# command line. You can also type commands directly into the Console below, but you must
# save them in this file for your assignment.

# We will again be using the data from the furniture seller, as we have in previous
# weeks. The dataset fexp contains data on the company's advertising and sales from 
# a three-week period, from November 4 to November 24, 2019. The data should be loaded 
# in the Environment to the right. If it is not, run the seven lines of code below to 
# read in the data and format it as a data frame.

# Do not change these seven lines or GradeScope will not work
library(readxl)
library(ggplot2)
library(dplyr)
fexp <- read_excel("FieldExperiment.xlsx")
fexp <- data.frame(fexp)
fexp$DATE <- as.Date(fexp$DATE)
fexp$WEEK <- factor(fexp$WEEK)


#1. We will be revisiting the field experiment data. For the first several plots, we 
# will be looking at data from the most populous DMA, "New York, NY". First, create a 
# data frame called fexpny that contains only the data from the New York DMA.
fexpny <- fexp %>% 
  filter(DMA_NAME == "New York, NY")

#2. Online advertising is often pay-per-click, meaning advertisers pay only when 
# someone clicks on their ad. We should observe a positive correlation between clicks 
# and spending in a channel. Create a scatter plot comparing the number of clicks from 
# Facebook prospecting ads (FACEBOOK_PROSPECTING_CLICKS) on the x axis and the amount 
# spent (FACEBOOK_PROSPECTING_SPEND) in that channel. Save the plot as FBProsp.
FBProsp <- ggplot(fexpny, aes(x = FACEBOOK_PROSPECTING_CLICKS, y = FACEBOOK_PROSPECTING_SPEND)) +
  geom_point()

#3. There is a readily apparent positive correlation (higher clicks are associated with 
# higher spending). We would also like to observe a positive correlation between our 
# advertising spending and our sales. Re-do the plot above, but this time map sales 
# (SHOPIFY_US_SALES) onto size. (Because this is a variable mapping, it should go into 
# the aesthetics layer, not the geometries layer.) Call the plot FBProsp2.
FBProsp2 <- ggplot(fexpny, aes(x = FACEBOOK_PROSPECTING_CLICKS, 
                               y = FACEBOOK_PROSPECTING_SPEND,
                               size = SHOPIFY_US_SALES)) +
  geom_point()


# If sales were strongly correlated with ad spending, we would observe larger circles 
# near the top of the plot and smaller circles near the bottom of the plot. We don't 
# observe this pattern, which means that ad spending is not strongly correlated with 
# sales. (Even though advertising does cause sales, the relationship is weak. In 
# addition, sales may occur after the advertisement that caused the sale.)

#4. We will now compare New York with its west coast counterpart, "Los Angeles, CA". 
# Create a new data frame, *fexp_nyla* with data from New York and Los Angeles. Plot
# Facebook retargeting clicks (FACEBOOK_RETARGETING_CLICKS) on the x axis by Facebook 
# retargeting spend (FACEBOOK_RETARGETING_SPEND) on the y axis; map size onto sales 
# and give different colors to the two DMAs. Some of the points overlap, so pass in 
# the appropriate parameter to make the points half-way opaque. (Since the opacity 
# has to do with the appearance of the plot, it will be specified in the geometries 
# layer.) Save the plot as FBRet.
# (https://campus.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2/geometries?ex=4)
fexp_nyla <- fexp %>% 
  filter(DMA_NAME %in% c("New York, NY", "Los Angeles, CA"))

FBRet <- ggplot(data = fexp_nyla, aes(x = FACEBOOK_RETARGETING_CLICKS,
                             y = FACEBOOK_RETARGETING_SPEND,
                             size = SHOPIFY_US_SALES,
                             color = DMA_NAME)) +
  geom_point(alpha = .5)

# The correlation between clicks and sales seems to be stronger in New York than in 
# Los Angeles.

#5. Let's do that plot again, but let's use shape instead of color to distinguish 
# data from the two DMAs. Save this new plot as FBRet2.
FBRet2 <- ggplot(data = fexp_nyla, aes(x = FACEBOOK_RETARGETING_CLICKS,
                                      y = FACEBOOK_RETARGETING_SPEND,
                                      size = SHOPIFY_US_SALES,
                                      shape = DMA_NAME)) +
  geom_point(alpha = .5)



# Note which parameter, color or shape, makes the two DMAs more distinguishable. 
# (According to the lesson, color is the more distinguishable trait.)

#6. Let's look at the most populous 51 DMAs, which all have a population (POPN)
# above 600,000). Create new data frame *fexp_top51* with the data from these 
# 51 DMAs.
fexp_top51 <- fexp %>% 
  filter(POPN > 600000)

#7. Using the fexp_top51 data frame, create a scatter plot with Google prospecting 
# impressions on the x axis and Google prospecting clicks on the y axis. Use a 50% 
# opacity parameter. Save the plot as GPC51.
GPC51 <- ggplot(data = fexp_top51, aes(x = GOOGLE_PROSPECTING_IMPRESSIONS, 
                                       y = GOOGLE_PROSPECTING_CLICKS)) +
  geom_point(alpha = .5)


#8. That plot is not especially informative, because there are so many datapoints 
# from so many different DMAs. Let's do that same plot, but this time the plot should 
# have only 51 points, one for each DMA, which should represent the total number of 
# impressions (TotImp) and the total number of clicks (TotClick) from that DMA. Save 
# the plot as GPC51b.
GPC51b <- fexp_top51 %>% 
  group_by(DMA_NAME) %>% 
  summarize(TotImp = sum(GOOGLE_PROSPECTING_IMPRESSIONS),
            TotClick = sum(GOOGLE_PROSPECTING_CLICKS)) %>% 
  ggplot(aes(x = TotImp, y = TotClick)) +
  geom_point(alpha = .5)


# Notice that when we remove a source of variation (clicks and impressions vary from 
# day to day, and we summed across day), the correlation becomes stronger.

#9. In general, regions with a higher population have higher advertising and higher 
# sales, for obvious reasons. Now let's see which regions are best for this company, 
# meaning they have a high level of sales relative to their population. Calculate the 
# ratio of sales (totaled across the 21 days; call it TotSales) to population (POPN) 
# for each of the top 51 DMAs. Call the ratio SalesPerCap. Store this information 
# in a data frame called top51_ratio.
top51_ratio <- fexp_top51 %>% 
  group_by(DMA_NAME) %>% 
  summarize(TotSales = sum(SHOPIFY_US_SALES),
            POPN = mean(POPN)) %>% 
  mutate(SalesPerCap = TotSales / POPN)

#10. Create a histogram of the ratios in *top51_ratio*. (You can keep the default 
# setting of 'bins = 30'.) Save the histogram as SPChist.
# (https://campus.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2/geometries?ex=7)
SPChist <- ggplot(data = top51_ratio, aes(x = SalesPerCap)) +
  geom_histogram()

# There are two outlier DMAs that really seem to like this company. 

#11. Now we'll create a bar plot of each region's SalesPerCap. In Lesson 3, you 
# learned to do this using geom_col. Use geom_col() to create this plot, but make 
# these two modifications: (1) Order the DMAs in descending ratio order by using 
# this command, *x = reorder(DMA_NAME, -SalesPerCap)*. The reorder command 
# reorders DMA_NAME by the second variable, SalesPerCap. The minus sign in front 
# of Ratio instructs R to order in descending (as opposed to ascending) order. 
# (2) Add this command after the geom_col():
# theme(axis.text.x = element_text(angle = 90)). This will turn the labels 90 
# degrees so we can read them. Otherwise, the DMA labels will overlap and be 
# unreadable. Save the plot as SPCBar.
SPCBar <- ggplot(data = top51_ratio, aes(x = reorder(DMA_NAME, -SalesPerCap), y = SalesPerCap)) +
         geom_col() +
         theme(axis.text.x = element_text(angle = 90))


#12. Execute the same code as above but replace geom_col() with geom_bar(). It will 
# produce an error. What is the nature of this error?
#  * ENTER ANSWER HERE *
ggplot(data = top51_ratio, aes(x = reorder(DMA_NAME, -SalesPerCap), y = SalesPerCap)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

#13. By default, geom_bar() counts the number of entries with a particular value. 
# If we want the y axis to reflect a particular value (like the ratio we've 
# calculated) instead of a count, we have to specify stat = "identity" within 
# the geom_bar function. Do that below.
ggplot(data = top51_ratio, aes(x = reorder(DMA_NAME, -SalesPerCap), y = SalesPerCap)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))


#14. Using *fexp_nyla*, create a scatter plot of GOOGLE_RETARGETING_IMPRESSIONS (x 
# axis) by GOOGLE_RETARGETING_CLICKS (y axis) and map color onto DMA and size onto 
# sales. (Let R choose colors by default.) Save the plot as GRscatter.
GRscatter <- ggplot(data = fexp_nyla, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                          y = GOOGLE_RETARGETING_CLICKS,
                                          color = DMA_NAME,
                                          size = SHOPIFY_US_SALES)) +
  geom_point()

#15. Let's re-create this same plot but with different colors for the points. In 
# the lesson, you chose bar colors using *scale_fill_brewer()*. That was because 
# *fill* refers to the color inside a shape, while *color* refers to the border 
# color. But most points don't have a fill color, so instead of using 
# *scale_fill_brewer()*, you'll use (can you guess?) *scale_color_brewer()*. Add 
# this command to the same plot above. Save the plot as GRscatterb.
GRscatterb <- ggplot(data = fexp_nyla, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                          y = GOOGLE_RETARGETING_CLICKS,
                                          color = DMA_NAME,
                                          size = SHOPIFY_US_SALES)) +
  geom_point() +
  scale_color_brewer()

#16. The chosen color for Los Angeles was a little too light for that background 
# color. One way to fix that would be to lighten the background color. Add the 
# classic theme to the plot, which uses a white background. Save it as GRscatterc.
# (https://campus.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2/themes?ex=6)
GRscatterc <- ggplot(data = fexp_nyla, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                           y = GOOGLE_RETARGETING_CLICKS,
                                           color = DMA_NAME,
                                           size = SHOPIFY_US_SALES)) +
  geom_point() +
  scale_color_brewer() +
  theme_classic()

# Another way to fix the lightness of the blue points for Los Angeles would be to 
# choose a different color palette within *scale_color_brewer()*. Within that 
# function, we specify *palette = * to select a color palette. There are many 
# palettes available, as shown in the file ColorBrewerOptions.png.


#17. Create the same scatter plot as in Question 16, but this time use the Set2 
# option within *scale_color_brewer*. (Put "Set2" in quotation marks.) Save the 
# plot as GRscatterd.
GRscatterd <- ggplot(data = fexp_nyla, aes(x = GOOGLE_RETARGETING_IMPRESSIONS,
                                           y = GOOGLE_RETARGETING_CLICKS,
                                           color = DMA_NAME,
                                           size = SHOPIFY_US_SALES)) +
  geom_point() +
  scale_color_brewer(palette = "Set2") +
  theme_classic()

#18. We want to continue our plotting practice with some additional bar plots. 
# But we want to create a new variable that categorizes DMAs by size. In your 
# *top51_ratio* data create a new variable, POPN2, that takes on these values: 
# 3 if POPN > 2 million; 2 if POPN > 1 million (but <= 2 million); 1 if POPN 
# <= 1 million.
top51_ratio <- top51_ratio %>% 
  mutate(POPN2 = ifelse(POPN > 2000000, 3, ifelse(POPN < 1000000, 1, 2)))


#19. Re-create the bar plot from Question 13 showing each region's ratio of 
# sales to population, in descending order. This time, map fill color onto the 
# new POPN2 variable. (Hint: POPN2 is interpreted by R to be a continuous 
# numeric variable, but it's actually a discrete variable. Use 
# as.factor(POPN2) so R treats the variable as discrete.) Save it as SPCBar2.
SPCBar2 <- ggplot(data = top51_ratio, aes(x = reorder(DMA_NAME, -SalesPerCap), 
                                         y = SalesPerCap,
                                         fill = as.factor(POPN2))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))


#20. Re-create this same plot but choose new colors for the bars. Use the 
# "Dark2" palette of Brewer. Save it as SPCBar3.
#(https://campus.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2/geometries?ex=12)
SPCBar3 <- ggplot(data = top51_ratio, aes(x = reorder(DMA_NAME, -SalesPerCap), 
                                          y = SalesPerCap,
                                          fill = as.factor(POPN2))) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90))

#21. Now for a little more practice with bar plots. We will now create a plot 
# that shows the total population of the regions within each POPN2 group. That 
# is, we will create a bar plot from the *top51_ratio* with three bars: one for 
# the total population of POPN2==1 regions, another for the total population of 
# POPN2==2 regions, and another for the total population of POPN2==3 regions. 
# You don't have to do a group_by and summarize statement to calculate the 
# total population. Just do a geom_bar (with stat = "identity") and map POPN2 
# onto x and POPN onto y. Wrap POPN2 in *as.factor()* so R recognizes that it's 
# not a continuous variable. Save the plot as popnbar.
popnbar <- ggplot(data = top51_ratio, aes(x = as.factor(POPN2), 
                               y = POPN)) +
  geom_bar(stat = "identity")

#22. Re-do that same plot, but color the bars by DMA. In other words, map 
# DMA_NAME onto bar color. (This is a variable mapping, so think about where 
# that command goes. Also, remember that you don't select bar colors with a 
# *color =* mapping.) Save it as popnbar2.
# (https://campus.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2/geometries?ex=10)
popnbar2 <- ggplot(data = top51_ratio, aes(x = as.factor(POPN2), 
                                          y = POPN,
                                          fill = DMA_NAME)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")

# With 51 DMAs in this data frame, the legend will be too large to display. 
# Suppress the legend by adding this theme command: 
# theme(legend.position = "none"). Use this command on all the remaining 
# plots to suppress the legend.


#23. Let's do some more practice with the position command. Copy the code 
# from Question 22, and add position = "dodge" to the geom_bar 
# command. Save the plot as popnbar3.
popnbar3 <- ggplot(data = top51_ratio, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.position = "none")

#24. Notice that adding position = "dodge" to the geom command makes the bars 
# "dodge" each other so they are no longer stacked. The default position for 
# geom_bar is position = "stack". Now try position = "fill". Save the plot
# as popnbar4.
popnbar4 <- ggplot(data = top51_ratio, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(legend.position = "none")


#25. What does the "fill" specification do?
# * ENTER ANSWER HERE *

#26. Let's do one last position. Use position = position_dodge(width = .5). 
# Also add an opacity parameter of .5. Save the plot as popnbar5.
popnbar5 <- ggplot(data = top51_ratio, aes(x = as.factor(POPN2), 
                                           y = POPN,
                                           fill = DMA_NAME)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = .5),
           alpha = .5) +
  theme(legend.position = "none")

#27. What did the *position_dodge()* function do in this case?
# * ENTER ANSWER HERE *

# Congratulations! You have completed another assignment! You should feel pretty 
# comfortable with creating plots using ggplot2.