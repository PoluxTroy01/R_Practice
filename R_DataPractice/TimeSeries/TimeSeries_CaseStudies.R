# ||| Manipulating Time Series Data in R |||

# xts objects have the functionality of a simple matrix while containing an index allowing easy
# manipulation over time

# ||| Encoding your flight data |||
# flights is a dataframe that contains a column called 'data' but is a character column so you 
# need to convert it to a Date object
# Load the xts package
library (xts)

# Convert date column to a time-based class
flights$date <- as.Date(flights$date)

# Convert flights to an xts object using as.xts, as this will be the index we can eliminate it
# most because we want to avoid redundancy
flights_xts <- as.xts( flights[ , -5], order.by = flights$date)

# Check the class of flights_xts
class(flights_xts)

# Examine the first five lines of flights_xts
head(flights_xts, 5)



# ||| Exploring the data |||
# Before any analysis can be done, it is critical to explore the basic qualities of your data, 
# including periodicity, scope, and comprehensiveness.
# Identify the periodicity of flights_xts
periodicity(flights_xts)

# Identify the number of periods in flights_xts, you can use ndays(), nmonths(), nyears()
nmonths(flights_xts)

# Find data on flights arriving in BOS in June 2014
flights_xts['2014-06']




# ||| Visualize flight data |||
# Use plot.xts() to view total monthly flights into BOS over time
plot.xts(flights_xts$total_flights)

# Use plot.xts() to view monthly delayed flights into BOS over time
plot.xts(flights_xts$delay_flights)

# Use plot.zoo() to view all four columns of data in their own panels
plot.zoo(flights_xts, plot.type = "multiple", ylab = labels)

# Use plot.zoo() to view all four columns of data in one panel
plot.zoo(flights_xts, plot.type = "single", lty = lty)
legend("right", lty = lty, legend = labels)




# ||| Calculate time series trends |||
# One of the most useful qualities of xts objects is the ability to conduct simple mathematical 
# equations across time. In your flight data, one valuable metric to calculate would be the 
# percentage of flights delayed, cancelled, or diverted each month. 
# Calculate percentage of flights delayed each month: pct_delay
flights_xts$pct_delay <- (flights_xts$delay_flights / flights_xts$total_flights) * 100

# Use plot.xts() to view pct_delay over time
plot.xts(flights_xts$pct_delay)

# Calculate percentage of flights cancelled each month: pct_cancel
flights_xts$pct_cancel <- (flights_xts$cancel_flights / flights_xts$total_flights) * 100

# Calculate percentage of flights diverted each month: pct_divert
flights_xts$pct_divert <- (flights_xts$divert_flights / flights_xts$total_flights) * 100

# Use plot.zoo() to view all three trends over time
plot.zoo(x = flights_xts[ , c("pct_delay", "pct_cancel", "pct_divert")])




# ||| Saving your files |||
# Save your xts object to rds file using saveRDS
saveRDS(object = flights_xts, file = "flights_xts.rds")

# Read your flights_xts data from the rds file
flights_xts2 <- readRDS("flights_xts.rds")

# Check the class of your new flights_xts2 object
class(flights_xts2)

# Examine the first five rows of your new flights_xts2 object
head(flights_xts2, 5)

###### Now when you have to share your file with someone who does not use R
# Export your xts object to a csv file using write.zoo
write.zoo(flights_xts, file = "flights_xts.csv", sep = ",")

# Open your saved object using read.zoo
flights2 <- read.zoo("flights_xts.csv", sep = ",", FUN = as.Date, header = TRUE, index.column = 1)

# Encode your new object back into xts
flights_xts2 <- as.xts(flights2)

# Examine the first five rows of your new flights_xts2 object
head(flights_xts2, 5)





# ||| Merging using rbind() |||
# Confirm that the date column in each object is a time-based class
class(temps_1$date)
class(temps_2$date)

# Encode your two temperature data frames as xts objects
temps_1_xts <- as.xts(temps_1[, -4], order.by = temps_1$date)
temps_2_xts <- as.xts(temps_2[, -4], order.by = temps_2$date)

# View the first few lines of each new xts object to confirm they are properly formatted
head(temps_1_xts)
head(temps_2_xts)

# Use rbind to merge your new xts objects
temps_xts <- rbind(temps_1_xts, temps_2_xts)

# View data for the first 3 days of the last month of the first year in temps_xts
first(last(first(temps_xts, "1 year"), "1 month"), "3 days")




# ||| Visualizing Boston winters |||
# Identify the periodicity of temps_xts
periodicity(temps_xts)

# Generate a plot of mean Boston temperature for the duration of your data
plot.xts(temps_xts$mean)

# Generate a plot of mean Boston temperature from November 2010 through April 2011
plot.xts(temps_xts$mean["2010-11/2011-04"])

# Use plot.zoo to generate a single plot showing mean, max, and min temperatures during the same period 
plot.zoo(temps_xts["2010-11/2011-04"], plot.type = "single", lty = lty)



# ||| Subsetting and adjusting periodicity |||
# Subset your temperature data to include only 2010 through 2015: temps_xts_2
temps_xts_2 <- temps_xts["2010/2015"]

# Use to.period to convert temps_xts_2 to monthly periodicity
temps_monthly <- to.period(temps_xts_2, period = "months", OHLC = FALSE, indexAt = "firstof")

# Compare the periodicity and duration of temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)





# ||| Generating a monthly average |||
# While the to.period() command is useful in many contexts, for your purposes it may not be useful 
# to select a single row as representative of the entire month.
# Instead, it makes more sense to generate average temperature values per month. To do so, you'll 
# need to manually calculate the monthly average using split() and lapply(), then generate a new 
# xts object using as.xts() This may seem like a complicated process, but you already have the 
# skills to do it!
# The subsetted xts object from the previous exercise, temps_xts_2, is preloaded in your workspace. 
# Also preloaded is an index object containing a vector of dates for the first day of each month 
# covered in the data.
# Split temps_xts_2 into separate lists per month
monthly_split <- split(temps_xts_2$mean , f = "months")

# Use lapply to generate the monthly mean of mean temperatures
mean_of_means <- lapply(monthly_split, FUN = mean)

# Use as.xts to generate an xts object of average monthly temperature data
temps_monthly <- as.xts(as.numeric(mean_of_means), order.by = index)
 
# Compare the periodicity and duration of your new temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)




# ||| Using merge() and plotting over time |||
# Use merge to combine your flights and temperature objects
flights_temps <- merge(flights_xts, temps_monthly)

# Examine the first few rows of your combined xts object
head(flights_temps)

# Use plot.zoo to plot these two columns in a single panel
plot.zoo(flights_temps[,c("pct_delay", "temps_monthly")], plot.type = "single", lty = lty)
legend("topright", lty = lty, legend = labels, bg = "white")





# Your temperature data revealed a few potential avenues for exploring the causes of flight delays 
# and cancellations. However, your client is insisting that flight arrival patterns in Boston are 
# influenced by visibility and wind, not temperature. Before moving forward, you'll need to collect 
# more data.

# After conducting extensive research, you've identified some relevant data on weekly average 
# visibility and wind speed in the Boston area. Which of the following steps would you take before 
# merging these data with your existing monthly xts object, flights_temps?

1) Encode the data to an xts object with a time-based index.
4) Convert the data to monthly periodicity using split() and lapply() to generate monthly averages.
5) Check the periodicity and duration of your xts objects before using merge().

Exactly! The three key steps here involve encoding your new data to xts objects, generating 
monthly averages from those objects, and checking the periodicity and duration before you attempt 
a merge.


# ||| Expanding your data |||
# Confirm the periodicity and duration of the vis and wind data
periodicity(vis)
pperiodicity(wind)

# Merge vis and wind with your existing flights_temps data
flights_weather <-  merge(flights_temps, vis, wind)

# View the first few rows of your flights_weather data
head(flights_weather)




				# ||| Economic Data |||

# Explore the economic data
# Get a summary of your GDP data
summary(gdp)

# Convert GDP date column to time object, this is because the date column is in quartiles
gdp$date <- as.yearqtr(gdp$date)

# Convert GDP data to xts
gdp_xts <- as.xts(gdp[, -1], order.by = gdp$date)

# Plot GDP data over time
plot.xts(gdp_xts)



# ||| Replace missing data - I |||
# The simplest technique is the na.locf() command, which carries forward the last observation before
# the missing data (hence, "last observation carried forward", or locf). This approach is often the 
# most appropriate way to handle missingness, especially when you have reasons to be conservative 
# about growth in your data.

# A similar approach works in the opposite direction by taking the first observation after the missing
# value and carrying it backward ("next observation carried backward", or nocb). This technique can 
# also be done using the na.locf() command by setting the fromLast argument to TRUE.

# Fill NAs in gdp_xts with the last observation carried forward
gdp_locf <- na.locf(gdp_xts)

# Fill NAs in gdp_xts with the next observation carried backward 
gdp_nocb <- na.locf(gdp_xts, fromLast = TRUE)

# Produce a plot for each of your new xts objects
par(mfrow = c(2,1))
plot.xts(gdp_locf, major.format = "%Y")
plot.xts(gdp_nocb, major.format = "%Y")

# Query for GDP in 1993 in both gdp_locf and gdp_nocb
gdp_locf['1993']
gdp_nocb['1993']






# ||| Replace missing data - II |||
# But what if you have reason to expect linear growth in your data? In this case, it may be more 
# useful to use linear interpolation, which generates new values between the data on either end of 
# the missing value weighted according to time.

# Fill NAs in gdp_xts using linear approximation
gdp_approx <- na.approx(gdp_xts)		# It uses interpolation to estimate linear values in time.

# Plot your new xts object
plot.xts(gdp_approx, major.format = "%Y")
  
# Query for GDP in 1993 in gdp_approx
gdp_approx['1993']



# Exploring unemployment data |||
# View a summary of your unemployment data
summary(unemployment)

# Use na.approx to remove missing values in unemployment data
unemployment <- na.approx(unemployment)

# Plot new unemployment data
plot.zoo(unemployment, plot.type = "single", lty = lty)
legend("topright", lty = lty, legend = labels, bg = "white")






# ||| Lagging unemployment |||
# This is monthly data
# Create a one month lag of US unemployment
us_monthlag <- lag(unemployment$us, k = 1)

# Create a one year lag of US unemployment
us_yearlag <- lag(unemployment$us, k = 12)

# Merge your original data with your new lags 
unemployment_lags <- merge(unemployment, us_monthlag, us_yearlag)

# View the first 15 rows of unemployment_lags
head(unemployment_lags, 15)







# ||| Differencing unemployment |||
# Differencing provides a very intuitive way to visualize growth trends over time. 
# Both types of indicators are easy to produce using commands in xts and zoo. 
# Generate monthly difference in unemployment
unemployment$us_monthlydiff <- diff(unemployment$us, lag = 1, differences = 1)

# Generate yearly difference in unemployment
unemployment$us_yearlydiff <- diff(unemployment$us, lag = 12, differences = 1)

# Plot US unemployment and annual difference
par(mfrow = c(2,1))
plot.xts(unemployment$us)
plot.xts(unemployment$us_yearlydiff, type = "h")






			# ||| Rolling functions |||
			# Apply a function over a particular window of your data
			# This windows can be discrete, for example "include all observations since the 
			# beginning of the year" or rolling, for example include the last 10 observations
			# in the data 

			# In the context of economic data you may be interested in the number of new jobs 
			# added since the beginning of the decade or the maximum level of unemployment 
			# generated since the beginning of the year

			# You can follow this:
			# Split the data according to period
				unemployment_yrs <- split(unemployment, f = "years")
			# Apply function within period
				unemployment_yrs <- lapply(unemployment_yrs, cummax)
			# Bind new data into xts object
				unemployment_ytd <- do.call(rbind, unemployment_yrs) 

			# You can use this function to 
				unemployment_avg <- rollapply(unemployment, width = 12, FUN = mean)

# ||| Add a discrete rolling sum to GDP data |||
# While it helps to know the amount of change from one period to the next, you may want to know 
# the total change since the beginning of the year. To generate this type of indicator, you can 
# use the split-lapply-rbind pattern.

# In this exercise, you'll return to the gdp data used earlier in the chapter. In addition to 
# static GDP values in each quarter, you'd like to generate a measure of GDP change from one quarter 
# to the next (using diff()) as well as a rolling sum of year-to-date GDP change (using split(), 
# lapply() and rbind().

# Add a quarterly difference in gdp
gdp$quarterly_diff <- diff(gdp, lag = 1, differences = 1)

# Split gdp$quarterly_diff into years
gdpchange_years <- split(gdp$quarterly_diff, f = "years")

# Use lapply to calculate the cumsum each year
gdpchange_ytd <- lapply(gdpchange_years, FUN = cumsum)

# Use do.call to rbind the results
gdpchange_xts <- do.call(rbind, gdpchange_ytd)

# Plot cumulative year-to-date change in GDP
plot.xts(gdpchange_xts, type = "h")




# ||| Add a continuous rolling average to unemployment data |||
# let's return to your monthly unemployment data. While you may be interested in static levels 
# of unemployment in any given month, a broader picture of the economic environment might call for 
# rolling indicators over several months. 

# Use rollapply to calculate the rolling yearly average US unemployment
unemployment$year_avg <- rollapply(unemployment$us, width = 12, FUN = mean)

# Plot all columns of US unemployment data
plot.zoo(unemployment[, c("us", "year_avg")], plot.type = "single", lty = lty, lwd = lwd)

# ||| Your rolling average helps smooth out some of the short term changes in unemployment from month 
# to month and provides a broader picture of the health of the US economy. |||




# ||| Manipulating MA unemployment data |||
# Now that you've added some lags, differences, and rolling values to your GDP and US unemployment 
# data, it's time to take these skills back to your assignment.

# Remember that your client wants information relevant to the Boston tourism industry. In addition 
# to data on the US economy in general, it may help to prepare some relevant indicators for your 
# Massachusetts economic data.

# n this exercise, you'll use your time series data manipulation skills to generate: 
# a one-year lag, a six-month first order difference, a six-month rolling average, 
# and a one-year rolling maximum in the MA unemployment rate. Your client is waiting!

# Add a one-year lag of MA unemployment
unemployment$ma_yearlag <- lag(unemployment$ma, k = 12)

# Add a six-month difference of MA unemployment
unemployment$ma_sixmonthdiff <- diff(unemployment$ma, lag = 6, differences = 1)

# Add a six-month rolling average of MA unemployment
unemployment$ma_sixmonthavg <- rollapply(unemployment$ma, width = 6, FUN = mean)
  
# Add a yearly rolling maximum of MA unemployment
unemployment$ma_yearmax <- rollapply(unemployment$ma, width = 12,FUN = max)

# View the last year of unemployment data
tail(unemployment$ma_yearmax) 



# ||| Sports Data |||
# Encoding and plotting Red Sox data
# View summary information about your redsox data
summary(redsox)

# Convert the date column to a time-based format
redsox$date<- as.Date(redsox$date)

# Convert your red sox data to xts
redsox_xts <- as.xts(redsox[,-1], order.by = redsox$date)

# Plot the Red Sox score and the opponent score over time
plot.zoo(redsox_xts[, c("boston_score", "opponent_score")])




# ||| Calculate a closing average |||
# Now that you've explored some trends in your Red Sox data, you want to produce some useful 
# indicators. In this exercise, you'll calculate the team's win/loss average at the end of each 
# season. In financial terms, you can think of this as the team's value at the close of the season.
# First, you'll identify wins based on the score of each game. You can do this using a simple 
# ifelse() command and the knowledge that the Red Sox win each game in which they score more points 
# than the opposing team.

# Second, you'll identify the date of the last game in each season using endpoints(). This command 
# identifies the last date in your object within certain periods.

# Finally, to calculate the closing win/loss average each season, simply use period.apply() on the 
# win_loss column of your data, specifying the close dates as the index, and mean as the function.

# Generate a new variable coding for red sox wins
redsox_xts$win_loss <- ifelse(redsox_xts$boston_score > redsox_xts$opponent_score, 1, 0)

# Identify the date of the last game each season
close <- endpoints(redsox_xts, on = "years")

# Calculate average win/loss record at the end of each season
period.apply(redsox_xts[, "win_loss"], close, mean)






# ||| Calculate and plot a seasonal average |||
# In the previous exercise you used endpoints() and period.apply() to quickly calculate the win/loss 
# average for the Boston Red Sox at the end of each season. But what if you need to know the 
# cumulative average throughout each season? Statisticians and sports fans alike often rely on this 
# average to compare a team with its rivals.

# To calculate a cumulative average in each season, you'll need to return to the split-lapply-rbind 
# formula practiced in Chapter Three. First, you'll split the data by season, then you'll apply a 
# cumulative mean function to the win_loss column in each season, then you'll bind the values back 
# into an xts object.

# A custom cummean() function, which generates a cumulative sum and divides by the number of values 
# included in the sum, has been generated for you. The redsox_xts data, including the win_loss column, 
# is available in your workspace.

This are the names of the data frame: "boston_score"   "opponent_score" "homegame"       "mlb"           
[5] "nfl"            "nhl"            "nba"            "season"        
[9] "win_loss"

This is the cummean function:
	> cummean()
function(x) {
  cumsum(x) / seq_along(x)
}

# Split redsox_xts win_loss data into years 
redsox_seasons <- split(redsox_xts$win_loss, f = "years")

# Use lapply to calculate the cumulative mean for each season
redsox_ytd <- lapply(redsox_seasons, cummean)

# Use do.call to rbind the results
redsox_winloss <- do.call(rbind, redsox_ytd)

# Plot the win_loss average for the 2013 season
plot.xts(redsox_winloss["2013"], ylim = c(0, 1))



# ||| Calculate and plot a rolling average |||
# The final baseball indicator you'd like to generate is the L10, or the moving win/loss average 
# from the previous ten games. While the cumulative win/loss average tells you how the team is doing 
# overall, the L10 indicator provides a more specific picture of the team's recent performance. 
# Beyond the world of sports, this measure is comparable to a financial indicator focused on recent 
# portfolio performance.

# Select only the 2013 season
redsox_2013 <- redsox_xts["2013"]

# Use rollapply to generate the last ten average
lastten_2013 <- rollapply(redsox_2013$win_loss, width = 10, FUN = mean)

# Plot the last ten average during the 2013 season
plot.xts(lastten_2013, ylim = c(0, 1))





#  Como nota: Great work! You've now used your time series data manipulation skills to generate 
# a closing average, a cumulative average, and a rolling average from basic sports scores. These 
# types of indicators have diverse applications, whether you're a baseball fan or a portfolio analyst. 






# ||| Extract weekend games ||| 
# Extract the day of the week of each observation
weekday <- .indexwday(sports)
head(weekday)

# Generate an index of weekend dates
weekend <- which(.indexwday(sports) == 6 | .indexwday(sports) == 0)

# Subset only weekend games
weekend_games <- sports[weekend]
head(weekend_games)





# ||| Calculate a rolling average across all sports |||
# Generate a subset of sports data with only homegames
homegames <- sports[sports$homegame == 1]

# Calculate the win/loss average of the last 20 home games
homegames$win_loss_20 <- rollapply(homegames$win_loss, width = 20, FUN = mean)

# Calculate the win/loss average of the last 100 home games
homegames$win_loss_100 <- rollapply(homegames$win_loss, width = 100, FUN = mean)

# Use plot.xts to generate
plot.zoo(homegames[, c("win_loss_20", "win_loss_100")], plot.type = "single", lty = lty, lwd = lwd)

