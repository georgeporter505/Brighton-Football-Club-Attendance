#' ---
#' title: Brighton FC attendance
#' author: You
#' date: Today
#' ---

# I have chosen to analyse the Premier League club Brighton&Hove Albion's
# attendance figures for this coursework. However upon researching the data
# it has been made clear that generally the home attendance figures at the clubs
# stadium (called the Amex Stadium) has little variation around the max capacity
# of 31,876. Reflecting on this I then considered analysing a subset of this
# data, perhaps focusing on Away attendance(number of spectators when playing at
# opposing teams stadiums). I fear that this would be heavily dependant on the
# capacity of those stadiums as well however, so i think a more interesting
# idea would be to model each away game as a percentage of that stadiums average
# in order to determine the "attractiveness" of Brighton FC as an away game,
# and then we could forecast that percentage to see trends in club popularity.
# perhaps even finally combining all the data into a yearly attractiveness score
# and comparing change over multiple years.

# Data Frame Creation --------------------------------------------------------

# Start with creating our dataframe
away_dates = c("2025-08-24","2025-08-27","2025-09-13","2025-09-23","2025-09-27",
               "2025-10-05","2025-10-25","2025-10-29","2025-11-09","2025-11-30",
               "2025-12-13","2025-12-27","2025-12-30","2026-01-07","2026-01-11",
               "2026-01-24","2026-02-11","2026-02-14","2026-02-21","2026-03-14")
away_opponents = c("Everton","Oxford United","Bournemouth","Barnsley","Chelsea",
                   "Wolves","Manchester Utd","Arsenal","Crystal Palace",
                   "Nottingham Forest","Liverpool","Arsenal","West Ham",
                   "Manchester City","Manchester Utd","Fullham","Aston Villa",
                   "Liverpool","Brentford","Sunderland")
attendance = c(51759,9010,11167,7790,39597,29504,74124,59292,24326,30522,60429,
               60209,62464,51728,73888,27239,39495,60277,17163,45497)
Brighton_Away = data.frame(date=as.Date(away_dates),Opponent=away_opponents,
                           Attendance= Attendance,stringsAsFactors=FALSE)
print(Brighton_Away)
Brighton_Away$stringAsFactors = NULL
# Now I will add the stadium names, averages and capacities into the data frame.
# create stadium lookup vectors and information columns

stadium_opponent = c("Everton","Oxford United","Bournemouth","Barnsley","Chelsea",
                     "Wolves","Manchester Utd","Arsenal","Crystal Palace",
                     "Nottingham Forest","West Ham",
                     "Manchester City","Fullham","Aston Villa",
                     "Liverpool","Brentford","Sunderland")
stadium_name = c("Hill Dickinson Stadium","Kassum Stadium","Vitality Stadium","Oakwell Stadium"
                  ,"Stamford Bridge","Molineux Stadium","Old Trafford","Emirates Stadium",
                 "Selhurst Park","The City Ground","London Stadium","Etihad Stadium",
                 "Craven Cottage","Villa Park","Anfield","Gtech Stadium","Stadium of Light")
stadium_cap = c(52769,12500,11307,23287,41631,31750,74879,60704,25486,30404,62500,
                55097,29589,42918,61276,17250,48707)
stadium_avg = c(52035,11358,11167,12211,39680,29830,73979,60216,24887,30334,62453,
                52206,27156,41802,60395,17094,46323)
stadium_lookup = data.frame(
    opponent = stadium_opponent,
    stadium = stadium_name,
    capacity = stadium_cap,
    avg_attendance = stadium_avg,
    stringAsFactors = FALSE
)
Brighton_Away = merge(
    Brighton_Away,
    stadium_lookup,
    by.x = "Opponent",
    by.y = "opponent",
    all.x = TRUE,
    sort = FALSE
)

# reorder by date as merge has aligned by opponent name
# also add a match number since row number is not chronological any more

Brighton_Away = Brighton_Away[order(Brighton_Away$date),]
Brighton_Away$match_number = 1:nrow(Brighton_Away)

# now calculate a demand or attractiveness score which compares Brighton's
# attendance to the average.

Brighton_Away$demand_index = (Brighton_Away$Attendance / Brighton_Away$avg_attendance) * 100
Brighton_Away$demand_index = round(Brighton_Away$demand_index, 0.1)
Brighton_Away$month = format(Brighton_Away$date, "%B")
Brighton_Away$day_of_week = weekdays(Brighton_Away$date)

# to use prophet we need this in a format with a DS column and y column so:
# I had issues creating this dataframe all at once so i had to construct it
# incrementally, I do not know why this worked but not the other way.

df_prophet = data.frame(ds = Brighton_Away$date, stringsAsFactors = FALSE)
df_prophet$y = Brighton_Away$demand_index
df_prophet$opponent = Brighton_Away$Opponent
df_prophet$stadium = Brighton_Away$stadium
df_prophet$attendance = Brighton_Away$Attendance
df_prophet$avg_attendance = Brighton_Away$avg_attendance
df_prophet$opponent_category = Brighton_Away$opponent_category
df_prophet$month = Brighton_Away$month
df_prophet$day_of_week = Brighton_Away$day_of_week
df_prophet$match_number = Brighton_Away$match_number

# Plotting ------------------------------------------------------------------
library(prophet)
m = prophet(df_prophet)
month_starts = seq(from = as.Date("2025-08-01"),
                   to = as.Date("2026-06-01"),
                   by = "month")
month_labels = format(month_starts, "%b %y")

plot(df_prophet$ds, df_prophet$y,
     type = "o",
     main = "Brighton Away Demand Index 2025-26",
     xlab = "Date",
     ylab = "Demand Index (Stadium Average = 100)",
     col = "blue",
     pch = 16,
     las = 2,
     xaxt = "n")
axis(1, at = month_starts, labels = month_labels, las = 2, cex.axis = 0.8)

# Note we have two big outliers in Barnsley and Oxford, which is notable as
# these are FA cup games where Brighton played lower league teams,
# probably resulting in lower attendance from home fans due to unfair odds



# okay now we will make future predictions as well as historical so we can analyse
# fit

historical = predict(m, df_prophet)
plot(df_prophet$ds, df_prophet$y,
     type = "p",
     main = "Prophet Model Fit - Historical Data",
     xlab = "Date",
     ylab = "Demand Index",
     col = "blue",
     pch = 16,
     las = 2)
lines(df_prophet$ds, historical$yhat,
      col = "red",
      lwd = 2)
polygon(c(df_prophet$ds, rev(df_prophet$ds)),
        c(historical$yhat_upper, rev(historical$yhat_lower)),
        col = rgb(1, 0, 0, 0.2),
        border = NA)
legend("bottomright",
       legend = c("Actual", "Predicted", "95% CI"),
       col = c("blue", "red", rgb(1, 0, 0, 0.2)),
       pch = c(16, NA, 15),
       lty = c(NA, 1, NA),
       bty = "n")
# this shows us what the model predicts for the data points we already have,
# as a reference for the models ability. Generally it is on point or at least
# within the confidence interval shown, however I believe it is slipping up due
# to our outliers. Later we will re-plot without them to see how the model adjusts.

# Brighton have only 4 remaining away games this season, lets predict their
# demand index.

n_remaining = 4
remaining_dates = as.Date(c("2026-04-11","2026-04-18","2026-05-02","2026-05-17"))
future = data.frame(ds = remaining_dates)
forecast = predict(m, future)
forecast$ds = as.Date(forecast$ds) #needed to standardise date format to plot

#combine these to plot

all_dates = c(df_prophet$ds, forecast$ds)
all_predictions = c(historical$yhat, forecast$yhat)
all_lower = c(historical$yhat_lower, forecast$yhat_lower)
all_upper = c(historical$yhat_upper, forecast$yhat_upper)

plot(df_prophet$ds, df_prophet$y,
     type = "o",
     main = "Brighton Away Demand Index: Historical & Forecast",
     xlab = "Date",
     ylab = "Demand Index (Stadium Average = 100)",
     col = "blue",
     pch = 16,
     xlim = range(all_dates),
     ylim = range(c(df_prophet$y, all_upper, all_lower)),
     las = 2,
     xaxt = "n")
axis(1, at = month_starts, labels = month_labels, las = 2, cex.axis = 0.8)


lines(df_prophet$ds, historical$yhat, col = "red", lty = 2, lwd = 2)
lines(forecast$ds, forecast$yhat, col = "darkgreen", lwd = 3)
points(forecast$ds, forecast$yhat, col = "darkgreen", pch = 17, cex = 1.5)

polygon(c(forecast$ds, rev(forecast$ds)),
        c(forecast$yhat_upper, rev(forecast$yhat_lower)),
        col = rgb(0, 1, 0, 0.2),
        border = NA) #this is the confidence interval on forecast data
abline(v = max(df_prophet$ds), col = "gray50", lty = 3, lwd = 2) #mark end of recorded data
legend("bottomright",
       legend = c("Actual Historical", "Prophet Fit", "Forecast", "95% CI"),
       col = c("blue", "red", "darkgreen", rgb(0, 1, 0, 0.2)),
       lty = c(1, 2, 1, NA),
       lwd = c(1, 2, 3, NA),
       pch = c(16, NA, 17, 15),
       bty = "n",
       cex = 0.8)

grid() # looks nice


# there is a large gap between the plot and the forecast, but it is because
# Brighton don't have an away game for almost a month between 14th March and
# 11th April

# Observing the forecast I do believe it is significantly impacted by the outliers
# and suggesting there is growth patterns, resulting in the future data being
# above 100 on the demand index. I expect if we remove the outliers the data
# should be more in line, and present less than or equal to 100.
# not to say that values of 100 aren't valid, merely not in line with the data.

prophet_plot_components(m, forecast)
# the trend here is suggesting growth, again possible skewed by FA cup games.
# Weekly seasonality shows Tuesday/Wednesday games having a lower demand,
# this seems logical in that fans are likely to have work and be unable to attend.


# Cleaning the Data ---------------------------------------------------------

#the matches we want to remove are number 2 and 4:

df_clean = df_prophet[!df_prophet$match_number %in% c(2,4),]
# we could reset our match numbers, but I'm choosing not to to retain clarity, as
# I'm keeping the other FA cup matches with premier league teams.
print(df_clean)
df_prophet_clean = prophet(df_clean)
forecast_clean = predict(df_prophet_clean, future)
forecast_clean$ds = as.Date(forecast_clean$ds)
historical_clean = predict(df_prophet_clean, df_clean)
historical_clean$ds = as.Date(historical_clean$ds)

plot(df_clean$ds, df_clean$y,
     type = "o",
     main = "Brighton Away Demand Index - Outliers Removed",
     xlab = "Date",
     ylab = "Demand Index (Stadium Average = 100)",
     col = "blue",
     pch = 16,
     xaxt = "n",
     las = 2,
     xaxt = "n",
     ylim = range(c(df_clean$y, forecast_clean$yhat)) + c(-5, 15),
     xlim = range(c(df_clean$ds, forecast_clean$ds)))
axis(1, at = month_starts, labels = month_labels, las = 2, cex.axis = 0.8)
grid()
abline(h = 100, col = "red", lty = 2, lwd = 2)
lines(historical_clean$ds, historical_clean$yhat, col = "orange", lty = 2, lwd = 2)
lines(forecast_clean$ds, forecast_clean$yhat, col = "darkgreen", lwd = 3)
points(forecast_clean$ds, forecast_clean$yhat, col = "darkgreen", pch = 17, cex = 1.5)
polygon(c(forecast_clean$ds, rev(forecast_clean$ds)),
        c(forecast_clean$yhat_upper, rev(forecast_clean$yhat_lower)),
        col = rgb(0, 1, 0, 0.2),
        border = NA)
text(df_clean$ds, df_clean$y + 3,
     labels = df_clean$opponent,
     cex = 0.5,
     col = "black",
     srt = 90)
legend("topleft",
       legend = c("Actual (cleaned)", "Prophet Fit", "Forecast", "95% CI", "Average Stadium Cap"),
       col = c("blue", "orange", "darkgreen", rgb(0, 1, 0, 0.2), "red"),
       lty = c(1, 2, 1, NA, 2),
       lwd = c(1, 2, 3, NA, 2),
       pch = c(16, NA, 17, 15, NA),
       bty = "n",
       cex = 0.7)

# This matches our expectations much better. By removing the outliers, the
# predicted path is below 100 and actually showing a decreasing trend. This
# fits when you consider it is towards the end of the season, with only 8 games
# left to play the position of each team is becoming more set in stone and so
# generally attendance will fall. Our furthest data point from the average here
# is now Aston villa, which contextually fits. Aston Villa historically always
# beat Brighton, especially at an away game, and considering they had given
# Brighton their worst defeat this season at home, fans likely were not excited
# to see the rematch.

prophet_plot_components(df_prophet_clean, forecast_clean)

#here we see the same variation between weekends and weekdays. However the trend
# has flipped completely, and now is declining. Without the extremely negative
# values of our outliers, we see that in truth the demand for Brighton away games
# is falling, likely due to end of season and the fact that Brighton are finishing
# in the middle of the league. Demand might pick up if the club was at either end
# as there would be excitement to watch them fight for the top positions or
# avoid being relegated at the bottom of the league.










