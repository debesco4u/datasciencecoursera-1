Live Earthquakes Shiny App
========================================================
author: Marco Pasin
date: 10 September 2017
autosize: true

App link:
<br>
<https://mcpasincoursera.shinyapps.io/live_earthquakes/>
<br>
Code:
<br>
<https://github.com/mcpasin/datasciencecoursera/blob/master/Developing%20Data%20Products/week4/live_quakes_app/app.R>

About the app
========================================================

The app shows the latest earthquakes as reported by [USGS] (https://earthquake.usgs.gov/).  Data is "live" in the sense that a live connection to USGS is built within the app to make sure you get the most updated information. 

- Places where earthquakes took place are plotted in a map and coloured according to their magnitude.
- Tooltips provide additional information about the quake. Click circles to make it visible.
- The 3 most recent earthquakes are shown in a separate table, plus additional stats are reported on the right sidebar.
- I am not an expert in earthquakes. But I think it's an interesting use case for building a real-time data app :)

Operations performed
========================================================

1. Retrieve the latest version of data available from USGS website. Data comes in a .csv file and reports quakes for the past 7 days (check the exact URL in the R code).
2. Subset in case the user chooses to see only data from yesterday.
3. Plot data on a map using *leaflet* library.
4. Calculate a few stats like max and number of occurrences.
5. Force a manual refresh of data if the user press the button "Update Data". 

How I categorized magnitude
========================================================




```r
# Categorize magnitude in terms of size

## Great	8 or more
## Major	7 to 7.9
## Strong	6 to 6.9
## Moderate	5 to 5.9
## Light	4 to 4.9
## Minor	up to 3.9

df$size <- cut(df$mag,breaks = c(-1, 3.9, 4.9, 5.9, 6.9, 7.9, 12), labels=c("minor", "light", "moderate", "strong", "major", "great 8+"))
```

A snapshot of the data
========================================================
<br>
Example of 5 most recent earthquakes reported

|time                     |place                       | magnitude| depth|type       |
|:------------------------|:---------------------------|---------:|-----:|:----------|
|2017-09-10T20:31:47.000Z |66km N of Pahrump, Nevada   |      1.20| 10.50|earthquake |
|2017-09-10T20:20:42.442Z |124km W of Cantwell, Alaska |      1.20| 10.20|earthquake |
|2017-09-10T20:14:07.790Z |3km SSE of Redlands, CA     |      0.86|  3.54|earthquake |
|2017-09-10T20:08:48.010Z |4km WSW of Volcano, Hawaii  |      1.86|  0.54|earthquake |
|2017-09-10T20:05:19.884Z |139km W of Cantwell, Alaska |      1.40|  6.90|earthquake |
