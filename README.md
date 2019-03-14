## Alexa Songs R Shiny Dashboard

This R Shiny dashboard displays data about songs that are played via Amazon Alexa. The data is stored in Google Spreadsheets via an IFTTT application. The dashboard contains four tabs: Frequency Plot, Year to Year Popularity, Top Songs Played, and About. The user can group frequency plots based on four different time units: Day, Week, Month, and Day of Week. The Year to Year Popularity tab contains a bump chart that can be grouped based on the top five most popular Artists or Albums played. The entire dashboard can be filtered with a date range.

#### Frequency Plots Tab
![AlexaListeningHistoryFreqPlots](https://user-images.githubusercontent.com/7990117/54166228-9f205900-443a-11e9-8a85-7b21a69fe8c6.png)

#### Year to Year Popularity Tab
![AlexaListeningHistoryBumpChart](https://user-images.githubusercontent.com/7990117/54166225-9b8cd200-443a-11e9-8364-34e5009fa488.png)

#### Top Songs Played Tab
![AlexaListeningHistoryTopSongs](https://user-images.githubusercontent.com/7990117/54166230-a21b4980-443a-11e9-8b40-d50938de92f8.png)

----

#### Data Provenance:

I played each song via the Amazon Alexa with the voice command, “Alexa, play [genre] music, or Alexa, play music by [artist].” When each song begins playing, the [IFTTT](https://ifttt.com/) application adds a new record to a google sheet using the [Keep a Google spreadsheet of the songs you listen to on Alexa](https://ifttt.com/applets/72041438d-keep-a-google-spreadsheet-of-the-songs-you-listen-to-on-alexa) applet. The google sheet consists of four columns with no headers (Example below).

Example Record:

| Field             	   | Value								               |
|:------------------------ |:------------------------------------------------- |
| **Date and Time** &nbsp; | March 15, 2018 at 06:56PM 						   |
| **Artists**			   | Modest Mouse 									   |
| **Song**				   | Dashboard 										   |
| **Album**				   | We Were Dead Before The Ship Even Sank [Explicit] |

I load the google sheet into R using the [Google Sheets R API](https://github.com/jennybc/googlesheets) then apply the following transformations to the data:
+ The data frame is given column names (date_time, artist, song, album).
+ Each record in the Date and Time column is converted from a character to a POSIXct date-time object.
+ The artist column is split into two columns (main_artist and featuring).
+ The song and album columns are cleaned to no longer have [Explicit], [Clean], (Album Version), etc tags. This allows for clean, explicit, and live versions of a song or album to be grouped together and counted as the same album.
+ Date, Week_Day, Week, Month, Year, and Time columns are added to the data frame.
+ Skipped songs are removed from the data. If two or more songs contain the same date-time stamp, only the last song is kept.

Resulting Record:

| Field 		         | Value								  |
|:--------------		 |:-------------------------------------- |
|**date_time**		     | 2018-03-15 18:56:00 					  |
|**main_artist**  &nbsp; | Modest Mouse 						  |
|**featuring**		     | NA 									  |
|**song**				 | Dashboard 							  |
|**album**				 | We Were Dead Before The Ship Even Sank |
|**Date**				 | 2018-03-15 							  |
|**Week_Day**			 | Thu 									  |
|**Week**				 | 11 									  |
|**Month**				 | Mar 									  |
|**Time**				 | 18:56 								  |
|**Year**				 | 2018 								  |

----

#### Resources:

Two blog posts were tremendously helpful to me while I was working on this project. [John Nguyen's college academic rankings bump chart](http://data-slinky.com/2016/07/31/bump_charts.html) provided the initial inspiration to visualize my listening history with a bump chart. I also found [Jay Blanco’s project](http://www.jayblanco.com/blog/2016/7/9/using-lastfm-and-r-to-understand-my-music-listening-habits) when researching for similar projects. I particularly like the crisp look of his lollipop charts. Both John and Jay's projects provided helpful code examples of charts I was interested in making for this project. 

**Package Information**

| Package            		| Resources |
|:------------------------- |:--------- |
| **RShiny**         		| [Package Site](https://shiny.rstudio.com/reference/shiny/1.1.0/) |
| **R Googlesheets** &nbsp; | [RDocumentation](https://www.rdocumentation.org/packages/googlesheets/versions/0.3.0), [Github](https://github.com/jennybc/googlesheets) |
| **dplyr**					| [RDocumentation](https://www.rdocumentation.org/packages/dplyr/versions/0.7.6), [Package Site](https://dplyr.tidyverse.org/) |
| **ggplot2**	   		    | [RDocumentation](https://www.rdocumentation.org/packages/ggplot2/versions/3.0.0), [Package Site](https://ggplot2.tidyverse.org/) | 
| **viridis**				| [RDocumentation](https://www.rdocumentation.org/packages/viridis/versions/0.5.1), [Github](https://github.com/sjmgarnier/viridis) |
| **ggrepel**		 		| [RDocumentation](https://www.rdocumentation.org/packages/ggrepel/versions/0.8.0), [Github](https://github.com/slowkow/ggrepel) |
| **datetime**				| [RDocumentation](https://www.rdocumentation.org/packages/datetime/versions/0.1.3) |
| **lubridate**				| [RDocumentation](https://www.rdocumentation.org/packages/lubridate/versions/1.7.4), [Package Site](https://lubridate.tidyverse.org/) |
| **tidyr** 			    | [RDocumentation](https://www.rdocumentation.org/packages/tidyr/versions/0.8.1), [Package Site](https://tidyr.tidyverse.org/) |
| **rlang**			 		| [RDocumentation](https://www.rdocumentation.org/packages/rlang/versions/0.2.1), [Github](https://github.com/r-lib/rlang) |
| **here** 			 		| [RDocumentation](https://www.rdocumentation.org/packages/here/versions/0.1), [Github](https://github.com/r-lib/here) |
| **markdown**				| [RDocumentation](https://www.rdocumentation.org/packages/markdown), [Github](https://github.com/rstudio/markdown) |
| **cowplot**				| [RDocumentation](https://www.rdocumentation.org/packages/cowplot), [Github](https://github.com/wilkelab/cowplot) |

