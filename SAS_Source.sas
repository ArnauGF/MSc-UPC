/********************************************************************************************
AUTHOR: Arnau Garcia Fernandez

Original data set link: https://github.com/VicPena/data/tree/master/nba-elo

Observations about the data set: The data set used in this work is from a public repository
of the GitHub of Victor Peña, who is a professor of this Master. See more observations and 
comments about the data set in the report of this work.
Comments about this script: in this script I will be adding the code for solve the different
problems proposed in the statements of the project. The structure of this script will be easy.
I will copy the statements and I will write the code. In addition, I will add the necessary
comments. Notwithstanding, the whole explanations and the comments of the results obtained
will be in the report.
********************************************************************************************/

/*1. Import the dataset to SAS and create the corresponding SAS dataset.*/

proc import OUT=nba 
            DATAFILE= "/home/u63714055/nbaallelo.xlsx" 
            DBMS=xlsx REPLACE;
            GETNAMES=YES; 
run;

/*We print the first 10 observations and export the table for put it in the report.*/
ODS HTML CLOSE; 
ODS PDF FILE='/home/u63714055/obs.pdf';
proc print data=nba(obs=10);
run;
ODS PDF CLOSE;
ODS HTML;


/*2. Give labels and formats to the variables of the dataset.*/

*We prepare the formats;
proc format;
 	value playoff 0='Regular season game' 1='Playoff game';
 	value copy 0='Not copy' 1='Copy';
 	value $location 'H'='Home' 'A'='Away' 'N'='Neutral';
 	value $result 'L'='Lost' 'W'='Win';
run; 

/*Previously to put the formats and the labels we run a proc contents to see if there is
any variable with a format that is not the expected.*/ 

proc contents data=nba;
run;
/*We observe that elo_i, elo_n, forecast, oppo_elo_i, oppo_elo_n, win_equiv have alfanumeric
format, but we are interested in numeric format for these variables. Thus, we transform
these variables into numeric variables.*/

*Put the formats and labels to the dataset;
data nba;
	set nba;
	label gameorder='Play order of game in NBA history' 
	game_id='Unique ID for each game'
	lg_id='Which league the game was played in' 
	_iscopy='If game_id has already occured for the opposing team in the same matchup'
	year_id='Season id, named based on year in which the season ended'
	date_game='Game date'
	is_playoffs='Flag for playoff games'
	team_id='Three letter code for team name'
	fran_id='Franchise id'
	pts='Points scored by team'
	elo_i2='Team elo entering the game'
	elo_n2='Team elo following the game'
	win_equiv2='Equivalent number of wins in a 82-game season for a team of elo_n quality'
	opp_id='Team id of opponent'
	opp_fran='Franchise id of opponent'
	opp_pts='Points scored by opponent'
	opp_elo_i2='Opponent elo entering the game'
	opp_elo_n2='Opponent elo following the game'
	game_location='Home (H), away (A), or neutral (N)'
	game_result='Win or loss for team in the team_id column'
	forecast2='Elo-based chances of winning for the team in the team_id column, based on elo ratings and game location';
	format is_playoffs playoff. _iscopy copy. game_location $location. game_result $result.
		elo_i2 9.5 elo_n2 9.5 forecast2 9.8 opp_elo_i2 9.5 opp_elo_n2 9.5 win_equiv2 9.5;
	elo_i2=elo_i;
	elo_n2=elo_n;
	forecast2=forecast;
	opp_elo_i2=opp_elo_i;
	opp_elo_n2=opp_elo_n;
	win_equiv2=win_equiv;
	drop notes elo_i elo_n forecast opp_elo_i opp_elo_n win_equiv;
run;


/*3. Carry out a descriptive analysis to check that there are no values of any variable out of
range or extreme observations which could be considered outliers. Indicate in the
script if everything was ok or not.*/

*Let's do some proc freq in order to study the frequencies of categorical variables;
proc freq data=nba;
	table game_location;
run;
*Indeed, we obtain the same frequence of games at Home and Away;

proc freq data=nba;
	table game_result;
run;
*50 vs 50 percent of games win vs lost, which is what we expect because the data is duplicate;

proc freq data=nba;
	table _iscopy;
run;
*indeed a half of the games are copies and the other half not copies;

proc freq data=nba;
	table is_playoffs;
run;
*As we expect, there are much more regular season games than playoff games;



*Now, we conduct a proc means for study the continous variables;
ODS HTML CLOSE; 
ODS PDF FILE='/home/u63714055/means.pdf';
proc means data=nba;
	var _numeric_;
run;
ODS PDF CLOSE;
ODS HTML;


*We have games with 0 points! We see which are these games;

proc print data=nba(where=(pts=0));
run;

*It is a game in 10/26/1972	of denver rockets vs virgina squires, if we search on the internet we see that it was a foul game;

proc print data=nba(where=(opp_pts=0));
run;

*It is the same game than before;

*We have an obs with min win_equiv 10.1525010, which is much less than the median (1277.11);
proc print data=nba(where=(win_equiv2<11));
run;
/*we can see that this low win equiv is due the Dallas mavericks of the 1993, see more info
about this team in  https://www.basketball-reference.com/teams/DAL/1994.html
we can see that the Dallas mavs was the worst team in the nba! They had a final 
record of 13-69. Then, this observations with low win equiv are not an outlier and are
correct.*/

/*4. If you find any error in the previous section or extreme observations, you should
decide what to do with them (remove? Convert them to missing? Keep them?) Write
down what you did and why.*/

*Answered in the report;

/*5. Obtain a complete description of three to five of the variables that seem most relevant*/

/*we are interested in the winning probability of a team taking into account the elo
then, we do a complet desrcription of the elo variables and the forecast one*/
proc means data=nba MEDIAN P25 p50 p75 mean std min max;
	var elo_i2 elo_n2 opp_elo_i2 opp_elo_n2 forecast2;
run;

/*6.Define groupings (make a categorization) of one or more of the original numerical
variables.*/

*Looking at the elo rating system (https://en.wikipedia.org/wiki/Elo_rating_system);
*We classify the elo ratings, we will be using United States Chess Federation ratings;

proc format;
 value elo_rating low -< 199 = 'Class J' 199-<399 = 'Class I' 
 	399-<599 = 'Class H' 599-<799='Class G' 799-<999='Class F'
 	999-<1199='Class E' 1199-<1399='Class D' 1399-<1599='Class C'
 	1599<-1799='Class B' 1799-<1999='Class A' 1999-<2199='Expert Class'
 	2199-<2399='National master Class' 2399-high='Senior Master Class'; 
run;

data nba2;
	set nba;
	format elo_i_rate elo_rating. elo_n_rate elo_rating. opp_elo_i_rate elo_rating.
		opp_elo_n_rate elo_rating.;
	elo_i_rate=elo_i2;
	elo_n_rate=elo_n2;
	opp_elo_i_rate=opp_elo_i2;
	opp_elo_n_rate=opp_elo_n2;
run;

*We can study the frequence of the different classes with respect the chess rating;
proc freq data=nba2;
	table elo_i_rate;
run;

*Now, we do categorization for the probabilities of win, using the forecast variable;
proc format;
	value win_prob low-<0.25='Probabilities of win<25%' 0.25-<0.5='Probabilities of win 25%-50%'
	0.5-<0.75='Probabilities of win 50%-75%' 0.75-high='Probabilities of win >75%';
run;

data nba2;
	set nba2;
	format win_prob win_prob.;
	win_prob=forecast2;
run;

*We study the frequencies in the categories created;
proc freq data=nba2;
	table win_prob;
run;
*We can compare it with the game results in order to see wether the win probabilities are working good;
proc freq data=nba2;
	table win_prob*game_result;
run;

/*7. Carry out some type of analysis of the continuous variables crossing them with some
categorical variable or with some of the categorizations defined in the previous section, 
after giving them the corresponding format.*/
/*
a. This analysis must include a statistical proc and a graphical proc as minimum.
b. Indicate before the execution of the PROCs, as a commentary, what is the
purpose of the analysis you wish to perform.
c. Using IML within SAS, call R to repeat the same analysis done in part a. 
Compare the results and discuss them.
*/

/*We analize the forecast variable (Elo-based chances of winning), with respect the game result variable.
We do this with the aim of assess how good is the elo score predicting the game results,
in the sense that the greater the value of the forecast variable, the more win probabilities
we expect.*/

proc means data=nba2 MEDIAN P25 p50 p75 mean std min max;
	var forecast2;
	class game_result;
run;

/*It seems that distribution of the data is as we expect, the wins has win chances predicted
of more than 0.5. Notwithstanding, the minimum and maximum observations are strange. We study
in deep this observations:*/
proc print data=nba2(where=(forecast2<0.03 and game_result="W"));
run;

/*we can observe that these two cases of very low probability of win, and then win,
are both in the same season (1993), and both for the Dallas Mavericks playing away.
See the results of this team in https://www.basketball-reference.com/teams/DAL/1994.html
we can see that the Dallas mavs was the worst team in the nba! But this is sport,
and the bad teams also win.*/

proc print data=nba2(where=(forecast2>0.96 and game_result="L"));
run;

/*Two of the three obs obtained are again related with the Dallas mavs of 1993.
The other observation is a game of the Jordan's Bulls loosing VS Orlando.*/

/*We do a boxplot for see what is the behaviour graphically.*/

proc sgplot data=nba2;
	vbox forecast2 / category=game_result;
run;

/*We can observe in the boxplot that the distribution of the data is as we expect,
for each game which result is win, the probabilities of win are in general more than 0.5.
The previous boxplot indicates that the elo rating system is working good.*/

/*We add the game location to the analysis*/
proc sgplot data=nba2;
	vbox forecast2 / category=game_result group=game_location;
run;
/*Indeed, the elo-based chances of winning are high for the categories win and do it playing
at home. What is what we expect if the elo rating is working well and can predict the 
game results. The elo-based chances of winning are low for lost games and games away. Again,
this is what we expect if the elo rating is working good.*/

/*Now, using the elo categorizations done in the previous section, we analyze the team elo
entering the game vs the forecast variable. We want to study if the better the elo class 
in the chess ranking, the higher the probability of victory*/
proc means data=nba2 MEDIAN P25 p50 p75 mean std min max;
	var forecast2;
	class elo_i_rate;
run;

proc sgplot data=nba2;
	vbox forecast2 / category=elo_i_rate;
run;

/*Indeed we obtain what we expect, the better the elo class the better the win probability.*/

/*Now, we cross the elo entering variable (continous variable) with the game_result variable.
With this, we want to see if the elo rating before the match is predicting well or not
the result of the match.*/
proc means data=nba2 MEDIAN P25 p50 p75 mean std min max;
	var elo_i2;
	class game_result;
run;
proc sgplot data=nba2;
	vbox elo_i2 / category=game_result;
run;
/*we see that in general, the elo entering behaves as we expect, but the differences
between the win or lost categories are not very large.*/

/*Now we do the proc IML part:*/

proc iml;

title "Statistics in R (called from SAS!)";

run ExportDataSetToR("WORK.nba2", "nba");

submit / R;

setwd("C:/Users/arnau.garcia.fernandez/Desktop")
library(ggplot2)
#summary of a numeric variable with respect a categorical variable using tapply
#first forecast vs game result
summary_result <- tapply(nba$forecast2, nba$game_result, 
                         function(x) c(mean = mean(x), sd = sd(x), median=median(x),
                                       min = min(x),  max=max(x), 
                                       quantile(x, probs = c(0.25, 0.5, 0.75))))
print(summary_result)
#printing the strange obs
nba[nba$forecast<0.03 & nba$game_result=="W",]
#the boxplot
ggplot(nba) + 
  aes(x=game_result, y=forecast2)+
  geom_boxplot()
#export the boxplot
dev.copy(png, filename="boxplot.png", width=5.25,height=5.25,units="in",  res=200)
dev.off()

#boxplot adding game location
ggplot(nba) + 
  aes(x=game_result, y=forecast2, fill=game_location)+
  geom_boxplot()
#export the boxplot
dev.copy(png, filename="boxplot1.png", width=5.25,height=5.25,units="in",  res=200)
dev.off()


#now we study forecast vs elo rate
summary_result <- tapply(nba$forecast2, nba$elo_i_rate, 
                         function(x) c(mean = mean(x), sd = sd(x), median=median(x),
                                       min = min(x),  max=max(x), 
                                       quantile(x, probs = c(0.25, 0.5, 0.75))))
print(summary_result)
#boxplot
ggplot(nba) + 
  aes(x=elo_i_rate, y=forecast2)+
  geom_boxplot()
#export the boxplot
dev.copy(png, filename="boxplot2.png", width=5.25,height=5.25,units="in",  res=200)
dev.off()


#now, elo entering the game vs game result
summary_result <- tapply(nba$elo_i2, nba$game_result, 
                         function(x) c(mean = mean(x), sd = sd(x), median=median(x),
                                       min = min(x),  max=max(x), 
                                       quantile(x, probs = c(0.25, 0.5, 0.75))))
print(summary_result)
#boxplot
ggplot(nba) + 
  aes(x=elo_i2, y=game_result)+
  geom_boxplot()
#export the boxplot
dev.copy(png, filename="boxplot3.png", width=5.25,height=5.25,units="in",  res=200)
dev.off()

endsubmit;
quit;

/*The results obtained are the same than the obtained in SAS. This is because we
have developed the R code with the aim of obtain the same objects than the ones
created with SAS.*/

/*8. Carry out other analyzes that may interest you. Indicate prior to the execution of the
PROCESSES, in the form of a comment, what is the purpose of the analysis you wish
to perform. */

*We want to check if there are more wins in home games than in away games;
proc freq data=nba;
	tables game_location*game_result;
run;
*As one can expect, there are more wins in home games than in away games;

*We study which teams have more wins;
proc freq data=nba;
	table team_id*game_result;
run;
*Boston celtics and lakers are the teams with more wins;

/*We want to study the evolution in scoring in the different decades of the league. 
For this we make a new categorization of the years by decades, and look at the scores 
of the teams.  */

proc format;
	value decades low-<1950='Decade 1940s' 1950-<1960='Decade 1950s' 
		1960-<1970='Decade of 1960s' 1970-<1980='Decade 1970s'
		1980-<1990='Decade of 1980s' 1990-<2000='Decade of 1990s'
		2000-<2010='Decade of 2000s' 2010-high='Decade of 2010s';
run;

data nba2;
	set nba2;
	format decades decades.;
	decades=year_id;
run;

proc sgplot data=nba2;
	vbox pts / category=decades;
run;

/*We can see that the evolution of the league in terms of points is quite stable. 
Keep in mind that we only have data up to 2015, and that it was more or less from 
those years onwards that the league had a big paradigm shift with the three-point shot.*/

/*9. Create a macro to repeat some of the processes you have done, for example, do the
same analysis for different countries / cities / etc, using macros.*/

/*We create a macro that given a data set, a categorical variable and a continous variable
returns a boxplot.*/

%macro boxplot_nba(dat, varcont, varcat); 
proc sgplot data=&dat;
	vbox &varcont / category=&varcat;
run;
%mend boxplot_nba;

/*we repeat some of the previous boxplots using the macro*/
%boxplot_nba(nba2,pts,decades);

%boxplot_nba(nba2, forecast2, elo_i_rate);

/*Now, we create a macro that given the name of the team returns a frequency table
of the game results per decade, and another frequency table with its elo rating per decade. In 
addition, we plot an histogram of the points scored by the team. This several operations
will be useful for asses how good was this team in the whole nba history, and for
see if the elo ratings per decade reflects good the team quality.*/


%macro team_info(dat, team); 
proc freq data=&dat(where=(fran_id=&team));
	table game_result*decades;
run;
proc freq data=&dat(where=(fran_id=&team));
	table elo_i_rate*decades;
run;
proc gchart data=&dat(where=(fran_id=&team));
vbar pts / type=freq;
run;
%mend team_info;

*We analyze the Boston Celtics;
%team_info(nba2, 'Celtics');
/*We can see that Celtics was a nice team in 1960s, they had a 70.45% of wins and in the
79% of the games (before play the game) they were a Class A team in the elo rating.
The Celtics of the 1970s was also nice. Looking at the histogram of points we can see
that the distribution seems normal centered to 107 points.*/

*We analize the Bucks, notice that the Bucks was created in 1968;
%team_info(nba2, 'Bucks');
/*We can observe that the best Bucks registered in our data are the 1980s Bucks, thry
had a 61.77% of wins and they was Class B team in the 62% of the games.*/

*We analize the Bucks, notice that the Bucks was created in 1995;
%team_info(nba2, 'Grizzlies');
/*Grizzlies in 2010s were good, they had 59.55% of wins. In more than 95% of games in the 
2010s the Grizzlies was a Class C or Class B team. Marc Gasol, an historic player 
of this franchise, probably helped in this good results. Notwithstanding, in the first
decade of the franchise they had a 81.08% of lost games. They was a Class D, Class E team. */



	

