#~ line if we are dealing with Macs
filename <- "~/Desktop/VixResearch/vix_numbers.csv"
#for windows, for mac
filename <- "C:\\Users\\sandovaj.AUTH\\Desktop\\VixResearch\\vix_numbers.csv"
vix_data <- read.csv(filename,header=TRUE,sep=",")
install.packages("sqldf","ggplot2","lubridate")
library(sqldf,ggplot2,lubridate)
vix_data$vix <- round(vix_data$vix)
vix_data["spx_nextd_retadj"] <- vix_data$spx_nextd_return + 1
vix_data["spx_retadj"] <- vix_data$spx_return + 1

#What do SPX returns look like objectively
spx_ret_dist <- sqldf("select avg(spx_return) avg_spx_return, stdev(spx_return) stdev_spx_return from vix_data")

#Count number of vix occurences per year
vix_occurences <- sqldf("select count(vix < 30) vix_count, vix, year from vix_data where year < 2016 group by year,vix")

#This is a file being written to Macs
write.csv(vix_occurences,"~\Desktop\VixResearch\vix_occurences.csv")

#Average positive return the day after a vix level
pos_vix_return <- sqldf("select vix, avg(spx_nextd_return) avg_nextd_return from vix_data where spx_nextd_return > 0 group by vix")
#Average negative return the day after a vix level

quantile(vix_data$vix,seq(0.1,0.9,0.1))

#Worst/best possible scenario in each volatility level
worst_case_df <- sqldf("select vix,min(spx_nextd_return) worst_return from vix_data where spx_nextd_return < 0 group by vix")
best_case_df <- sqldf("select vix,max(spx_nextd_return) best_return from vix_data where spx_nextd_return > 0 group by vix")
cases_df <- sqldf("select * from worst_case_df inner join best_case_df on worst_case_df.vix=best_case_df.vix")

#Average possible scenario in each volatility level
avg_neg_case_df <- sqldf("select vix,avg(spx_nextd_return) avg_worst_return from vix_data where spx_nextd_return < 0 group by vix")
avg_pos_case_df <- sqldf("select vix,avg(spx_nextd_return) avg_best_return from vix_data where spx_nextd_return > 0 group by vix")
average_cases_df <- sqldf("select * from avg_neg_case_df inner join avg_pos_case_df on avg_neg_case_df.vix=avg_pos_case_df.vix")

cases_df$vix <- NULL
average_cases_df$vix <- NULL

#Looking at averages, best and worst combined
average_tail_ret <- sqldf("select * from cases_df inner join average_cases_df on cases_df.vix = average_cases_df.vix")

#It's different to go from data files in R 
write.csv(average_tail_ret,"~/Desktop/VixResearch/average_tail_ret.csv")

#Try to break things into percentiles
vix_data["category"] <- 1
vix_data$category[vix_data$vix <= 12 ] <- 1
vix_data$category[vix_data$vix > 12 & vix_data$vix <= 13 ] <- 2
vix_data$category[vix_data$vix > 13 & vix_data$vix <= 15 ] <- 3
vix_data$category[vix_data$vix > 15 & vix_data$vix <= 16 ] <- 4
vix_data$category[vix_data$vix > 16 & vix_data$vix <= 18 ] <- 5
vix_data$category[vix_data$vix > 18 & vix_data$vix <= 20 ] <- 6
vix_data$category[vix_data$vix > 20 & vix_data$vix <= 22 ] <- 7
vix_data$category[vix_data$vix > 22 & vix_data$vix <= 25 ] <- 8
vix_data$category[vix_data$vix > 25 & vix_data$vix <= 29 ] <- 9
vix_data$category[vix_data$vix > 29] <- 10

#Let's breakup the last 10th into pieces, since it's where we have the largest dispersions
vix_data["subcategory"] <- 0
vix_data$subcategory[vix_data$vix <= 29 ] <- 0
vix_data$subcategory[vix_data$vix > 29 & vix_data$vix <= 31 ] <- 1
vix_data$subcategory[vix_data$vix > 31 & vix_data$vix <= 32 ] <- 2
vix_data$subcategory[vix_data$vix > 32 & vix_data$vix <= 36 ] <- 3
vix_data$subcategory[vix_data$vix > 36 & vix_data$vix <= 42 ] <- 4
vix_data$subcategory[vix_data$vix > 42] <- 5

#Let's look across various percentiles of data

worst_case_df1 <- sqldf("select category,min(spx_nextd_return) worst_return from vix_data where spx_nextd_return < 0 group by category")
best_case_df1 <- sqldf("select category,max(spx_nextd_return) best_return from vix_data where spx_nextd_return > 0 group by category")
cases_df1 <- sqldf("select * from worst_case_df1 inner join best_case_df1 on worst_case_df1.category=best_case_df1.category")

cases_df1$category <- NULL

avg_neg_case_df1 <- sqldf("select category,avg(spx_nextd_return) avg_worst_return from vix_data where spx_nextd_return < 0 group by category")
avg_pos_case_df1 <- sqldf("select category,avg(spx_nextd_return) avg_best_return from vix_data where spx_nextd_return > 0 group by category")
average_cases_df1 <- sqldf("select * from avg_neg_case_df1 inner join avg_pos_case_df1 on avg_neg_case_df1.category=avg_pos_case_df1.category")

average_cases_df1$category <- NULL

average_tail_ret1 <- sqldf("select * from cases_df1 inner join average_cases_df1 on cases_df1.category = average_cases_df1.category")

#Probability that the market is down 3 mos, 1 year, 3 years from now when it hits a 
#risk index, because the data is in weekdays, you can look at it 20 days from now, for
#example for 1 month

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

create_prob_risk <- function(days) {
	unique_vix_obs <- sort(unique(vix_data$vix))
	prob_market_down <- data.frame(unique_vix_obs)
	prob_market_down["prob_loss"] <- 0
	prob_market_down["prob_win"] <- 0
	prob_market_down["frequency"] <- 0
	prob_market_down["avg_gain"] <- 0
	prob_market_down["avg_loss"] <- 0
	count <- 1

	for(i in vix_data$vix) {
		location <- match(i,prob_market_down$unique_vix_obs)
		print
		if(is.na(vix_data$spx[count+days])) {break}
		if(vix_data$spx[count+days] < vix_data$spx[count]) {
			prob_market_down$prob_loss[location] <- prob_market_down$prob_loss[location] + 1
			prob_market_down$frequency[location] <- prob_market_down$frequency[location] + 1
			if(prob_market_down$avg_loss[location]== 0) {
				prob_market_down$avg_loss[location] <- (vix_data$spx[count+days]/vix_data$spx[count]) - 1
			} else {
				prob_market_down$avg_loss[location] <- (prob_market_down$avg_loss[location] + 
				(vix_data$spx[count+days]/vix_data$spx[count]) - 1)
			}
		} else {
			prob_market_down$prob_win[location] <- prob_market_down$prob_win[location] + 1
			prob_market_down$frequency[location] <- prob_market_down$frequency[location] + 1
			if(prob_market_down$avg_gain[location]== 0) {
				prob_market_down$avg_gain[location] <- (vix_data$spx[count+days]/vix_data$spx[count]) - 1
			} else {
				prob_market_down$avg_gain[location] <- (prob_market_down$avg_gain[location] + 
				(vix_data$spx[count+days]/vix_data$spx[count]) - 1)
			}
		}
		count <- count + 1
	}
	prob_market_down$prob_win <- percent(prob_market_down$prob_win/prob_market_down$frequency)
	prob_market_down$prob_loss <- percent(prob_market_down$prob_loss/prob_market_down$frequency)
	prob_market_down$avg_gain <- percent(prob_market_down$avg_gain/prob_market_down$frequency)
	prob_market_down$avg_loss <- percent(prob_market_down$avg_loss/prob_market_down$frequency)
	return(prob_market_down)
}

write.csv(create_prob_risk(20),"C:\\Users\\sandovaj.AUTH\\Desktop\\VixResearch\\prob_risk.csv")

#Compounded market returns sliced in different dimensions

active_return <- sqldf("select * from vix_data where category!=10")
active_return0 <- sqldf("select * from vix_data where subcategory == 0")
active_return1 <- sqldf("select * from vix_data where subcategory <= 1")
active_return2 <- sqldf("select * from vix_data where subcategory <= 2")
active_return3 <- sqldf("select * from vix_data where subcategory <= 3")
active_return4 <- sqldf("select * from vix_data where subcategory <= 4")
active_return5 <- sqldf("select * from vix_data where subcategory <= 5"

vix_levels <- as.numeric(unlist(sqldf("select distinct vix from vix_data order by vix")))
compounded_returns <- data.frame(vix_levels)
compounded_returns["total_comp_return"] <- 0
count <- 1
for(i in vix_levels) {
	temp <- subset(vix_data, vix < i)
	compounded_returns$total_comp_return[count] <- prod(temp$spx_nextd_retadj) - 1
	count <- count + 1
}

write.csv(compounded_returns,"C:\\Users\\sandovaj.AUTH\\Desktop\\VixResearch\\comp_ret.csv")

#What do returns look like if we took certain high vol environments

#Return Dispersions widen at vix = 30

return_dispersions <- data.frame(vix_levels)
return_dispersions["min"] <- 0
return_dispersions["max"] <- 0

for(i in vix_levels) {
	temp <- subset(vix_data, vix==i)
	a <- min(temp$spx_nextd_return)
	b <- max(temp$spx_nextd_return) 
	location <- match(i,return_dispersions$vix_levels)
	return_dispersions$min[location] <- a
	return_dispersions$max[location] <- b
}


return_dispersions_avg <- data.frame(vix_levels)
return_dispersions_avg["mean_loss"] <- 0
return_dispersions_avg["mean_gain"] <- 0

for(i in vix_levels) {
		temp1 <- subset(vix_data, vix==i & spx_nextd_return < 0)
		temp2 <- subset(vix_data, vix==i & spx_nextd_return >= 0)
		a <- mean(temp1$spx_nextd_return)
		b <- mean(temp2$spx_nextd_return)
		location <- match(i,return_dispersions_avg$vix_levels)
		return_dispersions_avg$mean_loss[location] <- a
		return_dispersions_avg$mean_gain[location] <- b
}

write.csv(return_dispersions_avg,"C:\\Users\\sandovaj.AUTH\\Desktop\\VixResearch\\avg_ret_disp.csv")


#Do years, months, quarters with more volatility have weaker returns
#What happens if we take out the worst months,
#What happens if we take the best performing months
#intraday volatility and slide 3


create_prob_risk_cat <- function(days) {
	unique_vix_cat <- sort(unique(vix_data$category))
	prob_market_down <- data.frame(unique_vix_cat)
	prob_market_down["prob_loss"] <- 0
	prob_market_down["prob_win"] <- 0
	prob_market_down["frequency"] <- 0
	prob_market_down["avg_gain"] <- 0
	prob_market_down["avg_loss"] <- 0
	prob_market_down["sharpe_ratio"] <- 0
	prob_market_down["expected_return"] <- 0
	count <- 1

	for(i in vix_data$category) {
		location <- match(i,prob_market_down$unique_vix_cat)
		print
		if(is.na(vix_data$spx[count+days])) {break}
		if(vix_data$spx[count+days] < vix_data$spx[count]) {
			prob_market_down$prob_loss[location] <- prob_market_down$prob_loss[location] + 1
			prob_market_down$frequency[location] <- prob_market_down$frequency[location] + 1
			if(prob_market_down$avg_loss[location]== 0) {
				prob_market_down$avg_loss[location] <- (vix_data$spx[count+days]/vix_data$spx[count]) - 1
			} else {
				prob_market_down$avg_loss[location] <- (prob_market_down$avg_loss[location] + 
				(vix_data$spx[count+days]/vix_data$spx[count]) - 1)
			}
		} else {
			prob_market_down$prob_win[location] <- prob_market_down$prob_win[location] + 1
			prob_market_down$frequency[location] <- prob_market_down$frequency[location] + 1
			if(prob_market_down$avg_gain[location]== 0) {
				prob_market_down$avg_gain[location] <- (vix_data$spx[count+days]/vix_data$spx[count]) - 1
			} else {
				prob_market_down$avg_gain[location] <- (prob_market_down$avg_gain[location] + 
				(vix_data$spx[count+days]/vix_data$spx[count]) - 1)
			}
		}
		count <- count + 1
	}
	prob_market_down$prob_win <- prob_market_down$prob_win/prob_market_down$frequency
	prob_market_down$prob_loss <- prob_market_down$prob_loss/prob_market_down$frequency
	prob_market_down$avg_gain <- prob_market_down$avg_gain/prob_market_down$frequency
	prob_market_down$avg_loss <- prob_market_down$avg_loss/prob_market_down$frequency
	prob_market_down$sharpe_ratio <- prob_market_down$avg_gain/prob_market_down$avg_loss
	prob_market_down$expected_return <- (prob_market_down$avg_gain*prob_market_down$prob_win) + (prob_market_down$avg_loss*prob_market_down$prob_loss)
	return(prob_market_down)
}





