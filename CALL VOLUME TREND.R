call_centre=read.csv("D:\\data ANALYTICS AND SCIENCE\\project data\\call volume trend analysis\\Call_Data.csv")
glimpse(call_centre)
call_centre
null_counts <- colSums(is.na(call_centre))
null_counts


# the average call time duration for all incoming calls received by agents (in each Time_Bucket).
average_time_duration <- aggregate(Call_Seconds..s. ~ Time_Bucket, data = call_centre, FUN = mean)
average_time_duration


# Plotting  the average_time_duration in a bar graph
barplot(average_time_duration$Call_Seconds..s., names.arg = average_time_duration$Time_Bucket, 
        xlab = "Time Bucket", ylab = "Average Call Seconds",
        main = "Average Call Duration by Time Bucket")





# Calculate the total number of calls and the number of answered calls for each time bucket
call_counts <- aggregate(Call_Status ~ Time_Bucket, data = call_centre, FUN = function(x) c(total_calls = length(x), answered_calls = sum(x == "answered")))
call_counts

# Create a bar graph to show the number of calls per time bucket
colors <- c("red", "green", "blue", "yellow", "orange")
barplot(calls_per_bucket, xlab = "Time Bucket", ylab = "Number of Calls", 
        main = "Number of Calls per Time Bucket", col = colors)


# Create a bar plot for number of total calls vs. answered calls
barplot(height = t(call_counts[, "Call_Status"]), beside = TRUE, 
        names.arg = call_counts[, "Time_Bucket"], 
        xlab = "Time Bucket", ylab = "Number of Calls",
        col = c("blue", "green"),
        legend.text = c("Total Calls", "Answered Calls"),
        main = "Number of Total Calls vs. Answered Calls")

# Add a legend
legend("topright", legend = c("Total Calls", "Answered Calls"), fill = c("blue", "green"))






"As you can see current abandon rate is approximately 30%. 
Propose a manpower plan required during each time bucket [between 9am to 9pm] to 
reduce the abandon rate to 10%. (i.e. You have to calculate minimum number of 
agents required in each time bucket so that at least 90 calls should be answered out of 100.) "

# Define the current and desired abandon rates
abandon_rate_current <- 0.30
abandon_rate_desired <- 0.10
# Define the average occupancy rate of agents
occupancy_rate <- 0.60
# Calculate the total number of calls and the number of answered calls for each time bucket
call_counts <- aggregate(Call_Status ~ Time_Bucket, data = call_centre, FUN = function(x) c(total_calls = length(x), answered_calls = sum(x == "answered")))
call_counts 

# Calculate the number of calls and the number of answered calls in each time bucket
total_calls <- call_counts$Call_Status[, "total_calls"]
total_calls
answered_calls <- call_counts$Call_Status[, "answered_calls"]
answered_calls
# Calculate the number of agents required in each time bucket
agents_required <- round((answered_calls / total_calls) / (1 - abandon_rate_desired) * 100)/ occupancy_rate
p=ceiling(agents_required)
# Print the minimum number of agents required in each time bucket
cat("Minimum Number of Agents Required per Time Bucket:\n")
cat("Time Bucket\t\tAgents Required\n")
cat("--------------------------------\n")
for (i in 1:length(agents_required)) {
  cat(call_counts$Time_Bucket[i], "\t\t", p[i], "\n")
}





"Let’s say customers also call this ABC insurance company in night but didn’t get 
answer as there are no agents to answer, this creates a bad customer experience for 
this Insurance company. Suppose every 100 calls that customer made during 9 Am to 9 Pm, 
customer also made 30 calls in night between interval [9 Pm to 9 Am] and distribution of 
those 30 calls are as follows:

Distribution of 30 calls coming in night for every 100 calls coming in between
9am - 9pm (i.e. 12 hrs slot)9pm- 10pm 	10pm - 11pm 	11pm- 12am	 12am- 1 am	
1am -2am 	2am -3am 	3am -4am 	4am - 5am 5am - 6am	 6am - 7am	 7am - 8am 	8am - 9am
3	3	2	2	1	1	1	1	3	4	4	5  respectively

Now propose a manpower plan required during each time bucket in a day.
Maximum Abandon rate assumption would be same 10%.

Assumption: An agent work for 6 days a week; On an average total unplanned leaves per agent 
is 4 days a month; An agent total working hrs is 9 Hrs out of which 1.5 Hrs 
goes into lunch and snacks in the office. On average an agent occupied for 60% of 
his total actual working Hrs (i.e 60% of 7.5 Hrs) on call with customers/ users. 
Total days in a month is 30 days."

# Define the distribution of calls in the night
night_calls_distribution <- c(3, 3, 2, 2, 1, 1, 1, 1, 3, 4, 4, 5)

# Define the maximum abandon rate assumption
abandon_rate_max <- 0.10

# Define the average working hours per day
working_hours_per_day <- 7.5

# Define the average occupancy rate of agents
occupancy_rate <- 0.60

# Calculate the total working hours of an agent available for calls
working_hours_available <- working_hours_per_day * occupancy_rate
working_hours_available

# Count the total number of calls and the number of answered calls for each time bucket
call_counts <- aggregate(Call_Status ~ Time_Bucket, data = call_centre, FUN = function(x) c(total_calls = length(x), answered_calls = sum(x == "answered")))
total_calls <- call_counts$Call_Status[, "total_calls"]
total_calls_sum <- sum(total_calls)
total_calls_sum

# Calculate the number of calls made during the night
night_total_calls <- (total_calls_sum * 30) / 100
night_total_calls

# Calculate the distribution of night calls
night_distribution <- night_total_calls * night_calls_distribution / sum(night_calls_distribution)
night_dist <- ceiling(night_distribution)
night_dist

# Define the time buckets
time_buckets <- c("9pm-10pm", "10pm-11pm", "11pm-12am", "12am-1am", "1am-2am", "2am-3am", "3am-4am", "4am-5am", "5am-6am", "6am-7am", "7am-8am", "8am-9am")

# Calculate the agents required for each time bucket in the night, considering the occupancy rate
agents_required_night <- night_dist / ((1 - abandon_rate_max) * 100) / occupancy_rate
agents_required_night 

# Print the time buckets along with the number of calls and agents required in the night
for (i in 1:length(time_buckets)) {
  cat(time_buckets[i], ": ", round(night_distribution[i]), "calls, ", round(agents_required_night[i]), "agents\n")
}













