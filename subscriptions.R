# subscription ratio table

subscriptions_query <- "WITH subscription_1 AS (
select users.school_id, subscriptions.kind, count(*) as subscriptions
from subscriptions
right join users on users.id = user_id
group by users.school_id, subscriptions.kind
order by school_id, kind
)
SELECT *,
sum(subscriptions) OVER (PARTITION BY school_id) as total_students,
subscriptions/sum(subscriptions) OVER (PARTITION BY school_id) as subscription_ratio
from subscription_1;"

subscriptions_ratio <- dbGetQuery(con, subscriptions_query)
subscriptions_ratio$school_id <- as.factor(subscriptions_ratio$school_id)
# make a plot of the subscriptions

subscriptions_ratio %>% ggplot(aes(x=school_id, y=subscriptions, fill=kind)) + geom_bar(stat="identity")


# in order to get total subscriptions
sub_ratio2 <- 
  subscriptions_ratio %>%
  filter(!is.na(kind)) %>%
  group_by(school_id) %>%
  summarise(total_subscriptions = sum(subscriptions), total_students = mean(total_students)) %>%
  mutate(ratio = total_subscriptions/total_students)

# Rate of subscriptions per school

subs_plot <- sub_ratio2 %>% 
  ggplot(aes(x=school_id, y=ratio, fill=school_id)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Paired", name="School ID") +
  ylab(label = "Conversion Rate (%) ") +
  xlab(label = "School ID") + 
  ggtitle("Rate of Subscriptions per School")


