head(users)

# get login counts
login_counts <- dbGetQuery(con, "select user_id, count(*) as num_of_logins from logins group by user_id;") 
login_counts %>% arrange(by=desc(num_of_logins))

# join login counts onto users

users2 <- left_join(users, login_counts, by=c("id" = "user_id"))
users2$num_of_logins[is.na(users2$num_of_logins)] <- 0
users2['sign_in_count'] <- users2['num_of_logins']
users2 <- users2 %>% select(-c(num_of_logins))

# get the count of votes per user
vote_counts <- 
  votes %>%
  group_by(user_id) %>%
  summarize(num_of_likes = sum(like_vote))

# join vote counts  
users2 <- left_join(users2, vote_counts, by=c("id" = "user_id"))
users2$num_of_likes[is.na(users2$num_of_likes)] <- 0
users2$votes_count <- users2$num_of_likes
users2 <- users2 %>% select(-c(num_of_likes))

users3 <- subset(users2, select = -c(referrals_count, year, downloads_count))  

#user_doc_counts
user_doc_counts <-
  documents %>%
  group_by(user_id) %>%
  summarize(documents_count = n())

# join it into the users table, and create a new updated version (ie: users3)
users3 <- left_join(users3, user_doc_counts, by=c("id" = "user_id"))
users3$documents_count <- users3$documents_count.y
users3 <- users3 %>% select(-c(documents_count.x, documents_count.y))
users3$documents_count[is.na(users3$documents_count)] <- 0

# get enrollments count
enrollments_user_count <-
  enrollments %>%
  group_by(user_id) %>%
  summarize(num_enrolled = n())

users3$enrollments_count[match(enrollments_user_count$user_id, users$id)] <- enrollments_user_count$num_enrolled

# need to join subscription info for users
users_final <- left_join(users3, 
                        (subscriptions %>% select(user_id, kind)),
                        by = c("id" = "user_id")) %>%
              mutate(has_subscription = !is.na(kind))

users_final$school_id <- as.factor(users_final$school_id)
users_final$country_id <- as.factor(users_final$country_id)


# MODEL
users_model <- glm(data=users_final, 
                   has_subscription ~ school_id + enrollments_count + votes_count + sign_in_count + documents_count + current_balance,
                   family=binomial())

summary(users_model)

users_model2 <- glm(data=users_final, 
                   has_subscription ~ enrollments_count + votes_count + sign_in_count + documents_count + current_balance,
                   family=binomial())

summary(users_model2)

step(users_model)
users_final_model <- glm(formula = has_subscription ~ school_id, family = binomial(), 
                         data = users_final)


lrtest(users_model2, users_model)

