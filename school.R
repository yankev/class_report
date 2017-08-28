# let's fix up the school dataframe to fill out the blanks
library(tidyr)
library(corrplot)

# get count of courses per school
course_counts <- courses %>%
  group_by(school_id) %>%
  summarize(course_count=n())

# get count of departments

departments_count <- departments %>%
  group_by(school_id) %>%
  summarize(dep_count=n())

# get the count of documents for each course
course_doc_count <- dbGetQuery(con, 
  "select courses.id, count(documents.id) from courses
  left join documents on documents.course_id = courses.id
  GROUP BY courses.id")

school_course_doc_count <- dbGetQuery(con, 
                               "select courses.school_id, courses.id, count(documents.id) from courses
                                left join documents on documents.course_id = courses.id
                               GROUP BY courses.id, courses.school_id;")

# get ratio of courses with documents vs # courses per school
school_course_doc_ratio <- 
  school_course_doc_count %>% mutate(has_docs = count > 0) %>%
  group_by(school_id) %>%
  summarise(has_doc_ratio = mean(has_docs), num_course_with_docs=sum(has_docs))

total_docs_count <- doc_count %>% group_by(school_id) %>% summarize(total_docs = sum(num_of_docs))

# update the schools df with new variables and information
schools2 <- left_join(schools, course_counts, by=c("id" = "school_id")) %>%
  left_join(departments_count, by=c("id" = "school_id")) %>%
  select(-c(departments_count, courses_count)) %>%
  rename(departments_count = dep_count, courses_count = course_count) %>%
  left_join(school_course_doc_ratio, by=c("id" = "school_id"))

schools2['id'] <- as.factor(schools2[['id']])

# get document counts per school
doc_count <- documents %>% 
  group_by(school_id, kind) %>%
  summarise(num_of_docs = n())
doc_count['school_id'] <- as.factor(doc_count[['school_id']])

doc_count_spread <- spread(doc_count, key = kind, value = num_of_docs) %>%
  mutate(total_docs = `1` + `2` + `3`) %>%
  rename(doc_1=`1`, doc_2=`2`, doc_3=`3`)

schools3 <- left_join(schools2, doc_count_spread, by=c("id" = "school_id"))

#get enrollments per school
enrollments_count <- enrollments %>%
  group_by(school_id) %>%
  summarise(num_of_enrollments = n())
enrollments_count['school_id'] <- as.factor(enrollments_count[['school_id']])

schools3 <- left_join(schools3, enrollments_count, by=c("id" = "school_id"))

# now let's join the output variable, subscriptions ratio to the table

# first let's alter the subscriptions ratio so that we record the total subscribers
# and not just people who aren't
subs_with_na <- is.na(subscriptions_ratio$kind)
subscriptions_ratio$subscriptions[subs_with_na] <- 
  subscriptions_ratio$total_students[subs_with_na] - subscriptions_ratio$subscriptions[subs_with_na]

subscriptions_ratio$subscription_ratio[subs_with_na] <-
  1 - subscriptions_ratio$subscription_ratio[subs_with_na]  
subscriptions_ratio$kind[subs_with_na] <- "total_subs"
#spread_subs_ratio <- spread(subscriptions_ratio, kind, subscriptions)
#spread_subs_ratio[is.na(spread_subs_ratio)] <- 0
#names(spread_subs_ratio)[7] <- "total_subs"

schools3 <- left_join(schools3, subscriptions_ratio, by=c("id" = "school_id"))

schools_final <- schools3
rm(schools3)

# deal with totals for now
nums <- sapply(schools_final, is.numeric)
corr_matrix <- cor((schools_final %>% filter(kind == "total_subs"))[nums])
sub_ratio_correlations <- corr_matrix[, "subscription_ratio"]

### PLOTS ####

sub_ratio_cor_plot <- corrplot(corr_matrix, method="ellipse", order="hclust")

doc_plot <-
  doc_count %>%
  ggplot(aes(x=school_id, y=num_of_docs, fill=kind)) +
  geom_bar(stat = "identity") +
  ylab(label = "Number of Documents") +
  xlab(label = "School ID") + 
  ggtitle("Number of Documents Per School Split on Type") +
  scale_fill_brewer(palette = "Paired", name="Kind of Document")

enrollment_plot <- 
  enrollments_count %>% 
  ggplot(aes(x=school_id, y=num_of_enrollments, fill=school_id)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Paired", name="School ID") +
  ylab(label = "Enrollments") +
  xlab(label = "School ID") + 
  ggtitle("Number of Enrollments Per School")

# now let's create another plot vs `has_doc_ratio`
has_doc_ratio_plot <- 
  schools2 %>%
  ggplot(aes(x=id, y=has_doc_ratio, fill=id)) +
  geom_bar(stat="identity") +
  ylab(label = "Ratio of courses with documents") +
  xlab(label = "School ID") + 
  ggtitle("Schools vs Ratio of Courses with Content")

# plot of school vs number courses with docs 
courses_with_docs_per_school_plot <- 
  schools2 %>%
  ggplot(aes(x=id, y=num_course_with_docs, fill=id)) +
  geom_bar(stat="identity") +
  ylab(label = "Number of courses with documents") +
  xlab(label = "School ID") + 
  ggtitle("Schools vs Ratio of Courses with Content")

#
courses_with_docs_per_school_plot <- 
  schools2 %>%c
  ggplot(aes(x=id, y=num_course_with_docs, fill=id)) +
  geom_bar(stat="identity") +
  ylab(label = "Number of courses with documents") +
  xlab(label = "School ID") + 
  ggtitle("Schools vs Ratio of Courses with Content")
  
multiplot(doc_plot, enrollment_plot, subs_plot, has_doc_ratio_plot, cols=2)


# let's try a model sir
model <- glm(subscription_ratio ~ courses_count + departments_count + has_doc_ratio + num_course_with_docs + total_docs + num_of_enrollments + total_students, 
             data = schools_final %>% filter(kind == "total_subs"), 
             family=gaussian)

summary(model)

model2 <- lm(subscription_ratio ~ courses_count + departments_count + has_doc_ratio + num_course_with_docs + total_docs + num_of_enrollments + total_students, 
             data = schools_final %>% filter(kind == "total_subs"))
summary(model2)

model3 <- glm(subscriptions ~ courses_count + departments_count + has_doc_ratio + num_course_with_docs + total_docs + num_of_enrollments + total_students, 
              data = schools_final %>% filter(kind == "total_subs"), 
              family=poisson)
summary(model3)
coef(model3)
exp(coef(model3))

model4 <- glm(subscriptions ~ courses_count + departments_count + has_doc_ratio + num_course_with_docs + total_docs + num_of_enrollments + total_students, 
              data = schools_final %>% filter(kind == "total_subs"), 
              family=quasipoisson)
summary(model4)

# unfortunately these models aren't that good, particularly because the degrees of freedom are incredibly low
# we onyl have 10 data points


library(lmtest)
base_model <- glm(subscriptions ~ 1, 
                  data = schools_final %>% filter(kind == "total_subs"), 
                  family=poisson)

lrtest(base_model, model3)
