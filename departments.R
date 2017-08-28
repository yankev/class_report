# department analysis

dep2 <- left_join(departments, ce, by=c("id" = "department_id"))
dep2 <- dep2 %>% group_by(id) %>% summarise(total_subs = sum(total_subs, rm.na=TRUE), total_enrollments=sum(enrollment_count_2, rm.na=TRUE))
dep2 <- dep2 %>% left_join(departments, dep2, by=c("id"))
# need to remove all departments with no enrollments
dep2 <- dep2 %>% filter(!is.na(total_enrollments), !is.na(total_subs))

num_dep2 <- dep2[, c("total_subs", "total_enrollments", "courses_count", "documents_count", "enrollments_count")]
dep_corr_matrix <- cor(num_dep2)
corrplot(dep_corr_matrix, method="ellipse")

# let's make some scatter plots

ggplot(data=dep2, aes(x=documents_count, y=total_subs)) + geom_point()


#models
dep_model <-
  glm(data=dep2, 
    total_subs ~ total_enrollments + courses_count + documents_count + enrollments_count,
    family=poisson)

summary(dep_model)


# let's evaluate the difference between total_enrollments and total_subs
diffs <- dep2 %>% mutate(diff = total_enrollments - total_subs) %>% select(diff)
library(psych)
describe(diffs$diff)
ggplot(data=diffs, aes(x=diff)) + geom_histogram(bins=30)
ggplot(data=dep2, aes(x=total_subs)) + geom_histogram(bins=30)


# let's extended the department df with counts

# we'll have to group the courses table by # of docs
ce_group_by_dep <- 
  ce %>% 
  group_by(department_id) %>% 
  summarise(num_docs_dep = sum(num_docs, na.rm=TRUE), num_courses=n())

full_dep <- left_join(dep2, ce_group_by_dep, by=c("id" = "department_id"))
full_dep


# actually we should remove some of the columns that we don't want
full_dep <- full_dep %>% select(id, school_id, total_subs, total_enrollments, num_docs_dep, num_courses)
full_dep$school_id <- as.factor(full_dep$school_id)

num_dep2 <- full_dep[, c("total_subs", "total_enrollments", "num_courses", "num_docs_dep")]
dep_corr_matrix <- cor(num_dep2)
hchart(dep_corr_matrix)

corrplot(dep_corr_matrix, method="ellipse")


new_dep_model <-
  glm(data=full_dep, 
      total_subs ~ school_id + num_docs_dep,
      family=poisson())

summary(new_dep_model)
step(new_dep_model)
exp(coef(new_dep_model))





summary(dep_model)  