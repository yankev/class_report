# we need a response variable to consider when looking at course subscription rates

# we first need to figure out which students are enrolled in the course
course_enrollments <-
  left_join(courses, enrollments, by=c("id" = "course_id"))

course_enrollments <- course_enrollments[-c(11, 12, 14)]
names(course_enrollments)[10] <- "enrollment_id"
course_enrollments <- 
  left_join(course_enrollments, users_final %>% select(id, has_subscription),
            by=c("user_id" = "id"))

ce2 <-
  course_enrollments %>%
  group_by(id) %>%
  summarize(total_subs = sum(has_subscription, na.rm = TRUE),
            enrollment_count_2 = sum(!is.na(enrollment_id)))
            
ce <- 
  right_join(courses, ce2, c("id")) %>%
  mutate(sub_ratio = total_subs/enrollment_count_2,
         sub_ratio_full = total_subs/enrollments_count)

# no real linear relatonships

ggplot(data=ce_nums, aes(x = documents_count, y=total_subs)) + geom_point()
ggplot(data=ce_nums, aes(x = enrollment_count_2, y=total_subs)) + geom_point()

#ce with doc counts

ce <- left_join(ce, 
               documents %>% group_by(course_id) %>% summarise(num_docs = n()),
               by=c("id" = "course_id"))

# lets get the values that are actual numbers in this case

#ce_nums_bool <- sapply(ce, is.numeric)
#ce_nums <- ce[, ce_nums_bool]

ce_nums <- ce %>% select(total_subs, enrollment_count_2, sub_ratio, num_docs)
row.has.na <- apply(ce_nums, 1, function(x){any(is.na(x)) || any(is.nan(x) || any(is.infinite(x)))})
ce_nums_clean <- ce_nums[!row.has.na,]
ce_corr_matrix <- cor(ce_nums_clean)
hchart(ce_corr_matrix)


ce_corr_matrix[, "total_subs"]
corrplot(ce_corr_matrix)

