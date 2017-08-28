# get document counts per school
doc_count <- documents %>% 
  group_by(school_id, kind) %>%
  summarise(num_of_docs = n())

doc_count['school_id'] <- as.factor(doc_count[['school_id']])

doc_plot <- 
  doc_count %>%
  ggplot(aes(x=school_id, y=num_of_docs, fill=kind)) +
  geom_bar(stat = "identity") +
  ylab(label = "Number of Documents") +
  xlab(label = "School ID") + 
  ggtitle("Number of Documents Per School Split on Type") +
  scale_fill_brewer(palette = "Paired", name="Kind of Document")

#get enrollments per school
enrollments_count <- enrollments %>%
  group_by(school_id) %>%
  summarise(num_of_enrollments = n())

enrollments_count['school_id'] <- as.factor(enrollments_count[['school_id']])

enrollment_plot <- 
  enrollments_count %>% 
  ggplot(aes(x=school_id, y=num_of_enrollments, fill=school_id)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Paired", name="School ID") +
  ylab(label = "Enrollments") +
  xlab(label = "School ID") + 
  ggtitle("Number of Enrollments Per School")

multiplot(doc_plot, enrollment_plot, subs_plot, has_doc_ratio_plot, cols=2)
