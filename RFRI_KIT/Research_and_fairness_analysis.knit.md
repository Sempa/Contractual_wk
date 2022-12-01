
<!-- rnb-text-begin -->

---
title: "Research Fairness and Research Intergrity: A cross-sectional surevy"
author: "Dr Joseph B Sempa"
date: "2022-12-01"
output:
  word_document:
    fig_caption: yes
    toc: yes
  pdf_document:
    fig_caption: yes
    toc: yes
  html_document:
    fig_caption: yes
    toc: yes
  html_notebook:
    toc: yes
always_allow_html: true
classoption: landscape
---


<!-- rnb-text-end -->



<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Demographics

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Out of the 145 respondents, only 118 people responded to the question about country of origin. Majority 41 (34.7%) were from sub-Saharan Africa working many of whom were based in the global south. The second highest group were from Europe and Central Asia, with 32 (27.1%), many of whom work were based in the global north. Only one person was from the middle-East and North America, currently based in the global south. However, 27 (18.6%) respondents did not respond to this question.

Majority, 41 (35.3%) of the respondents were from Biostatistics/Epidemiology discipline of global health of whom majority twenty-two were operating from the global south, accounting for 34.4% from the global south. Biology/medicine was the second highest global heal discipline among our respondents with 23 (27.6%) and was also the second highest among country of origin with 18 (28.1%) from the global south. Only one respondent was from the Mathematics/computer science discipline and based in global north. However, we had 29 (20%) non-response rate on this question.

Majority of the respondents 59 (50.4%) were of the male-gender, accounting for 37 (56.9%) from the global south. The global south still had majority 28 (43.1%)female-gender representation. However, 28 (19.3%) respondents did not respond to this question.

Majority of the respondents 65 (55.6%) were established researchers with >10years post education, accounting for 35 (74.5%) from the global north. The Mid-career (3-10 years post-education) were the second highest 36 (30.8%), accounting for 27 (42.2%) based in the global south. However, we had 28 (19.3%) non-response rate on this question.

For the highest academic rank, we had a non-response rate of 27 (18.6%). however, majority of the respondents 34 (28.8%) had Ph.D degrees, accounting for 23 (35.4%) from the global south. The Associate Professor or Full Professor were the second highest 33 (28.0%), accounting for 20 (42.6%) from the global south. However, we had 28 (19.3%) non-response rate on this question. Respondents with a bachelor or Master degree were 29 (24.6%), accounting for 21 (32.3%) from the global south.

# Prevalence of responsible research practices and Fair research practices 

<!-- We used a cutoff score of 70 in both cases since the maximum score is 140 in both cases. -->

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Of the 145 respondents to the survey, 126 (86.9%) filled in the responsible research practices (RRP) questionnaire. Using a cutoff score of 70, which is the half the total possible score on the RRP tool, all respondents to this questionnaire from the global south and global north passed the questions and there was no statistically significant difference in the median score between global south 111 (95, 123) compared to global north 107 (97, 117) (P = 0.4). While, for fair research practices, with the same cutoff conditions and a response rate of 116 (80%), an equal number of two individuals failed and there was a statistically significant difference in the median score between global south 116 (99, 127) compared to global north 104 (90, 118) (P = 0.010).

<!-- # Responsible research practices and fair research practices by the question which of the following best describes you? -->
<!-- ```{r, message = FALSE, cache = TRUE, warning=FALSE, echo = FALSE, results='asis'} -->
<!-- dt02 <- final_dataset %>% -->
<!--   dplyr::select(rpq1, rpq2, rpq3, rpq4, rpq5, rpq6, rpq7, rpq8, rpq9, rpq10, rpq11, rpq12, rpq13, rpq14, rpq15, rpq16, rpq17, rpq18, rpq19, rpq20, fpq1, fpq2, fpq3, fpq4, fpq5, fpq6, fpq7, fpq8, fpq9, fpq10, fpq11, fpq12, fpq13, fpq14, fpq15, fpq16, fpq17, fpq18, fpq19, fpq20, `responsible research Practices score`, `fair research practices score`, `Which of the following best describes you?`) %>% -->
<!--   tbl_summary(by = `Which of the following best describes you?`,  -->
<!--             # missing = "no", -->
<!--             type = list(c('rpq1', 'rpq2', 'rpq3', 'rpq4', 'rpq5', 'rpq6', 'rpq7', 'rpq8', 'rpq9', 'rpq10', 'rpq11', 'rpq12', 'rpq13', 'rpq14', 'rpq15', 'rpq16', 'rpq17', 'rpq18', 'rpq19', 'rpq20', 'fpq1', 'fpq2', 'fpq3', 'fpq4', 'fpq5', 'fpq6', 'fpq7', 'fpq8', 'fpq9', 'fpq10', 'fpq11', 'fpq12', 'fpq13', 'fpq14', 'fpq15', 'fpq16', 'fpq17', 'fpq18', 'fpq19', 'fpq20', 'responsible research Practices score', 'fair research practices score') ~ 'continuous'), -->
<!--             statistic = list(c('rpq1', 'rpq2', 'rpq3', 'rpq4', 'rpq5', 'rpq6', 'rpq7', 'rpq8', 'rpq9', 'rpq10', 'rpq11', 'rpq12', 'rpq13', 'rpq14', 'rpq15', 'rpq16', 'rpq17', 'rpq18', 'rpq19', 'rpq20', 'fpq1', 'fpq2', 'fpq3', 'fpq4', 'fpq5', 'fpq6', 'fpq7', 'fpq8', 'fpq9', 'fpq10', 'fpq11', 'fpq12', 'fpq13', 'fpq14', 'fpq15', 'fpq16', 'fpq17', 'fpq18', 'fpq19', 'fpq20', 'responsible research Practices score', 'fair research practices score') ~ "{median} ({p25}, {p75})") -->
<!--             ) %>%  -->
<!--   # add_p() %>% -->
<!--   add_n() %>%  -->
<!--   modify_header(label = "Variable") %>% # update the column header -->
<!--   bold_labels() -->
<!-- dt02 -->
<!-- ``` -->


# Re-categorised RI and RF scores by location (Global North or South)


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->

null device 
          1 

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Responsible research practices and fair research practices by location (Global North or South)

In this analysis, I have removed those who chose the option prefer not to say

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->

Error in `summarise()`:
! Problem while computing `..1 = across(where(!is.na), list(mean = mean, sum = sum))`.
ℹ The error occurred in group 0: character(0).
Caused by error in `across()`:
! invalid argument type
Backtrace:
  1. ... %>% ...
 27. base::.handleSimpleError(`<fn>`, "invalid argument type", base::quote(!is.na))
 28. rlang h(simpleError(msg, call))
 29. handlers[[1L]](cnd)

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


About responsible research practices, when respondents were asked whether in the last three years, they gave *insufficient* attention to the skills or expertise essential to perform my studies, global south respondents had a significantly higher score 7.00 (6.00, 7.00) compared to the global north 6.00 (5.75, 7.00) (P = 0.040). When asked whether In the last three years, I *insufficiently* supervised or mentored junior co-workers had a significantly higher score 7.00 (6.00, 7.00) compared to the global north 6.00 (5.00, 7.00) (P = 0.002). There were no statistically significant differences in all other 18 questions in the Responsible research practices.

About fair research practices, when respondents were asked whether in the research they have been involved in over the last three years, they did not consult representatives of affected populations during the preparation stage of research, global south respondents had a significantly higher score 7.00 (6.00, 7.00) compared to the global north 5.00 (3.00, 6.00) (P <0.001). When asked whether in the research they have been involved in over the last three years, they did not consult end users of research during the preparation stage of research, the global south had a significantly higher score 6.00 (4.00, 7.00) compared to the global north 5.00 (4.00, 6.00) (P = 0.032). When asked whether in the research they have been involved in over the last three years there were clear *decision-making processes* agreed upon by all study partners, the global south had a significantly higher score 7.00 (6.00, 7.00) compared to the global north 6.00 (5.00, 6.50) (P = 0.003). When asked whether in the research they have been involved in over the last three years there were no clear and fair data ownership agreements, the global south had a significantly higher score 7.00 (6.00, 7.00) compared to the global north 6.00 (5.00, 7.00) (P = 0.045). When asked whether in the research they have been involved in over the last three years, whenever data was made openly accessible, strategies were put in place to encourage secondary analyses by local researchers, the global south had a significantly higher score 6.00 (3.25, 7.00) compared to the global north 4.00 (3.00, 6.00) (P = 0.037). There were no statistically significant differences in all other 15 questions in the Fair research practices.

# Multivariate model - responsible research Practices


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

At Univariate analysis, only academic rank of the respondents had a category that was significantly associated with responsible research practices. Bachelor or Master degree was associated with -17 (95%CI:-27, -6.9)	(P = 0.001) reduction in the responsible research score compared to Associate Professor or Full Professor. Other variables were not statistically significantly associated with responsible research practices. In the full model, after adjusting for Location, years of involvement, gender, country of origin and discipline of global health, Bachelor or Master degree was associated with -16.5 (95%CI:-30.6, -2.29) (P = 0.025) reduction in the responsible research practice score compared to Associate Professor or Full Professor. While in the reduced model (after stepwise regression analysis) Bachelor or Master degree was still associated with -18.3 (95%CI:-28.7, -7.84)	(P <0.001) reduction in the responsible research score compared to Associate Professor or Full Professor adjusting for location.


# Multivariate model - Fair research Practices


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->

`
<!-- rnb-html-begin eyJtZXRhZGF0YSI6eyJjbGFzc2VzIjpbInNoaW55LnRhZyJdLCJzaXppbmdQb2xpY3kiOltdfX0= -->

<div id="ilxhprkpkv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ilxhprkpkv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ilxhprkpkv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ilxhprkpkv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ilxhprkpkv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ilxhprkpkv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ilxhprkpkv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ilxhprkpkv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ilxhprkpkv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ilxhprkpkv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ilxhprkpkv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ilxhprkpkv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ilxhprkpkv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ilxhprkpkv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ilxhprkpkv .gt_from_md > :first-child {
  margin-top: 0;
}

#ilxhprkpkv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ilxhprkpkv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ilxhprkpkv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ilxhprkpkv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ilxhprkpkv .gt_row_group_first td {
  border-top-width: 2px;
}

#ilxhprkpkv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilxhprkpkv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ilxhprkpkv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ilxhprkpkv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ilxhprkpkv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilxhprkpkv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ilxhprkpkv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ilxhprkpkv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ilxhprkpkv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ilxhprkpkv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilxhprkpkv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ilxhprkpkv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilxhprkpkv .gt_left {
  text-align: left;
}

#ilxhprkpkv .gt_center {
  text-align: center;
}

#ilxhprkpkv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ilxhprkpkv .gt_font_normal {
  font-weight: normal;
}

#ilxhprkpkv .gt_font_bold {
  font-weight: bold;
}

#ilxhprkpkv .gt_font_italic {
  font-style: italic;
}

#ilxhprkpkv .gt_super {
  font-size: 65%;
}

#ilxhprkpkv .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#ilxhprkpkv .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ilxhprkpkv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ilxhprkpkv .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#ilxhprkpkv .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#ilxhprkpkv .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
  <table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Location</td>
<td class="gt_row gt_center">108</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Global South</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Global North</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-10</td>
<td class="gt_row gt_center">-19, -1.6</td>
<td class="gt_row gt_center">0.022</td></tr>
    <tr><td class="gt_row gt_left">accademic_rank</td>
<td class="gt_row gt_center">108</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Associate Professor or Full Professor</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Bachelor or Master degree</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-11</td>
<td class="gt_row gt_center">-23, 1.7</td>
<td class="gt_row gt_center">0.10</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Other (please specify)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">9.0</td>
<td class="gt_row gt_center">-18, 36</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">PhD degree</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-3.2</td>
<td class="gt_row gt_center">-15, 8.2</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Postdoc or Assistant Professor</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-8.0</td>
<td class="gt_row gt_center">-22, 5.9</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Prefer not to disclose</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-3.1</td>
<td class="gt_row gt_center">-36, 30</td>
<td class="gt_row gt_center">0.9</td></tr>
    <tr><td class="gt_row gt_left">year_involment_research</td>
<td class="gt_row gt_center">108</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Early Career (&lt; 3 years post-education)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Established (&gt;10 years post-education)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">3.5</td>
<td class="gt_row gt_center">-12, 19</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Mid-career (3-10 years post-education)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0.56</td>
<td class="gt_row gt_center">-16, 17</td>
<td class="gt_row gt_center">>0.9</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Prefer not to disclose</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-55</td>
<td class="gt_row gt_center">-101, -8.1</td>
<td class="gt_row gt_center">0.023</td></tr>
    <tr><td class="gt_row gt_left">gender</td>
<td class="gt_row gt_center">108</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-0.81</td>
<td class="gt_row gt_center">-9.5, 7.9</td>
<td class="gt_row gt_center">0.9</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Prefer not to disclose</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-33</td>
<td class="gt_row gt_center">-78, 12</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">country_origin</td>
<td class="gt_row gt_center">108</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">East Asia and Pacific</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Europe and Central Asia</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">2.1</td>
<td class="gt_row gt_center">-13, 17</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Middle East and North Africa</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">27</td>
<td class="gt_row gt_center">-19, 73</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">North America</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">5.9</td>
<td class="gt_row gt_center">-12, 24</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Prefer Not To Disclose</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-15</td>
<td class="gt_row gt_center">-61, 31</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">South Asia</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6.4</td>
<td class="gt_row gt_center">-11, 24</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Sub-Saharan Africa</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">15</td>
<td class="gt_row gt_center">0.24, 29</td>
<td class="gt_row gt_center">0.049</td></tr>
    <tr><td class="gt_row gt_left">discipline_global_health</td>
<td class="gt_row gt_center">108</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Biology/Medicine</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"><97></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Biostatistics/Epidemiology</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-6.1</td>
<td class="gt_row gt_center">-17, 4.6</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Mathematics/Computer science</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-17</td>
<td class="gt_row gt_center">-63, 28</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Other (please specify)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6.9</td>
<td class="gt_row gt_center">-5.6, 19</td>
<td class="gt_row gt_center">0.3</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Political sciences/Health economics</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">3.6</td>
<td class="gt_row gt_center">-15, 22</td>
<td class="gt_row gt_center">0.7</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Prefer not to disclose</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">22</td>
<td class="gt_row gt_center">-24, 67</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Sociology/Anthropology</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">-7.6</td>
<td class="gt_row gt_center">-26, 11</td>
<td class="gt_row gt_center">0.4</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">1</sup> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>


<!-- rnb-html-end -->
`{=html}
Call:
glm(formula = fpq_score ~ Location + year_involment_research, 
    family = gaussian, data = finalmodel_dataset)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-73.234  -11.468    4.848   15.995   36.894  

Coefficients:
                                                              Estimate Std. Error t value Pr(>|t|)    
(Intercept)                                                   109.0495     7.3943  14.748   <2e-16 ***
LocationGlobal North                                          -13.1485     4.4524  -2.953   0.0039 ** 
year_involment_researchEstablished (>10 years post-education)   6.2052     7.7983   0.796   0.4280    
year_involment_researchMid-career (3-10 years post-education)  -0.8156     8.1353  -0.100   0.9203    
year_involment_researchPrefer not to disclose                 -59.0495    22.9549  -2.572   0.0115 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 472.2539)

    Null deviance: 56211  on 107  degrees of freedom
Residual deviance: 48642  on 103  degrees of freedom
AIC: 978.38

Number of Fisher Scoring iterations: 2

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


At Univariate analysis, coming from the global north was associated with -10 (95%CI:-19, -1.6)	(P = 0.022) reduction the fair research practices scores compared to respondent from global south, while those who preferred not to say their number of years of involvement in research  had a -55	(95%CI:-101, -8.1)	(P = 0.023) reduction in fair research practices scores compared to Early Career (< 3 years post-education). Respondent from the sub-Saharan region had a 15 (95%CI:0.24, 29)	(P = 0.049) increase in fair research practices scores compared to those from East Asia and Pacific. Other variables were not statistically significantly associated with fair research practices. 
In the full model, after adjusting for Location, years of involvement, gender, country of origin and discipline of global health, respondents who preferred not to disclose their years of involvement in research had a -50.0 (95%CI:-98.7, -1.25)	(P = 0.048) reduction in the fair research scores compared to Early Career (< 3 years post-education). While in the reduced model (after stepwise regression analysis) respondents who preferred not to disclose their years of involvement in research had a -59.0 (95%CI:-104, -14.1)	(P = 0.012) reduction in the fair research scores compared to Early Career (< 3 years post-education) after adjusting for location and global north respondents had a -13.1 (95%CI:-21.9, -4.42) (P = 0.004) reduction in the fair research practices score compared to the global south adjusting for year of involvement.. 

# Analysis for those who responded N/A
In this analysis, I have only tabulated those who said N/A in any of the 40 questions

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->

