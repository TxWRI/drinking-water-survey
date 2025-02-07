Perceptions of Drinking Water and Safety: A Public Survey of U.S.
Residents
================

This is the data and code repository associated with the draft
manuscript, “Perceptions of Drinking Water and Safety: A Public Survey
of U.S. Residents” currently in submission. Authors include: Karissa
Palmer, T. Allen Berthold, Audrey McCrary, Stephanie deVilleneuve,
Michael Schramm, and Holli R. Leggette

This project uses the [renv](https://rstudio.github.io/renv/) and
[targets](https://docs.ropensci.org/targets/) R packages to facilitate
reproducibility. Once the project is downloaded and opened in RStudio
install the renv package:

``` r
install.packages("renv")
```

The packages and package versions used in the project can be installed
and restored using:

``` r
renv::restore()
```

To reproduce the analysis you will need a Census API key which can be
obtained from <https://api.census.gov/data/key_signup.html>. Then use
targets to rerun the analysis. This will take a little while to run:

``` r
tidycensus::census_api_key("API KEY GOES HERE")
targets::tar_make()
```

Survey responses used in our analysis are stored in the `clean_survey`
target:

``` r
targets::tar_read(clean_survey)
```

    ## # A tibble: 1,100 × 91
    ##    SEX   AGEP  RACE5 HOUSING HOWNSHP INCOME HHSIZE SCHL  RACE2 Q5    Q6    Q7   
    ##    <fct> <fct> <fct> <fct>   <fct>   <fct>   <dbl> <fct> <fct> <fct> <fct> <lgl>
    ##  1 Male  25:34 Asian Townho… Rent    $50,0…      5 Bach… Non-… Priv… Filt… TRUE 
    ##  2 Fema… 65+   Whit… Apartm… Rent    < $25…      1 High… White Publ… Filt… FALSE
    ##  3 Fema… 18:24 Whit… Single… Rent    $25,0…      3 Mast… White Publ… Bott… FALSE
    ##  4 Fema… 45:54 Two … Apartm… Rent    $25,0…      2 Other Non-… Publ… Bott… TRUE 
    ##  5 Fema… 35:44 Whit… Single… Own     $25,0…      4 Mast… White I do… Filt… FALSE
    ##  6 Fema… 35:44 Whit… Single… Own     $75,0…      5 High… White Publ… Bott… FALSE
    ##  7 Fema… 55:64 Whit… Single… Rent    < $25…      3 High… White Publ… Unfi… FALSE
    ##  8 Fema… 35:44 Blac… Apartm… Rent    $50,0…      2 High… Non-… Publ… Bott… TRUE 
    ##  9 Male  45:54 Two … Townho… Rent    $25,0…      2 Doct… Non-… Priv… Unfi… TRUE 
    ## 10 Fema… 35:44 Blac… Apartm… Rent    $25,0…      2 High… Non-… Priv… Filt… TRUE 
    ## # ℹ 1,090 more rows
    ## # ℹ 79 more variables: Q7_1 <fct>, Q7_2 <fct>, Q7_3 <fct>, Q7_4 <fct>,
    ## #   Q7_5 <fct>, Q7_6 <fct>, Q7_7 <fct>, Q7_8 <fct>, Q7_9 <fct>, Q7_10 <fct>,
    ## #   Q7_11 <fct>, Q7_12 <fct>, Q8 <int>, Q9 <lgl>, Q9_1 <fct>, Q9_2 <fct>,
    ## #   Q9_3 <fct>, Q9_4 <fct>, Q9_5 <fct>, Q9_6 <fct>, Q9_7 <fct>, Q9_8 <fct>,
    ## #   Q9_9 <fct>, Q9_10 <fct>, Q9_11 <fct>, Q9_12 <fct>, Q10_1 <fct>,
    ## #   Q10_2 <fct>, Q10_3 <fct>, Q10_4 <fct>, Q10_5 <fct>, Q10_6 <fct>, …

## Models

### RQ1

Research Question 1 was inferred using a weighted proportional odds
model: $$
\ln \left( \frac{P(Y\leq j)}{P(Y>j)} \right) = \alpha_j - \beta_1 x_1 - \cdots - \beta_n x_n 
$$

where the left hand side of the equation is the log odds of the
cumulative probability of an outcome being at up to and including level
$j$. The outcome levels are ordinal rating 1,…,10 in response to the
question: “How safe do you believe your drinking water is for
consumption?” Predictor variables included sex/gender, age category,
race/ethnicity, education, homeownership, income, primary tap water
supply, and experience with any of (1) odd taste, (2) odd smell, (3)
discolored water, or (4) cloudy water.

The RQ1 model is stored in target `m1`:

``` r
gtsummary::tbl_regression(targets::tar_read(m1), exponentiate = TRUE)
```

<div id="peebzddfjn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#peebzddfjn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#peebzddfjn thead, #peebzddfjn tbody, #peebzddfjn tfoot, #peebzddfjn tr, #peebzddfjn td, #peebzddfjn th {
  border-style: none;
}
&#10;#peebzddfjn p {
  margin: 0;
  padding: 0;
}
&#10;#peebzddfjn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#peebzddfjn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#peebzddfjn .gt_title {
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
&#10;#peebzddfjn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#peebzddfjn .gt_heading {
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
&#10;#peebzddfjn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#peebzddfjn .gt_col_headings {
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
&#10;#peebzddfjn .gt_col_heading {
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
&#10;#peebzddfjn .gt_column_spanner_outer {
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
&#10;#peebzddfjn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#peebzddfjn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#peebzddfjn .gt_column_spanner {
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
&#10;#peebzddfjn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#peebzddfjn .gt_group_heading {
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
  text-align: left;
}
&#10;#peebzddfjn .gt_empty_group_heading {
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
&#10;#peebzddfjn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#peebzddfjn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#peebzddfjn .gt_row {
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
&#10;#peebzddfjn .gt_stub {
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
&#10;#peebzddfjn .gt_stub_row_group {
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
&#10;#peebzddfjn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#peebzddfjn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#peebzddfjn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#peebzddfjn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#peebzddfjn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#peebzddfjn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#peebzddfjn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#peebzddfjn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#peebzddfjn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#peebzddfjn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#peebzddfjn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#peebzddfjn .gt_footnotes {
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
&#10;#peebzddfjn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#peebzddfjn .gt_sourcenotes {
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
&#10;#peebzddfjn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#peebzddfjn .gt_left {
  text-align: left;
}
&#10;#peebzddfjn .gt_center {
  text-align: center;
}
&#10;#peebzddfjn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#peebzddfjn .gt_font_normal {
  font-weight: normal;
}
&#10;#peebzddfjn .gt_font_bold {
  font-weight: bold;
}
&#10;#peebzddfjn .gt_font_italic {
  font-style: italic;
}
&#10;#peebzddfjn .gt_super {
  font-size: 65%;
}
&#10;#peebzddfjn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#peebzddfjn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#peebzddfjn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#peebzddfjn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#peebzddfjn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#peebzddfjn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#peebzddfjn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#peebzddfjn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#peebzddfjn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">SEX</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate" class="gt_row gt_center">0.76</td>
<td headers="conf.low" class="gt_row gt_center">0.59, 0.97</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.13</td>
<td headers="conf.low" class="gt_row gt_center">0.05, 0.31</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18:24</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    25:34</td>
<td headers="estimate" class="gt_row gt_center">0.94</td>
<td headers="conf.low" class="gt_row gt_center">0.59, 1.49</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    35:44</td>
<td headers="estimate" class="gt_row gt_center">0.79</td>
<td headers="conf.low" class="gt_row gt_center">0.48, 1.30</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    45:54</td>
<td headers="estimate" class="gt_row gt_center">0.46</td>
<td headers="conf.low" class="gt_row gt_center">0.28, 0.76</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55:64</td>
<td headers="estimate" class="gt_row gt_center">0.72</td>
<td headers="conf.low" class="gt_row gt_center">0.43, 1.21</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    65+</td>
<td headers="estimate" class="gt_row gt_center">1.14</td>
<td headers="conf.low" class="gt_row gt_center">0.68, 1.93</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RACE2</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-white</td>
<td headers="estimate" class="gt_row gt_center">0.78</td>
<td headers="conf.low" class="gt_row gt_center">0.57, 1.07</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHL</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Some high school</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High school graduate or GED</td>
<td headers="estimate" class="gt_row gt_center">1.24</td>
<td headers="conf.low" class="gt_row gt_center">0.58, 2.66</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Associate degree</td>
<td headers="estimate" class="gt_row gt_center">1.03</td>
<td headers="conf.low" class="gt_row gt_center">0.46, 2.28</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Bachelor's degree</td>
<td headers="estimate" class="gt_row gt_center">1.51</td>
<td headers="conf.low" class="gt_row gt_center">0.68, 3.36</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Master's degree</td>
<td headers="estimate" class="gt_row gt_center">2.00</td>
<td headers="conf.low" class="gt_row gt_center">0.88, 4.54</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Doctorate or terminal degree</td>
<td headers="estimate" class="gt_row gt_center">1.68</td>
<td headers="conf.low" class="gt_row gt_center">0.69, 4.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.95</td>
<td headers="conf.low" class="gt_row gt_center">0.37, 2.47</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HOWNSHP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rent</td>
<td headers="estimate" class="gt_row gt_center">0.68</td>
<td headers="conf.low" class="gt_row gt_center">0.51, 0.89</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &lt; $25,000</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $25,000 - $49,999</td>
<td headers="estimate" class="gt_row gt_center">1.19</td>
<td headers="conf.low" class="gt_row gt_center">0.82, 1.73</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $50,000 - $74,999</td>
<td headers="estimate" class="gt_row gt_center">1.07</td>
<td headers="conf.low" class="gt_row gt_center">0.72, 1.60</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $75,000 - $99,999</td>
<td headers="estimate" class="gt_row gt_center">0.90</td>
<td headers="conf.low" class="gt_row gt_center">0.57, 1.42</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $100,000 - $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.50</td>
<td headers="conf.low" class="gt_row gt_center">0.96, 2.34</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt; $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.82</td>
<td headers="conf.low" class="gt_row gt_center">0.90, 3.71</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - municipal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - rural water district</td>
<td headers="estimate" class="gt_row gt_center">0.81</td>
<td headers="conf.low" class="gt_row gt_center">0.58, 1.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private supply - well, river, pond, rainwater</td>
<td headers="estimate" class="gt_row gt_center">1.54</td>
<td headers="conf.low" class="gt_row gt_center">1.10, 2.17</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I don't know</td>
<td headers="estimate" class="gt_row gt_center">0.99</td>
<td headers="conf.low" class="gt_row gt_center">0.62, 1.59</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q7</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate" class="gt_row gt_center">0.32</td>
<td headers="conf.low" class="gt_row gt_center">0.25, 0.41</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

### RQ2

Research question 2 asks how much trust does the public have in drinking
water entities. Again we fit a proportion-odds model to ordinal rating
levels (No responsibility, Some responsibility, Moderate responsibility,
Most responsibility, Full Responsibility) in response to the question,
“What level of responsibility should the following entities have for
making sure drinking water is safe?” Our predictor variable of interest
are drinking water entities: Federal Government, State government, Local
government, Water utility, Landlord/property manager, and household
residents.

Results for this model are stored in target `m4`:

``` r
gtsummary::tbl_regression(targets::tar_read(m4), exponentiate = TRUE)
```

<div id="tdyamuppga" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tdyamuppga table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#tdyamuppga thead, #tdyamuppga tbody, #tdyamuppga tfoot, #tdyamuppga tr, #tdyamuppga td, #tdyamuppga th {
  border-style: none;
}
&#10;#tdyamuppga p {
  margin: 0;
  padding: 0;
}
&#10;#tdyamuppga .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#tdyamuppga .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#tdyamuppga .gt_title {
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
&#10;#tdyamuppga .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#tdyamuppga .gt_heading {
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
&#10;#tdyamuppga .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tdyamuppga .gt_col_headings {
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
&#10;#tdyamuppga .gt_col_heading {
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
&#10;#tdyamuppga .gt_column_spanner_outer {
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
&#10;#tdyamuppga .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#tdyamuppga .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#tdyamuppga .gt_column_spanner {
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
&#10;#tdyamuppga .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#tdyamuppga .gt_group_heading {
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
  text-align: left;
}
&#10;#tdyamuppga .gt_empty_group_heading {
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
&#10;#tdyamuppga .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#tdyamuppga .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#tdyamuppga .gt_row {
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
&#10;#tdyamuppga .gt_stub {
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
&#10;#tdyamuppga .gt_stub_row_group {
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
&#10;#tdyamuppga .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#tdyamuppga .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#tdyamuppga .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tdyamuppga .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#tdyamuppga .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#tdyamuppga .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tdyamuppga .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tdyamuppga .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#tdyamuppga .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#tdyamuppga .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#tdyamuppga .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tdyamuppga .gt_footnotes {
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
&#10;#tdyamuppga .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tdyamuppga .gt_sourcenotes {
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
&#10;#tdyamuppga .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tdyamuppga .gt_left {
  text-align: left;
}
&#10;#tdyamuppga .gt_center {
  text-align: center;
}
&#10;#tdyamuppga .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#tdyamuppga .gt_font_normal {
  font-weight: normal;
}
&#10;#tdyamuppga .gt_font_bold {
  font-weight: bold;
}
&#10;#tdyamuppga .gt_font_italic {
  font-style: italic;
}
&#10;#tdyamuppga .gt_super {
  font-size: 65%;
}
&#10;#tdyamuppga .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#tdyamuppga .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#tdyamuppga .gt_indent_1 {
  text-indent: 5px;
}
&#10;#tdyamuppga .gt_indent_2 {
  text-indent: 10px;
}
&#10;#tdyamuppga .gt_indent_3 {
  text-indent: 15px;
}
&#10;#tdyamuppga .gt_indent_4 {
  text-indent: 20px;
}
&#10;#tdyamuppga .gt_indent_5 {
  text-indent: 25px;
}
&#10;#tdyamuppga .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#tdyamuppga div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">ENTITY</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Federal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Local</td>
<td headers="estimate" class="gt_row gt_center">1.99</td>
<td headers="conf.low" class="gt_row gt_center">1.68, 2.35</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Property</td>
<td headers="estimate" class="gt_row gt_center">0.63</td>
<td headers="conf.low" class="gt_row gt_center">0.53, 0.75</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Resident</td>
<td headers="estimate" class="gt_row gt_center">0.48</td>
<td headers="conf.low" class="gt_row gt_center">0.40, 0.57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    State</td>
<td headers="estimate" class="gt_row gt_center">1.70</td>
<td headers="conf.low" class="gt_row gt_center">1.44, 2.01</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Utility</td>
<td headers="estimate" class="gt_row gt_center">3.05</td>
<td headers="conf.low" class="gt_row gt_center">2.55, 3.64</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SEX</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate" class="gt_row gt_center">1.01</td>
<td headers="conf.low" class="gt_row gt_center">0.91, 1.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.64</td>
<td headers="conf.low" class="gt_row gt_center">0.32, 1.28</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18:24</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    25:34</td>
<td headers="estimate" class="gt_row gt_center">1.16</td>
<td headers="conf.low" class="gt_row gt_center">0.95, 1.43</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    35:44</td>
<td headers="estimate" class="gt_row gt_center">1.87</td>
<td headers="conf.low" class="gt_row gt_center">1.52, 2.30</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    45:54</td>
<td headers="estimate" class="gt_row gt_center">1.63</td>
<td headers="conf.low" class="gt_row gt_center">1.32, 2.00</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55:64</td>
<td headers="estimate" class="gt_row gt_center">1.46</td>
<td headers="conf.low" class="gt_row gt_center">1.17, 1.81</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    65+</td>
<td headers="estimate" class="gt_row gt_center">1.86</td>
<td headers="conf.low" class="gt_row gt_center">1.51, 2.30</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RACE2</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-white</td>
<td headers="estimate" class="gt_row gt_center">1.05</td>
<td headers="conf.low" class="gt_row gt_center">0.92, 1.20</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHL</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Some high school</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High school graduate or GED</td>
<td headers="estimate" class="gt_row gt_center">1.17</td>
<td headers="conf.low" class="gt_row gt_center">0.87, 1.57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Associate degree</td>
<td headers="estimate" class="gt_row gt_center">1.22</td>
<td headers="conf.low" class="gt_row gt_center">0.90, 1.66</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Bachelor's degree</td>
<td headers="estimate" class="gt_row gt_center">1.18</td>
<td headers="conf.low" class="gt_row gt_center">0.87, 1.61</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Master's degree</td>
<td headers="estimate" class="gt_row gt_center">1.24</td>
<td headers="conf.low" class="gt_row gt_center">0.90, 1.70</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Doctorate or terminal degree</td>
<td headers="estimate" class="gt_row gt_center">1.24</td>
<td headers="conf.low" class="gt_row gt_center">0.80, 1.92</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.19</td>
<td headers="conf.low" class="gt_row gt_center">0.81, 1.76</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HOWNSHP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rent</td>
<td headers="estimate" class="gt_row gt_center">1.06</td>
<td headers="conf.low" class="gt_row gt_center">0.94, 1.19</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &lt; $25,000</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $25,000 - $49,999</td>
<td headers="estimate" class="gt_row gt_center">1.19</td>
<td headers="conf.low" class="gt_row gt_center">1.01, 1.40</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $50,000 - $74,999</td>
<td headers="estimate" class="gt_row gt_center">1.05</td>
<td headers="conf.low" class="gt_row gt_center">0.88, 1.25</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $75,000 - $99,999</td>
<td headers="estimate" class="gt_row gt_center">1.08</td>
<td headers="conf.low" class="gt_row gt_center">0.90, 1.31</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $100,000 - $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.41</td>
<td headers="conf.low" class="gt_row gt_center">1.15, 1.73</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt; $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.08</td>
<td headers="conf.low" class="gt_row gt_center">0.81, 1.45</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SOURCE</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - municipal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - rural water district</td>
<td headers="estimate" class="gt_row gt_center">0.88</td>
<td headers="conf.low" class="gt_row gt_center">0.77, 1.01</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private supply - well, river, pond, rainwater</td>
<td headers="estimate" class="gt_row gt_center">0.67</td>
<td headers="conf.low" class="gt_row gt_center">0.57, 0.78</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I don't know</td>
<td headers="estimate" class="gt_row gt_center">0.81</td>
<td headers="conf.low" class="gt_row gt_center">0.66, 0.98</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.07</td>
<td headers="conf.low" class="gt_row gt_center">0.03, 0.15</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

A second model was fit to ordinal rating levels (Do not trust at all,
Somewhat trust, Moderately trust, Mostly trust, Fully trust) in response
to the questions, “What is your level of trust in the following entities
for making sure your drinking water is safe for consumption?” Our
predictor variable of interest are drinking water entities. Results are
stored in target `m5`.

``` r
gtsummary::tbl_regression(targets::tar_read(m5), exponentiate = TRUE)
```

<div id="rkrdskhrji" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rkrdskhrji table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rkrdskhrji thead, #rkrdskhrji tbody, #rkrdskhrji tfoot, #rkrdskhrji tr, #rkrdskhrji td, #rkrdskhrji th {
  border-style: none;
}
&#10;#rkrdskhrji p {
  margin: 0;
  padding: 0;
}
&#10;#rkrdskhrji .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#rkrdskhrji .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rkrdskhrji .gt_title {
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
&#10;#rkrdskhrji .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#rkrdskhrji .gt_heading {
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
&#10;#rkrdskhrji .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkrdskhrji .gt_col_headings {
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
&#10;#rkrdskhrji .gt_col_heading {
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
&#10;#rkrdskhrji .gt_column_spanner_outer {
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
&#10;#rkrdskhrji .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rkrdskhrji .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rkrdskhrji .gt_column_spanner {
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
&#10;#rkrdskhrji .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rkrdskhrji .gt_group_heading {
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
  text-align: left;
}
&#10;#rkrdskhrji .gt_empty_group_heading {
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
&#10;#rkrdskhrji .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rkrdskhrji .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rkrdskhrji .gt_row {
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
&#10;#rkrdskhrji .gt_stub {
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
&#10;#rkrdskhrji .gt_stub_row_group {
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
&#10;#rkrdskhrji .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rkrdskhrji .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rkrdskhrji .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkrdskhrji .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rkrdskhrji .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rkrdskhrji .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkrdskhrji .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkrdskhrji .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rkrdskhrji .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkrdskhrji .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rkrdskhrji .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rkrdskhrji .gt_footnotes {
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
&#10;#rkrdskhrji .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkrdskhrji .gt_sourcenotes {
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
&#10;#rkrdskhrji .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rkrdskhrji .gt_left {
  text-align: left;
}
&#10;#rkrdskhrji .gt_center {
  text-align: center;
}
&#10;#rkrdskhrji .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rkrdskhrji .gt_font_normal {
  font-weight: normal;
}
&#10;#rkrdskhrji .gt_font_bold {
  font-weight: bold;
}
&#10;#rkrdskhrji .gt_font_italic {
  font-style: italic;
}
&#10;#rkrdskhrji .gt_super {
  font-size: 65%;
}
&#10;#rkrdskhrji .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rkrdskhrji .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rkrdskhrji .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rkrdskhrji .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rkrdskhrji .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rkrdskhrji .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rkrdskhrji .gt_indent_5 {
  text-indent: 25px;
}
&#10;#rkrdskhrji .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#rkrdskhrji div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">ENTITY</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Federal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Local</td>
<td headers="estimate" class="gt_row gt_center">1.43</td>
<td headers="conf.low" class="gt_row gt_center">1.21, 1.69</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Property</td>
<td headers="estimate" class="gt_row gt_center">1.07</td>
<td headers="conf.low" class="gt_row gt_center">0.90, 1.27</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Resident</td>
<td headers="estimate" class="gt_row gt_center">3.14</td>
<td headers="conf.low" class="gt_row gt_center">2.63, 3.75</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    State</td>
<td headers="estimate" class="gt_row gt_center">1.38</td>
<td headers="conf.low" class="gt_row gt_center">1.17, 1.62</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Utility</td>
<td headers="estimate" class="gt_row gt_center">2.21</td>
<td headers="conf.low" class="gt_row gt_center">1.87, 2.63</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SEX</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate" class="gt_row gt_center">0.85</td>
<td headers="conf.low" class="gt_row gt_center">0.76, 0.94</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.51</td>
<td headers="conf.low" class="gt_row gt_center">0.25, 1.03</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18:24</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    25:34</td>
<td headers="estimate" class="gt_row gt_center">1.14</td>
<td headers="conf.low" class="gt_row gt_center">0.93, 1.39</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    35:44</td>
<td headers="estimate" class="gt_row gt_center">1.01</td>
<td headers="conf.low" class="gt_row gt_center">0.83, 1.24</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    45:54</td>
<td headers="estimate" class="gt_row gt_center">0.80</td>
<td headers="conf.low" class="gt_row gt_center">0.64, 0.99</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55:64</td>
<td headers="estimate" class="gt_row gt_center">0.90</td>
<td headers="conf.low" class="gt_row gt_center">0.73, 1.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    65+</td>
<td headers="estimate" class="gt_row gt_center">0.69</td>
<td headers="conf.low" class="gt_row gt_center">0.57, 0.85</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RACE2</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-white</td>
<td headers="estimate" class="gt_row gt_center">1.01</td>
<td headers="conf.low" class="gt_row gt_center">0.89, 1.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHL</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Some high school</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High school graduate or GED</td>
<td headers="estimate" class="gt_row gt_center">1.48</td>
<td headers="conf.low" class="gt_row gt_center">1.12, 1.96</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Associate degree</td>
<td headers="estimate" class="gt_row gt_center">1.44</td>
<td headers="conf.low" class="gt_row gt_center">1.07, 1.95</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Bachelor's degree</td>
<td headers="estimate" class="gt_row gt_center">1.50</td>
<td headers="conf.low" class="gt_row gt_center">1.11, 2.03</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Master's degree</td>
<td headers="estimate" class="gt_row gt_center">2.62</td>
<td headers="conf.low" class="gt_row gt_center">1.92, 3.57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Doctorate or terminal degree</td>
<td headers="estimate" class="gt_row gt_center">1.67</td>
<td headers="conf.low" class="gt_row gt_center">1.12, 2.51</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.60</td>
<td headers="conf.low" class="gt_row gt_center">1.09, 2.36</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HOWNSHP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rent</td>
<td headers="estimate" class="gt_row gt_center">0.73</td>
<td headers="conf.low" class="gt_row gt_center">0.65, 0.82</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &lt; $25,000</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $25,000 - $49,999</td>
<td headers="estimate" class="gt_row gt_center">0.98</td>
<td headers="conf.low" class="gt_row gt_center">0.84, 1.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $50,000 - $74,999</td>
<td headers="estimate" class="gt_row gt_center">1.02</td>
<td headers="conf.low" class="gt_row gt_center">0.87, 1.21</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $75,000 - $99,999</td>
<td headers="estimate" class="gt_row gt_center">0.88</td>
<td headers="conf.low" class="gt_row gt_center">0.73, 1.05</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $100,000 - $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.54</td>
<td headers="conf.low" class="gt_row gt_center">1.27, 1.87</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt; $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.52</td>
<td headers="conf.low" class="gt_row gt_center">1.11, 2.08</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SOURCE</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - municipal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - rural water district</td>
<td headers="estimate" class="gt_row gt_center">1.09</td>
<td headers="conf.low" class="gt_row gt_center">0.95, 1.25</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private supply - well, river, pond, rainwater</td>
<td headers="estimate" class="gt_row gt_center">0.92</td>
<td headers="conf.low" class="gt_row gt_center">0.79, 1.06</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I don't know</td>
<td headers="estimate" class="gt_row gt_center">1.03</td>
<td headers="conf.low" class="gt_row gt_center">0.86, 1.22</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.16</td>
<td headers="conf.low" class="gt_row gt_center">0.05, 0.52</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

Next we explored predictors associated with changes in the probability
of trust ratings for drinking water utilities specifically. Here we fit
the proportional odds model to responses of trust level for drinking
water utilities with sex/gender, age category, race/ethnicity,
education, homeownership, income, primary tap water supply, and
experience with any of (1) odd taste, (2) odd smell, (3) discolored
water, or (4) cloudy water as predictors. Results are stored in target
`m3`

``` r
gtsummary::tbl_regression(targets::tar_read(m3), exponentiate = TRUE)
```

<div id="jwhlmfaldd" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jwhlmfaldd table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jwhlmfaldd thead, #jwhlmfaldd tbody, #jwhlmfaldd tfoot, #jwhlmfaldd tr, #jwhlmfaldd td, #jwhlmfaldd th {
  border-style: none;
}
&#10;#jwhlmfaldd p {
  margin: 0;
  padding: 0;
}
&#10;#jwhlmfaldd .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#jwhlmfaldd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jwhlmfaldd .gt_title {
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
&#10;#jwhlmfaldd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#jwhlmfaldd .gt_heading {
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
&#10;#jwhlmfaldd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jwhlmfaldd .gt_col_headings {
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
&#10;#jwhlmfaldd .gt_col_heading {
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
&#10;#jwhlmfaldd .gt_column_spanner_outer {
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
&#10;#jwhlmfaldd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jwhlmfaldd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jwhlmfaldd .gt_column_spanner {
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
&#10;#jwhlmfaldd .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jwhlmfaldd .gt_group_heading {
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
  text-align: left;
}
&#10;#jwhlmfaldd .gt_empty_group_heading {
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
&#10;#jwhlmfaldd .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jwhlmfaldd .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jwhlmfaldd .gt_row {
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
&#10;#jwhlmfaldd .gt_stub {
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
&#10;#jwhlmfaldd .gt_stub_row_group {
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
&#10;#jwhlmfaldd .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jwhlmfaldd .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jwhlmfaldd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jwhlmfaldd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jwhlmfaldd .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jwhlmfaldd .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jwhlmfaldd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jwhlmfaldd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jwhlmfaldd .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jwhlmfaldd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jwhlmfaldd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jwhlmfaldd .gt_footnotes {
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
&#10;#jwhlmfaldd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jwhlmfaldd .gt_sourcenotes {
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
&#10;#jwhlmfaldd .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jwhlmfaldd .gt_left {
  text-align: left;
}
&#10;#jwhlmfaldd .gt_center {
  text-align: center;
}
&#10;#jwhlmfaldd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jwhlmfaldd .gt_font_normal {
  font-weight: normal;
}
&#10;#jwhlmfaldd .gt_font_bold {
  font-weight: bold;
}
&#10;#jwhlmfaldd .gt_font_italic {
  font-style: italic;
}
&#10;#jwhlmfaldd .gt_super {
  font-size: 65%;
}
&#10;#jwhlmfaldd .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jwhlmfaldd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jwhlmfaldd .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jwhlmfaldd .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jwhlmfaldd .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jwhlmfaldd .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jwhlmfaldd .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jwhlmfaldd .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jwhlmfaldd div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">SEX</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate" class="gt_row gt_center">0.88</td>
<td headers="conf.low" class="gt_row gt_center">0.68, 1.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.19</td>
<td headers="conf.low" class="gt_row gt_center">0.01, 2.56</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18:24</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    25:34</td>
<td headers="estimate" class="gt_row gt_center">0.91</td>
<td headers="conf.low" class="gt_row gt_center">0.55, 1.49</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    35:44</td>
<td headers="estimate" class="gt_row gt_center">0.73</td>
<td headers="conf.low" class="gt_row gt_center">0.44, 1.21</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    45:54</td>
<td headers="estimate" class="gt_row gt_center">0.53</td>
<td headers="conf.low" class="gt_row gt_center">0.30, 0.93</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55:64</td>
<td headers="estimate" class="gt_row gt_center">0.85</td>
<td headers="conf.low" class="gt_row gt_center">0.49, 1.48</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    65+</td>
<td headers="estimate" class="gt_row gt_center">0.71</td>
<td headers="conf.low" class="gt_row gt_center">0.42, 1.20</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RACE2</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-white</td>
<td headers="estimate" class="gt_row gt_center">0.86</td>
<td headers="conf.low" class="gt_row gt_center">0.64, 1.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHL</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Some high school</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High school graduate or GED</td>
<td headers="estimate" class="gt_row gt_center">1.92</td>
<td headers="conf.low" class="gt_row gt_center">0.93, 3.98</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Associate degree</td>
<td headers="estimate" class="gt_row gt_center">1.71</td>
<td headers="conf.low" class="gt_row gt_center">0.80, 3.70</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Bachelor's degree</td>
<td headers="estimate" class="gt_row gt_center">2.19</td>
<td headers="conf.low" class="gt_row gt_center">1.02, 4.68</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Master's degree</td>
<td headers="estimate" class="gt_row gt_center">3.34</td>
<td headers="conf.low" class="gt_row gt_center">1.49, 7.49</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Doctorate or terminal degree</td>
<td headers="estimate" class="gt_row gt_center">2.27</td>
<td headers="conf.low" class="gt_row gt_center">0.76, 6.82</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.81</td>
<td headers="conf.low" class="gt_row gt_center">0.70, 4.73</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HOWNSHP</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Own</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Rent</td>
<td headers="estimate" class="gt_row gt_center">0.69</td>
<td headers="conf.low" class="gt_row gt_center">0.53, 0.90</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &lt; $25,000</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $25,000 - $49,999</td>
<td headers="estimate" class="gt_row gt_center">0.79</td>
<td headers="conf.low" class="gt_row gt_center">0.55, 1.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $50,000 - $74,999</td>
<td headers="estimate" class="gt_row gt_center">0.86</td>
<td headers="conf.low" class="gt_row gt_center">0.58, 1.29</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $75,000 - $99,999</td>
<td headers="estimate" class="gt_row gt_center">0.70</td>
<td headers="conf.low" class="gt_row gt_center">0.44, 1.11</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    $100,000 - $200,000</td>
<td headers="estimate" class="gt_row gt_center">1.53</td>
<td headers="conf.low" class="gt_row gt_center">0.98, 2.39</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    &gt; $200,000</td>
<td headers="estimate" class="gt_row gt_center">0.87</td>
<td headers="conf.low" class="gt_row gt_center">0.40, 1.90</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - municipal</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public supply - rural water district</td>
<td headers="estimate" class="gt_row gt_center">1.07</td>
<td headers="conf.low" class="gt_row gt_center">0.77, 1.49</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private supply - well, river, pond, rainwater</td>
<td headers="estimate" class="gt_row gt_center">0.76</td>
<td headers="conf.low" class="gt_row gt_center">0.55, 1.06</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I don't know</td>
<td headers="estimate" class="gt_row gt_center">0.88</td>
<td headers="conf.low" class="gt_row gt_center">0.57, 1.37</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q7</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="estimate" class="gt_row gt_center">0.73</td>
<td headers="conf.low" class="gt_row gt_center">0.57, 0.93</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>

### RQ3

We used a weighted multinomial regression model to explore the effects
of predictor variables on choices in primary drinking water source
(unfiltered tap water, filtered tap water, and bottled water).
Demographic variables, tap water sources, and experience with
taste/odor/color issues were the independent variables. Results are
stored in target `m2`:

``` r
m2 <- targets::tar_read(m2)
gtsummary::tbl_regression(m2, exponentiate = TRUE)
```

    ## ! `broom::tidy()` failed to tidy the model.

    ## ✔ `tidy_parameters()` used instead.

    ## ℹ Add `tidy_fun = broom.helpers::tidy_parameters` to quiet these messages.

    ## ✖ Unable to identify the list of variables.
    ## 
    ## This is usually due to an error calling `stats::model.frame(x)`or `stats::model.matrix(x)`.
    ## It could be the case if that type of model does not implement these methods.
    ## Rarely, this error may occur if the model object was created within
    ## a functional programming framework (e.g. using `lappy()`, `purrr::map()`, etc.).

<div id="uhqnjbbfgn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uhqnjbbfgn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#uhqnjbbfgn thead, #uhqnjbbfgn tbody, #uhqnjbbfgn tfoot, #uhqnjbbfgn tr, #uhqnjbbfgn td, #uhqnjbbfgn th {
  border-style: none;
}
&#10;#uhqnjbbfgn p {
  margin: 0;
  padding: 0;
}
&#10;#uhqnjbbfgn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#uhqnjbbfgn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#uhqnjbbfgn .gt_title {
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
&#10;#uhqnjbbfgn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#uhqnjbbfgn .gt_heading {
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
&#10;#uhqnjbbfgn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uhqnjbbfgn .gt_col_headings {
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
&#10;#uhqnjbbfgn .gt_col_heading {
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
&#10;#uhqnjbbfgn .gt_column_spanner_outer {
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
&#10;#uhqnjbbfgn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#uhqnjbbfgn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#uhqnjbbfgn .gt_column_spanner {
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
&#10;#uhqnjbbfgn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#uhqnjbbfgn .gt_group_heading {
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
  text-align: left;
}
&#10;#uhqnjbbfgn .gt_empty_group_heading {
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
&#10;#uhqnjbbfgn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#uhqnjbbfgn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#uhqnjbbfgn .gt_row {
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
&#10;#uhqnjbbfgn .gt_stub {
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
&#10;#uhqnjbbfgn .gt_stub_row_group {
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
&#10;#uhqnjbbfgn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#uhqnjbbfgn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#uhqnjbbfgn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uhqnjbbfgn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#uhqnjbbfgn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#uhqnjbbfgn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uhqnjbbfgn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uhqnjbbfgn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#uhqnjbbfgn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#uhqnjbbfgn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#uhqnjbbfgn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uhqnjbbfgn .gt_footnotes {
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
&#10;#uhqnjbbfgn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uhqnjbbfgn .gt_sourcenotes {
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
&#10;#uhqnjbbfgn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uhqnjbbfgn .gt_left {
  text-align: left;
}
&#10;#uhqnjbbfgn .gt_center {
  text-align: center;
}
&#10;#uhqnjbbfgn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#uhqnjbbfgn .gt_font_normal {
  font-weight: normal;
}
&#10;#uhqnjbbfgn .gt_font_bold {
  font-weight: bold;
}
&#10;#uhqnjbbfgn .gt_font_italic {
  font-style: italic;
}
&#10;#uhqnjbbfgn .gt_super {
  font-size: 65%;
}
&#10;#uhqnjbbfgn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#uhqnjbbfgn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#uhqnjbbfgn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#uhqnjbbfgn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#uhqnjbbfgn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#uhqnjbbfgn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#uhqnjbbfgn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#uhqnjbbfgn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#uhqnjbbfgn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>exp(Beta)</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">(Intercept):1</td>
<td headers="estimate" class="gt_row gt_center">0.70</td>
<td headers="conf.low" class="gt_row gt_center">0.21, 2.31</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">(Intercept):2</td>
<td headers="estimate" class="gt_row gt_center">0.50</td>
<td headers="conf.low" class="gt_row gt_center">0.16, 1.58</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SEXFemale:1</td>
<td headers="estimate" class="gt_row gt_center">1.06</td>
<td headers="conf.low" class="gt_row gt_center">0.73, 1.54</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SEXFemale:2</td>
<td headers="estimate" class="gt_row gt_center">1.49</td>
<td headers="conf.low" class="gt_row gt_center">1.02, 2.17</td>
<td headers="p.value" class="gt_row gt_center">0.038</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SEXOther:1</td>
<td headers="estimate" class="gt_row gt_center">0.22</td>
<td headers="conf.low" class="gt_row gt_center">0.01, 4.17</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SEXOther:2</td>
<td headers="estimate" class="gt_row gt_center">0.51</td>
<td headers="conf.low" class="gt_row gt_center">0.04, 6.17</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP25:34:1</td>
<td headers="estimate" class="gt_row gt_center">0.95</td>
<td headers="conf.low" class="gt_row gt_center">0.42, 2.13</td>
<td headers="p.value" class="gt_row gt_center">0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP25:34:2</td>
<td headers="estimate" class="gt_row gt_center">1.22</td>
<td headers="conf.low" class="gt_row gt_center">0.53, 2.81</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP35:44:1</td>
<td headers="estimate" class="gt_row gt_center">0.52</td>
<td headers="conf.low" class="gt_row gt_center">0.24, 1.14</td>
<td headers="p.value" class="gt_row gt_center">0.10</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP35:44:2</td>
<td headers="estimate" class="gt_row gt_center">0.96</td>
<td headers="conf.low" class="gt_row gt_center">0.43, 2.13</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP45:54:1</td>
<td headers="estimate" class="gt_row gt_center">0.29</td>
<td headers="conf.low" class="gt_row gt_center">0.13, 0.64</td>
<td headers="p.value" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP45:54:2</td>
<td headers="estimate" class="gt_row gt_center">0.69</td>
<td headers="conf.low" class="gt_row gt_center">0.32, 1.50</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP55:64:1</td>
<td headers="estimate" class="gt_row gt_center">0.29</td>
<td headers="conf.low" class="gt_row gt_center">0.13, 0.65</td>
<td headers="p.value" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP55:64:2</td>
<td headers="estimate" class="gt_row gt_center">0.58</td>
<td headers="conf.low" class="gt_row gt_center">0.25, 1.33</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP65+:1</td>
<td headers="estimate" class="gt_row gt_center">0.28</td>
<td headers="conf.low" class="gt_row gt_center">0.13, 0.62</td>
<td headers="p.value" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">AGEP65+:2</td>
<td headers="estimate" class="gt_row gt_center">0.74</td>
<td headers="conf.low" class="gt_row gt_center">0.33, 1.69</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RACE2Non-white:1</td>
<td headers="estimate" class="gt_row gt_center">1.17</td>
<td headers="conf.low" class="gt_row gt_center">0.74, 1.85</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RACE2Non-white:2</td>
<td headers="estimate" class="gt_row gt_center">1.59</td>
<td headers="conf.low" class="gt_row gt_center">1.01, 2.52</td>
<td headers="p.value" class="gt_row gt_center">0.046</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLHigh school graduate or GED:1</td>
<td headers="estimate" class="gt_row gt_center">2.37</td>
<td headers="conf.low" class="gt_row gt_center">0.93, 6.03</td>
<td headers="p.value" class="gt_row gt_center">0.069</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLHigh school graduate or GED:2</td>
<td headers="estimate" class="gt_row gt_center">1.08</td>
<td headers="conf.low" class="gt_row gt_center">0.50, 2.37</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLAssociate degree:1</td>
<td headers="estimate" class="gt_row gt_center">1.46</td>
<td headers="conf.low" class="gt_row gt_center">0.54, 3.94</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLAssociate degree:2</td>
<td headers="estimate" class="gt_row gt_center">0.70</td>
<td headers="conf.low" class="gt_row gt_center">0.30, 1.64</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLBachelor's degree:1</td>
<td headers="estimate" class="gt_row gt_center">4.31</td>
<td headers="conf.low" class="gt_row gt_center">1.59, 11.7</td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLBachelor's degree:2</td>
<td headers="estimate" class="gt_row gt_center">1.24</td>
<td headers="conf.low" class="gt_row gt_center">0.51, 3.01</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLMaster's degree:1</td>
<td headers="estimate" class="gt_row gt_center">3.06</td>
<td headers="conf.low" class="gt_row gt_center">1.09, 8.59</td>
<td headers="p.value" class="gt_row gt_center">0.034</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLMaster's degree:2</td>
<td headers="estimate" class="gt_row gt_center">1.07</td>
<td headers="conf.low" class="gt_row gt_center">0.41, 2.76</td>
<td headers="p.value" class="gt_row gt_center">0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLDoctorate or terminal degree:1</td>
<td headers="estimate" class="gt_row gt_center">2.81</td>
<td headers="conf.low" class="gt_row gt_center">0.72, 11.0</td>
<td headers="p.value" class="gt_row gt_center">0.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLDoctorate or terminal degree:2</td>
<td headers="estimate" class="gt_row gt_center">0.63</td>
<td headers="conf.low" class="gt_row gt_center">0.16, 2.50</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLOther:1</td>
<td headers="estimate" class="gt_row gt_center">1.23</td>
<td headers="conf.low" class="gt_row gt_center">0.33, 4.56</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SCHLOther:2</td>
<td headers="estimate" class="gt_row gt_center">1.57</td>
<td headers="conf.low" class="gt_row gt_center">0.53, 4.67</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HOWNSHPRent:1</td>
<td headers="estimate" class="gt_row gt_center">1.28</td>
<td headers="conf.low" class="gt_row gt_center">0.86, 1.91</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HOWNSHPRent:2</td>
<td headers="estimate" class="gt_row gt_center">1.64</td>
<td headers="conf.low" class="gt_row gt_center">1.07, 2.50</td>
<td headers="p.value" class="gt_row gt_center">0.022</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$25,000 - $49,999:1</td>
<td headers="estimate" class="gt_row gt_center">1.35</td>
<td headers="conf.low" class="gt_row gt_center">0.79, 2.30</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$25,000 - $49,999:2</td>
<td headers="estimate" class="gt_row gt_center">1.22</td>
<td headers="conf.low" class="gt_row gt_center">0.72, 2.07</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$50,000 - $74,999:1</td>
<td headers="estimate" class="gt_row gt_center">2.20</td>
<td headers="conf.low" class="gt_row gt_center">1.23, 3.93</td>
<td headers="p.value" class="gt_row gt_center">0.008</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$50,000 - $74,999:2</td>
<td headers="estimate" class="gt_row gt_center">1.56</td>
<td headers="conf.low" class="gt_row gt_center">0.86, 2.83</td>
<td headers="p.value" class="gt_row gt_center">0.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$75,000 - $99,999:1</td>
<td headers="estimate" class="gt_row gt_center">1.83</td>
<td headers="conf.low" class="gt_row gt_center">0.96, 3.50</td>
<td headers="p.value" class="gt_row gt_center">0.068</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$75,000 - $99,999:2</td>
<td headers="estimate" class="gt_row gt_center">1.66</td>
<td headers="conf.low" class="gt_row gt_center">0.84, 3.27</td>
<td headers="p.value" class="gt_row gt_center">0.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$100,000 - $200,000:1</td>
<td headers="estimate" class="gt_row gt_center">1.52</td>
<td headers="conf.low" class="gt_row gt_center">0.77, 3.00</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME$100,000 - $200,000:2</td>
<td headers="estimate" class="gt_row gt_center">0.93</td>
<td headers="conf.low" class="gt_row gt_center">0.45, 1.90</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME&gt; $200,000:1</td>
<td headers="estimate" class="gt_row gt_center">2.73</td>
<td headers="conf.low" class="gt_row gt_center">0.94, 7.97</td>
<td headers="p.value" class="gt_row gt_center">0.066</td></tr>
    <tr><td headers="label" class="gt_row gt_left">INCOME&gt; $200,000:2</td>
<td headers="estimate" class="gt_row gt_center">2.51</td>
<td headers="conf.low" class="gt_row gt_center">0.78, 8.08</td>
<td headers="p.value" class="gt_row gt_center">0.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5Public supply - rural water district:1</td>
<td headers="estimate" class="gt_row gt_center">1.37</td>
<td headers="conf.low" class="gt_row gt_center">0.82, 2.28</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5Public supply - rural water district:2</td>
<td headers="estimate" class="gt_row gt_center">1.25</td>
<td headers="conf.low" class="gt_row gt_center">0.74, 2.11</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5Private supply - well, river, pond, rainwater:1</td>
<td headers="estimate" class="gt_row gt_center">0.83</td>
<td headers="conf.low" class="gt_row gt_center">0.50, 1.39</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5Private supply - well, river, pond, rainwater:2</td>
<td headers="estimate" class="gt_row gt_center">0.80</td>
<td headers="conf.low" class="gt_row gt_center">0.47, 1.38</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5I don't know:1</td>
<td headers="estimate" class="gt_row gt_center">1.16</td>
<td headers="conf.low" class="gt_row gt_center">0.60, 2.26</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q5I don't know:2</td>
<td headers="estimate" class="gt_row gt_center">1.60</td>
<td headers="conf.low" class="gt_row gt_center">0.85, 3.00</td>
<td headers="p.value" class="gt_row gt_center">0.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q7Yes:1</td>
<td headers="estimate" class="gt_row gt_center">1.07</td>
<td headers="conf.low" class="gt_row gt_center">0.75, 1.52</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Q7Yes:2</td>
<td headers="estimate" class="gt_row gt_center">1.40</td>
<td headers="conf.low" class="gt_row gt_center">0.96, 2.04</td>
<td headers="p.value" class="gt_row gt_center">0.081</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
