# From Data to Manuscript in R

Winter 2022: CHDV 20550/30550, MACS 30550, MAPS 30550, PSYC 30550

Tuesdays and Thursdays 09.30-10.50 Chicago time

Office hours: Tuesdays 11.00–12.30 in Rosenwald 318A or by appointment

**Instructor:** Marisa Casillas (mcasillas@uchicago.edu)

This course tackles the basic skills needed to build an integrated research report with the R programming language. We will cover every step from data to manuscript including: Using R's libraries to clean up and re-format messy datasets, preparing data sets for analysis, running statistical tools, generating clear and attractive figures and tables, and knitting those bits of code together with your manuscript writing. The result will be a reproducible, open-science friendly report that you can easily update after finishing data collection or receiving comments from readers. Never copy-paste your way through a table again! The R universe is large, so this course will focus specifically on: The core R libraries, the tidyverse library, and R Markdown. Students will also learn about the use of GitHub for version control.
 
**Course materials:**

* Most required readings will be drawn from one of the following great (and free! and online!) resources:
	* [R for Data Science](https://r4ds.had.co.nz/) by Hadley Wickham.
	* [R Markdown for Scientists](https://rmd4sci.njtierney.com/) by Nicholas Tierney
* Also check out [R Markdown: The Definitive Guide](https://bookdown.org/yihui/rmarkdown/) by Yihui Xie, J. J. Allaire, and Garrett Grolemund

Any other course materials will be available online on Canvas and the ChatterLab course webpage.

----

# Grading

Students enrolled in this course will be graded on the following basis:


| Component | Undergraduates |
|-----------|----------------|
| Mini assignments | 60% |
| Participation | 20% |
| Scientific report | 20% |


### Mini assignments

There is a mini assignment for each lecture. Each assignment will involve working with your own data, data provided for that assignment, or both and is designed to give you hands-on practice with using R. Assignments typically begin in class and may be completed after class (each one is highlighted in <span style="color:orange">orange</span>) on a pass/fail basis. Students must push their completed assignment to the appropriate GitHub repository, tagging Dr. Casillas with the assignment number so that she receives a notification.

_When are the assignments due?_
Assignments listed as an 'in-class activity' are due before the start of the next class. Assignments listed as 'pre-class preparation' are due before that class begins.

_Need help?_ We will do much of this work in class, so ask your questions while we're together. Each student will be assigned to a support group of other students to help answer questions (in addition to access to help from Dr. Casillas during office hours). Finally, you're strongly encouraged to start developing your search skills when it comes to debugging your code (look for advice and solutions from others who have faced similar problems on sites like stackoverflow and github)!

### Participation

Students are generally expected to come to class prepared to practice using R together. That means: come with your charged laptop, prepared with any data you are using and any installed software that is required (see the 'preparation' note for each class). Be prepared to share your screen with your support group and with Dr. Casillas. That said, please stay in touch about your limitations regarding in-person participation; Please do not come to class if you are feeling at all ill. Students earn participation credit through their attendance and participation in their support group and discussion of course content in office hours.

### Scientific report

Your final assignment will be a scientific report in R Markdown that: is developed via a GitHub repository; includes data read-in, pre-processing, and analysis; includes at least one figure and one table with captions and in-text references; includes citations; and includes at least 1500 words of the manuscript in at least four subsections (Introductions, Methods, Results, Discussion; unless otherwise agreed with Dr. Casillas).  **Each student must come to office hours to negotiate their manuscript plan with Dr. Casillas by the end of Week 7. Grading will be contingent on the completion of that agreed-upon plan for each individual student.**

----

# Course schedule


## Module 1: GitHub basics

### Tuesday, 04 January 2022 (1.1): Part 1

#### Pre-class preparation
None!

#### In-class activities
* Presentation: Syllabus and course goals (and non-goals)
* Presentation: Introduction to Git and GitHub
* Laptops out:
    * Support groups
    * <span style="color:orange">Creating a repo, sharing it, creating a .gitignore, and making your first commit</span>
    * [Request an education premium account](https://education.github.com/) to get free private repositories

### Thursday, 06 January 2022 (1.2): Part 2

#### Pre-class preparation
* Read the [dataset selection guidelines](./course_instructions/instructions-data_selection_guidelines.md) for the class <!--To do-->
* Select a dataset for you to use in the class. Contact Dr. Casillas ASAP if you are unsure which to choose.
 	
#### In-class activities
* Share: What dataset will you be working with?
* Laptops out:
    * <span style="color:orange">Push a README briefly describing the study/dataset you will be working with. Imagine this from the perpsective of someone visiting your repository who hopes to review your code and work with your data. **NOTE: If you intend to make this repository public, never push any private data. It will be stored in your history even if you delete it!** Make at least three commits during this process.</span>
    * Manage repository settings

## Module 2: R and tidyverse basics

### Tuesday, 11 January 2022 (2.1): Part 1

#### Pre-class preparation
* All students: Confirm that your dataset is okay for the course with Dr. Casillas (note: this should be done via email, not GitHub)
* Install R and RStudio (if you're unsure how to do this, check these Appendices [A](https://rstudio-education.github.io/hopr/starting.html) and [B]() from Garrett Grolemund's _Hands-On Programming with R_)
* Read about [R basics (Ch 2)](https://intro2r.com/basics_r.html), [data types (Ch 3)](https://intro2r.com/data_r.html), and [programming basics (especially 7.3, 7.4, and 7.5)](https://intro2r.com/prog_r.html) in Douglas et al.'s _An Introduction to R_; if you are brand new to R, try to follow along with the example code in your RStudio console

#### In-class activities
* Presentation: R, RStudio, packages, and a first glimpse at tidyverse
* Laptops out:
    * Confirm/troubleshoot installations
    * <span style="color:orange">Hello world demo and code commenting</span>

### Thursday, 13 January 2022 (2.2): Part 2

#### Pre-class preparation
* Install the `tidyverse` package in RStudio (overview [here](https://tidyverse.tidyverse.org/))
* <span style="color:orange">Prepare and push an anonymous version of your dataset for use in class</span>
* Read about [tibbles](https://r4ds.had.co.nz/tibbles.html), [data import](https://r4ds.had.co.nz/data-import.html), and [pipes](https://r4ds.had.co.nz/pipes.html) in Hadley Wickham's _R for Data Science_

#### In-class activities
* Presentation: Data tables, data classes, functions, pipes, tibbles, and readr basics
* Laptops out:
    * Read in and write out demo
    * <span style="color:orange">Read in and write out your own dataset; troubleshoot in support groups</span>

### Tuesday, 18 January 2022 (3.1): Part 3

#### Pre-class preparation
* Read about [dplyr](https://r4ds.had.co.nz/transform.html) and [tidyr](https://r4ds.had.co.nz/tidy-data.html) in Hadley Wickham's _R for Data Science_

#### In-class activities
* Presentation: Basic data manipulation and preparation functions
* Laptops out:
    * dplyr and tidyr demo
    * <span style="color:orange">Create a mockup of your desired data format (push as a spreadsheet file and describe in the README)</span>

### Thursday, 20 January 2022 (3.2): Part 4

#### Pre-class preparation
* Review the [cheatsheets](https://www.rstudio.com/resources/cheatsheets/) for tibble, readr, dplyr, and tidyr
* Prepare any supplementary data you need for your desired format

#### In-class activities
* Laptops out:
    * <span style="color:orange">Data preparation demo</span>
    * Plan how you will prep your own dataset

### Tuesday, 25 January 2022 (4.1): Part 5

#### Pre-class preparation
* Continue planning how you will prep your dataset

#### In-class activities
* Laptops out:
    * <span style="color:orange">Implement your solution; troubleshoot in support groups</span>

## Module 3: Plotting your data

### Thursday, 27 January 2022 (4.2): Part 1

#### Pre-class preparation
* Read about [ggplot](https://r4ds.had.co.nz/data-visualisation.html) in Hadley Wickham's _R for Data Science_
* Finish implementing your data preparation if you didn't manage to complete that in class

#### In-class activities
* Presentation: Introduction to ggplot
* Laptops out:
    * ggplot one-variable demo
    * group exercise: recreate this plot

### Tuesday, 01 February 2022 (5.1): Part 2

#### Pre-class preparation
* Imagine two plots you would like to see of your data

#### In-class activities
* Presentation: Plotting multiple variables at once
* Laptops out:
    * ggplot two- and three-variable demo
    * <span style="color:orange">group exercise: recreate this plot</span>

### Thursday, 03 February 2022 (5.2): Part 3

#### Pre-class preparation
* Create initial drafts of your planned plots

#### In-class activities
* Presentation: Customizing: Axes, labels, and facets
* Laptops out:
    * ggplot customization demo
    * group exercise: recreate this plot

### Tuesday, 08 February 2022 (6.1): Part 4

#### Pre-class preparation
* Adjust the initial drafts of your planned plots
* Review the [cheatsheet](https://www.rstudio.com/resources/cheatsheets/) for ggplot
* Brainstorm what else you would like to change about the figures and how you could implement that change

#### In-class activities
* Presentation: Customizing: colors, shapes, and themes
* Laptops out:
    * <span style="color:orange">ggplot customization demo</span>
    * group exercise: recreate this plot

## Module 4: R Markdown and Papaja basics
("papaja" is a package for "**P**reparing **APA** **J**ournal **A**rticles")

### Thursday, 10 February 2022 (6.2):  Part 1

#### Pre-class preparation
* <span style="color:orange">Go through your existing code and make sure you have informative comments</span>
* Install the `papaja` package in Rstudio (follow the instructions [here](https://github.com/crsh/papaja))
* Read about the [basics of RMarkdown](https://r4ds.had.co.nz/r-markdown.html) in Hadley Wickham's _R for Data Science_

#### In-class activities
* Presentation: Introduction to RMarkdown, code chunks, formatting, and knitting
* Laptops out:
    * create and compare rmd files
    * insert comments into the code chunks and in the text

### Tuesday, 15 February 2022 (7.1): Part 2

#### Pre-class preparation
* Review markdown syntax for text formatting (see the rightmost column on [this cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/rmarkdown.pdf))

#### In-class activities
* Presentation: Introduction to Papaja
* Laptops out:
    * Initialize a papaja .rmd file
    * <span style="color:orange">Insert your author information, libraries, and read-in, prep, and plotting code chunks</span>
    * <span style="color:orange">Insert a footnote</span>

### Thursday, 17 February 2022 (7.2): Part 3

#### Pre-class preparation
**REMINDER: You must meet with Marisa by the end of this week (i.e., before Friday, February 18th at 17.00 Chicago time) to negotiate your plan for the scientific report assignment, which is due on Tuesday of Finals Week**

#### In-class activities
* Presentation: Tables, figures, images, captions, and chunk references
* Laptops out:
    * group demo based on chapters [9](https://rmd4sci.njtierney.com/figures-tables-captions-.html), [10](https://rmd4sci.njtierney.com/customising-your-figures.html), and [12](https://rmd4sci.njtierney.com/start.html) of Nicholas Tierney's _RMarkdown for Scientists_
    * <span style="color:orange">Add one table, one figure, and one image to your rmd, each with captions and a chunk reference in the text.</span>

### Tuesday, 22 February 2022 (8.1): Part 4

#### Pre-class preparation
* <span style="color:orange">Insert real text/[lorem ipsum](https://www.lipsum.com/)-style text/outline into your rmd, including references to your code chunks, and see whether it knits to the desired style</span>
* <span style="color:orange">Brainstorm what else you would like to change about report and how you could implement that change; add as a commented-out 'to-do' list in your document</span>

#### In-class activities
* Presentation: Sourcing code and organizing rmds for others' eyes
* Laptops out:
    * Brainstorm with group about how to organize code
    * Come up with a plan for reorganizing your code and adding comments to maximize re-use potential (even if this is just for your future self)

## Module 5: Bibtex and in-line references

### Thursday, 24 February 2022 (8.2): Part 1

#### Pre-class preparation
* Implement your code reorganization from the last class; ensure that it knits

#### In-class activities
* Presentation: BibTeX part 1
* Laptops out:
    * <span style="color:orange">Create a .bib file with your references</span>
    * <span style="color:orange">Start implementing in-text references in your rmd</span>

### Tuesday, 01 March 2022 (9.1): Part 2

#### Pre-class preparation
* Review [rmarkdown citation](https://rmd4sci.njtierney.com/citing-articles-bibliography-styles.html) in Nicholas Tierney's _RMarkdown for Scientists_

#### In-class activities
* Presentation: BibTeX part 2
* Laptops out:
    * Citation styles demo
    * Add non-cited references
    * <span style="color:orange">Finish implementing in-text references in your rmd</span>

### Thursday, 03 March 2022 (9.2): Part 3

#### Pre-class preparation
None!

#### In-class activities
* Presentation: In-line code
* Laptops out:
	* in-line reference addition demo
	* <span style="color:orange">add some in-line references to your rmd</span>
* Group share: Current .rmd, goals for final version, and troubleshooting

## Final deadline (Tuesday, 08 March 2022)
Scientific report due


# Contact
**Email:** mcasillas@uchicago.edu

**Office:** Rosenwald 318A

**Office hours:** TBA

_If you require any accommodations for this course, as soon as possible please provide your instructor with a copy of your Accommodation Determination Letter (provided to you by the Student Disability Services office) so that you may discuss with him/her how your accommodations may be implemented in this course.The University of Chicago is committed to ensuring the full participation of all students in its programs. If you have a documented disability (or think you may have a disability) and, as a result, need a reasonable accommodation to participate in class, complete course requirements, or benefit from the University's programs or services, you are encouraged to contact Student Disability Services as soon as possible. To receive reasonable accommodation, you must be appropriately registered with Student Disability Services.  Please contact the office at 773-834-4469/TTY 773-795-1186 or gmoorehead@uchicago.edu, or visit the website at disabilities.uchicago.edu.  Student Disability Services is located in Room 233 in the Administration Building located at 5801 S. Ellis Avenue._
