# The Impact of Elicitation and Contrasting Narratives on Engagement, Recall and Attitude Change with News Articles Containing Data Visualization
### Milad Rogha , Subham Sah, Alireza Karduni , Douglas Markant, and Wenwen Dou

### Abstract
News articles containing data visualizations play an important role in informing the public on issues ranging from public health to politics. 
Recent research on the persuasive appeal of data visualizations suggests that prior attitudes can be notoriously difficult to change. 
Inspired by an NYT article, we designed two experiments to evaluate the impact of elicitation and contrasting narratives on attitude change, recall, and engagement.
We hypothesized that eliciting prior beliefs leads to more elaborative thinking that ultimately results in higher attitude change, better recall, and engagement. Our findings revealed that visual elicitation leads to higher engagement in terms of feelings of surprise. While there is an overall attitude change across all experiment conditions, we did not observe a significant effect of belief elicitation on attitude change. With regard to recall error, while participants in the draw trend elicitation exhibited significantly lower recall error than participants in the categorize trend condition, we found no significant difference in recall error when comparing elicitation conditions to no elicitation. In a follow-up study, we added contrasting narratives with the purpose of making the main visualization (communicating data on the focal issue) appear strikingly different. Compared to the results of study 1, we found that contrasting narratives improved engagement in terms of surprise and interest but interestingly resulted in higher recall error and no significant change in attitude. We discuss the effects of elicitation and contrasting narratives in the context of topic involvement and the strengths of temporal trends encoded in the data visualization.


Please find the pre-print here: [Link to the pre-print](https://arxiv.org/abs/2401.05511)


## Replicability Instructions
In order to reproduce the analysis depicted in the paper, please follow the steps below. Here we provide a step-by-step instructions to reproduce one of the figures in the paper (Figure 8.)

### 1. Install R and R Studio
All analyses were conducted in R version 4.2.3. Download and Install R and R Studio from [RStudio Desktop](https://posit.co/download/rstudio-desktop/) .

### 2. Load the Project file
In the R studio go to file \-> Open project... \-> 
select and open the file `Data and Analysis.Rproj` from `1_analysis_and_results/R_and_Py/Data and Analysis.Rproj`

### 3. Run the scripts

Scripts are located in `1_analysis_and_results/R_and_Py/analysis/`

You can run `trendelicitation_study1.R` or `trendelicitation_study2.R` in any order.
However, to be able to run `trendelicitation_combined.R`, you need to run the previous scripts first.


**Note**: When openning each script, there may be packages missing. In such cases, a notification appears on the top of the screen, prompting to install the missing packages. Simply click on the Install missing packages link and the program automatically downloads the missing packages.

### 4. Plots

After running each script the results and plots appear in their respective windows.