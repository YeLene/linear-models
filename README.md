## Linear models

 This project contains two examples of application of the linear modeling approach for statistical analysis of data:  
 
- [Analysis of the dependance of the protein content in milk on the cow's diet using Mixed Liner Models](mixed_linear_models/Cows-milk-project-eng.md)

  *I investigated how the protein content in milk correlates on the cow's diet, taking into account the time after calving, using data from Milk dataset (Diggle et al. 1994) from the nlme package, where the cattle are grouped according to whether they are fed a diet with barley alone, with barley and lupins, or with lupins alone.*
  
  *According to the obtained results, the protein content in milk correlates with the cow's diet and on the time passed after calving (Akaike information criterion test). For cows on a mixed diet (barley + lupins) and for cows that ate only lupins, the protein content in milk was lower by 0.094 and 0.198 percentage points respectively, than for cows that were fed only with barley. At the same time, the more time passed after calving, the less protein the cow’s milk contained (Akaike information criterion test).*

- [Analysis of the correlation of the probability of death from a heart attack with a set of factors using Generalized Linear Models](generalized_linear_models/Heart-attack-deaths-project-eng.md)

    *This study aimed to identify patient characteristics (AgeGroup, Delay, Severity, Region) that might be predictive for death from heart attack. The "heart" dataset ( glm2 package) that holds data on 16949 people who experienced a heart attack and is obtained from a double blind randomised clinical trial, was used.*
   
   *The analysis revealed statistically significant interactions between the age group factor and    how soon after a heart attack treatment was obtained (likelihood ratio test, p = 0.001).     Similarly, between the age group factor and the region where the patient was treated (p = 0.03).*
   
   *The most significant differences were observed between the following combinations of factors of age group and time of treatment after a heart attack:*

  *- The prognosis of survival for patients from the 65-75 age category who were treated within 2-4 hours after the attack was significantly less favorable than for patients younger than 65 years who
waited for help for more than 4 hours - 6.6 times less favourable (Tukey’s test, p < $10^{-10}$); less than 2 hours - 3.4 times less favourable (p <  $10^{-5}$); 2-4 hours - 3.6 times less favourable (p <  $10^{-5}$). Also, such patients died 2.7 times more often than patients older than
75 years, but who were treated in the first 2 hours (p <  $10^{-4}$) and 2.9 times more often if they waited 4 hours (p <  $10^{-5}$);*

  *- If patients from the 65-75 age group were treated no later than 2 hours after the attack, the survival rate increased 2.3 times compared to the same age patients who waited for treatment 2-4 hours (Tukey’s test, p <  $10^{-10}$);*

  *- Older than 75 years old patients, whose treatment started after 2-4 hours, died 4 times more often than patients younger than 65 years with a delay in treatment of more than 4 hours (Tukey’s test, p <  $10^{-4}$)*

  *- Patients aged 65-75, whose treatment started later than after 4 hours, died 3 times more often than patients younger than 65 with a similar delay in treatment (Tukey’s test, p = 0.002);*

  *- Patients younger than 65 with a delay in treatment of more than 4 hours, survived 3 times more often than patients aged 65-75 with a similar delay (Tukey’s test, p = 0.002), and a delay of less than 2 hours (p = 0.003).*
